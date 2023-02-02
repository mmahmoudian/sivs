#' Stable Iterative Variable Selection
#'
#' @description The name is an acronym for Stable Iterative Variable Selection.
#' This function will iteratively run a machine learning method that can
#' incorporate a shrinkage method using multiple random seeds in order to find
#' the smallest set of features that can robustly be predictive.
#'
#' @param x The input data. Each row should represent a sample and each column should represent a feature.
#' @param y Response variable. It should be of class factor for classification and of class Surv for survival.
#' @param test.ratio How much of the data should be cut and used for testing
#' @param method The internal machine learning method to be used
#' @param iter.count How many iterations should the function go through
#' @param nfolds How many folds should the training cross-validations have
#' @param sample.grouping A character, numeric or factor vector to specify how the samples should be grouped/bundled together in the cross-validation binning. If set to NULL the grouping will be skipped. Samples with the same value will always be kept together in the same bins in cross-validation. This is especially useful when having multiple samples from the same individual. Default is NULL.
#' @param parallel.cores How many cores should be used in the iterative process. The value should be the number of threads in numeric form, or any of these values: "max", "grace", FALSE, NULL. If set to "max", all cores will be used and in large datasets you might face your computer struggling and ultimately errors. If set to "grace", one core will be left out so that it can be used by other processes in the machine. If set to NULL of FALSE, the code will run sequentially and without the parallel backend.
#' @param progressbar Logical. If the progressbar should be shown. Default is TRUE.
#' @param verbose Character. How detailed the progress should be reported. The value should be a character vector of length 1. "detailed" will report every single step. "general" will report only main steps. "none" or FALSE will suppress any reporting.
#' @param return.fits Logical. Whether the fit object for each iterative run should be returned. Having the fits in the final object would significantly increase the final object size. Default is FALSE.
#' @param return.roc Logical. Whether the ROC object for each iterative run should be returned. Having the fits in the final object would significantly increase the final object size. Default is FALSE.
#' @param return.sessionInfo Logical. Whether the utils::sessionInfo() be included in the final object. This is useful for reproducibility purposes. Default is TRUE.
#' @param lib.paths A character vector that contains the paths that the dependency libraries are in it. REMEMBER to set this if you are using packrat.
#' @param debug.mode Whether or not the debug mode should be enabled.
#' @param ... Other parameters to be passed to the training method. For example the value of alpha in glmnet.
#' 
#' @return An object with S3 class "sivs".
#'   run.info$call:         The call that produced this object
#'   run.info$sessionInfo:  The object produced by utils::sessionInfo()
#' 
#' @examples
#' \dontrun{
#' # considering that you have your data object as `DATA` where you have rows
#' # as samples and columns as features, and the response value as a vector
#' # named `RESP`:
#' 
#' # simple defult run
#' sivs_object <- sivs(x = DATA, y = RESP)
#' 
#' # simple run with using only 3 CPU cores
#' sivs_object <- sivs(x = DATA, y = RESP, parallel.cores = 3)
#' 
#' 
#' # get the variable importance values
#' sivs_object$vimp
#' 
#' # get the condision that the sivs was ran in
#' sivs_object$run.info$call
#' sivs_object$run.info$sessionInfo
#' }
#' 
#' ## WORKING EXAMPLE
#' ## Note that this example does not logically make sense as iris data has only
#' ## 4 columns and there is no need for SIVS to take care of feature selection
#' ## therefore this example is only here for testing purposes.
#' 
#' tmp <- subset(x = iris, subset = Species != "setosa")
#' 
#' tmp <- varhandle::unfactor(tmp)
#' 
#' sivs_obj <- sivs(x = tmp[, c("Sepal.Length", "Sepal.Width",
#'                              "Petal.Length", "Petal.Width")],
#'                  y = factor(tmp$Species),
#'                  family = "binomial",
#'                  verbose = "detailed",
#'                  progressbar = FALSE,
#'                  nfolds = 3,
#'                  parallel.cores = FALSE,
#'                  iter.count = 20)
#' 
#' 
#' @import stats
#' @import utils
#' @import foreach
#' @import doParallel
#' @import glmnet
#' @importFrom pROC roc auc
#'
#' @export

sivs <- function(x, y, test.ratio = 1/3, method = "glmnet",
                 iter.count = 100, nfolds = 10, sample.grouping = NULL,
                 parallel.cores = "grace", progressbar = TRUE,
                 verbose = "general", return.fits = FALSE,
                 return.roc = FALSE, return.sessionInfo = TRUE,
                 lib.paths = .libPaths(), debug.mode = FALSE, ...){
    

    #-------[ initialize some variables ]-------#
    {
        # define the methods that this function can run
        acceptable.methods <- c("glmnet")
        
        # define the values that are acceptable as the parallel.cores
        acceptable.parallel.cores <- c("max", "grace")
        
        # define the classes of columns that are acceptable
        acceptable.column.classes <- c("numeric", "factor", "integer")
        
        # define the classes of y argument that are acceptable
        acceptable.y.classes <- c("factor", "numeric", "double", "integer", "Surv")
        
        # define the values of verbose argument that are acceptable
        acceptable.verbose <- c("detailed", "general", "none", FALSE)
        
        # create an environment to store certain variables inside and access them from different frames of the code
        sivs.internal.env <- new.env()
        
        
        ## to bypass a note in R CMD check about i being undefined global variable
        ## https://nathaneastwood.github.io/2019/08/18/no-visible-binding-for-global-variable/
        #if(getRversion() >= "2.15.1")  utils::globalVariables(c("i"))
        ## or alternatively:
        ## https://github.com/Rdatatable/data.table/issues/850
        i <- NULL
    }
    
    
    #-------[ internal functions ]-------#
    {
        
        func.variable.generation <- function(variable.name, max.depth = 10){
            ## Description:
            ##   A function to find the given variable's parent generation
            ##   position
            ## 
            ## Arguments:
            ##   variable.name:  Character vector of length 1. The variable
            ##                     name that we should search for
            ##   
            ##   max.depth:      Numeric. The maximum number of generations 
            ##                     the function should go back to check. The
            ##                     `sys.nframe()` dynamically get the frame
            ##                     number of the place this function is
            ##                     called from. This eliminates the need for
            ##                     manually set the max.depth argument.
            
            
            # return the minimum depth
            min(which(sapply(1:max.depth,
                                function(i){is.element(variable.name,
                                                    ls(envir = parent.frame(n = i)))
                                })))
        }
        
        
        func.warning <- function(..., width = 80){
            ## Description:
            ##   a function to propperly add softwrapped text generate warning
            ##
            ## Arguments:
            ##   ...:    The text
            ##   
            ##   width:  width of softwrapping
            
            warning(paste(paste("  ",
                                strwrap(x = paste(unlist(list(...)),
                                            collapse = ""),
                                        width = width)),
                            collapse = "\n"),
                    call. = FALSE)
        }
        
        
        func.cat <- function(..., importance = 1, new.line = TRUE){
            ## Description:
            ##   A function to cat the messages we want
            ## 
            ## Arguments:
            ##   ...:         Character. The text we want to show to user
            ##   
            ##   importance:  Numeric of length 1. The higher the values,
            ##                  the larger verbose value needed for showing
            ##                  the message
            ##   
            ##   new.line:    Logical. Whether the "\n" should be appended
            ##                  to the end of the text.
            
            # # find verbose variable parent generation position (this is useful to be dunamic since this function is being used in different levels of the code)
            # tmp.verbose.generation <- func.variable.generation("verbose")
            
            
            # if importance is greater than or equal to the numeric verbose value
            # if(importance >= get(x = "verbose", envir = parent.frame(n = tmp.verbose.generation))){
            #     cat(..., ifelse(new.line, "\n", ""))
            # }
            
            if(importance >= get(x = "verbose", envir = sivs.internal.env)){
                cat(..., ifelse(new.line, "\n", ""))
            }
        }
        
        
        func.zscore <- function(x){
            ## Description:
            ##   A function to perform zscore standardization on a vector of numbers
            ##    even when the standard deviation is zero! The base::scale() would
            ##    return NaN if sd(x)==0.
            ## 
            ## Arguments:
            ##   x:  A numeric vector
            as.numeric((x - mean(x = x, na.rm = TRUE)) / (sd(x = x, na.rm = TRUE) ^ as.logical(sd(x))))
        }
        
        
        func.create.foldID <- function(grouping = sample.grouping, k = nfolds, seed){
            ## Description:
            ##   a function to calculate the fold for cross-validation.
            ##
            ## Arguments:
            ##   grouping: a non-factor vector that defines the groupping of samples.
            ##             For example groups samples that are from the same patient.
            ##   k:        Number of folds
            ##   seed:     a seed for shuffling
            
            
            # get the unique groupIDs and shuffle them
            set.seed(seed)
            groupIDs <- sample(x = unique(grouping), size = length(unique(grouping)), replace = FALSE)
            
            # create a vector of folds for each of the patients
            folds <- cut(seq_along(groupIDs), k, labels = FALSE)
            
            # assign fold numbers to each sample of each patient
            collective_folds <- rep(NA, length(grouping))
            for(i in seq_along(groupIDs)){
                tmp_mask <- is.element(grouping, groupIDs[i])
                collective_folds[tmp_mask] <- folds[i]
            }
            
            return(collective_folds)
        }
    }
    
    
    #-------[ check input ]-------#
    ## make sure the input value are good enough for this function
    {
        #-------[ verbose ]-------#
        {
            if(!is.element(verbose, acceptable.verbose)){
                stop("The value for the 'verbose' argument should be one of the following:\n\t", paste(acceptable.verbose, collapse = ", "))
            }else{
                # convert FALSE to "none"
                verbose <- ifelse(verbose == FALSE, "none", verbose)
                
                # convert the verbose value to numerical representative (minimum accpetable importance of the reporting)
                verbose <- switch(verbose,
                                  detailed = 1,
                                  general = 2,
                                  none = Inf)
                
                # add the variable into our internal environment
                assign(x = "verbose",
                        value = verbose,
                        envir = sivs.internal.env)
            }
        }
        
        func.cat("Checking input arguments", importance = 2)
        
        #-------[ formula ]-------# >>>>>>> should be removed if we are not going to use formula <<<<<<<
        {
            # # check if it is a valid formula
            # func.cat("\t formula", new.line = FALSE)
            # 
            # if(!inherits(x = formula, what = "formula")){
            #     stop("Please provide a correct formula")
            # }
            # 
            # func.cat("\t[OK]")
        }
        
        #-------[ x ]-------#
        {
            func.cat("\t| x", new.line = FALSE)
            
            if(!inherits(x, c("matrix", "data.frame"))){
                stop("The input for the 'x' argument should be matrix or dataframe.")
            }else{
                # make sure the data is always data.frame when we begin the process
                x <- as.data.frame(x, stringsAsFactors = FALSE)
            }
            
            # make sure that the provided data has at least dimension of 2 by 2
            if((ncol(x) < 2) | (nrow(x) < 2)){
                stop("The input for the 'x' argument should have at least 2 features (columns) and at least 2 samples (rows)")
            }
            
            func.cat("              [OK]")
        }
        
        #-------[ y ]-------#
        {
            func.cat("\t| y", new.line = FALSE)
            
            # if the y is a vector
            if(inherits(y, acceptable.y.classes)){
                # special checks for class Surv
                if(class(y) == "Surv"){
                    # check if the x and y have the same length
                    if(nrow(y) != nrow(x)){
                        stop("The 'y' and 'x' argumets should have the same as the number of rows.")
                    }
                    
                }else{
                    # check if the x and y have the same length
                    if(length(y) != nrow(x)){
                        stop("The length of 'y' argument should be the same as the number of rows in 'x' argument.")
                    }
                }
                
                # check if the response value (y argument) does not have missing values
                if(any(is.na(y))){
                    stop("There should not be any missing values (NA) in the 'y' argument values.")
                }
                
            }else{
                stop("The value of 'y' argument is not appropriate, please check the documentation of the function.")
            }
            
            func.cat("              [OK]")
        }
        
        #-------[ test.ratio ]-------#
        {
            func.cat("\t| test.ratio", new.line = FALSE)
            
            if((test.ratio <= 0) | (test.ratio >= 1) | (length(test.ratio) != 1)){
                stop("The value for the 'test.ratio' argument should be a numeric vector of length 1 with value between (but not equal to) 0 and 1.")
            }
            
            func.cat("     [OK]")
        }
        
        #-------[ method ]-------#
        {
            func.cat("\t| method", new.line = FALSE)
            
            if((!is.element(method, acceptable.methods)) | (length(method) != 1)){
                stop("The value for the 'method' argument should be one of the following:\n\t", paste(acceptable.methods, collapse = ", "))
            }
            
            func.cat("         [OK]")
        }
        
        #-------[ iter.count ]-------#
        {
            func.cat("\t| iter.count", new.line = FALSE)
            
            if((length(iter.count) != 1) | any(!varhandle::check.numeric(v = iter.count, only.integer = TRUE))){
                stop("The value provided for the argument 'iter.count' should be a vector of length 1 containing a positive integer number.")
            }else{
                # convert to numeric and change the number to positive just in case user makes a mistake
                iter.count <- as.numeric(abs(iter.count))
            }
            
            func.cat("     [OK]")
        }
        
        #-------[ nfold ]-------#
        {
            func.cat("\t| nfold", new.line = FALSE)
            
            if((length(nfolds) != 1) | any(!varhandle::check.numeric(v = nfolds, only.integer = TRUE))){
                stop("The value provided for the argument 'nfolds' should be a vector of length 1 containing a positive integer number.")
            }else{
                # convert to numeric and change the number to positive just in case user makes a mistake
                nfolds <- as.numeric(abs(nfolds))
            }
            
            func.cat("          [OK]")
        }
        
        #-------[ sample.grouping ]-------#
        {
            func.cat("\t| sample.grouping", new.line = FALSE)
            
            if(!is.null(sample.grouping)){
                if(!is.atomic(sample.grouping)){
                    stop('The object provided for `sample.grouping` should be a character, numeric or factor vector indicating the grouping of the samples.')
                }else if(length(sample.grouping) != nrow(x)){
                    stop('The object provided for `sample.grouping` should have the length equal to the number of samples in the object you provided for `x`.')
                }
                
                if(is.factor(sample.grouping)){
                    sample.grouping <- varhandle::unfactor(sample.grouping)
                }
                
                if(length(unique(sample.grouping))<2){
                    stop('The object provided for `sample.grouping` should have at least two groups! Check the documentation by typing:\n?sivs')
                }
            }
            
            func.cat("[OK]")
        }
        
        #-------[ parallel.cores ]-------#
        {
            func.cat("\t| parallel.cores", new.line = FALSE)
            
            # if user have set the parallel.cores to NULL or FALSE
            if(is.null(parallel.cores) | all(parallel.cores == FALSE)){
                `%mydo%` <- foreach::`%do%`
                
            }else{
                `%mydo%` <- foreach::`%dopar%`
                
                # if the length of the vector is more than 1
                if((length(parallel.cores) != 1)){
                    # complain
                    stop("The value provided for the argument 'parallel.cores' should be a vector of length 1 containing a positive integer number or either of \"max\", \"grace\", or NULL.")
                # if the value is not an integer and also is not any of the acceptable values
                }else if((!varhandle::check.numeric(v = parallel.cores, only.integer = TRUE)) &
                            (!is.element(tolower(parallel.cores), c(acceptable.parallel.cores)))){
                    # complain
                    stop("The value provided for the argument 'parallel.cores' should be a vector of length 1 containing a positive integer number or either of \"max\", \"grace\", or NULL.")
                }
                
                # if parallel.cores is numeric
                if(varhandle::check.numeric(v = parallel.cores, only.integer = TRUE)){
                    # convert to numeric and change the number to positive just in case user makes a mistake
                    parallel.cores <- as.numeric(abs(parallel.cores))
                    
                    ## check if the provided core number is not larger than the actual total number of cores. if it is throw a warning
                    if(parallel.cores > parallel::detectCores()){
                        # change it to grace mode
                        parallel.cores <- parallel::detectCores() - 1
                        
                        # produce proper warning
                        func.warning("The value provided for the argument 'parallel.cores' is larger than the total number of existing core which is ", parallel::detectCores(), ". The code will replace your value to:", parallel.cores)
                    }
                    
                }else{
                    # convert the string into lower case
                    parallel.cores <- tolower(parallel.cores)
                    
                    if(parallel.cores == "max"){
                        # set the number to maximum possible
                        parallel.cores <- parallel::detectCores()
                    }else if(parallel.cores == "grace"){
                        # set the number to maximum possible but leave one out for the grace
                        parallel.cores <- parallel::detectCores() - 1
                    }else{
                        # complain
                        stop("The value provided for the argument 'parallel.cores' should be a vector of length 1 containing a positive integer number or either of \"max\", \"grace\", or NULL.")
                    }
                }

                ## if number of features is less than number of cores, truncate
                ## the reserved cores. This was reported in the following:
                ## https://github.com/mmahmoudian/sivs/issues/3
                tmp_warning_msg <- NULL
                if(parallel.cores > ncol(x)){
                    tmp_warning_msg <- paste0("Number of assigned CPU cores is ",
                                              parallel.cores,
                                              " but the total number of features are less than that (",
                                              ncol(x),
                                              "). We have reduced the number of cores to avoid paralellization errors.")
                    parallel.cores <- ncol(x) - 1
                }
                
                # add the variable into our internal environment
                assign(x = "parallel.cores", value = parallel.cores, envir = sivs.internal.env)
            }

            func.cat(" [OK]")
            
            # generate proper message if the last if condition is met
            if(!is.null(tmp_warning_msg)){
                func.cat("\t| \t|", tmp_warning_msg, importance = 2)
            }
        }
        
        #-------[ progressbar ]-------#
        {
            func.cat("\t| progressbar", new.line = FALSE)
            
            if(!is.logical(progressbar)){
                stop("The value provided for the 'progressbar' argument should be TRUE or FALSE")
            }
            
            func.cat("    [OK]")
        }
        
        #-------[ return.fits ]-------#
        {
            func.cat("\t| return.fits", new.line = FALSE)
            
            if(!is.logical(return.fits)){
                stop("The value provided for the 'return.fits' argument should be TRUE or FALSE")
            }
            
            func.cat("    [OK]")
        }
        
        #-------[ return.roc ]-------#
        {
            func.cat("\t| return.roc", new.line = FALSE)
            
            if(!is.logical(return.fits)){
                stop("The value provided for the 'return.roc' argument should be TRUE or FALSE")
            }
            
            func.cat("     [OK]")
        }
        
        #-------[ debug.mode ]-------#
        {
            func.cat("\t| debug.mode", new.line = FALSE)
            
            if(!is.logical(debug.mode)){
                stop("The value provided for the 'debug.mode' argument should be TRUE or FALSE")
            }
            
            func.cat("     [OK]")
        }
        
        #-------[ ... ]-------#
        {
            # get the list of arguments that have been passed to this function through ... object
            tmp.dots <- list(...)
            
            # remove foldid is provided by user
            if(is.element("foldid", names(tmp.dots)) & (method == "glmnet")){
                stop('The argument `foldid` cannot be defined in sivs as this function needs to permute the groups. If you want to group some samples together (e.g grouping all samples of the same patient together), please use `sample.grouping` argument. For more information please read the documentation y typing:\n?sivs')
            }
        }
        
        
        func.cat(" ")
        
        # store the function's call to be returned for the reference of the use
        function.call <- deparse(match.call())
    }
    
    
    #-------[ prepare x and y ]-------#
    {
        func.cat("Prepare x and y", importance = 2)
        
        
        #-------[ check for unacceptable columns ]-------#
        {
            # if there are any columns that are not numeric, factor or integer
            if(any(!is.element(sapply(x, class), acceptable.column.classes))){
                # produce proper warning
                func.warning("Data should have only ",
                                paste(acceptable.column.classes, collapse = " or "),
                                " columns. The following columns have been removed since they don't match this criteria:\n",
                                paste(colnames(x)[!is.element(sapply(x, class), acceptable.column.classes)], collapse = ", "))
                
                # remove columns that are not acceptable
                x <- x[, is.element(sapply(x, class), acceptable.column.classes)]
                
                if(ncol(x) < 2){
                    stop("The data provided as value for the 'x' argument had",
                            "many columns that were not among the acceptable",
                            "classes. After removing them", ncol(x), "is left",
                            "which is not useful. This function needs at least 2",
                            "valid columns to run.")
                }
            }
            
        }
        
        
        #-------[ convert integer to numeric ]-------#
        {
            # # get a mask of integer columns (TRUE would be integer and FLASE would be others)
            # tmp.indicies <- (sapply(x, class) == "integer")
            # 
            # # if there is any integer columns in the data
            # if(any(tmp.indicies)){
            #     
            #     # report to user about the step and amount of computation
            #     func.cat("\t| convert",
            #              sum(tmp.indicies),
            #              paste0("integer column",
            #                     ifelse(sum(tmp.indicies) > 1, "s", "")),
            #              "to numeric")
            #     
            #     x[tmp.indicies] <- lapply(x[tmp.indicies], as.numeric)
            #     
            # }
        }
        
        
        #-------[ remove features based on certain criteria ]-------#
        {
            func.cat("\t| Remove features based on following criteria")
            
            #-------[ one class or zero variance feature ]-------#
            {
                func.cat("\t| \t| Remove one class or zero variance features")
                
                # get a vector of TRUE/FALSE for the columns that are constructed from one value (these columns have no variation)
                tmp.one.class <- (sapply(x, function(a){length(unique(a))}) == 1)
                
                # remove these features from x
                x <- x[, !tmp.one.class]
                
                func.cat("\t| \t| \t| ", sum(tmp.one.class), "features have been removed")
            }
        }
        
        
        #-------[ standardize the numeric (non-factor) columns ]-------#
        {
            func.cat("\t| Standardize the numeric (non-factor) columns")
            
            ## get the colnames of the those which are not factor
            # if there is any factor column
            if(any(sapply(x, is.factor))){
                # get the name of columns that are not factor (They must be numeric)
                tmp.columns.to.zscore <- colnames(x)[-which(sapply(x, is.factor))]
            }else{
                tmp.columns.to.zscore <- colnames(x)
            }
            
            
            # if user wanted to run the code in parallel
            if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
                func.cat("\t|\t| Registring Parallel backend:")
                
                # set the seed in case the parallel package choose the cores in a random manner
                set.seed(1)
                
                # create cluster in quite mode
                invisible({
                    if(debug.mode){
                        cl <- parallel::makeCluster(parallel.cores, outfile = "")
                    }else{
                        cl <- parallel::makeCluster(parallel.cores)
                    }
                })
                
                # register parallel backend
                doParallel::registerDoParallel(cl)
                
                # run the function in all the instances of the cluster cl
                parallel::clusterCall(cl, function(x){
                    .libPaths(unique(c(.libPaths(), x)))
                    #require("glmnet")
                    #require("pROC")
                }, x = lib.paths)
                
                # if user wants to have progressbar
                if(progressbar){
                    # initiate the progressbar
                    pb <- txtProgressBar(min = 0, max = parallel.cores, style = 3)
                }
                
                
                # create bins. break the numeric columns into bins in a way that we end up having n bins when n = parallel.cores
                tmp.bins <- split(x = tmp.columns.to.zscore,
                                    f = rep(x = 1:parallel.cores,
                                            each = ceiling(length(tmp.columns.to.zscore) / parallel.cores))[1:length(tmp.columns.to.zscore)])
                
                # standardize in parallel
                tmp.zscore.res <- foreach::foreach(i = 1:length(tmp.bins),
                                                   .inorder = TRUE,
                                                   .combine = cbind.data.frame) %mydo% {
                                                       
                                                       
                                                       # if user wants to have progressbar
                                                       if(progressbar){
                                                           # increment the progressbar
                                                           setTxtProgressBar(pb = pb, value = getTxtProgressBar(pb) + 1)
                                                       }
                                                       
                                                       
                                                       lapply(x[, tmp.bins[[i]]], func.zscore)
                                                   }
                
                
                # if user wants to have progressbar
                if(progressbar){
                    # close the progressbar
                    close(pb)
                }
                
                
                # if user wanted to run the code in parallel
                if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
                    func.cat("\t|\t| Terminating the parallel backend")
                    
                    # close the backend parallel cluster
                    parallel::stopCluster(cl)
                }
            }else{
                # standardize without parallel backend
                tmp.zscore.res <-  lapply(x[, tmp.columns.to.zscore], func.zscore)
            }
            
            
            # replace the original columns with the zscored ones
            x[, tmp.columns.to.zscore] <- tmp.zscore.res
            
        }
        
        
        #-------[ method specific preperation ]-------#
        {
            func.cat("\t| Method specific preperation")
            
            #-------[ glmnet ]-------#
            {
                if(method == "glmnet"){
                    
                    func.cat("\t| \t| glmnet")
                    
                    #-------[ prepare training set ]-------#
                    {
                        func.cat("\t| \t| \t| prepare training set")
                        
                        # if the data has missing value
                        if(any(sapply(x, is.na))){
                            # get the total number of missing values in the provided data
                            tmp.NA.count <- length((sapply(x, is.na)))
                            
                            # get the index of the rows that should be removed for having NAs
                            tmp.rows.to.be.removed <- as.numeric(which(apply(x, 1, function(tmp){any(is.na(tmp))})))
                            
                            ## for y variable
                            if(inherits(y, c("Surv"))){
                                y <- y[-tmp.rows.to.be.removed, ]
                            }else{
                                y <- y[-tmp.rows.to.be.removed]
                            }
                            
                            # for x variable
                            x <- x[-tmp.rows.to.be.removed, ]
                            
                            # complain and move on
                            func.warning("The method you have chosen is \"glmnet\" and it cannot handle missing values. The provided data as 'x' argument contains ",
                                            tmp.NA.count,
                                            " missing value",
                                            ifelse(tmp.NA.count > 1, "s", ""),
                                            ". In total ",
                                            length(tmp.rows.to.be.removed),
                                            " row",
                                            ifelse(length(tmp.rows.to.be.removed) != 1, "s", ""),
                                            " containing missing values ",
                                            ifelse(length(tmp.rows.to.be.removed) != 1, "were", "was"),
                                            " removed.")
                        }
                        
                        
                        # convert to data.matrix. This is requred by glmnet for some reason!
                        x <- data.matrix(x)
                        
                    }
                    
                    
                    #-------[ check family argument ]-------#
                    {
                        func.cat("\t| \t| \t| check family argument")
                        
                        ## initiate the family variable if it is not defined by user
                        if(!is.element("family", names(tmp.dots))){
                            tmp.dots[["family"]] <- NULL
                            ## or if the user have made the mistake of assigning anything but character vector to argument 'family'
                        }else if(class(tmp.dots[["family"]]) != "character"){
                            tmp.dots[["family"]] <- NULL
                            ## if user have added more than 1 family
                        }else if(length(tmp.dots[["family"]]) != 1){
                            
                        }
                        
                        # if family is not specifically defined by user
                        if(is.null(tmp.dots[["family"]])){
                            
                            # if the response is a vector
                            if(inherits(y, c("factor"))){
                                if(length(levels(y)) == 2){
                                    tmp.dots[["family"]] <- "binomial"
                                }else{
                                    tmp.dots[["family"]] <- "multinomial"
                                }
                                
                                # produce proper warning
                                func.warning("The method \"glmnet\" needs the argument 'family' to be filled. ",
                                                "Since it was not provided by the user and based on the factor levels ",
                                                "of 'y' argument, the value of argument 'family' has set to \"", tmp.dots[["family"]], "\".")
                                
                                # if the class of response variable is survival
                            }else if(inherits(y, c("Surv"))){
                                tmp.dots[["family"]] <- "cox"
                            }else{
                                stop("The chosen method is \"glmnet\" but the value of 'y' argument is not of class \"factor\" or \"Surv\".")
                            }
                        }
                    }
                    
                    
                    #-------[ make sure y is a vector and not of class Surv ]-------#
                    {
                        ## for y variable, if it is of class Surv
                        if(inherits(y, c("Surv"))){
                            # only keep the binary outcome
                            y.response.status <- y[, 2]
                        }else{
                            y.response.status <- y
                        }
                    }
                }
            }
            
        }
    }
    
    
    #-------[ the iterative ]-------#
    {
        func.cat("Iterative training step", importance = 2)
        
        # if user wanted to run the code in parallel
        if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
            func.cat("\t| Registring Parallel backend:")
            
            # set the seed in case the parallel package choose the cores ina random manner
            set.seed(1)
            
            # create cluster in quite mode
            invisible({
                if(debug.mode){
                        cl <- parallel::makeCluster(parallel.cores, outfile = "")
                    }else{
                        cl <- parallel::makeCluster(parallel.cores)
                    }
            })
            
            # register parallel backend
            doParallel::registerDoParallel(cl)
            
            # run the function in all the instances of the cluster cl
            parallel::clusterCall(cl, function(x){
                .libPaths(new = unique(c(.libPaths(), x)))
                #require("glmnet")
                #require("pROC")
            }, x = lib.paths)
        }
        
        
        func.cat("\t| Iterative training ...")
        
        
        # if user wants to have progressbar
        if(progressbar){
            # initiate the progressbar
            pb <- txtProgressBar(min = 0, max = iter.count, style = 3)
        }
        
        
        iterative.res <- foreach::foreach(i = 1:iter.count, .inorder = TRUE) %mydo% {
            
            suppressPackageStartupMessages({
                requireNamespace("glmnet")
                requireNamespace("pROC")
                })
            
            
            tmp.args <- tmp.dots
            
            # calculate the foldIDs if user wants and append it to cv.glmnet arguments
            if(!is.null(sample.grouping)){
                tmp.args$foldid <- func.create.foldID(grouping = sample.grouping, k = nfolds, seed = i)
            }
            
            
            # create certain vectors to store the possible error and warnings
            tmp.fit.warning <- tmp.fit.error <- 0
            
            
            # set the seed based on this iteration
            set.seed(i)
            
            
            tmp.fit <- tryCatch({
                
                # # perform the training
                # glmnet::cv.glmnet(x = x,
                #           y = y,
                #           nfolds = nfolds,
                #           parallel = FALSE,
                #           family = family,
                #           ...)
                
                
                # perform the training
                do.call(what = glmnet::cv.glmnet,
                        args = c(list(x = x,
                                        y = y,
                                        nfolds = nfolds,
                                        parallel = FALSE),
                                    tmp.args))
                
                
            }, warning = function(w) {
                
                assign(x = "tmp.fit.warning", envir = parent.frame(n = func.variable.generation("tmp.fit.warning")), value = 1)
                return(NA)
                
                
            }, error = function(e) {
                
                assign(x = "tmp.fit.error", envir = parent.frame(n = func.variable.generation("tmp.fit.error")), value = 1)
                return(NA)
            })
            
            
            # report if a warning was occored in this specific run
            if(tmp.fit.warning){
                func.warning("We have got warning for the following iteration: ", i)
                
            }
            
            # report if an error has occored in this specific run
            if(tmp.fit.error){
                func.warning("We have got error for the following iteration: ", i)
            }
            
            
            # if modelling was done without error (not NA)
            if(!is.logical(tmp.fit)){
                
                ## extract the coefficients of features in form of a dataframe
                tmp.coef <- coef(object = tmp.fit, s = "lambda.min")
                tmp.coef <- data.frame(names = row.names(tmp.coef), coef = as.vector(tmp.coef))
                
                
                # perform prediction
                tmp.pred <- predict(object = tmp.fit, newx = x, s = "lambda.min", type = "response")
                
                
                # calculate the roc
                tmp.roc <- pROC::roc(response = y.response.status, predictor = as.numeric(tmp.pred), quiet = TRUE)
                
                # extract the auc from roc
                tmp.auc <- as.numeric(pROC::auc(tmp.roc))
                
                
            # if modelling was done with error
            }else{
                tmp.fit <- tmp.coef <- tmp.pred <- tmp.roc <- tmp.auc <- NA
            }
            
            
            # if user wants to have progressbar
            if(progressbar){
                # increment the progressbar
                setTxtProgressBar(pb = pb, value = i)
            }
            
            
            # if user do not want to keep the fit object
            if(!return.fits){
                tmp.fit <- NULL
            }
            
            # if user do not want to keep the ROC object
            if(!return.roc){
                tmp.roc <- NULL
            }
            
            
            # prepare the output of the foreach
            tmp <- list(fit  = tmp.fit,
                        coef = tmp.coef,
                        pred = tmp.pred,
                        roc  = tmp.roc,
                        auc  = tmp.auc,
                        seed = i)
            
            # remove NULL items
            tmp <- tmp[!sapply(tmp, is.null)]
            
            
            return(tmp)
        }
        
        
        # if user wants to have progressbar
        if(progressbar){
            cat("\r")
            # close the progressbar
            close(pb)
        }
        
        
        # if user wanted to run the code in parallel
        if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
            func.cat("\t| Terminating the parallel backend")
            
            # close the backend parallel cluster
            parallel::stopCluster(cl)
        }
        
        names(iterative.res) <- paste0("iteration.", 1:length(iterative.res))
        
    }
    
    # perform invisible garbage collection
    invisible(capture.output(gc()))
    
    
    #-------[ generate the features ranking ]-------#
    {
        func.cat("Generate the features ranking", importance = 2)
        
        # based on the iterative.res, go through all the items in it, extract
        # the coef dataframe, fix the name of their coef column to also
        # represent the iterations' name and finally merge all these dataframes
        # into one big dataframe considering the feature name columns.
        
        # remove the runs that could not converge (and as the result have NA instead of coefficients' dataframe)
        clean.iterative.res <- iterative.res[!sapply(lapply(iterative.res, "[[", "coef"), is.logical)]
        
        # if there were not any runs that have converged
        if(length(clean.iterative.res) == 0){
            stop("In the Iterative step, none of the ", length(iterative.res),
                 " runs got converged. This means that sivs cannot progress to",
                 " subsequent steps, and you should use different",
                 " 'method' or normalize your data differently.")
        }
        
        coef.df <- Reduce(function(...){ merge(...,
                                                by = "names",
                                                all = TRUE) },
                            sapply(names(clean.iterative.res),
                                    FUN = function(item) {
                                        temp <- clean.iterative.res[[item]]$coef
                                        colnames(temp)[2] <- paste0("coef.", item)
                                        return(temp)
                                    },
                                    simplify = FALSE))
        
        ## set the feature names to be the rownames
        row.names(coef.df) <- coef.df[, 1]
        coef.df <- coef.df[, -1]
        
        # calculate how many times each feature has been given a coefficient other than 0 and them sort them decreasingly
        selection.freq <- sort(x = apply(coef.df, 1, function(f){
            sum(f != 0)
            }), decreasing = TRUE)
        
    }
    
    
    # perform invisible garbage collection
    invisible(capture.output(gc()))
    
    
    #-------[ calculate variable importance (vimp) ]-------#
    {
        func.cat("Calculate variable importance (vimp)", importance = 2)
        
        # calculate how many times a feature got a non-zero coefficient (in other words how many times a feature has been selected)
        tmp.vimp.freq <- apply(coef.df, 1, function(r){sum(r != 0)})
        
        # calculate the ratio of selection.
        tmp.vimp.freq.ratio <- tmp.vimp.freq / ncol(coef.df)
        
        # calculate the absolute value of median of coefficients of each feature
        tmp.vimp.coef.abs.median <- apply(coef.df, 1, function(r){
            r[r==0] <- NA
            abs(median(x = r, na.rm = TRUE))
            })
        
        # calculate the inter quertile range for coefficinets of each feature
        tmp.vimp.coef.IQR <- apply(coef.df, 1, function(r){
            r[r==0] <- NA
            IQR(x = r, na.rm = TRUE)
        })
        
        # check if the coefficients signs changes for each of the features
        tmp.vimp.coef.sign <- apply(coef.df, 1, function(r){any(diff(sign(r[r != 0])) != 0)})
        
        # calculate the variale importance
        tmp.vimp <- (1 - as.numeric(tmp.vimp.coef.sign)) * ((tmp.vimp.freq.ratio * tmp.vimp.coef.abs.median) / (tmp.vimp.coef.IQR + 1))
        
        # handle possible NaN
        tmp.vimp[is.nan(tmp.vimp)] <- 0
        
        # sort the features based on their scores decreasingly
        tmp.vimp <- sort(x = tmp.vimp, decreasing = TRUE)
        
        # remove the intercept form the vimp list if it exists
        tmp.vimp <- tmp.vimp[!is.element(names(tmp.vimp), "(Intercept)")]
    }
    
    
    #-------[ recursive feature elimination (rfe) ]-------#
    {
        ## from the list of features that were selected at least once in the
        ## iterative approach, we start by removing the features one by one
        ## in a recursive training approach to find the minimal number of
        ## features that can be used. This is purely as suggestion to user.
        ## We start from features with the lowest non-zero vimp scores.
        
        func.cat("Recursive feature elimination (rfe)", importance = 2)
        
        # get the list of features with vimp more than zero, starting from features with lowest vimp
        tmp.rfe.input.features <- names(sort(x = tmp.vimp[tmp.vimp > 0], decreasing = FALSE))
        
        # check if there are more than 2 significant features left so that we can do rfe
        if(length(tmp.rfe.input.features) > 2){
            
            # add "baseline" to the list of features
            tmp.rfe.input.features <- c("baseline", tmp.rfe.input.features)
            
            
            # if user wanted to run the code in parallel
            if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
                func.cat("\t| Registring Parallel backend:")
                
                # set the seed in case the parallel package choose the cores ina random manner
                set.seed(1)
                
                # create cluster in quite mode
                invisible({
                    if(debug.mode){
                        cl <- parallel::makeCluster(parallel.cores, outfile = "")
                    }else{
                        cl <- parallel::makeCluster(parallel.cores)
                    }
                })
                
                # register parallel backend
                doParallel::registerDoParallel(cl)
                
                # run the function in all the instances of the cluster cl
                parallel::clusterCall(cl, function(x){
                    .libPaths(unique(c(.libPaths(), x)))
                    #require("glmnet")
                    #require("pROC")
                }, x = lib.paths)
            }
            
            
            # create a collective list variable
            rfe.res <- list()
            
            # create a collective variable to store the warnings and errors
            rfe.catches <- data.frame(feature = character(),
                                      seed    = numeric(),
                                      warning = character(),
                                      error   = character(),
                                      stringsAsFactors = FALSE)
            
            
            # iterate through the features
            for(j in head(x = tmp.rfe.input.features, n = -2)){
                
                # exclude features until after the currect feature and use the rest
                tmp.features <- tmp.rfe.input.features[(match(j, tmp.rfe.input.features) + 1):length(tmp.rfe.input.features)]
                
                func.cat("\t| Testing the ",
                         paste0(ifelse(j == "baseline", "", "effect of removing: "),
                                j,
                                ifelse(j == "baseline", "           ", ""),
                                "\t\t [features left: ",
                                length(tmp.features),
                                "]"))
                
                # if user wants to have progressbar
                if(progressbar){
                    # initiate the progressbar
                    pb <- txtProgressBar(min = 0, max = iter.count, style = 3)
                }
                
                
                tmp.rfe.res <- foreach(i = 1:iter.count, .inorder = TRUE) %mydo% {
                    
                    suppressPackageStartupMessages({
                        requireNamespace("glmnet")
                        requireNamespace("pROC")
                    })
                    
                    
                    # make a copy of the extra arguments to be passed to cv.glmnet
                    tmp.args <- tmp.dots
                    
                    # calculate the foldIDs if user wants and append it to cv.glmnet arguments
                    if(!is.null(sample.grouping)){
                        tmp.args$foldid <- func.create.foldID(grouping = sample.grouping, k = nfolds, seed = i)
                    }
                    
                    
                    # create certain vectors to store the possible error and warnings
                    tmp.fit.warning <- tmp.fit.error <- 0
                    
                    # set the seed based on this iteration
                    set.seed(i)
                    
                    tmp.fit <- NULL
                    
                    tmp.try <- tryCatch({
                        
                        # perform the training
                            tmp.fit <- do.call(what = glmnet::cv.glmnet,
                                            args = c(list(x = x[, tmp.features],
                                                                    y = y,
                                                                    nfolds = nfolds,
                                                                    parallel = FALSE),
                                                        tmp.args))
                        
                        
                    }, warning = function(w) {
                        
                        assign(x = "tmp.fit.warning", envir = parent.frame(n = 5), value = 1)
                        
                        rfe.catches[nrow(rfe.catches) + 1, ] <- c(j, i, w, "")
                        
                    }, error = function(e) {
                        
                        # func.cat(e, importance = 10)
                        # func.cat(paste("iteration", i, "faced an error\n"), importance = 10)
                        
                        # func.cat(ls(envir = parent.frame(n = 4)), sep = ", ", importance = 10)
                        assign(x = "tmp.fit.error", envir = parent.frame(n = 5), value = 1)
                        rfe.catches[nrow(rfe.catches) + 1, ] <- c(j, i, "", e)
                        return(NA)
                    })
                    
                    
                    # report if a warning was occurred in this specific run
                    if(tmp.fit.warning){
                        func.warning("We have got warning while eliminating the ", j, " feature in iteration ", i)
                        
                    }
                    
                    # report if an error has occurred in this specific run
                    if(tmp.fit.error){
                        func.warning("We have got error while eliminating the ", j, " feature in iteration ", i)
                    }
                    
                    
                    # if modeling was done without error (it is not NA)
                    if((sum(tmp.fit.warning, tmp.fit.error) == 0) & (!is.null(tmp.fit))){
                        
                        ## extract the coefficients of features in form of a dataframe
                        tmp.coef <- coef(object = tmp.fit, s = "lambda.min")
                        tmp.coef <- data.frame(names = row.names(tmp.coef), coef = as.vector(tmp.coef))
                        
                        
                        # perform prediction
                        tmp.pred <- predict(object = tmp.fit, newx = x[, tmp.features], s = "lambda.min", type = "response")
                        
                        
                        # calculate the roc
                        tmp.roc <- pROC::roc(response = y.response.status, predictor = as.numeric(tmp.pred), quiet = TRUE)
                        
                        # extract the auc from roc
                        tmp.auc <- as.numeric(pROC::auc(tmp.roc))
                        
                        
                    # if modeling was done with error
                    }else{
                        tmp.fit <- tmp.coef <- tmp.pred <- tmp.roc <- tmp.auc <- NA
                    }
                    
                    
                    # if user wants to have progressbar
                    if(progressbar){
                        # increment the progressbar
                        setTxtProgressBar(pb = pb, value = i)
                    }
                    
                    
                    # if user do not want to keep the fit object
                    if(!return.fits){
                        tmp.fit <- NULL
                    }
                    
                    # if user do not want to keep the ROC object
                    if(!return.roc){
                        tmp.roc <- NULL
                    }
                    
                    
                    # prepare the output of the foreach
                    tmp <- list(fit  = tmp.fit,
                                coef = tmp.coef,
                                pred = tmp.pred,
                                roc  = tmp.roc,
                                auc  = tmp.auc,
                                seed = i)
                    
                    # remove NULL items
                    tmp <- tmp[!sapply(tmp, is.null)]
                    
                    
                    return(tmp)
                }
                
                # fix the names of the iterations for this
                names(tmp.rfe.res) <- paste0("iteration.", 1:length(tmp.rfe.res))
                
                # append the object of iteration to the collective list
                rfe.res <- c(rfe.res, list(tmp.rfe.res))
                
                # fix the name  of the collective list
                names(rfe.res) <- c(head(x = names(rfe.res), n = -1), j)
                
                
                # if user wants to have progressbar
                if(progressbar){
                    cat("\r\r")
                    # close the progressbar
                    close(pb)
                }
                
            }
            
            
            # if user wanted to run the code in parallel
            if(!(is.null(parallel.cores) | all(parallel.cores == FALSE))){
                func.cat("\t| Terminating the parallel backend")
                
                # close the backend parallel cluster
                parallel::stopCluster(cl)
            }
            
            
            ## check if there has been any warning or error reported in this
            ## step. if nothing has been captured, set NULL to the
            ## collective variable to exclude it form the final object
            if(nrow(rfe.catches) != 0){
                func.cat("During the rfe step one or more issues",
                            "happened. You can find the details in the",
                            "sivs object under the \"rfe.issues\" element",
                            "which can be accessed by adding $rfe.issues",
                            "to the end of the sivs object.",
                            importance = 2)
                
                
            }else{
                rfe.catches <- NULL
            }
            
            
        # if there are 2 or less significant features, we cannot perform rfe
        }else{
            func.cat("\t| The rfe step is skipped due to lack of enough",
                        "significant features. Check the warnings() for more",
                        "detailed info.", importance = 2)
            
            func.warning("We need more than 2 significant features for the ",
                            "rfe (Recursive Feature Elimination) step. There ",
                            "are only ", length(tmp.rfe.input.features),
                            " significant feature ",
                            ifelse(length(tmp.rfe.input.features) > 1, "s", ""),
                            " left. As the result the rfe step is skipped.")
            
            
            ## assign NULL to the both objects that would have been filled
            ## if we were not in the else section of this if! Setting them
            ## to NULL would remove them from the final object of the
            ## function.
            rfe.res <- NULL
            rfe.catches <- NULL
        }
        
    }
    
    
    # perform invisible garbage collection
    invisible(capture.output(gc()))
    
    if(return.sessionInfo){
        tmp.sessionInfo <- utils::sessionInfo()
    }else{
        tmp.sessionInfo <- NULL
    }
    
    # form the final object
    final.object <- list(iterative.res      = iterative.res,
                            selection.freq  = selection.freq,
                            vimp            = tmp.vimp,
                            rfe             = rfe.res,
                            rfe.issues      = rfe.catches,
                            run.info        = list(call = function.call,
                                                   sessionInfo = tmp.sessionInfo))
    
    # remove NULL items
    final.object <- final.object[!sapply(final.object, is.null)]
    
    # set the class of the final object
    class(final.object) <- "sivs"
    
    # return the final object
    return(final.object)
}





