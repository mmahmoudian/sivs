#' A plotting function for sivs object
#' 
#' @description A function to plot the object of the sivs function.
#' 
#' @param x The object that is produced by the sivs function.
#' @param type Which plot do you want to have. Acceptable values are "frequency", "coef", and "rfe".
#' @param suggestion_strictness The strictness value that indicates how much the thresholds should be strict. For more details visit help page of `suggest()` function. If you want ru suppress the suggestion on the plot, set this to NULL.
#' @param ... The other argument you might want to pass to each plot.
#' 
#' @details The rfe plot ignores the existance of intercept since intercept
#' cannot be removed in the recursive feature elimination step. This
#' is the reason that the number of features with vimp greater than 0
#' are differnt from the presented "Number of Features Left" in the
#' rfe plot by 1.
#' 
#' @return Does not return anything. This function is only used for plotting
#' and is not meant to return any value.
#' 
#' @examples
#' \dontrun{
#' # to see all plots
#' layout(mat = matrix(c(1,2,3,3), nrow = 2, byrow = TRUE))
#' plot(x = sivs_object)
#' layout(1)
#' 
#' # to plot only the Recursive Feature Elimination (rfe) results
#' plot(x = sivs_object, type = "rfe")
#' 
#' # suppress suggestion on rfe plot
#' plot(x = sivs_object, type = "rfe", suggestion_strictness = NULL)
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
#' plot(sivs_obj, type = "frequency")
#' plot(sivs_obj, type = "coef")
#' plot(sivs_obj, type = "rfe")
#' 
#' 
#' @import graphics
#' @import stats
#' @import utils
#' 
#' @export

plot.sivs <- function(x, type = c("frequency", "coef", "rfe"),
                        suggestion_strictness = c(0.01, 0.05), ...){
    
    #-------[ initialize some variables ]-------#
    {
        # define the types that this function can plot
        acceptable.types <- c("frequency", "coef", "rfe")
    }
    
    
    #-------[ check input ]-------#
    {
        #-------[ x ]-------#
        {
            if(!is.sivs(object = x)){
                stop("This function can only handle an object from type \"sivs\".")
            }
        }
        
        
        #-------[ type ]-------#
        {
            if(any(!is.element(type, acceptable.types))){
                stop("The 'type' argument should be any combination of the following as a character vector:\n\t", paste(acceptable.types, collapse = ", "))
            }
        }
        
        
        #-------[ suggestion_strictness ]-------#
        {
            if(!is.null(suggestion_strictness)){
                if(!is.numeric(suggestion_strictness)){
                    stop("The 'suggestion_strictness' argument should be a numerical vector with values from 0 to 1")
                }else if(any(suggestion_strictness < 0) | any(suggestion_strictness > 1)){
                    stop("The 'suggestion_strictness' argument should be a numerical vector with values from 0 to 1")
                }
            }
            
        }
        
        
        #-------[ ... ]-------#
        {
            dots <- list(...)
        }
    }
    
    
    #-------[ prepare data ]-------#
    {
        #-------[ sivs call ]-------#
        {
            # extract the call string and turn it into a named list
            tmp.call <- as.list(parse(text = x$run.info$call)[[1]])[-1]
        }
        
        
        #-------[ intercept ]-------#
        {
            # if the 'intercept' argument was defined by
            if(is.element("intercept", names(tmp.call))){
                # extract the value of the 'intercept' and convert it into logical
                intercept <- as.character(x = as.character(tmp.call$intercept), as.is = TRUE)
                
                # if there was an issue converting the 'intercept' to logical
                if(is.na(intercept)){
                    # complain
                    stop("The intercept value used in the sivs function should have been defined as a logical vector with length 1. (Acceptable values are 0, 1, F, T, FALSE, TRUE")
                }
            # If user have not defined the argument 'intercept' in their original call
            }else{
                intercept <- TRUE
            }
        }
        
        # make sure we backup user's par options
        oldpar <- par(no.readonly = TRUE)
        on.exit(par(oldpar))
    }
    
    
    #-------[ plotting ]-------#
    {
        if(is.element("frequency", type)){
            
            # extract the ranked features and put them in increasing order because of the boxplot
            tmp.selection.freq <- sort(x = x$selection.freq, decreasing = FALSE)
            
            # if intercept was defined as FALSE and still intercept was among the features
            if((!intercept) & is.element("(Intercept)", names(tmp.selection.freq))){
                # remove intercept from the feature list
                tmp.selection.freq <- tmp.selection.freq[-match("(Intercept)", names(tmp.selection.freq))]
            }
            
            # only keep those that have been selected atleast once
            tmp.selection.freq <- tmp.selection.freq[tmp.selection.freq > 0]
            
            barplot(tmp.selection.freq,
                    horiz = TRUE,
                    las = 2,
                    col = "cadetblue3",
                    cex.names = 0.6,
                    xlab = "Selection Frequency",
                    ...)
            
            
            # valid.iterations <- sum(!sapply(lapply(x$iterative.res, "[[", "coef"), is.logical))
            # total.iterations <- length(x$iterative.res)
            
            axis(side = 3,
                    # at = seq(from = 0, to = 1, by = 0.1) * length(x$iterative.res),
                    at = seq(from = 0, to = 1, by = 0.1) * sum(!sapply(lapply(x$iterative.res, "[[", "coef"), is.logical)),
                    labels = paste0(seq(from = 0, to = 100, by = 10), "%"),
                    lty = 2,
                    cex.axis = 0.7)
            
            
        }
        
        
        if(is.element("coef", type)){
            
            clean.iterative.res <- x$iterative.res[!sapply(lapply(x$iterative.res, "[[", "coef"), is.logical)]
            
            # extract the coefficients and put them in a dataframe
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
            
            ## set the column names as the rownames and remove that column
            row.names(coef.df) <- coef.df$names
            coef.df <- coef.df[, -match("names", colnames(coef.df))]
            
            
            # only keep those that have been selectedt atleast once
            coef.df <- coef.df[apply(coef.df, 1, function(x){any(x != 0)}), ]
            
            
            # transpose the dataframe to have the features as columns
            coef.df <- t(coef.df)
            
            coef.df <- coef.df[, order(apply(coef.df, 2, median), decreasing = TRUE)]
            
            # there is a chance that the user has defined plot title under
            # the names "main" or "title". if so use that, otherwise use a
            # pre-defined one
            if (any(is.element(c("main", "title"), names(dots)))){
                the.title <- dots[[which(is.element(names(dots), c("main", "title")))[1]]]
                dots <- dots[-which(is.element(names(dots), c("main", "title")))]
            }else{
                the.title <- "Coefficients of features during iterative approach"
            }
            
            
            boxplot(x = coef.df, col = "darkolivegreen3", horizontal = FALSE, main = the.title, ylab = "Coefficient", las = 2, cex.axis = 0.7, ... = dots)
            abline(h = 0, col = "gray", lty = 2)
            
        }
        
        
        if(is.element("rfe", type)){
            # make sure the rfe result is part of the final object
            if(!is.element("rfe", names(x))){
                ## This can happen if the sample size of the data that was
                ## provided to sivs was so small that sivs has skipped going
                ## through the rfe step.
                
                # complain
                warning("The provided object does not have rfe section and as the result the rfe cannot be plotted.")
            }else{
                # get the vimp of those that have vimp more than zero, then sort them increasingly
                tmp.vimp <- sort(x$vimp[x$vimp > 0], decreasing = FALSE)
                
                
                # remove the last two items in the vimp since they have not been excluded in rfe step due to being the most significant features
                tmp.vimp <- head(x = tmp.vimp, n = -2)
                
                
                # extract the AUCs from the
                tmp.rfe <- lapply(x$rfe,
                                    function(x){
                                        sapply(x, "[[", "auc")
                                    })
                
                # get which rfe runs converged (a logical vector)
                valid.rfe <- sapply(x$rfe, function(x){
                    any(!is.na(sapply(x, "[[", "coef")))
                })
                
                
                par(mar = c(5, 4, 4, 4))
                barplot(c(NA, tmp.vimp),
                        col       = "steelblue1",
                        las       = 2,
                        yaxt      = "n",
                        ylim      = c((0 - (max(tmp.vimp) * 0.01)), (max(tmp.vimp) + (max(tmp.vimp) * 0.05))), # adding a bit of padding on the top and bottom
                        names.arg = names(tmp.rfe),
                        density = ifelse(valid.rfe, -1, 50),
                        border = ifelse(valid.rfe, "black", "lightgray"),
                        cex.names = 0.7)
                axis(side     = 4,
                        las      = 2,
                        col      = "steelblue3",
                        tcl      = 0.4,
                        at       = round(seq(from = 0, to = max(tmp.vimp), length.out = 7), digits = 2),
                        labels   = round(seq(from = 0, to = max(tmp.vimp), length.out = 7), digits = 2),
                        mgp      = c(0, 0.3, 2),
                        lwd      = 1.5,
                        cex.axis = 0.7,
                        col.axis = "steelblue3")
                # tcl control the length of the tick marks. positive values will
                #     make the tick being drawn inside the plot.
                # mgp takes three values, the first one control how much line
                #     between plot and axis title the second between plot and
                #     axis labels and the third between plot and axis line.
                mtext(side = 4, text = "Variable Importance Score", col = "steelblue3", cex = 0.8, line = 2)
                par(new = TRUE)
                boxplot(x = tmp.rfe, xaxt = "n", xlab = "", ylab = "AUROC", las = 2, cex.lab = 0.8, cex.axis = 0.7, tcl = 0.4)
                
                axis(side = 3, at = 1:length(tmp.rfe), labels = (length(tmp.vimp) + 2):2, tcl = 0.4, mgp = c(0, -0.1, 1), lwd = 1.5, cex.axis = 0.7, col.axis = "peachpuff4", col = "peachpuff4")
                mtext(side = 3, text = "Number of Features Left", cex = 0.8, line = 1.1, col = "peachpuff4")
                
                
                # there is a chance that the user has defined plot title under
                # the names "main" or "title". if so use that, otherwise use a
                # pre-defined one
                if (any(is.element(c("main", "title"), names(dots)))){
                    the.title <- dots[[which(is.element(names(dots), c("main", "title")))[1]]]
                    dots <- dots[-which(is.element(names(dots), c("main", "title")))]
                }else{
                    the.title <- "Effect of Feature Elimination"
                }
                title(main = the.title, line = 2.5)
                
                
                #-------[ plotting cutoff based on strictness ]-------#
                {
                    # if user have asked for specific strictness to be plotted over
                    if(!is.null(suggestion_strictness)){
                        # extract the median of AUCs from the sivs object
                        tmp.median.AUROCs <- sapply(x$rfe,
                                                function(x){
                                                    median(sapply(x, "[[", "auc"))
                                                })
                        
                        
                        # iterate though the provided suggestion_strictness
                        for(i in suggestion_strictness){
                            # calculate the cutoff value
                            AUC_cutoff <- ((1 - i) * (max(na.omit(tmp.median.AUROCs)) - min(na.omit(tmp.median.AUROCs)))) + min(na.omit(tmp.median.AUROCs))
                            feature_cutoff <- min(which(tmp.median.AUROCs < AUC_cutoff)) - 0.5
                            
                            
                            abline(v = feature_cutoff, col = "red", lty = 2)
                            
                            
                            tmp.cutoff.msg <- paste(" Strictness:", round(i, digits = 2), " ")
                            
                            # tmp.strictness.bg <- paste(rep(intToUtf8(9608),
                            #                                  nchar(tmp.cutoff.msg)),
                            #                              collapse = "")
                            tmp.strictness.bg <- paste(rep("-",
                                                            nchar(tmp.cutoff.msg) - 2),
                                                        collapse = "")
                            # Encoding(tmp.strictness.bg) <- "ISOLatin9.enc"
                                # capabilities()
                                # l10n_info()
                                # iconvlist()
                                # list.files(system.file('enc', package = 'grDevices'))
                            mtext(text = tmp.strictness.bg,
                                    side = 3,
                                    at = feature_cutoff,
                                    line = -12,
                                    cex = 0.8,
                                    col = "white",
                                    # col = "green",
                                    padj = 0.56,
                                    las = 2,
                                    family = "mono",
                                    font = 2)
                            mtext(text = tmp.cutoff.msg,
                                    side = 3,
                                    at = feature_cutoff,
                                    line = -12,
                                    cex = 0.8,
                                    col = "red",
                                    las = 2,
                                    family = "mono",
                                    font = 2)
                        }
                    }
                }
            }
        }
    }
}
