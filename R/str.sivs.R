#' Structure of sivs object
#' 
#' @description This function shows the structure of a an object of either class
#' "sivs" or "list" and shows the internal structure of the object in
#' human-readable format. sivs object is a complex S3 object and it might be a
#' deterrent to users to get to know if better. This function is aiming to
#' facilitate the experience of user.
#' 
#' @param object An object of class "sivs" or "list".
#' @param max_depth A numerical value of length 1 indicating how many layers the
#' function should dive into.
#' @param max_leaves A numericl vector of length 1 indicating how many of the
#' objects of each list should be shown.
#' @param max_width A numerical vector of length 1 indicating the maximum width
#' of the terminal that should be used by the function. If there are any lines
#' larger than this value, they will be truncated. Default is the terminal size
#' that is retuned by R.
#' @param ... potential further arguments (required for Method/Generic reasons).
#' 
#' @return The function uses `cat` to output general structure of sivs object
#' in human readable format in a tree-like structure.
#'
#' @examples
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
#' str(sivs_obj)
#' 
#' 
#' @export

str.sivs <- function(object,
                     max_depth = 2,
                     max_leaves = 2,
                     max_width = options("width")$width,
                     ...){

    #-------[ initial settings ]-------#
    {
        init_accaptable_classes_object <- c("sivs", "list")
    }
    
    
    #-------[ check input ]-------#
    {
        #-------[ object ]-------#
        {
            if(!any(is.element(class(object), init_accaptable_classes_object))){
                stop("The argument `object` should be a list-like object, meaning it should have one of the following classes:\n",
                        paste(init_accaptable_classes_object, collapse = ", "))
            }
        }
        
        #-------[ max_depth ]-------#
        {
            if(!is.numeric(max_depth)){
                stop("The argument `max_depth` must be a numeric vector of length 1")
            }
        }
    }
    
    
    #-------[ internal functions ]-------#
    {
        func_explore <- function(lst, layer = "", showThreeDots = FALSE){
            
            for(i in seq_len(length(lst))){
                
                #-------[ prepare non-list object info ]-------#
                {
                    if(inherits(lst[[i]], what = c("data.frame", "matrix"))){
                        tmp_info <- paste0(nrow(lst[[i]]), " obs, and ", ncol(lst[[i]]), " variables")
                        
                    }else if(inherits(lst[[i]], what = "factor")){
                        tmp_info <- paste0(paste0("levels=c(", paste(levels(lst[[i]]), collapse = ", "), ") "), "length = ", length(lst[[i]]))
                        
                    }else if(inherits(lst[[i]], what = c("numeric", "integer"))){
                        tmp_info <- paste0("min=", round(x = min(lst[[i]]), digits = 2),
                                            ", med=", round(x = median(lst[[i]]), digits = 2),
                                            ", ave=", round(x = mean(lst[[i]]), digits = 2),
                                            ", max=", round(x = max(lst[[i]]), digits = 2))
                        
                    }else if(inherits(lst[[i]], what = "character")){
                        tmp_info <- paste0("length=", length(lst[[i]]), paste0(" [", paste(head(lst[[i]], 10), collapse = ", "),  ifelse(length(lst[[i]])>10, ", ...", ""), "]"))
                        
                    }else if(inherits(lst[[i]], what = "list")){
                        tmp_info <- ""
                        
                    }else{
                        tmp_info <- paste0("class = c(", paste0(class(lst[[i]]), collapse = ", "), ")}: ")
                        
                    }
                }
                
                # create indentation layer to be used and passed to the next recusion
                tmp_layer <- paste0(layer,
                                    ifelse((i == length(lst)) & (showThreeDots == FALSE),
                                            "    ",
                                            "\u2502   "))
                
                
                # genaret the output text
                output <- paste0(layer,
                                    ifelse((i == length(lst)) & (showThreeDots == FALSE),
                                        "\u2514\u2500\u2500 ",
                                        "\u251C\u2500\u2500 "),
                                    "[", ifelse(is.null(names(lst)[i]), "", names(lst)[i]), "]: ",
                                    tmp_info)
                
                # make sure the string does not pass the width of the screen (no line overflow)
                if(nchar(output) > max_width){
                    output <- paste0(substr(x = output, start = 1, stop = max_width-3), "...", collapse = "")
                }
                
                cat(output, "\n")
                
                # if the class of the object of this iteration is among those that we should dive into
                if(any(is.element(class(lst[[i]]), init_accaptable_classes_object))){
                    # recursion
                    func_explore(head(x = lst[[i]], n = max_leaves), layer = tmp_layer, showThreeDots = ifelse(length(lst[[i]]) > max_leaves, TRUE, FALSE))
                    
                }
                
                # add extra space between lists sith leaves
                if((i == length(lst)) & (showThreeDots == FALSE)){
                    if(nchar(trimws(layer)) != 0){
                        cat(layer, "\n")
                    }
                    
                }
            }
            
            
            if(showThreeDots){
                cat(paste0(layer, "\u2514\u2500\u2500 ...\n"))
                cat(layer, "\n")
            }
        }
    }
    
    
    #-------[ process ]-------#
    {
        # begin the recursive function
        func_explore(object)
        
        # print any comment the object has
        cat("\ncomment: ", ifelse(is.null(comment(object)), "<empty>", comment(object)), "\n")
    }
}
