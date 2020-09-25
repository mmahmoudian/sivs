#' Extract Coefficients from sivs object
#'
#' @description A function to extract the coefficients of "iterative.res" step
#' or any part of "rfe" such as "sivs_object$rfe$baseline" from a sivs object.
#'
#' @param object An object of class "sivs"
#' @param step  A a character string of length 1. It should either specify the
#' step ("iterative.res" or "rfe"), or step$subsetp (e.g "rfe$baseline").
#' @param ... potential further arguments (required for Method/Generic reasons).
#' 
#' @examples
#' \dontrun{
#' # getting the coefficients of features for the baseline runs in rfe
#' coef(object = sivs_object, step = "rfe$baseline")
#' }
#' 
#' @export


coef.sivs <- function(object, step = "iterative.res", ...){
    
    #-------[ initial settings ]-------#
    {
        # valid_step_values <- c("iterative.res", "rfe", "rfe\\$.+")
        valid_step_values <- c("iterative.res", "rfe(\\$.+)?")
    }
    
    #-------[ check input ]-------#
    {
        #-------[ object ]-------#
        {
            # if the provided object is not of class sivs
            if(!any(is.element(class(object), c("sivs")))){
                stop("The object provided for argument `object` is not of class sivs! Use sivs::sivs() to generate valid sivs object.")
            }
        }
        
        
        #-------[ step ]-------#
        {
            # make sure the step is character
            if(!is.character(step)){
                stop("The value provided for argument `step` should be a character vector of length 1.")
            }
            
            # make sure the step has length of 1
            if(length(step)>1){
                stop("The value provided for argument `step` should be a character vector of length 1.")
            }
            
            # make sure the step match the general valid patterns
            if(!any(sapply(valid_step_values, function(p){grepl(x = step, pattern = p)}))){
                stop("The value provided for argument `step` is not valid. It should be either be 'iterative.res', 'rfe' or 'rfe$xxxx' where `xxxx` is an element of the rfe step")
            }
            
            # if step value contains $ (dollar sign)
            if(any(grepl(x = step, pattern = "\\$"))){
                step_parts <- unlist(strsplit(x = step, split = "\\$"))
                substep <- step_parts[2]
                step <- step_parts[1]
                
                if(!any(is.element(substep, names(object[[step]])))){
                    # get the function call
                    function.call <- match.call()
                    
                    stop("The value provided for argument `step` is not valid. It should be either be 'iterative.res', 'rfe' or 'rfe$xxxx' where `xxxx` is an element of the rfe step. The one you provided (after $) is not among the available substeps in the provided sivs_object. Try the following to see the valid options:\n\n\t",
                            paste0("names(", function.call$object, "$", step, ")"))
                }
                
            }else if(step == "rfe"){
                substep <- "baseline"
                
            }else{
                substep <- NULL
                
            }
        }
    }
    
    
    #-------[ main ]-------#
    {
        # if user wants to operate in iterative.res
        if(is.null(substep)){
            
            coef_df <- Reduce(function(...){ merge(...,
                                                    by = "names",
                                                    all = TRUE) },
                                sapply(names(object[[step]]),
                                        FUN = function(item) {
                                            temp <- object[[step]][[item]]$coef
                                            
                                            if(is.logical(temp)){
                                                temp <- data.frame(names = NA, col2 = NA, stringsAsFactors = FALSE)
                                            }
                                            
                                            colnames(temp)[2] <- paste0("coef.", item)
                                            return(temp)
                                        },
                                        simplify = FALSE))
            
            
            # check if user have specified substep (basically if user wants to operate in rfe)
        }else{
            coef_df <- Reduce(function(...){ merge(...,
                                                    by = "names",
                                                    all = TRUE) },
                                sapply(names(object[[step]][[substep]]),
                                        FUN = function(item) {
                                            
                                            temp <- object[[step]][[substep]][[item]]$coef
                                            
                                            if(is.logical(temp)){
                                                temp <- data.frame(names = NA, col2 = NA, stringsAsFactors = FALSE)
                                            }
                                            
                                            colnames(temp)[2] <- paste0("coef.", item)
                                            return(temp)
                                        },
                                        simplify = FALSE))
        }
        
        # remove the row that has NA in the feature name (this happens when the ML code annot converge and we have to insert NA as coefficient)
        coef_df <- coef_df[-which(is.na(coef_df[, "names"])), ]
        
        return(coef_df)
    }
    
}
