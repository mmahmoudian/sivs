#' Validate sivs Object
#' 
#' A function to validate if the given object is truely from sivs function
#' 
#' @param object Idealy the object that is produced by the sivs function.
#' 
#' @return This function will return TRUE if it detects the function is truely a
#' sivs object, otherwise it will return FALSE.
#'
#' @export

is.sivs <- function(object){
    
    #-------[ initialize some variables ]-------#
    {
        # define the list of accepatble names that the S3 object should have
        acceptable.object.names <- c("iterative.res", "selection.freq", "vimp", "rfe", "rfe.issues", "run.info", "x", "y")
        
        # define the types that this function can plot
        acceptable.types <- c("frequency", "coef", "rfe")
    }
    
    
    #-------[ object ]-------#
    {
        final <- TRUE
        
        # check if the given object has the correct class
        if((!inherits(x = object, what = "sivs")) | (!is.list(object))){
            final <- FALSE
        
        # make sure if the object has the correct structure
        }else{
            if(!all(is.element(names(object), acceptable.object.names))){
                final <- FALSE
            }
        }
    }
    
    return(final)
}
