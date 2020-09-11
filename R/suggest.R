#' Cutoff Suggestion for sivs Object
#'
#' A function to suggest the user a set of features based on sivs object and the provided strictness
#'
#' @param object The object that is produced by the sivs function.
#' @param strictness A numerical vector of length 1 showing how strict the suggestion should be, ranging from 0 to 1 where 0 is less strict and 1 is the most strict. Default value is 0.01. For more information, check the Details section.
#' @param plot A logical vector of length 1 indicating whether the suggestion should also be plotted in the "rfe" plot. The same plot can be generated via plot.sivs() function when the suggestion_strictness is set according to the strictness argument of this function.
#'
#' @details This function tries to narrow down the list of VIMP features in sivs object into a smaller feature list based on provided strictness coefficient. This function practically defines a threshold for AUCs in the rfe (Recursive Feature Elimination) step of sivs. Any run with any set of features that are above the AUC threshold will be eliminated and all the features that were contributing into having an AUC lower than the threshold are returned. The cutoff is defined as:
#'     ((1 - strictness) * (max(median_AUROCs) - min(median_AUROCs))) + min(median_AUROCs)
#'     where median_AUROCs is the median of AUROCs for each run in rfe step of
#'     sivs. Note that this function is supposed to give the feature space based
#'     on the cutoff and hence the intercept (if exists in the VIMP) will be
#'     excluded from the output.
#'
#' @return A character vector that contains the names of suggested features based
#' on the defined strictness.
#' 
#' @examples
#' \dontrun{
#' # Defult use
#' suggest(sivs_object)
#' 
#' # get the suggested features and also plot it with strictness of 0.01
#' suggest(object = sivs_object, strictness = 0.01, plot = TRUE)
#' }
#'
#' @export

suggest <- function(object, strictness = 0.01, plot = FALSE){
    
    #-------[ check input ]-------#
    ## make sure the input value are good enough for this function
    {
        #-------[ object ]-------#
        {
            if(!is.sivs(object = object)){
                stop("This function can only handle an object from type \"sivs\".")
            }
            
            
            # make sure the rfe result is part of the object
            if(!is.element("rfe", names(object))){
                ## This can happen if the sample size of the data that was
                ## provided to sivs was so small that sivs has skipped going
                ## through the rfe step.
                
                # complain
                stop("The provided object does not have rfe section and as ",
                        "the result the rfe cannot be plotted. During the sivs run You have been warned that sivs function could not perform the rfe step, perhaps due to low number of remained features.")
            }
        }
        
        
        #-------[ strictness ]-------#
        {
            if((strictness < 0) | (strictness > 1) | (length(strictness) != 1)){
                stop("The value for the 'strictness' argument should be a numeric vector of length 1 with value between 0 and 1.")
            }
        }
        
        
        #-------[ plot ]-------#
        {
            if(!is.logical(plot)){
                stop("The value provided for the 'plot' argument should be TRUE or FALSE")
            }
        }
    }
    
    
    #-------[ main ]-------#
    {
        if(plot){
            plot.sivs(object = object,
                        type = "rfe",
                        suggestion_strictness = strictness)
        }
        
        # this is special case and should not happen, but if user really
        #  wants to have ZERO strictness, then we give them all features!
        if(strictness == 0){
            final <- names(object$vimp)
        }else{
            # extract the median of AUCs from the sivs object
            median_AUROCs <- sapply(object$rfe,
                                    function(x){
                                        median(sapply(x, "[[", "auc"))
                                    })
            
            # calculate the cutoff value
            AUC_cutoff <- ((1 - strictness) * (max(na.omit(median_AUROCs)) - min(na.omit(median_AUROCs)))) + min(na.omit(median_AUROCs))
            
            VIMP_features <- names(sort(x = object$vimp[object$vimp > 0], decreasing = TRUE))
            
            last_suggested_feature <- which(is.element(VIMP_features, head(names(which(median_AUROCs < AUC_cutoff)), n = 1)))
            
            final <- VIMP_features[1:last_suggested_feature]
        }
        
        
        return(final)
    }
}