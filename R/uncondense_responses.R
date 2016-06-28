#' Separate and Concatenate Multiple Responses
#' 
#' \code{uncondense_responses} - Separate multiple responses
#' 
#' @param x A vector of concatenated responses or a vector of responses to concatenate.
#' @param \ldots ignored.
#' @export
#' @examples 
#' (x <- condense_responses(LETTERS[1:5]))
#' uncondense_responses(x)
uncondense_responses <- function(x, ...){

    lapply(strsplit(x, ">>>,<<<"), function(y) gsub(">>>|<<<", "", y))
}

#' Separate and Concatenate Multiple Responses
#' 
#' \code{condense_responses} - Concatenate multiple responses
#' 
#' @export
condense_responses <- function(x, ...){
    if (length(x) > 1) {
        x <- paste(paste0('<<<', x, '>>>'), collapse=',')
    }
    x
}

