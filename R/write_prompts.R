#' Write a Prompt rds File for Turk
#' 
#' \code{add_prompts} - Adds the ID enriched prompt .rds file directly to the project directory.
#' 
#' @param project.path A path the the projects directory.
#' @param prompts A vector of promts to show the person that will be judged.
#' @param path Path of where to put the .rds file.
#' @param \ldots ignored.
#' @export
#' @return Returns the \code{project.path} so it can be used with 
#' \code{make_project} and \code{add_responses} within a \pkg{magrittr} chain.
#' @examples 
#' temp <- tempdir()
#' prompts <- c("I am sam", "He is greg", "We are us.")
#' write_prompts(prompts = prompts, path = file.path(temp, "prompts.rds"))
#' readRDS(file.path(temp, "prompts.rds"))
#' 
#' \dontrun{
#' ## In a magrittr pipeline
#' library(dplyr)
#' prompts <- c("I am sam", "He is greg", "We are us.")
#' responses <- c("Orange", "Yellow", "Green", "Blue", "Red", "Purple")
#' 
#' make_project(
#'     path = "L:/swiper/shinyTurk/turk_items/ce_question_tagging_validation",
#'     key.value = 9999
#' ) %>%
#'     add_prompts(prompts) %>%
#'     add_responses(
#'         responses = responses,
#'         googleform_url = 'https://docs.google.com/forms/d/1iLiHtE6xFUzYMN8iB3AXD5xRYVY_oI5Cps1uaujZGng', 
#'         type = "checkbox"        
#'     )   
#' }
add_prompts <- function(projects.path, prompts, ...){

    write_prompts(prompts, file.path(projects.path, "prompts.rds"))
    return(invisible(projects.path))
}

#' Write a Prompt rds File for Turk
#' 
#' \code{write_prompts} - Create an ID enriched prompt data.frame and save as .rds file.
#' 
#' @export
write_prompts <- function(prompts, path = "prompts.rds", ...){

    x <- seq_along(prompts)
    mn <- nchar(max(x))
    id <- paste0(sapply(x, function(y) paste(rep(0, mn - nchar(y)), collapse="")), x)
    saveRDS(data.frame(id = id, prompts = prompts, stringsAsFactors = FALSE), file = path)

}


    
