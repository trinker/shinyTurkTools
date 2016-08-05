#' Create a Turk Task Project
#' 
#' Creates a turk task project in the project directory, adds an 
#' `instructions.md`, and optionally adds a key value to the `key.csv` file.
#' 
#' @param path A path to the projects dirctory plau the project name.
#' @param key.value A value (key code) that will be used in to log into the 
#' project in the shinyTurk.
#' @param key.path A path the where `key.csv` is.
#' @param additional.cols Additional column  values to add to the key.  For 
#' example the key may contain an additional institution name column.  
#' @param \ldots ignored.
#' @export
#' @return Returns the \code{project.path} so it can be used with 
#' \code{add_prompts} and \code{add_responses} within a \pkg{magrittr} chain.
#' @examples 
#' \dontrun{
#' make_project(
#'     path = "L:/swiper/shinyTurk/turk_items/ce_question_tagging_validation",
#'     key.value = 9998
#' )
#' }
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
make_project <- function(path, key.value, key.path = file.path(dirname(path), 'key.csv'), additional.cols = NULL, ...){

    path <- gsub("\\s+", "_", path)
    if (path == Sys.getenv("R_HOME")) stop("path can not be `R_HOME`")

    if (!missing(key.value)) {
        key <- read.csv(key.path, colClasses = c('character', 'character'))
        if (as.character(key.value) %in% key[[1]]) {
            stop(paste0("`key.value` can not match a key already present in ", key.path, 
                ".\n\nPlease change `key.value` or edit this file manually."))
        }
        if (basename(path) %in% key[[2]]) {
            stop(paste0("The project name, `", basename(path), 
                "`, matches a project name already found in", key.path, 
                ".\n\nPlease change the project name or edit this file manually."))
        }
    }

    if (file.exists(path)) {
        message(paste0("\"", path, "\" already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No"))
        if (ans == "2") {
            stop("project aborted")
        }
        else {
            unlink(path, recursive = TRUE, force = FALSE)
        }
    }
    suppressWarnings(invisible(dir.create(path, recursive = TRUE)))
   
    if (!missing(key.value)) {
        key[nrow(key) + 1,] <- c(key.value, basename(path), additional.cols)
        write.csv(key, file = key.path, row.names=FALSE)
    }

    cat("{Markdown formatted instructions go here}\n", file=file.path(path, 'instructions.md'))

    return(path)
}


