#' Write a Response rds File for Turk
#' 
#' \code{add_responses} - Adds the list of responses, google form link, and 
#' response type .rds file directly to the project directory.
#' 
#' @param project.path A path the the projects directory.
#' @param responses A vector of responses to eb repeated each time 'next' is clicked.
#' @param googleform_url A google form url (google form should be a url to a google form with 
#' a text box for (a) user, (b) question ID (c) the clicked response in that order).
#' @param path Path of where to put the .rds file.
#' @param type The type of response.  Currently (radio is supported).  Plan to add slider 
#' (numeric) and checkbox types as well.
#' @param \ldots Other arguments passed to \code{sliderInput} (\code{c("min", "max", "value", "step")})
#' @export
#' @return Returns the \code{project.path} so it can be used with 
#' \code{make_project} and \code{add_prompts} within a \pkg{magrittr} chain.
#' @examples
#' temp <- tempdir()
#' 
#' ## radio responses
#' responses1 <- c("Very Poor", "Poor", "Neutral", "Good", "Very Good")
#' 
#' write_responses(
#'     responses = responses1, 
#'     googleform_url = 'https://docs.google.com/forms/d/1iLiHtE6xFUzYMN8iB3AXD5xRYVY_oI5Cps1uaujZGng', 
#'     path = file.path(temp, "responses_radio.rds"),
#'     type = "radio"
#' )
#' readRDS(file.path(temp, "responses_radio.rds"))
#' 
#' ## checkbox responses
#' responses2 <- c("Orange", "Yellow", "Green", "Blue", "Red", "Purple")
#' 
#' write_responses(
#'     responses = responses2, 
#'     googleform_url = 'https://docs.google.com/forms/d/1iLiHtE6xFUzYMN8iB3AXD5xRYVY_oI5Cps1uaujZGng', 
#'     path = file.path(temp, "responses_checkbox.rds"),
#'     type = "checkbox"
#' )
#' readRDS(file.path(temp, "responses_checkbox.rds"))
#' 
#' ## slider responses
#' write_responses(
#'     responses = "Rate the statement poor-1 to great-5", 
#'     googleform_url = 'https://docs.google.com/forms/d/1iLiHtE6xFUzYMN8iB3AXD5xRYVY_oI5Cps1uaujZGng', 
#'     path = file.path(temp, "responses_slider.rds"),
#'     type = "slider",
#'     max = 5, 
#'     min = 1,
#'     value = 3,
#'     step = .001
#' )
#' readRDS(file.path(temp, "responses_slider.rds"))
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
add_responses <- function(projects.path, responses, googleform_url, type = "radio", ...){

    write_responses(responses, responses=responses, googleform_url=googleform_url, 
        path=file.path(projects.path, "responses.rds"), type =type, ...)
    return(invisible(projects.path))
}


#' Write a Response rds File for Turk
#' 
#' \code{write_responses} - Create a list of responses, google form link, and response type and save as .rds file.
#' 
#' @export
write_responses <- function(responses, googleform_url, path = "responses.rds", type = "radio", ...){

    stopifnot(type %in% c('radio', 'slider', 'checkbox'))
    dots <- list(...)

    ## error handling different types
    ## error ahndling for slider type
    if (type == "slider" && !all(c("min", "max", "value", "step") %in% names(dots))) {
        stop('If \'type = slider\' add arguments for c("min", "max", "value") as well')
    }

    saveRDS(list(responses = responses, type = list(type=type, ...), googleform_url = googleform_url), file = path)

}
