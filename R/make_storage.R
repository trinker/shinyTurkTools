#' Make a Storage Base for Turk Projects
#' 
#' Make a storage directory to place related shinyTurk projects.
#' 
#' @param path A path to the storage location.
#' @param \ldots ignored.
#' @return Returns the path to the storage directory invisibly.
#' @export
#' @examples
#' \dontrun{
#'  make_storage('test_storage')
#' }
make_storage <- function (path, ...) {
    if (file.exists(path)) {
        message(paste0("\"", path, "\" already exists:\nDo you want to overwrite?\n"))
        ans <- menu(c("Yes", "No"))
        if (ans == "2") {
            stop("project aborted")
        } else {
            unlink(path, recursive = TRUE, force = FALSE)
        }
    }
    suppressWarnings(invisible(dir.create(path, recursive = TRUE)))

    write.csv(
        data.frame(key = c(''), project = c('')), 
        file = file.path(path, "key.csv"),
        row.names=FALSE
    )
   
    return(invisible(path))
}