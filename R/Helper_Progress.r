#' @title Create a progress bar to be used for progress tracking in package functions
#'
#' @description Creates a progress bar as defined by the progress package for use in other functions.
#'
#' @param iterLength Numeric. How many iterations the progress bar ought to track.
#' @param text Character. What text to display as the action being performed in the progress bar.
#'
#' @importFrom progress progress_bar
#'
#' @return A progress progress bar object.
#'
#' @author Erik Kusch
#'
#' @examples
#' Iter_n <- 100
#' pb <- Helper_Progress(iterLength = Iter_n, text = "Elapsing")
#' for (i in 1:Iter_n) {
#'     Sys.sleep(0.05)
#'     pb$tick(tokens = list(layer = i))
#' }
Helper_Progress <- function(iterLength, text) {
    progress_bar$new(
        format = paste(text, "(:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta"),
        total = iterLength,
        width = getOption("width"),
        clear = FALSE
    )
}
