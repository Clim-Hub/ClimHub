### PROGRESS BAR CREATION ========================================================
#' Create a progress bar to be used for progress tracking in package functions
#'
#' Creates a progress bar as defined by the progress package for use in other functions.
#'
#' @param IterLength Numeric. How many iterations the progress bar ought to track.
#' @param Text Character. What text to display as the action being performed in the progress bar.
#'
#' @importFrom progress progress_bar
#'
#' @return A progress progress bar object.
#'
#' @examples
#' Iter_n <- 100
#' pb <- Helper.Progress(IterLength = Iter_n, Text = "Elapsing")
#' for(i in 1:Iter_n){
#'     Sys.sleep(0.25)
#'     pb$tick(tokens = list(layer = i))
#' }
#' @export
Helper.Progress <- function(IterLength, Text) {
    progress_bar$new(
        format = paste(Text, "(:current/:total) | [:bar] Elapsed: :elapsed | Remaining: :eta"),
        total = IterLength,
        width = getOption("width"),
        clear = FALSE
    )
}