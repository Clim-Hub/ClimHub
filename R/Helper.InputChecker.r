### DOWNLOAD ARGUMENT INPUT CHECKING ========================================================
#' Check validity of user input
#'
#' Loops over contents of a named list of lists where each sublist contains the pointers Input, Allowed, and Operator.
#'
#' @param InCheck_ls
#'
#' @return Nothing. Only generates an error if a check did not succeed.
#'
#' @examples
#' Variable <- "Maximum Air Temperature"
#' DateStart <- "1971-01-01"
#' DateStop <- "1971-08-31"
#' Model <- "CNRM_CCLM"
#' Scenario <- "rcp85"
#'
#' Start <- as.POSIXct(paste0(DateStart, "00:00:00"), tz = "UTC")
#' Stop <- as.POSIXct(paste0(DateStop, "00:00:00"), tz = "UTC")
#'
#' InCheck_ls <- list(
#'     Variable = list(
#'         Input = Variable,
#'         Allowed = Meta.Variables("KlimaiNorge2100")$name,
#'         Operator = "in"
#'     ),
#'     Time = list(
#'         Input = c(Start, Stop),
#'         Allowed = Meta.QuickFacts("KlimaiNorge2100")$time$extent,
#'         Operator = "exceeds"
#'     ),
#'     Models = list(
#'         Input = Model,
#'         Allowed = Meta.QuickFacts("KlimaiNorge2100")$models,
#'         Operator = "in"
#'     ),
#'     Scenarios = list(
#'         Input = Scenario,
#'         Allowed = Meta.QuickFacts("KlimaiNorge2100")$scenarios,
#'         Operator = "in"
#'     )
#' )
#'
#' Helper.InputChecker(InCheck_ls)
#' @export
Helper.InputChecker <- function(InCheck_ls) {
    Checked_ls <- lapply(InCheck_ls, FUN = function(Loop_ls) {
        CheckReturn <- FALSE

        if (Loop_ls$Operator == "in") {
            CheckReturn <- Loop_ls$Input %in% Loop_ls$Allowed
            CheckReturn <- sum(!CheckReturn == 0)
        }

        if (Loop_ls$Operator == "exceeds") {
            CheckReturn <- (sum(
                Loop_ls$Input[1] < Loop_ls$Allowed[1], # check lower bound
                Loop_ls$Input[2] > Loop_ls$Allowed[2] # check upper bound
            ) == 0)
        }

        CheckReturn
    })
    Checked_vec <- unlist(Checked_ls)

    if (length(Checked_vec[!Checked_vec]) > 0) {
        stop(paste0("Your function input seems misspecified. Request validation fails for ", paste(names(Checked_vec)[!Checked_vec], collapse = ","), ". Please reinvestigate your specification of this/these argument(s)."))
    }
}
