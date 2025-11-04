#' Check Validity of User Input
#' @title Check Validity of User Input
#' @description Loops over contents of a named list of lists where each sublist contains the pointers Input, Allowed, and Operator.
#'
#' @param inputCheck Optional, list. A named list of lists where each sublist contains three elements: Input (the value to check), 
#'   Allowed (the allowed values or range), and Operator (either "in" for membership or "exceeds" for range checks).
#'
#' @return Nothing. Only generates an error if a check did not succeed.
#' 
#' @author Erik Kusch
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
#' inputCheck <- list(
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
#' Helper_InputChecker(inputCheck)
Helper_InputChecker <- function(inputCheck) {
    # Check each element in the input list
    Checked_ls <- lapply(inputCheck, FUN = function(Loop_ls) {
        CheckReturn <- FALSE

        # Check if input values are in allowed values
        if (Loop_ls$Operator == "in") {
            CheckReturn <- Loop_ls$Input %in% Loop_ls$Allowed
            CheckReturn <- sum(!CheckReturn == 0)
        }

        # Check if input range is within allowed bounds
        if (Loop_ls$Operator == "exceeds") {
            CheckReturn <- (sum(
                Loop_ls$Input[1] < Loop_ls$Allowed[1], # check lower bound
                Loop_ls$Input[2] > Loop_ls$Allowed[2] # check upper bound
            ) == 0)
        }

        CheckReturn
    })
    Checked_vec <- unlist(Checked_ls)

    # Stop execution if any checks fail
    if (length(Checked_vec[!Checked_vec]) > 0) {
        stop(paste0("Your function input seems misspecified. Request validation fails for ", 
                   paste(names(Checked_vec)[!Checked_vec], collapse = ","), 
                   ". Please reinvestigate your specification of this/these argument(s)."))
    }
}