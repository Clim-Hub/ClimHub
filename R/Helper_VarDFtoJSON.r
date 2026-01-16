#' @title Write Data Frame to JSON
#'
#' @description Writes a data frame to a JSON file. Designed to simplify creation of JSON files from variable metadata data frames.
#'
#' @param df A data frame to be written to JSON.
#' @param outfile Character string with the path to the output JSON file.
#'
#' @importFrom jsonlite toJSON
#'
#' @return A JSON file saved to the specified path.
#'
#' @author Erik Kusch
#'
#' @examples
#' NORA3_df <- Discovery_Variables(dataSet = "NORA3")
#' Helper_VarDFtoJSON(NORA3_df, "NORA3_variables.json")
#'
Helper_VarDFtoJSON <- function(df, outfile = "variables.json") {
    # Ensure character columns (avoid factors in JSON)
    df[] <- lapply(df, as.character)

    # Convert to JSON: each row = one element
    json_txt <- jsonlite::toJSON(
        df,
        pretty = TRUE,
        auto_unbox = TRUE,
        na = "null"
    )

    # Write to file
    writeLines(json_txt, outfile)
}
