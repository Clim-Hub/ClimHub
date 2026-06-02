#!/usr/bin/env Rscript
# Refresh .github/ArgumentUsage.xlsx from the current package source.
# The workbook is treated as a template so row order, column order, widths,
# styles, and legacy labels are preserved where possible. Function signatures
# come from R/, argument descriptions come from man/*.Rd, and the workbook is
# only rewritten when the generated XML actually changes.

args <- commandArgs(trailingOnly = TRUE)
xlsx_path <- if (length(args) >= 1) args[[1]] else ".github/ArgumentUsage.xlsx"

normalize_ws <- function(x) {
    trimws(gsub("[[:space:]]+", " ", x))
}

normalize_arg_key <- function(x) {
    x <- normalize_ws(x)
    x[x %in% c("...", "\u2026", "\\dots", "\\dots{}")] <- "..."
    x
}

xml_escape <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub("\"", "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&apos;", x, fixed = TRUE)
    x
}

xml_unescape <- function(x) {
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("&quot;", "\"", x, fixed = TRUE)
    x <- gsub("&apos;", "'", x, fixed = TRUE)
    x <- gsub("&amp;", "&", x, fixed = TRUE)
    x
}

read_text <- function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "")
}

write_text <- function(path, text) {
    writeLines(text, path, useBytes = TRUE)
}

extract_matches <- function(text, pattern) {
    hits <- gregexpr(pattern, text, perl = TRUE)
    if (hits[[1]][1] == -1) {
        return(character())
    }
    regmatches(text, hits)[[1]]
}

extract_first <- function(text, pattern) {
    match <- regexec(pattern, text, perl = TRUE)
    groups <- regmatches(text, match)[[1]]
    if (length(groups) < 2) {
        return(NA_character_)
    }
    groups[[2]]
}

col_from_number <- function(index) {
    stopifnot(index >= 1)
    result <- character()
    while (index > 0) {
        remainder <- (index - 1) %% 26
        result <- c(intToUtf8(65 + remainder), result)
        index <- (index - 1) %/% 26
    }
    paste(result, collapse = "")
}

col_to_number <- function(label) {
    chars <- utf8ToInt(label)
    total <- 0L
    for (char in chars) {
        total <- total * 26L + (char - 64L)
    }
    total
}

get_named_value <- function(values, name) {
    if (name %in% names(values)) {
        return(unname(values[[name]]))
    }
    ""
}

rd_flatten <- function(node) {
    if (is.null(node)) {
        return("")
    }
    if (is.character(node)) {
        return(paste(node, collapse = ""))
    }
    if (!is.list(node)) {
        return(as.character(node))
    }
    paste(vapply(node, rd_flatten, character(1)), collapse = "")
}

parse_function_specs <- function(r_dir = "R") {
    # Discover top-level package functions directly from source without
    # evaluating their bodies. Dot-prefixed lifecycle hooks are skipped.
    files <- sort(list.files(r_dir, pattern = "[.][Rr]$", full.names = TRUE))
    specs <- list()

    for (file in files) {
        expressions <- as.list(parse(file = file, keep.source = FALSE))
        for (expr in expressions) {
            if (!is.call(expr) || length(expr) < 3) {
                next
            }
            if (!identical(as.character(expr[[1]]), "<-")) {
                next
            }
            if (!is.symbol(expr[[2]]) || !is.call(expr[[3]])) {
                next
            }
            if (!identical(expr[[3]][[1]], as.name("function"))) {
                next
            }

            function_name <- as.character(expr[[2]])
            if (startsWith(function_name, ".")) {
                next
            }

            function_object <- eval(expr[[3]], envir = baseenv())
            formal_names <- names(formals(function_object))
            if (is.null(formal_names)) {
                formal_names <- character()
            }
            specs[[function_name]] <- formal_names
        }
    }

    specs
}

parse_rd_arguments <- function(function_names, man_dir = "man") {
    # Extract the canonical argument text from each generated Rd file.
    docs <- list()

    for (function_name in function_names) {
        rd_path <- file.path(man_dir, paste0(function_name, ".Rd"))
        if (!file.exists(rd_path)) {
            stop("Missing Rd file for function: ", function_name)
        }

        rd <- suppressWarnings(tools::parse_Rd(rd_path))
        rd_tags <- vapply(rd, function(node) attr(node, "Rd_tag"), character(1))
        argument_section_index <- which(rd_tags == "\\arguments")

        if (!length(argument_section_index)) {
            docs[[function_name]] <- stats::setNames(character(), character())
            next
        }

        argument_section <- rd[[argument_section_index[[1]]]]
        item_tags <- vapply(argument_section, function(node) attr(node, "Rd_tag"), character(1))
        items <- argument_section[item_tags == "\\item"]

        descriptions <- character()
        argument_names <- character()

        for (item in items) {
            argument_name <- normalize_arg_key(rd_flatten(item[[1]]))
            description <- normalize_ws(rd_flatten(item[[2]]))

            if (!nzchar(argument_name)) {
                next
            }

            argument_names <- c(argument_names, argument_name)
            descriptions <- c(descriptions, description)
        }

        if (anyDuplicated(argument_names)) {
            stop("Duplicate argument documentation in ", rd_path)
        }

        docs[[function_name]] <- stats::setNames(descriptions, argument_names)
    }

    docs
}

validate_documentation <- function(function_specs, rd_docs) {
    # Fail fast if a function signature and its Rd file disagree.
    for (function_name in names(function_specs)) {
        formal_keys <- normalize_arg_key(function_specs[[function_name]])
        doc_keys <- names(rd_docs[[function_name]])

        missing_docs <- setdiff(formal_keys, doc_keys)
        extra_docs <- setdiff(doc_keys, formal_keys)

        if (length(missing_docs) || length(extra_docs)) {
            message("Formal arguments: ", paste(formal_keys, collapse = ", "))
            message("Documented args: ", paste(doc_keys, collapse = ", "))
            stop(
                "Function documentation mismatch for ", function_name,
                if (length(missing_docs)) paste0("; missing docs: ", paste(missing_docs, collapse = ", ")) else "",
                if (length(extra_docs)) paste0("; extra docs: ", paste(extra_docs, collapse = ", ")) else ""
            )
        }
    }
}

parse_shared_strings <- function(path) {
    # The workbook uses shared strings for all populated cells, so these need
    # to be decoded before the existing sheet can be matched and rebuilt.
    xml <- read_text(path)
    string_nodes <- extract_matches(xml, "(?s)<si>.*?</si>")

    vapply(string_nodes, function(node) {
        text_nodes <- extract_matches(node, "(?s)<t(?: xml:space=\"preserve\")?>.*?</t>")
        pieces <- vapply(text_nodes, function(text_node) {
            xml_unescape(extract_first(text_node, "(?s)^<t(?: xml:space=\"preserve\")?>(.*?)</t>$"))
        }, character(1))
        paste(pieces, collapse = "")
    }, character(1))
}

parse_sheet_template <- function(path, shared_strings) {
    # Read the existing sheet so current argument rows and function columns can
    # be preserved instead of re-sorted from scratch on each run.
    sheet_xml <- read_text(path)
    row_nodes <- extract_matches(sheet_xml, "(?s)<row\\b[^>]*>.*?</row>")
    rows <- list()

    for (row_node in row_nodes) {
        row_number <- as.integer(extract_first(row_node, 'r="([0-9]+)"'))
        cell_nodes <- extract_matches(row_node, "(?s)<c\\b[^>]*>.*?</c>")
        row_values <- character()

        for (cell_node in cell_nodes) {
            ref <- extract_first(cell_node, 'r="([A-Z]+[0-9]+)"')
            cell_type <- extract_first(cell_node, 't="([^"]+)"')
            raw_value <- extract_first(cell_node, "(?s)<v>(.*?)</v>")

            if (is.na(ref) || is.na(raw_value)) {
                next
            }

            value <- if (!is.na(cell_type) && identical(cell_type, "s")) {
                shared_strings[as.integer(raw_value) + 1L]
            } else {
                xml_unescape(raw_value)
            }

            row_values[ref] <- value
        }

        rows[[as.character(row_number)]] <- row_values
    }

    header_values <- rows[["1"]]
    header_refs <- names(header_values)
    header_cols <- sub("[0-9]+$", "", header_refs)
    header_order <- order(vapply(header_cols, col_to_number, integer(1)))
    header_values <- header_values[header_order]
    header_cols <- header_cols[header_order]

    data_row_numbers <- sort(setdiff(as.integer(names(rows)), 1L))
    argument_rows <- lapply(data_row_numbers, function(row_number) {
        row_values <- rows[[as.character(row_number)]]
        list(
            row_number = row_number,
            display = get_named_value(row_values, paste0("A", row_number)),
            usage = get_named_value(row_values, paste0("B", row_number))
        )
    })

    list(
        xml = sheet_xml,
        headers = unname(header_values[!(header_cols %in% c("A", "B"))]),
        argument_rows = argument_rows
    )
}

legacy_function_aliases <- c(
    "Helper_InputCheck" = "Helper_InputChecker",
    "Metrics_ETTCDI" = "Metrics_ETCCDI"
)

argument_description_overrides <- c(
    extent = paste(
        "Optional. The extent to subset the data to, in coordinates of the projection",
        "or decimal degrees of longitude and latitude. A numeric vector of length 4",
        "with values minimum and maximum X/longitude and minimum and maximum",
        "Y/latitude, in that order. Valid ranges depend on the dataset being queried.",
        "Defaults to NULL, returning full spatial range of data."
    ),
    CFVariable = paste(
        "A CFVariable object to be processed."
    )
)

order_functions <- function(existing_headers, current_functions) {
    # Preserve the current workbook column order, apply known legacy header
    # mappings, expand the old grouped Discovery column, and append anything new.
    ordered <- character()
    used <- character()
    removed_headers <- character()
    replaced_headers <- character()
    discovery_functions <- current_functions[grepl("^Discovery_", current_functions)]

    add_function <- function(function_name) {
        if (function_name %in% current_functions && !(function_name %in% used)) {
            ordered <<- c(ordered, function_name)
            used <<- c(used, function_name)
        }
    }

    for (header in existing_headers) {
        if (header %in% current_functions) {
            add_function(header)
            next
        }

        if (header %in% names(legacy_function_aliases)) {
            mapped_name <- unname(legacy_function_aliases[[header]])
            if (mapped_name %in% current_functions) {
                add_function(mapped_name)
                replaced_headers <- c(replaced_headers, paste0(header, " -> ", mapped_name))
            } else {
                removed_headers <- c(removed_headers, header)
            }
            next
        }

        if (identical(header, "Discovery_*")) {
            remaining_discovery <- discovery_functions[!(discovery_functions %in% used)]
            if (length(remaining_discovery)) {
                for (function_name in remaining_discovery) {
                    add_function(function_name)
                }
                replaced_headers <- c(
                    replaced_headers,
                    paste0("Discovery_* -> ", paste(remaining_discovery, collapse = ", "))
                )
            } else {
                removed_headers <- c(removed_headers, header)
            }
            next
        }

        removed_headers <- c(removed_headers, header)
    }

    appended <- current_functions[!(current_functions %in% used)]
    for (function_name in appended) {
        add_function(function_name)
    }

    list(
        ordered = ordered,
        removed_headers = removed_headers,
        replaced_headers = replaced_headers,
        appended = appended
    )
}

build_existing_argument_map <- function(argument_rows) {
    argument_map <- list()

    for (row in argument_rows) {
        key <- normalize_arg_key(row$display)
        if (key %in% names(argument_map)) {
            stop("Duplicate argument row in workbook template after normalization: ", key)
        }
        argument_map[[key]] <- row
    }

    argument_map
}

build_argument_order <- function(existing_argument_rows, function_specs, ordered_functions) {
    # Keep existing argument row order for surviving arguments, then append
    # newly introduced arguments in first-seen source order.
    source_order <- unique(unlist(lapply(ordered_functions, function(function_name) {
        normalize_arg_key(function_specs[[function_name]])
    }), use.names = FALSE))

    ordered_keys <- character()
    used <- character()

    for (row in existing_argument_rows) {
        key <- normalize_arg_key(row$display)
        if (key %in% source_order && !(key %in% used)) {
            ordered_keys <- c(ordered_keys, key)
            used <- c(used, key)
        }
    }

    c(ordered_keys, source_order[!(source_order %in% used)])
}

build_doc_index <- function(ordered_functions, rd_docs) {
    doc_index <- list()

    for (function_name in ordered_functions) {
        docs <- rd_docs[[function_name]]
        for (arg_key in names(docs)) {
            doc_index[[arg_key]] <- c(doc_index[[arg_key]], docs[[arg_key]])
        }
    }

    doc_index
}

resolve_argument_description <- function(arg_key, existing_argument_map, doc_index) {
    # Existing workbook text wins to avoid churn. New arguments must either
    # have a single documented description or an explicit override above.
    if (!is.null(existing_argument_map[[arg_key]])) {
        existing_usage <- existing_argument_map[[arg_key]]$usage
        if (nzchar(normalize_ws(existing_usage))) {
            return(existing_usage)
        }
    }

    if (arg_key %in% names(argument_description_overrides)) {
        return(unname(argument_description_overrides[[arg_key]]))
    }

    candidates <- unique(normalize_ws(unlist(doc_index[[arg_key]])))
    candidates <- candidates[nzchar(candidates)]

    if (!length(candidates)) {
        stop("No documented description found for argument: ", arg_key)
    }

    if (length(candidates) > 1) {
        stop(
            "Conflicting descriptions found for new argument ", arg_key, ": ",
            paste(candidates, collapse = " | "),
            ". Add a canonical entry to argument_description_overrides in ",
            ".github/scripts/update_argument_usage.R."
        )
    }

    candidates[[1]]
}

resolve_argument_display <- function(arg_key, existing_argument_map) {
    if (!is.null(existing_argument_map[[arg_key]]) && nzchar(existing_argument_map[[arg_key]]$display)) {
        return(existing_argument_map[[arg_key]]$display)
    }
    arg_key
}

new_string_pool <- function() {
    values <- character()

    list(
        index = function(value) {
            match_index <- match(value, values)
            if (is.na(match_index)) {
                values <<- c(values, value)
                match_index <- length(values)
            }
            match_index - 1L
        },
        values = function() values
    )
}

make_shared_strings_xml <- function(values, total_count) {
    si_nodes <- vapply(values, function(value) {
        preserve_space <- grepl("(^\\s)|(\\s$)", value)
        sprintf(
            "<si><t%s>%s</t></si>",
            if (preserve_space) ' xml:space="preserve"' else "",
            xml_escape(value)
        )
    }, character(1))

    paste0(
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
        '<sst xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" count="',
        total_count,
        '" uniqueCount="',
        length(values),
        '">',
        paste(si_nodes, collapse = ""),
        "</sst>"
    )
}

make_sheet_data_xml <- function(argument_rows, ordered_functions, function_specs) {
    # Rebuild the single worksheet as the same argument/function matrix:
    # row 1 = headers, column A = argument names, column B = descriptions,
    # remaining cells = X markers for argument usage.
    pool <- new_string_pool()
    total_cells <- 0L
    total_columns <- length(ordered_functions) + 2L

    make_cell <- function(ref, value, style = NULL) {
        total_cells <<- total_cells + 1L
        attributes <- c(sprintf('r="%s"', ref))
        if (!is.null(style)) {
            attributes <- c(attributes, sprintf('s="%s"', style))
        }
        attributes <- c(attributes, 't="s"')
        sprintf(
            "<c %s><v>%d</v></c>",
            paste(attributes, collapse = " "),
            pool$index(value)
        )
    }

    header_cells <- c(
        make_cell("A1", "Argument", style = 2),
        make_cell("B1", "Usage", style = 2),
        vapply(seq_along(ordered_functions), function(index) {
            make_cell(paste0(col_from_number(index + 2L), "1"), ordered_functions[[index]], style = 2)
        }, character(1))
    )

    row_nodes <- character(length(argument_rows) + 1L)
    row_nodes[[1]] <- sprintf(
        '<row r="1" spans="1:%d" s="2" customFormat="1" x14ac:dyDescent="0.3">%s</row>',
        total_columns,
        paste(header_cells, collapse = "")
    )

    function_arguments <- lapply(function_specs[ordered_functions], normalize_arg_key)

    for (row_index in seq_along(argument_rows)) {
        sheet_row <- row_index + 1L
        argument_row <- argument_rows[[row_index]]
        cells <- c(
            make_cell(paste0("A", sheet_row), argument_row$display, style = 1),
            make_cell(paste0("B", sheet_row), argument_row$usage)
        )

        marker_cells <- vapply(seq_along(ordered_functions), function(function_index) {
            function_name <- ordered_functions[[function_index]]
            if (!(argument_row$key %in% function_arguments[[function_name]])) {
                return("")
            }
            make_cell(paste0(col_from_number(function_index + 2L), sheet_row), "X")
        }, character(1))
        marker_cells <- marker_cells[nzchar(marker_cells)]

        row_nodes[[row_index + 1L]] <- sprintf(
            '<row r="%d" spans="1:%d" x14ac:dyDescent="0.3">%s</row>',
            sheet_row,
            total_columns,
            paste(c(cells, marker_cells), collapse = "")
        )
    }

    list(
        shared_strings = pool$values(),
        shared_string_count = total_cells,
        sheet_data = paste0("<sheetData>", paste(row_nodes, collapse = ""), "</sheetData>"),
        total_columns = total_columns,
        total_rows = length(argument_rows) + 1L
    )
}

update_sheet_xml <- function(template_xml, new_dimension, new_cols_xml, new_sheet_data_xml) {
    updated <- template_xml
    updated <- sub('<dimension ref="[^"]*"/>', sprintf('<dimension ref="%s"/>', new_dimension), updated)
    updated <- sub("(?s)<cols>.*?</cols>", new_cols_xml, updated, perl = TRUE)
    updated <- sub("(?s)<sheetData>.*?</sheetData>", new_sheet_data_xml, updated, perl = TRUE)
    updated
}

repack_xlsx <- function(tempdir, destination) {
    # Keep the rest of the xlsx package untouched and only replace the rebuilt
    # OpenXML payload after the new parts have been written to the temp tree.
    destination <- normalizePath(destination, winslash = "/", mustWork = FALSE)
    zip_path <- tempfile(fileext = ".xlsx")
    files <- list.files(tempdir, recursive = TRUE, all.files = TRUE, no.. = TRUE)
    old_wd <- setwd(tempdir)
    on.exit(setwd(old_wd), add = TRUE)

    utils::zip(zipfile = zip_path, files = files, flags = "-q -r9X")

    if (!file.exists(zip_path)) {
        stop("Failed to rebuild workbook at ", destination)
    }

    if (!file.copy(zip_path, destination, overwrite = TRUE)) {
        stop("Failed to copy rebuilt workbook to ", destination)
    }
}

if (!file.exists(xlsx_path)) {
    stop("Workbook does not exist: ", xlsx_path)
}

# Main flow:
# 1. unpack the current workbook
# 2. read existing layout from sheet1/sharedStrings
# 3. derive current functions and docs from source
# 4. reconcile old layout with new source-of-truth
# 5. write the workbook back only if the XML payload changed
tempdir <- tempfile("argument-usage-")
dir.create(tempdir)
on.exit(unlink(tempdir, recursive = TRUE), add = TRUE)

utils::unzip(xlsx_path, exdir = tempdir)

shared_strings_path <- file.path(tempdir, "xl", "sharedStrings.xml")
sheet_path <- file.path(tempdir, "xl", "worksheets", "sheet1.xml")

if (!file.exists(shared_strings_path) || !file.exists(sheet_path)) {
    stop("Workbook template is missing required OpenXML parts")
}

existing_shared_xml <- read_text(shared_strings_path)
existing_sheet_xml <- read_text(sheet_path)
shared_strings <- parse_shared_strings(shared_strings_path)
sheet_template <- parse_sheet_template(sheet_path, shared_strings)

function_specs <- parse_function_specs("R")
ordered_source_functions <- names(function_specs)
rd_docs <- parse_rd_arguments(ordered_source_functions, "man")
validate_documentation(function_specs, rd_docs)

existing_argument_map <- build_existing_argument_map(sheet_template$argument_rows)
function_order <- order_functions(sheet_template$headers, ordered_source_functions)
ordered_functions <- function_order$ordered
ordered_argument_keys <- build_argument_order(sheet_template$argument_rows, function_specs, ordered_functions)
doc_index <- build_doc_index(ordered_functions, rd_docs)

argument_rows <- lapply(ordered_argument_keys, function(arg_key) {
    list(
        key = arg_key,
        display = resolve_argument_display(arg_key, existing_argument_map),
        usage = resolve_argument_description(arg_key, existing_argument_map, doc_index)
    )
})

sheet_data <- make_sheet_data_xml(argument_rows, ordered_functions, function_specs)
last_column <- col_from_number(sheet_data$total_columns)
new_dimension <- sprintf("A1:%s%d", last_column, sheet_data$total_rows)
new_cols_xml <- sprintf(
    '<cols><col min="1" max="1" width="19.109375" customWidth="1"/><col min="2" max="2" width="44.44140625" customWidth="1"/><col min="3" max="%d" width="2.77734375" customWidth="1"/></cols>',
    sheet_data$total_columns
)
new_sheet_xml <- update_sheet_xml(
    template_xml = sheet_template$xml,
    new_dimension = new_dimension,
    new_cols_xml = new_cols_xml,
    new_sheet_data_xml = sheet_data$sheet_data
)
new_shared_xml <- make_shared_strings_xml(
    values = sheet_data$shared_strings,
    total_count = sheet_data$shared_string_count
)

message("Workbook function columns: ", length(ordered_functions))
message("Workbook argument rows: ", length(argument_rows))

if (length(function_order$replaced_headers)) {
    message("Replaced legacy headers: ", paste(function_order$replaced_headers, collapse = "; "))
}
if (length(function_order$removed_headers)) {
    message("Removed stale headers: ", paste(function_order$removed_headers, collapse = ", "))
}
if (length(function_order$appended)) {
    message("Appended new function columns: ", paste(function_order$appended, collapse = ", "))
}

if (identical(existing_shared_xml, new_shared_xml) && identical(existing_sheet_xml, new_sheet_xml)) {
    message("ArgumentUsage workbook is already up to date.")
    quit(status = 0)
}

write_text(shared_strings_path, new_shared_xml)
write_text(sheet_path, new_sheet_xml)
repack_xlsx(tempdir, xlsx_path)
message("Updated ", xlsx_path)
