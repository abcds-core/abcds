#' @title expand_lookup
#' @description Expands the look-up table in cases where there are ranges in the scores
#' @param data The data set containing the variable with the ranges
#' @param variable The variable containing the range as indicated by a dash between two numeric values
#' @return The expanded look-up table with the variable in descending order
#' @details Expands the look-up table in cases where there are ranges in the scores
#' @seealso
#'  \code{\link[rlang]{:=}}
#'  \code{\link[tibble]{tibble}}
#' @rdname expand_lookup
#' @importFrom rlang `:=`
#' @importFrom tibble tibble
#' @keywords internal

expand_lookup <- function(data, variable) {
  `:=` <- rlang::`:=`

  lookupTable <-
    do.call(
      rbind,
      lapply(seq_along(data[[variable]]), FUN = function(i) {
        if (grepl("-", data[[variable]][i])) {
          bounds <- as.numeric(strsplit(data[[variable]][i], "-")[[1]])
          tibble::tibble(
            !!variable := seq(bounds[1], bounds[2]),
            data[i, -which(colnames(data) == variable)]
          )
        } else {
          tibble::tibble(
            !!variable := as.numeric(data[[variable]][i]),
            data[i, -which(colnames(data) == variable)]
          )
        }
      })
    )

  lookupTable[order(lookupTable[[variable]], decreasing = TRUE), ]
}


#' @title deploy_kbit2_app
#' @description Deploys the KBIT-2 Calculator Shiny app on a local host server
#' @return Launches the KBIT-2 Calculator Shiny app
#' @details Deploys the KBIT-2 Calculator Shiny app on a local host server
#' @importFrom shiny runApp
#' @export

deploy_kbit2_app <- function() {
  shiny::runApp(system.file("app/kbit2/app.R", package = "abcds"))
}

#' @title abcds_join
#' @description A flexible join option takes a dplyr join function as an argument
#' @inheritParams dplyr::full_join
#' @param join_type The type of dplyr join to perform. Must be a string, symbol, or function (e.g.,
#'   "inner_join", `inner_join`, `dplyr::inner_join`)
#' @return A tibble containing the joined data
#' @details A flexible join option takes a dplyr join function as an argument
#' @seealso
#'  \code{\link[rlang]{sym}}, \code{\link[rlang]{eval_tidy}}, \code{\link[rlang]{is_symbol}}, \code{\link[rlang]{is_call}}, \code{\link[rlang]{abort}}
#' @rdname abcds_join
#' @export
#' @importFrom rlang sym eval_tidy is_symbol is_call abort

abcds_join <- function(x, y, by, join_type) {
  join_fn <- tryCatch(
    {
      rlang::eval_tidy(rlang::ensym(join_type), env = asNamespace("dplyr"))
    },
    error = function(e) {
      rlang::eval_tidy(join_type)
    }
  )

  join_fn(x, y, by = by)
}


#' @title Split Factor Labels into Binary Columns
#'
#' @description
#' Converts a variable containing multiple coded values separated by a delimiter
#' into separate binary columns for each level using a dictionary.
#'
#' @param data A data frame containing the variable to split.
#' @param dictionary A list containing the levels and labels for the variable.
#' @param variable The variable in \code{data} to split (unquoted).
#' @param ids The ID variables used to merge the split factor levels back to the original
#'   dataset, Default: c("subject_label", "event_sequence")
#' @param delim A character string used to separate multiple values in the variable.
#'   Default: \code{"|"}.
#'
#' @return
#' A data frame with the original data and additional columns for each level of
#' the split factor, coded as 0/1.
#'
#' @details
#' This function is useful for handling variables that allow multiple selections
#' (e.g., race, comorbidities). It splits the values, converts them to factors
#' according to a dictionary, creates binary indicator columns, and joins them
#' back to the original data.
#'
#' @examples
#' \dontrun{
#' split_factor_labels(data = mydata, dictionary = mydict, variable = de_race)
#' }
#'
#' @rdname split_factor_labels
#' @importFrom tidyr pivot_wider separate_longer_delim
#' @keywords internal

split_factor_labels <- function(
  data,
  dictionary,
  variable,
  ids = c("subject_label", "event_sequence"),
  delim = "|"
) {
  variable <- as.character(rlang::ensym(variable))

  original_data <- data

  data <- tidyr::separate_longer_delim(
    data[, c(ids, variable)],
    cols = variable,
    delim = delim
  )

  variableLevels <- dictionary[
    dictionary[["field_name"]] == variable,
    "field_code"
  ]

  variableLabels <- dictionary[
    dictionary[["field_name"]] == variable,
    "field_code_label"
  ]

  data[[variable]] <- factor(
    data[[variable]],
    levels = variableLevels,
    labels = variableLabels
  )

  data$value <- 1L

  data <- tidyr::pivot_wider(
    data[!is.na(data[[variable]]), ],
    id_cols = ids,
    names_from = variable,
    values_fill = 0,
    values_fn = max
  )

  original_data <-
    abcds_join(
      x = original_data,
      y = data,
      by = ids,
      join_type = full_join
    )

  return(original_data)
}


#' @title .clean_file_names
#' @keywords internal
#' @noRd
#' @importFrom purrr map_chr

.clean_file_names <- function(files, remove_csv = TRUE) {
  patterns <- c(
    "[0-9]{2}[A-Za-z]{3}[0-9]{4}",
    "[0-9]{4}_[0-9]{2}_[0-9]{2}",
    "[0-9]{4}[0-9]{2}[0-9]{2}",
    "_|_-_|__|___|-",
    "ABC DS|ABCDS|Brickman lab|abcds|  ",
    "\\s+\\."
  )
  replacements <- c("\\1", "\\1", "\\1", " ", "", "\\.")
  for (i in 1:length(patterns)) {
    files <- gsub(pattern = patterns[i], replacement = replacements[i], files)
    if (i == length(patterns)) {
      files <- trimws(files)
      files <- purrr::map_chr(files, .title_case)
    }
  }

  if (remove_csv) {
    files <- gsub(".csv$", "", files)
  }

  return(files)
}

.title_case <- function(x) {
  words <- strsplit(x, " ")[[1]]
  words <- purrr::map_chr(
    words,
    ~ paste0(
      toupper(substr(.x, 1, 1)),
      substr(.x, 2, nchar(.x))
    )
  )

  paste(words, collapse = " ")
}


.detect_person <- function(file) {
  if (grepl("Control", file, ignore.case = TRUE)) {
    return("control")
  } else {
    return("participant")
  }
}

.print_description <- function() {
  message(
    "  ADDS: Alzheimer's Disease Down Syndrome
  - New York, NY (Columbia University/New York State Institute for Basic Research in Developmental Disabilities);
  - Boston, MA (Massachusetts General Hospital, Harvard University);
  - Irvine, CA (the University of California, Irvine)\n
  NiAD: Neurodegeneration in Aging Down Syndrome
  - Pittsburgh, PA (University of Pittsburgh);
  - Madison, WI (University of Wisconsin);
  - St. Louis, MO (Washington University);
  - Cambridge, England (University of Cambridge)"
  )
}
