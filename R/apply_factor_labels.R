#' @title apply_factor_labels
#' @description Apply factor labels to an existing data set from the ABC-DS codebook
#' @param data An data set existing in the user's global environment for
#'   which to apply the factor labels
#' @param ids The ID variables used to merge the split factor levels back to the original
#'   dataset, Default: c("subject_label", "event_sequence")
#' @return A data set that converts character variables to factors
#' @details Apply factor labels to an existing data set from the ABC-DS codebook.
#'   This function must be used before the `apply_labels` function or the labels
#'   will be removed from the data set.
#' @rdname apply_factor_labels
#' @export

apply_factor_labels <- function(
  data,
  ids = c("subject_label", "event_sequence")
) {
  load(system.file("extdata/codebook.RData", package = "abcds"))

  check_all <- c("de_race", "mrseqs", "hh_chd_type")

  for (i in names(data)) {
    if (i %in% codebook$field_name) {
      if (i %in% check_all) {
        data <- split_factor_labels(
          data,
          codebook,
          !!i,
          ids,
          delim = "|"
        )
      } else {
        data[[i]] <- factor(
          data[[i]],
          levels = codebook$field_code[which(codebook$field_name == i)],
          labels = codebook$field_code_label[which(codebook$field_name == i)]
        )
      }
    }
  }

  return(data)
}
