#' Organize a directory of study files into a structured layout
#'
#' Sorts the files at the top level of `directory` into subfolders by type,
#' renaming each file to a cleaned, snake_case table name. Before moving
#' anything, a key mapping original file names to their new names is written to
#' `.abcdsmeta/file_key.csv`, which allows the operation to be reversed with
#' [restore_original_files()].
#'
#' Files are routed by matching their name against a fixed set of patterns, in
#' this order:
#'
#' \describe{
#'   \item{`controls/`}{name contains "controls"}
#'   \item{`codebook/`}{name contains "dictionary" or "codebook"}
#'   \item{`documents/`}{`.pdf` or `.docx` extension}
#'   \item{`participants/`}{`.csv` extension}
#' }
#'
#' The first match wins, so the order is deliberate: a file named
#' `data_dictionary.pdf` is routed to `codebook/`, not `documents/`.
#'
#' @param directory Path to the directory to organize. Only files at the top
#'   level are considered; existing subfolders are left untouched.
#'
#' @return `directory`, invisibly. Called for its side effects on the file
#'   system.
#'
#' @section Side effects:
#' Creates `.abcdsmeta/` and one subfolder per matched destination, writes
#' `.abcdsmeta/file_key.csv`, and moves the matched files off the top level.
#' The key is written before any file is moved, so a partial failure can still
#' be recovered with [restore_original_files()].
#'
#' @section Errors:
#' Aborts without modifying the directory if `.abcdsmeta/file_key.csv` already
#' exists (a signal the directory has already been organized), if two files
#' clean to the same table name, or if any file matches none of the routing
#' patterns. Warns and returns early if there is nothing to organize.
#'
#' @seealso [restore_original_files()] to undo the result.
#'
#'
#' @export
#' @importFrom cli cli_abort cli_warn
#' @importFrom tibble tibble
#' @importFrom utils write.csv
#' @importFrom purrr walk

organize_files <- function(directory = NULL) {
  directory <- check_abcds_directory(directory)
  metadir <- file.path(directory, ".abcdsmeta")
  key_path <- file.path(metadir, "file_key.csv")

  if (file.exists(key_path)) {
    cli::cli_abort(
      "Stopping because {.file .abcdsmeta/file_key.csv} was found."
    )
  }

  files <- list.files(
    directory,
    pattern = "\\.(csv|docx|pdf)$",
    full.names = TRUE
  )

  if (length(files) == 0L) {
    cli::cli_warn("No files to organize in {.path {directory}}.")
    return(invisible(directory))
  }

  file_names <- basename(files)
  short_file_names <- .clean_file_names(file_names, remove_csv = FALSE)
  table_names <- gsub("\\s+", "_", tolower(short_file_names))

  key <- data.frame(file_names, short_file_names, table_names)

  folders <- c(
    controls = "controls",
    codebook = "dictionary|codebook",
    documents = "\\.(pdf|docx)$",
    participants = "\\.csv$"
  )

  hits <- vapply(
    folders,
    grepl,
    logical(length(files)),
    x = basename(files),
    ignore.case = TRUE
  )

  dest <- names(folders)[max.col(hits, ties.method = "first")]
  dest[rowSums(hits) == 0L] <- NA_character_

  if (anyNA(dest)) {
    cli::cli_abort(c(
      "No destination folder for {sum(is.na(dest))} file{?s}.",
      x = "{.file {basename(files[is.na(dest)])}}"
    ))
  }

  dir.create(metadir, showWarnings = FALSE)
  utils::write.csv(key, file = key_path, row.names = FALSE)

  purrr::walk(
    unique(dest),
    ~ dir.create(file.path(directory, .x), showWarnings = FALSE)
  )

  ok <- file.rename(files, file.path(directory, dest, key$table_names))
  if (!all(ok)) {
    cli::cli_abort(c(
      "Failed to move {sum(!ok)} file{?s}.",
      i = "{.file .abcdsmeta/file_key.csv} was written, so {.fn restore_original_files} can recover."
    ))
  }

  invisible(directory)
}

#' Restore an organized directory to its original file layout
#'
#' Reverses [organize_files()]. Reads `.abcdsmeta/file_key.csv`, copies each
#' organized file back to the top level of `directory` under its original name,
#' and then removes the subfolders created during organization.
#'
#' @param directory Path to a directory previously processed by
#'   [organize_files()].
#'
#' @return `directory`, invisibly. Called for its side effects on the file
#'   system.
#'
#' @section Side effects:
#' Copies files back to the top level of `directory`, overwriting any existing
#' file of the same name, then deletes the `documents/`, `controls/`,
#' `codebook/`, `participants/`, and `.abcdsmeta/` subfolders. Deletion only
#' happens once every copy has been confirmed, so a failed restore leaves the
#' organized layout intact.
#'
#' @section Errors:
#' Aborts if `.abcdsmeta/file_key.csv` is missing, if any organized file has no
#' corresponding entry in the key, or if any copy fails. In the latter two
#' cases nothing is deleted.
#'
#' @seealso [organize_files()], which produces the layout this function undoes.
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom utils read.csv

restore_original_files <- function(directory = NULL) {
  directory <- check_abcds_directory(directory)
  # fmt: skip
  subfolders <- c("documents", "controls", "codebook", "participants", ".abcdsmeta")

  key_path <- file.path(directory, ".abcdsmeta", "file_key.csv")

  if (!file.exists(key_path)) {
    cli::cli_abort(
      "Could not find {.file .abcdsmeta/file_key.csv} to restore original files."
    )
  }

  key <- utils::read.csv(key_path)

  files <- list.files(
    file.path(directory, setdiff(subfolders, ".abcdsmeta")),
    pattern = "\\.(csv|docx|pdf)$",
    full.names = TRUE,
    recursive = TRUE
  )

  idx <- match(basename(files), key$table_names)
  if (anyNA(idx)) {
    cli::cli_abort(c(
      "No {.file file_key.csv} entry for {sum(is.na(idx))} file{?s}.",
      x = "{.file {basename(files[is.na(idx)])}}"
    ))
  }

  ok <- file.copy(
    files,
    file.path(directory, key$file_names[idx]),
    overwrite = TRUE
  )
  if (!all(ok)) {
    cli::cli_abort(c(
      "Failed to restore {sum(!ok)} file{?s}. Nothing was deleted.",
      x = "{.file {basename(files[!ok])}}"
    ))
  }

  unlink(file.path(directory, subfolders), recursive = TRUE)
  invisible(directory)
}
