#' Build a DuckDB database from a directory of study CSVs
#'
#' Reads every top-level CSV in `directory` (skipping any file whose name
#' contains "dictionary" or "codebook") into its own table in a DuckDB
#' database at `directory/abcds.duckdb`, using the same cleaned, snake_case
#' naming as [organize_files()]. A `key` table recording the mapping from
#' original file name to table name is added alongside the data tables.
#'
#' @param directory Path to a directory of CSV files, or `NULL` to resolve
#'   the default ABC-DS directory via [check_abcds_directory()].
#'
#' @return The path to the created `.duckdb` file, invisibly. Called
#'   primarily for its side effect of creating the database.
#'
#' @section Side effects:
#' Creates or overwrites `directory/abcds.duckdb`. Any table whose name
#' already exists in the database (including from a previous run) is dropped
#' and recreated; tables not derived from the current CSVs are left alone.
#' Prints a one-line confirmation with the row count for each table created.
#'
#' @section Errors:
#' Aborts before touching the database if no matching CSVs are found, or if
#' two files clean to the same table name. The database connection is always
#' closed on exit, including on error.
#'
#' @seealso [organize_files()] for the naming convention used for table
#'   names; [check_abcds_directory()] for how the default directory is
#'   resolved.
#'
#' @export
#' @importFrom cli cli_abort
#' @importFrom tibble tibble
#' @importFrom DBI dbConnect dbDisconnect dbQuoteIdentifier dbExistsTable dbRemoveTable dbExecute dbGetQuery
#' @importFrom duckdb duckdb duckdb_register duckdb_unregister
#' @importFrom readr read_csv
#' @importFrom purrr walk2

generate_duck_database <- function(directory = NULL) {
  directory <- check_abcds_directory(directory)

  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  files <- files[
    !grepl('dictionary|codebook', basename(files), ignore.case = TRUE)
  ]

  if (length(files) == 0L) {
    cli::cli_abort("No CSV files found in {.path {directory}}.")
  }

  file_names <- basename(files)
  short_file_names <- .clean_file_names(file_names)
  table_names <- gsub("\\s+", "_", tolower(short_file_names))

  tables <- data.frame(file_names, short_file_names, table_names)

  if (anyDuplicated(tables$table_names)) {
    dupes <- unique(tables$table_names[duplicated(tables$table_names)])
    cli::cli_abort(c(
      "Cleaned file names are not unique.",
      x = "{.file {dupes}}"
    ))
  }

  db_path <- file.path(directory, "abcds.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = db_path)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Reads in dataset and adds to a duckdb
  create_table <- function(con, data, table_name) {
    if (is.character(data)) {
      data <- suppressWarnings(
        readr::read_csv(data, show_col_types = FALSE)
      )
    }
    duckdb::duckdb_register(con, "temp_table", data)
    on.exit(duckdb::duckdb_unregister(con, "temp_table"), add = TRUE)

    ident <- DBI::dbQuoteIdentifier(con, table_name)
    if (DBI::dbExistsTable(con, table_name)) {
      DBI::dbRemoveTable(con, table_name)
    }

    DBI::dbExecute(
      con,
      paste0("CREATE TABLE ", ident, " AS SELECT * FROM temp_table")
    )

    row_count <- DBI::dbGetQuery(
      con,
      paste0("SELECT COUNT(*) as n FROM ", ident)
    )$n

    message(
      "\u2713 Table '",
      table_name,
      "' created with ",
      format(row_count, big.mark = ","),
      " rows"
    )
  }

  purrr::walk2(
    files,
    tables$table_names,
    ~ create_table(con = con, data = .x, table_name = .y)
  )

  create_table(con, data = tables, table_name = "key")

  invisible(db_path)
}
