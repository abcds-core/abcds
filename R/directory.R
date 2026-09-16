#' Check ABCDS Data Directory
#'
#' Verifies that the ABCDS LONI data directory exists and contains expected CSV files.
#' If the directory is not set, provides helpful instructions for setup. This function
#' is typically called automatically when the package loads.
#'
#' @param directory Character string specifying the path to the ABCDS LONI data directory.
#'   If `NULL` (default), retrieves the path from the `ABCDS_LONI_DATA_DIRECTORY`
#'   environment variable.
#'
#' @return Invisibly returns the directory path if valid, or `NULL` if the directory
#'   is not set, doesn't exist, or doesn't contain expected LONI files.
#'
#' @details
#' The function performs the following checks:
#' \itemize{
#'   \item If no directory is set, displays welcome message with setup instructions
#'   \item If directory doesn't exist, warns user and suggests updating the path
#'   \item If no expected LONI CSV files are found, alerts user to potential issues
#' }
#'
#' Expected LONI files are defined in the internal `loni_file_names` object.
#'
#' @seealso [set_abcds_directory()] to configure the data directory path
#'
#' @examples
#' \dontrun{
#' # Check if directory is properly configured
#' check_abcds_directory()
#'
#' # Check a specific directory
#' check_abcds_directory("~/ABCDS/data")
#' }
#'
#' @keywords internal

check_abcds_directory <- function(directory = NULL) {
  if (is.null(directory)) {
    directory <- Sys.getenv("ABCDS_LONI_DATA_DIRECTORY")
  }

  files <- tryCatch(
    .clean_file_names(list.files(
      directory,
      pattern = "\\.csv$",
      full.names = FALSE
    )),
    error = function(e) NULL
  )

  if (directory == "") {
    cli::cat_rule(center = "Welcome to the ABCDS R Package!", width = 72)

    cli::cat_boxx(
      c(
        "It looks like you have not set up your ABCDS data directory yet.",
        "",
        "This directory should point to the folder on your computer where the",
        "ABCDS LONI data are stored (for example: '~/ABCDS/data').",
        "",
        "Once selected, we will save this path to your user environment so that",
        "you do not have to set it again each time you load the package."
      ),
      padding = c(0, 1, 0, 1),
      border_style = "double",
    )

    cli::cat_line()
    cli::cat_bullet("To set this up now, run:")
    cli::cat_line("  abcds::set_abcds_directory()", col = "cyan")
    cli::cat_bullet(
      "This will prompt you to choose a folder and store it in your .Renviron file."
    )
    cli::cat_line()
    cli::cat_line(
      "You can change it anytime by re-running the same function."
    )
    return(invisible(NULL))
  }

  if (!dir.exists(directory)) {
    cli::cli_alert_warning(
      "Your saved ABCDS directory no longer exists or has been moved:"
    )
    cli::cli_text("  {cli::col_yellow(directory)}")
    cli::cli_alert_info(
      "Please run `abcds::set_abcds_directory()` to update the location."
    )
    return(invisible(NULL))
  }

  if (!file.exists(file.path(directory, "abcds.duckdb"))) {
    if (is.null(files) || !any(files %in% loni_file_names)) {
      cli::cli_alert_warning("No expected LONI CSV files found in:")
      cli::cli_text("\u00A0\u00A0{cli::col_yellow(directory)}")
      cli::cli_alert_info("It may have been moved or is incomplete.")
      cli::cli_alert_info(
        "Please re-run `abcds::set_abcds_directory()` to update your setup."
      )
      return(invisible(NULL))
    }
  }

  invisible(directory)
}


#' Set ABCDS Data Directory
#'
#' Interactively configure the path to your ABCDS LONI data directory and optionally
#' save it to your `.Renviron` file for persistent storage across R sessions.
#'
#' @param env_var_name Character string specifying the environment variable name to use.
#'   Default is `"ABCDS_LONI_DATA_DIRECTORY"`.
#'
#' @return Invisibly returns the directory path that was configured.
#'
#' @details
#' This function guides you through an interactive setup process:
#' \enumerate{
#'   \item Prompts for the full path to your ABCDS LONI data directory
#'   \item Validates that the directory exists
#'   \item Checks for expected LONI CSV files in the directory
#'   \item Optionally saves the path to your `.Renviron` file for persistence
#' }
#'
#' The function will continue prompting until a valid directory with expected LONI
#' files is provided. Windows-style backslashes are automatically converted to
#' forward slashes for cross-platform compatibility.
#'
#' If you choose to save the path to `.Renviron`, you'll need to restart R for
#' the change to take effect.
#'
#' @note
#' Expected LONI files are defined in the internal `loni_file_names` object.
#' The function uses cli package for formatted console output.
#'
#' @seealso [check_abcds_directory()] to verify the configured directory
#'
#' @examples
#' \dontrun{
#' # Launch interactive setup
#' set_abcds_directory()
#'
#' # Use a custom environment variable name
#' set_abcds_directory(env_var_name = "MY_ABCDS_PATH")
#' }
#'
#' @export

set_abcds_directory <- function(env_var_name = "ABCDS_LONI_DATA_DIRECTORY") {
  cli::cat_rule(center = "ABCDS Directory Setup", width = 72)
  cli::cat_line()
  cli::cli_alert_info(
    "Please enter the full path to your ABCDS LONI directory."
  )
  cli::cli_alert_info("Example: ~/ABCDS/data or C:/Users/yourname/ABCDS")
  repeat {
    directory <- gsub("[\"']", "", trimws(readline(prompt = ">>> ")))

    # Convert Windows-style backslashes to forward slashes
    directory <- gsub("\\\\", "/", directory)

    # Validate directory exists
    if (!dir.exists(directory)) {
      cli::cli_alert_danger("That directory does not exist. Please try again.")
      next
    }

    # Check for expected files
    files <- tryCatch(
      .clean_file_names(list.files(
        directory,
        pattern = "\\.csv$",
        full.names = FALSE
      )),
      error = function(e) character()
    )

    if (length(files) == 0) {
      cli::cli_alert_warning("No CSV files found in this directory.")
      next
    }

    if (any(files %in% loni_file_names)) {
      cli::cli_alert_success("Found expected LONI files!")
      break
    } else {
      cli::cli_alert_warning(
        "No expected LONI files detected. Please try again."
      )
    }
  }

  # Ask whether to store permanently
  answer <- tolower(trimws(readline(
    "Would you like to save this path to your .Renviron? (y/n): "
  )))

  if (answer %in% c("y", "yes")) {
    renv_path <- path.expand("~/.Renviron")
    lines <- if (file.exists(renv_path)) readLines(renv_path) else character()
    lines <- lines[!grepl(paste0("^", env_var_name, "="), lines)]
    lines <- c(lines, paste0(env_var_name, '=', '"', directory, '"'))
    writeLines(lines, renv_path)

    cli::cli_alert_success("Saved {env_var_name} to your .Renviron file.")
    cli::cli_alert_success("ABCDS directory verified:")
    cli::cli_text("\u00A0\u00A0{cli::col_green(directory)}")
    cli::cli_alert_info("Please restart R for the change to take effect.")
  } else {
    cli::cli_alert_info(
      "Directory not saved. You can set it again anytime by running `set_abcds_directory()`."
    )
  }

  invisible(directory)
}
