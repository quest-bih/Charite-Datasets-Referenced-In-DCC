# R/dcc_csv_to_rds.R

dcc_csv_to_rds <- function(
    folder_path,
    output_folder = folder_path,
    output_file_name = "dcc_combined.rds"
) {
  if (!dir.exists(folder_path)) {
    stop("The supplied DCC folder does not exist.", call. = FALSE)
  }
  
  csv_files <- list.files(
    path = folder_path,
    pattern = "\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  if (length(csv_files) == 0) {
    stop("No CSV files were found in the supplied DCC folder.", call. = FALSE)
  }
  
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  dcc <- csv_files |>
    purrr::map(
      \(file_path) {
        readr::read_csv(
          file_path,
          show_col_types = FALSE
        )
      }
    ) |>
    dplyr::bind_rows()
  
  output_path <- file.path(output_folder, output_file_name)
  
  saveRDS(dcc, output_path)
  
  output_path
}