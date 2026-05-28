save_rds_cr <- function(object, file) {
  dir_path <- dirname(file)
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
  }
  
  saveRDS(object, file = file)
} # wrapper for saveRDS() with automatic directory creation
