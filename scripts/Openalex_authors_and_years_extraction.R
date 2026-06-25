# Instructions:
#
# 1. Run section "Select File" (requires selecting a csv or rds file
#    with columns "doi_dcc" and "doi_charite")
# 2. Run the "Run function" section.

# Select File -------------------------------------------------------------

if (!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(
  tidyverse,
  openalexR,
  rcrossref,
  tcltk
)

selected_file <- tclvalue(
  tkgetOpenFile(title = "Please select a CSV or RDS file with columns \"doi_dcc\" and \"doi_charite\"")
)

if (selected_file == "") {
  stop("No input file selected!")
}

selected_output_folder <- tclvalue(
  tkchooseDirectory(title = "Please select an output folder")
)

if (selected_output_folder == "") {
  stop("No output folder selected!")
}

file_ext <- selected_file |>
  tools::file_ext() |>
  tolower()

df <- switch(
  file_ext,
  csv = read.csv(selected_file, stringsAsFactors = FALSE),
  rds = readRDS(selected_file),
  stop("Please select a .csv or .rds file.")
)

if (!inherits(df, "data.frame")) {
  stop("The selected file must contain a data frame / tibble.")
}

required_cols <- c("doi_dcc", "doi_charite")
missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  stop(
    paste0(
      "The selected input must contain these columns: ",
      paste(required_cols, collapse = ", "),
      ". Missing: ",
      paste(missing_cols, collapse = ", ")
    )
  )
}

# Helpers -----------------------------------------------------------------

normalize_doi_column <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_remove("^doi:") |>
    dplyr::na_if("")
}

normalize_doi_vector <- function(x) {
  x <- normalize_doi_column(x)
  x <- x[!is.na(x)]
  unique(x)
}

NestedDataFrameSize <- function(x) {
  x <- lapply(x, function(y) if (is.data.frame(y)) nrow(y) else 0)
  x <- unlist(x)
  x
}

UnnestDataFrame <- function(x, pid) {
  valid_x <- x[!is.na(x) & sapply(x, is.data.frame)]
  valid_pid <- pid[!is.na(x) & sapply(x, is.data.frame)]
  
  if (length(valid_x) == 0) {
    return(data.frame())
  }
  
  df.x <- bind_rows(valid_x)
  tcx <- NestedDataFrameSize(valid_x)
  df.x$PID <- rep(valid_pid, times = tcx)
  df.x
}

# OpenAlex ----------------------------------------------------------------

openalex_extract <- function(doi_vector) {
  dois <- normalize_doi_vector(doi_vector)
  
  if (length(dois) == 0) {
    return(list())
  }
  
  dois <- paste0("doi:", dois)
  dois <- sprintf("%s", dois)
  
  results_list <- list()
  error_dois <- character()
  
  process_dois <- function(dois_to_process) {
    local_error_dois <- character()
    
    for (doi in dois_to_process) {
      query <- tryCatch(
        {
          oa_fetch(
            entity = "works",
            identifier = doi,
            verbose = TRUE
          )
        },
        error = function(e) {
          message(paste("Error fetching DOI:", doi))
          message("Error details: ", conditionMessage(e))
          local_error_dois <<- c(local_error_dois, doi)
          NULL
        }
      )
      
      if (!is.null(query)) {
        results_list[[doi]] <<- query
      }
    }
    
    local_error_dois
  }
  
  error_dois <- process_dois(dois)
  
  while (length(error_dois) > 0) {
    message("The following DOIs had errors:")
    print(error_dois)
    
    retry <- readline(prompt = "Would you like to retry processing them? (y/n): ")
    
    if (tolower(retry) == "y") {
      error_dois <- process_dois(error_dois)
    } else {
      break
    }
  }
  
  results_list
}

extract_openalex_metadata <- function(doi_vector) {
  empty_result <- tibble::tibble(
    doi_lookup = character(),
    publication_year = numeric(),
    authors = character()
  )
  
  results <- openalex_extract(doi_vector)
  
  if (length(results) == 0) {
    return(empty_result)
  }
  
  results_unnested <- UnnestDataFrame(results, names(results))
  
  if (!inherits(results_unnested, "data.frame") || nrow(results_unnested) == 0) {
    return(empty_result)
  }
  
  if (!"doi" %in% names(results_unnested)) {
    return(empty_result)
  }
  
  if (!"publication_year" %in% names(results_unnested)) {
    results_unnested$publication_year <- NA_real_
  }
  
  authors_tbl <- if ("author" %in% names(results_unnested)) {
    UnnestDataFrame(results_unnested$author, results_unnested$doi) |>
      dplyr::rename(
        doi = PID,
        authors = au_display_name
      ) |>
      dplyr::group_by(doi) |>
      dplyr::summarise(
        authors = stringr::str_c(authors, collapse = ";"),
        .groups = "drop"
      )
  } else {
    tibble::tibble(
      doi = character(),
      authors = character()
    )
  }
  
  results_unnested |>
    dplyr::select(doi, publication_year) |>
    dplyr::distinct() |>
    dplyr::left_join(authors_tbl, by = "doi") |>
    dplyr::mutate(
      doi_lookup = doi |>
        as.character() |>
        stringr::str_to_lower() |>
        stringr::str_remove("^doi:")
    ) |>
    dplyr::distinct(doi_lookup, .keep_all = TRUE) |>
    dplyr::select(doi_lookup, publication_year, authors)
}

# Define a function to process the dataframe ------------------------------

process_dataframe <- function(df) {
  df_prepared <- df |>
    dplyr::mutate(
      doi_dcc_lookup = normalize_doi_column(doi_dcc),
      doi_charite_lookup = normalize_doi_column(doi_charite)
    )
  
  dcc_metadata <- extract_openalex_metadata(df_prepared$doi_dcc_lookup) |>
    dplyr::rename(
      doi_dcc_lookup = doi_lookup,
      doi_dcc_year = publication_year,
      doi_dcc_authors = authors
    )
  
  charite_metadata <- extract_openalex_metadata(df_prepared$doi_charite_lookup) |>
    dplyr::rename(
      doi_charite_lookup = doi_lookup,
      doi_charite_year = publication_year,
      doi_charite_authors = authors
    )
  
  final_results <- df_prepared |>
    dplyr::left_join(dcc_metadata, by = "doi_dcc_lookup") |>
    dplyr::left_join(charite_metadata, by = "doi_charite_lookup") |>
    dplyr::select(-doi_dcc_lookup, -doi_charite_lookup)
  
  final_results_authors_different <- final_results |>
    dplyr::filter(
      is.na(doi_dcc_authors) |
        is.na(doi_charite_authors) |
        doi_dcc_authors != doi_charite_authors
    )
  
  list(
    final_results = final_results,
    final_results_authors_different = final_results_authors_different
  )
}

# Run function ------------------------------------------------------------

processed_results <- process_dataframe(df)

final_results <- processed_results$final_results
final_results_authors_different <- processed_results$final_results_authors_different

base_name <- tools::file_path_sans_ext(basename(selected_file))

save_path_full <- file.path(
  selected_output_folder,
  paste0(base_name, "_final_results.rds")
)

save_path_authors_different <- file.path(
  selected_output_folder,
  paste0(base_name, "_final_results_authors_different.rds")
)

saveRDS(final_results, file = save_path_full)
saveRDS(final_results_authors_different, file = save_path_authors_different)