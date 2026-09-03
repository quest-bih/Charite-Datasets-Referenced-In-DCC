# Load libraries

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  tidyverse,
  openalexR,
  tcltk
)

# Select files ------------------------------------------------------------

charite_file <- tcltk::tclvalue(
  tcltk::tkgetOpenFile(
    title = "Select the RDS file containing doi_charite"
  )
)

if (charite_file == "") {
  stop("No Charité DOI file selected.")
}

results_file <- tcltk::tclvalue(
  tcltk::tkgetOpenFile(
    title = "Select the broken results RDS file"
  )
)

if (results_file == "") {
  stop("No results file selected.")
}

selected_output_folder <- tcltk::tclvalue(
  tcltk::tkchooseDirectory(
    title = "Select an output folder"
  )
)

if (selected_output_folder == "") {
  stop("No output folder selected.")
}

charite_df <- readRDS(charite_file)
broken_results <- readRDS(results_file)

# Helpers -----------------------------------------------------------------

normalize_doi_column <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_remove("^doi:\\s*") |>
    stringr::str_remove("^https?://(?:dx\\.)?doi\\.org/") |>
    dplyr::na_if("")
}

normalize_author_list <- function(x) {
  x |>
    dplyr::coalesce("") |>
    stringr::str_replace_all("[‐-‒–—−]", "-") |>
    stringr::str_split(";") |>
    purrr::pluck(1) |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    (\(authors) authors[authors != ""])() |>
    unique()
}

no_author_overlap <- function(x, y) {
  x_authors <- normalize_author_list(x)
  y_authors <- normalize_author_list(y)
  
  length(intersect(x_authors, y_authors)) == 0
}

NestedDataFrameSize <- function(x) {
  x |>
    lapply(
      \(y) {
        if (is.data.frame(y)) {
          nrow(y)
        } else {
          0
        }
      }
    ) |>
    unlist()
}

UnnestDataFrame <- function(x, pid) {
  valid_rows <- !is.na(x) & vapply(
    x,
    is.data.frame,
    logical(1)
  )
  
  valid_x <- x[valid_rows]
  valid_pid <- pid[valid_rows]
  
  if (length(valid_x) == 0) {
    return(data.frame())
  }
  
  unnested <- dplyr::bind_rows(valid_x)
  unnested$PID <- rep(
    valid_pid,
    times = NestedDataFrameSize(valid_x)
  )
  
  unnested
}

# Extract Charité metadata ------------------------------------------------

charite_dois <- charite_df |>
  dplyr::transmute(
    doi_charite_lookup = normalize_doi_column(doi_charite)
  ) |>
  dplyr::filter(!is.na(doi_charite_lookup)) |>
  dplyr::distinct() |>
  dplyr::pull(doi_charite_lookup)

openalex_results <- list()

for (doi in charite_dois) {
  message(
    "Fetching ",
    match(doi, charite_dois),
    " of ",
    length(charite_dois),
    ": ",
    doi
  )
  
  query <- tryCatch(
    openalexR::oa_fetch(
      entity = "works",
      identifier = paste0("doi:", doi),
      verbose = FALSE
    ),
    error = function(error) {
      message(
        "Could not fetch ",
        doi,
        ": ",
        conditionMessage(error)
      )
      
      NULL
    }
  )
  
  if (!is.null(query)) {
    openalex_results[[doi]] <- query
  }
}

if (length(openalex_results) == 0) {
  stop("OpenAlex returned no results.")
}

results_unnested <- UnnestDataFrame(
  openalex_results,
  names(openalex_results)
)

authors_tbl <- UnnestDataFrame(
  results_unnested$authorships,
  results_unnested$doi
) |>
  dplyr::rename(
    doi = PID,
    doi_charite_authors = display_name
  ) |>
  dplyr::group_by(doi) |>
  dplyr::summarise(
    doi_charite_authors = stringr::str_c(
      doi_charite_authors,
      collapse = ";"
    ),
    .groups = "drop"
  )

charite_metadata <- results_unnested |>
  dplyr::select(
    doi,
    publication_year
  ) |>
  dplyr::distinct() |>
  dplyr::left_join(
    authors_tbl,
    by = "doi"
  ) |>
  dplyr::mutate(
    doi_charite_lookup = normalize_doi_column(doi)
  ) |>
  dplyr::distinct(
    doi_charite_lookup,
    .keep_all = TRUE
  ) |>
  dplyr::transmute(
    doi_charite_lookup,
    doi_charite_year = publication_year,
    doi_charite_authors
  )

# Repair results ----------------------------------------------------------

final_results <- broken_results |>
  dplyr::select(
    -dplyr::any_of(
      c(
        "doi_charite_year",
        "doi_charite_authors"
      )
    )
  ) |>
  dplyr::mutate(
    doi_charite_lookup = normalize_doi_column(doi_charite)
  ) |>
  dplyr::left_join(
    charite_metadata,
    by = "doi_charite_lookup"
  ) |>
  dplyr::select(-doi_charite_lookup)

final_results_authors_different <- final_results |>
  dplyr::filter(
    purrr::map2_lgl(
      doi_dcc_authors,
      doi_charite_authors,
      no_author_overlap
    )
  )

# Save results ------------------------------------------------------------

base_name <- results_file |>
  basename() |>
  tools::file_path_sans_ext()

saveRDS(
  final_results,
  file.path(
    selected_output_folder,
    paste0(base_name, "_charite_repaired.rds")
  )
)

saveRDS(
  final_results_authors_different,
  file.path(
    selected_output_folder,
    paste0(
      base_name,
      "_charite_repaired_authors_different.rds"
    )
  )
)

message(
  "Finished.\n",
  "Full rows: ",
  nrow(final_results),
  "\nRows without common authors: ",
  nrow(final_results_authors_different)
)

all_sources_dcc_joined_condensed_dist_w_au_year_au_ov_no_shared_doi <- final_results_authors_different |>
  dplyr::mutate(
    doi_dcc_clean = doi_dcc |>
      stringr::str_trim() |>
      stringr::str_to_lower() |>
      stringr::str_remove("^https?://(?:dx\\.)?doi\\.org/"),
    doi_charite_clean = doi_charite |>
      stringr::str_trim() |>
      stringr::str_to_lower() |>
      stringr::str_remove("^https?://(?:dx\\.)?doi\\.org/")
  ) |>
  dplyr::filter(
    is.na(doi_dcc_clean) |
      is.na(doi_charite_clean) |
      doi_dcc_clean != doi_charite_clean
  ) |>
  dplyr::select(-doi_dcc_clean, -doi_charite_clean)

saveRDS(
  all_sources_dcc_joined_condensed_dist_w_au_year_au_ov_no_shared_doi,
  file.path(
    selected_output_folder,
    "all_sources_dcc_joined_condensed_dist_w_au_year_au_ov_no_shared_doi.rds"
  )
)

removed_shared_doi <- final_results_authors_different |>
  dplyr::mutate(
    doi_dcc_clean = doi_dcc |>
      stringr::str_trim() |>
      stringr::str_to_lower() |>
      stringr::str_remove("^https?://(?:dx\\.)?doi\\.org/"),
    doi_charite_clean = doi_charite |>
      stringr::str_trim() |>
      stringr::str_to_lower() |>
      stringr::str_remove("^https?://(?:dx\\.)?doi\\.org/")
  ) |>
  dplyr::filter(
    !is.na(doi_dcc_clean),
    !is.na(doi_charite_clean),
    doi_dcc_clean == doi_charite_clean
  ) |>
  dplyr::select(-doi_dcc_clean, -doi_charite_clean)


saveRDS(
  removed_shared_doi,
  file.path(
    selected_output_folder,
    "all_sources_dcc_joined_condensed_dist_removed_shared_doi.rds"
  )
)