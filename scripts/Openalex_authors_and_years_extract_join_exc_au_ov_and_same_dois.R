# OpenAlex authors and publication years extraction -----------------------

# Instructions:
#
# 1. Run section "Select File" and select a CSV or RDS file containing
#    columns "doi_dcc" and "doi_charite".
# 2. Select an output folder.
# 3. Run the remaining script.
#
# OpenAlex metadata is retrieved for distinct DOIs. A second automatic
# retrieval attempt is made for DOIs with missing author or year metadata.
#
# The general workflow is:
#
# - Read and validate the original result table.
# - Normalize DCC and Charité DOI values into a consistent format.
# - Query OpenAlex once per distinct DOI during the first extraction pass.
# - Automatically retry distinct DOIs whose year or author metadata
#   remains missing.
# - Keep successful first-pass values and use the second pass only to
#   fill remaining missing values.
# - Join the finalized OpenAlex metadata back onto the original table.
# - Remove rows with demonstrated shared authors.
# - Remove rows where normalized DCC and Charité DOIs are identical.
# - Save intermediate processing stages as RDS files for traceability.
# - Save the final filtered result as an RDS file.


# Select File -------------------------------------------------------------

# Install the pacman package if it is not already available.
# pacman is used below to load all required R packages conveniently.
if (!require(pacman)) install.packages("pacman")
library(pacman)

# Load packages required by the workflow.
#
# tidyverse:
#   Data manipulation, string processing, functional programming,
#   and tibble helpers.
#
# openalexR:
#   Queries publication metadata from OpenAlex.
#
# rcrossref:
#   Loaded by the original workflow. It is not directly used below,
#   but is kept so the executable code remains unchanged.
#
# tcltk:
#   Provides graphical file and folder selection dialogs.
pacman::p_load(
  tidyverse,
  openalexR,
  rcrossref,
  tcltk
)

# Ask the user to select the original CSV or RDS result table.
#
# The selected file must contain at least:
#
#   doi_dcc
#   doi_charite
#
# The original table may contain many rows where the same DOI occurs
# repeatedly. OpenAlex extraction will later be performed only on
# distinct normalized DOI values.
selected_file <- tclvalue(
  tkgetOpenFile(
    title = paste0(
      "Please select a CSV or RDS file with columns ",
      "\"doi_dcc\" and \"doi_charite\""
    )
  )
)

# Stop immediately if the file-selection dialog was cancelled.
if (selected_file == "") {
  stop("No input file selected!")
}

# Ask the user to select the directory where all intermediate and final
# RDS files should be written.
selected_output_folder <- tclvalue(
  tkchooseDirectory(
    title = "Please select an output folder"
  )
)

# Stop immediately if no output directory was selected.
if (selected_output_folder == "") {
  stop("No output folder selected!")
}

# Determine the extension of the selected input file.
#
# The extension is converted to lowercase so that, for example,
# ".RDS" and ".rds" are treated equivalently.
file_ext <- selected_file |>
  tools::file_ext() |>
  tolower()

# Read the selected file using the appropriate function.
#
# CSV:
#   read.csv() is used and strings are not automatically converted
#   into factors.
#
# RDS:
#   readRDS() restores the saved R object directly.
#
# Any other extension causes the script to stop.
df <- switch(
  file_ext,
  csv = read.csv(
    selected_file,
    stringsAsFactors = FALSE
  ),
  rds = readRDS(selected_file),
  stop("Please select a .csv or .rds file.")
)

# Verify that the selected file actually produced a data.frame/tibble.
#
# This prevents later dplyr operations from failing on an unrelated
# object that happened to be stored inside an RDS file.
if (!inherits(df, "data.frame")) {
  stop(
    "The selected file must contain a data frame / tibble."
  )
}

# Define the DOI columns that are mandatory for this workflow.
required_cols <- c(
  "doi_dcc",
  "doi_charite"
)

# Determine whether any required columns are absent from the input.
missing_cols <- setdiff(
  required_cols,
  names(df)
)

# Stop with a detailed error message if either required DOI column
# is missing.
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

# Extract the filename without its directory and extension.
#
# This becomes the common prefix for the intermediate and final
# output filenames.
#
# Example:
#
# all_sources_dcc_joined_condensed_dist.rds
#
# becomes:
#
# all_sources_dcc_joined_condensed_dist
base_name <- selected_file |>
  basename() |>
  tools::file_path_sans_ext()


# Helpers -----------------------------------------------------------------

# Normalize DOI values into one consistent representation.
#
# This function is used everywhere DOI equality or DOI joining matters.
# Using one centralized normalization rule avoids situations where
# different parts of the workflow clean DOIs differently.
#
# Examples that all become:
#
#   10.1000/xyz
#
# include:
#
#   10.1000/xyz
#   https://doi.org/10.1000/xyz
#   DOI: 10.1000/XYZ
#
# Processing steps:
#
# - Convert values to character.
# - Remove leading/trailing whitespace.
# - Convert letters to lowercase.
# - Remove a leading "doi:" prefix.
# - Remove DOI.org / dx.doi.org URL prefixes.
# - Convert empty strings into genuine R NA values.
normalize_doi_column <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    stringr::str_to_lower() |>
    stringr::str_remove("^doi:\\s*") |>
    stringr::str_remove(
      "^https?://(?:dx\\.)?doi\\.org/"
    ) |>
    dplyr::na_if("")
}

# Normalize a DOI vector specifically for OpenAlex extraction.
#
# In addition to applying normalize_doi_column(), this function:
#
# - removes missing DOI values because they cannot be queried;
# - keeps only unique DOI values.
#
# The unique() step is important for large datasets:
# if one DOI occurs hundreds of times in the original result table,
# OpenAlex is queried only once for that DOI during a given pass.
normalize_doi_vector <- function(x) {
  x <- normalize_doi_column(x)
  x <- x[!is.na(x)]
  unique(x)
}

# Normalize a semicolon-separated author list before comparing authors
# between a DCC publication and a Charité publication.
#
# This function deliberately converts missing author metadata into an
# empty author list. Therefore:
#
#   NA vs NA
#   NA vs "Smith"
#
# do NOT count as demonstrated author overlap.
#
# Only an actual normalized author name occurring on both sides is
# considered overlap.
#
# Processing steps:
#
# - Replace NA with an empty string.
# - Normalize several visually different Unicode dash characters
#   into the standard "-" character.
# - Split semicolon-separated author strings into individual authors.
# - Remove unnecessary surrounding/repeated whitespace.
# - Convert names to lowercase for case-insensitive matching.
# - Remove empty author entries.
# - Remove duplicate author names.
#
# The Unicode-dash normalization helps names such as:
#
#   Smith-Jones
#   Smith–Jones
#   Smith—Jones
#
# compare consistently.
normalize_author_list <- function(x) {
  x |>
    dplyr::coalesce("") |>
    stringr::str_replace_all(
      "[‐-‒–—−]",
      "-"
    ) |>
    stringr::str_split(";") |>
    purrr::pluck(1) |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    (\(authors) authors[authors != ""])() |>
    unique()
}

# Determine whether two author strings have NO demonstrated author overlap.
#
# The function:
#
# - normalizes both author lists;
# - finds authors appearing in both;
# - returns TRUE when the intersection is empty.
#
# TRUE:
#   keep the row during the "no shared authors" filtering stage.
#
# FALSE:
#   at least one author occurs on both sides, so the row belongs in
#   the shared-author exclusion table.
#
# Important:
#
# Missing author metadata does not itself count as overlap because
# normalize_author_list(NA) produces an empty author list.
no_author_overlap <- function(x, y) {
  x_authors <- normalize_author_list(x)
  y_authors <- normalize_author_list(y)
  
  length(
    intersect(
      x_authors,
      y_authors
    )
  ) == 0
}

# Count the number of rows contained in each nested data.frame.
#
# openalexR can return lists containing data.frames nested inside
# list elements. This helper determines how many rows each nested
# data.frame contributes when those results are combined.
#
# Non-data.frame elements contribute zero rows.
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

# Combine a list of nested data.frames into one regular data.frame
# while preserving the identifier associated with each nested result.
#
# Arguments:
#
# x:
#   List containing OpenAlex result data.frames.
#
# pid:
#   Identifier associated with each element of x.
#
# The identifier is repeated according to the number of rows in each
# nested data.frame and written into a new column named PID.
#
# This is used both for:
#
# - top-level OpenAlex work results;
# - nested authorship data.
UnnestDataFrame <- function(x, pid) {
  # Identify which list elements are actually data.frames.
  valid_rows <- vapply(
    x,
    is.data.frame,
    logical(1)
  )
  
  # Keep only valid nested data.frames.
  valid_x <- x[valid_rows]
  
  # Keep the corresponding identifiers.
  valid_pid <- pid[valid_rows]
  
  # If nothing valid remains, return an empty data.frame instead of
  # attempting bind_rows() on an unusable object.
  if (length(valid_x) == 0) {
    return(data.frame())
  }
  
  # Combine all valid nested data.frames into one table.
  unnested <- dplyr::bind_rows(valid_x)
  
  # Repeat each originating identifier for the number of rows that
  # came from that nested data.frame.
  unnested$PID <- rep(
    valid_pid,
    times = NestedDataFrameSize(valid_x)
  )
  
  unnested
}

# Save an intermediate processing object as an RDS checkpoint.
#
# All checkpoint filenames are constructed from:
#
#   original input filename + supplied suffix
#
# and are written into the selected output directory.
#
# Saving checkpoints makes the workflow traceable: if a row disappears
# later, the earlier processing stages can be inspected without rerunning
# the entire OpenAlex extraction.
save_checkpoint <- function(
    object,
    suffix,
    output_folder = selected_output_folder) {
  
  # Build the complete output path.
  save_path <- file.path(
    output_folder,
    paste0(
      base_name,
      suffix
    )
  )
  
  # Save the supplied R object.
  saveRDS(
    object,
    save_path
  )
  
  # Report the exact saved path in the R console.
  message(
    "Saved: ",
    save_path
  )
  
  # Return the path invisibly so the function can be used without
  # printing an additional value to the console.
  invisible(save_path)
}


# OpenAlex ----------------------------------------------------------------

# Query OpenAlex for a vector of DOI values.
#
# Important behavior:
#
# - DOI normalization and deduplication happen before querying.
# - Each distinct DOI is queried individually.
# - Progress is continuously displayed in the console as:
#
#     current / total (percentage)
#
# - The progress display uses "\r" so the same console line is updated
#   rather than printing one permanent line for every successful DOI.
# - An OpenAlex/API error for one DOI does not terminate the whole run.
# - There is NO interactive "retry? y/n" prompt.
# - Failed or incomplete metadata will be dealt with later by the
#   automatic second-pass logic.
#
# The returned object is a named list containing successful OpenAlex
# result objects.
openalex_extract <- function(
    doi_vector,
    progress_label = "OpenAlex") {
  
  # Normalize DOI values, remove NA values, and retain only distinct DOIs.
  dois <- normalize_doi_vector(
    doi_vector
  )
  
  # If there are no usable DOIs, return an empty list immediately.
  if (length(dois) == 0) {
    return(list())
  }
  
  # Add the "doi:" prefix expected by OpenAlex identifiers.
  #
  # Example:
  #
  #   10.1000/xyz
  #
  # becomes:
  #
  #   doi:10.1000/xyz
  identifiers <- paste0(
    "doi:",
    dois
  )
  
  # This list will collect successful OpenAlex responses.
  results_list <- list()
  
  # Total number of distinct DOI requests in this extraction pass.
  total_dois <- length(identifiers)
  
  # Process each distinct DOI sequentially.
  for (i in seq_along(identifiers)) {
    identifier <- identifiers[[i]]
    
    # Continuously update one console line showing extraction progress.
    #
    # Example:
    #
    # DCC first attempt: 1842 / 12755 (14.4%)
    cat(
      sprintf(
        "\r%s: %d / %d (%.1f%%)",
        progress_label,
        i,
        total_dois,
        100 * i / total_dois
      )
    )
    
    # Force the RStudio console to display the updated progress
    # immediately instead of waiting for the output buffer.
    flush.console()
    
    # Query OpenAlex.
    #
    # Errors are caught so one failed DOI does not stop an overnight run.
    query <- tryCatch(
      {
        openalexR::oa_fetch(
          entity = "works",
          identifier = identifier,
          verbose = FALSE
        )
      },
      error = function(error) {
        # Move error reporting onto a new line instead of writing it
        # over the active progress-counter line.
        cat("\n")
        
        # Report the DOI and OpenAlex error details.
        message(
          "Could not fetch ",
          identifier,
          ": ",
          conditionMessage(error)
        )
        
        # Return NULL for this DOI so processing can continue.
        NULL
      }
    )
    
    # Keep only successful, non-NULL responses.
    #
    # Failed requests are simply absent from results_list and will later
    # appear as missing metadata when a complete metadata index is built.
    if (!is.null(query)) {
      results_list[[identifier]] <- query
    }
  }
  
  # Finish the progress display with a newline.
  cat("\n")
  
  # Return all successful OpenAlex result objects.
  results_list
}

# Convert raw OpenAlex work responses into a compact DOI metadata table.
#
# The resulting columns are:
#
#   doi_lookup
#   publication_year
#   authors
#
# Each doi_lookup represents one normalized DOI.
#
# Authors for a publication are collapsed into one semicolon-separated
# string so they can later be compared row-by-row after joining.
extract_openalex_metadata <- function(
    doi_vector,
    progress_label = "OpenAlex") {
  
  # Define the structure that should be returned if no usable metadata
  # can be extracted.
  #
  # Returning a correctly structured empty tibble keeps later joins and
  # helper functions predictable.
  empty_result <- tibble::tibble(
    doi_lookup = character(),
    publication_year = numeric(),
    authors = character()
  )
  
  # Run the actual OpenAlex DOI queries.
  results <- openalex_extract(
    doi_vector = doi_vector,
    progress_label = progress_label
  )
  
  # If no DOI returned a successful result, return the predefined
  # empty metadata table.
  if (length(results) == 0) {
    return(empty_result)
  }
  
  # Combine the top-level OpenAlex result data.frames.
  #
  # names(results) contains the identifier associated with each result.
  results_unnested <- UnnestDataFrame(
    results,
    names(results)
  )
  
  # Stop metadata extraction gracefully if the unnested result contains
  # no usable rows.
  if (
    !inherits(
      results_unnested,
      "data.frame"
    ) ||
    nrow(results_unnested) == 0
  ) {
    return(empty_result)
  }
  
  # A DOI field is necessary for linking OpenAlex metadata back to
  # the requested DOI values.
  if (!"doi" %in% names(results_unnested)) {
    return(empty_result)
  }
  
  # Some OpenAlex responses may lack a publication_year field.
  #
  # Create it explicitly as NA so downstream code can use one
  # consistent metadata schema.
  if (
    !"publication_year" %in%
    names(results_unnested)
  ) {
    results_unnested$publication_year <- NA_real_
  }
  
  # Start with an empty author table.
  #
  # If authorship information exists and can be unpacked below,
  # this object will be replaced with the extracted author metadata.
  authors_tbl <- tibble::tibble(
    doi = character(),
    authors = character()
  )
  
  # OpenAlex stores authorship information as nested data.
  #
  # Only attempt to unnest it when the expected authorships column exists.
  if (
    "authorships" %in%
    names(results_unnested)
  ) {
    # Expand the nested authorship data into one regular table.
    #
    # results_unnested$doi is passed as the parent identifier so each
    # author remains associated with the correct publication DOI.
    unnested_authors <- UnnestDataFrame(
      results_unnested$authorships,
      results_unnested$doi
    )
    
    # Continue only if authorship rows exist and contain the fields
    # needed to identify the publication and author display name.
    if (
      nrow(unnested_authors) > 0 &&
      all(
        c(
          "PID",
          "display_name"
        ) %in%
        names(unnested_authors)
      )
    ) {
      authors_tbl <- unnested_authors |>
        # Rename PID back to doi and give display_name the generic
        # metadata field name "authors".
        dplyr::rename(
          doi = PID,
          authors = display_name
        ) |>
        # Do not collapse missing author names into the final string.
        dplyr::filter(
          !is.na(authors)
        ) |>
        # Gather all authors belonging to the same DOI.
        dplyr::group_by(doi) |>
        # Store the complete author list as one semicolon-separated
        # string per publication.
        dplyr::summarise(
          authors = stringr::str_c(
            authors,
            collapse = ";"
          ),
          .groups = "drop"
        )
    }
  }
  
  # Build the compact metadata table used by the rest of the workflow.
  results_unnested |>
    # Keep only DOI and publication year from the top-level work result.
    dplyr::select(
      doi,
      publication_year
    ) |>
    # Remove duplicate DOI/year combinations that may appear after
    # unpacking OpenAlex results.
    dplyr::distinct() |>
    # Attach the semicolon-separated author strings.
    dplyr::left_join(
      authors_tbl,
      by = "doi"
    ) |>
    # Normalize OpenAlex's DOI representation using exactly the same
    # DOI-normalization function used for the original input table.
    #
    # This is crucial for reliable later joins.
    dplyr::mutate(
      doi_lookup =
        normalize_doi_column(doi)
    ) |>
    # A missing normalized DOI cannot be used for joining.
    dplyr::filter(
      !is.na(doi_lookup)
    ) |>
    # Keep at most one metadata row per normalized DOI.
    dplyr::distinct(
      doi_lookup,
      .keep_all = TRUE
    ) |>
    # Return only the standardized metadata fields needed downstream.
    dplyr::select(
      doi_lookup,
      publication_year,
      authors
    )
}

# Build a complete metadata index covering EVERY DOI that was requested,
# including DOIs for which OpenAlex returned no result.
#
# Why this is necessary:
#
# extract_openalex_metadata() contains only metadata that could actually
# be extracted. If an OpenAlex request failed completely, that DOI could
# otherwise disappear from the metadata table.
#
# This helper starts from the full set of requested distinct DOIs and
# left-joins the fetched metadata. As a result, an unsuccessful DOI remains
# present with publication_year/authors = NA.
#
# Those NA values are then visible to the automatic second-attempt logic.
create_metadata_index <- function(
    doi_vector,
    fetched_metadata) {
  
  # Normalize and deduplicate the requested DOI list.
  requested_dois <- normalize_doi_vector(
    doi_vector
  )
  
  # Create one row per requested distinct DOI, then attach whatever
  # metadata OpenAlex successfully returned.
  tibble::tibble(
    doi_lookup = requested_dois
  ) |>
    dplyr::left_join(
      fetched_metadata,
      by = "doi_lookup"
    )
}

# Identify DOI values whose metadata is incomplete.
#
# A DOI is considered incomplete when AT LEAST ONE of these is missing:
#
#   publication_year
#   authors
#
# Therefore:
#
# year present + authors NA  -> retry
# year NA      + authors present -> retry
# year NA      + authors NA  -> retry
#
# Only distinct DOI values are returned.
get_incomplete_dois <- function(metadata) {
  metadata |>
    dplyr::filter(
      is.na(publication_year) |
        is.na(authors)
    ) |>
    dplyr::pull(doi_lookup) |>
    unique()
}

# Fill missing first-pass metadata with values obtained during the
# automatic second OpenAlex attempt.
#
# Existing successful values are never overwritten.
#
# Example:
#
# First pass:
#   publication_year = 2020
#   authors = NA
#
# Second pass:
#   publication_year = 2020
#   authors = "Smith;Jones"
#
# Final:
#   publication_year = 2020
#   authors = "Smith;Jones"
#
# dplyr::coalesce() guarantees that the original value has priority
# whenever it is already non-missing.
fill_missing_metadata <- function(
    original_metadata,
    retry_metadata) {
  
  # If the second attempt produced no metadata rows at all, simply return
  # the first-pass metadata unchanged.
  if (nrow(retry_metadata) == 0) {
    return(original_metadata)
  }
  
  # Rename retry fields so first-pass and second-pass values can coexist
  # temporarily after joining.
  retry_metadata <- retry_metadata |>
    dplyr::rename(
      retry_publication_year =
        publication_year,
      retry_authors = authors
    )
  
  original_metadata |>
    # Attach second-pass metadata by normalized DOI.
    dplyr::left_join(
      retry_metadata,
      by = "doi_lookup"
    ) |>
    # Keep first-pass values when present.
    #
    # Use retry values only where the corresponding first-pass field
    # is still NA.
    dplyr::mutate(
      publication_year = dplyr::coalesce(
        publication_year,
        retry_publication_year
      ),
      authors = dplyr::coalesce(
        authors,
        retry_authors
      )
    ) |>
    # Drop the temporary retry columns and restore the standard
    # metadata-table structure.
    dplyr::select(
      doi_lookup,
      publication_year,
      authors
    )
}

# Perform the automatic second OpenAlex attempt for one metadata source.
#
# This function is used independently for:
#
#   DCC metadata
#   Charité metadata
#
# It does not ask the user whether to retry, so a long-running workflow
# can continue unattended.
#
# Only DOIs whose year OR authors remain missing after the first pass
# are queried again.
#
# DOIs with complete metadata are not unnecessarily re-fetched.
run_second_attempt <- function(
    metadata,
    progress_label) {
  
  # Determine which distinct DOIs still have incomplete metadata.
  incomplete_dois <- get_incomplete_dois(
    metadata
  )
  
  # Skip the second OpenAlex pass entirely when all metadata is complete.
  if (length(incomplete_dois) == 0) {
    message(
      progress_label,
      ": no incomplete metadata found. ",
      "No second attempt needed."
    )
    
    return(metadata)
  }
  
  # Report how many distinct incomplete DOI values are about to be retried.
  message(
    progress_label,
    ": second attempt for ",
    length(incomplete_dois),
    " distinct DOI(s)."
  )
  
  # Query only the incomplete DOI values.
  retry_fetched <- extract_openalex_metadata(
    doi_vector = incomplete_dois,
    progress_label = paste0(
      progress_label,
      " second attempt"
    )
  )
  
  # Build a complete retry index so DOIs that fail again are still
  # represented with NA metadata.
  retry_metadata <- create_metadata_index(
    doi_vector = incomplete_dois,
    fetched_metadata = retry_fetched
  )
  
  # Patch the first-pass metadata.
  #
  # Existing non-NA values remain final.
  # Retry values are used only to fill missing fields.
  fill_missing_metadata(
    original_metadata = metadata,
    retry_metadata = retry_metadata
  )
}

# Combine DCC and Charité metadata into one table for convenient
# checkpoint saving.
#
# The source of each metadata row is explicitly recorded in doi_source.
#
# This combined object is for inspection/checkpointing only.
# DCC and Charité metadata are kept separately for the actual join
# back onto the original result table.
combine_metadata_for_output <- function(
    dcc_metadata,
    charite_metadata) {
  
  dplyr::bind_rows(
    # Label all DCC DOI metadata.
    dcc_metadata |>
      dplyr::mutate(
        doi_source = "dcc",
        .before = 1
      ),
    
    # Label all Charité DOI metadata.
    charite_metadata |>
      dplyr::mutate(
        doi_source = "charite",
        .before = 1
      )
  )
}


# Prepare DOI columns -----------------------------------------------------

# Create normalized DOI lookup columns inside a working copy of the
# ORIGINAL input table.
#
# The original doi_dcc and doi_charite columns are preserved exactly
# as they were supplied.
#
# The lookup columns are temporary standardized keys used to:
#
# - identify distinct DOI values;
# - query OpenAlex consistently;
# - join metadata back to the correct original rows.
df_prepared <- df |>
  dplyr::mutate(
    doi_dcc_lookup =
      normalize_doi_column(doi_dcc),
    doi_charite_lookup =
      normalize_doi_column(doi_charite)
  )

# Extract the distinct normalized DCC DOI values.
#
# Even if one DOI occurs many times in df_prepared, it occurs only once
# in distinct_dcc_dois and is therefore queried only once per pass.
distinct_dcc_dois <- normalize_doi_vector(
  df_prepared$doi_dcc_lookup
)

# Extract the distinct normalized Charité DOI values.
distinct_charite_dois <- normalize_doi_vector(
  df_prepared$doi_charite_lookup
)

# Report the total number of rows in the original result table.
message(
  "Input rows: ",
  nrow(df_prepared)
)

# Report how many distinct DCC DOI requests may need to be processed.
message(
  "Distinct DCC DOIs: ",
  length(distinct_dcc_dois)
)

# Report how many distinct Charité DOI requests may need to be processed.
message(
  "Distinct Charité DOIs: ",
  length(distinct_charite_dois)
)


# First metadata extraction -----------------------------------------------

# Start the first OpenAlex pass for all distinct DCC DOIs.
message(
  "\nStarting first OpenAlex extraction for DCC DOIs."
)

# Fetch DCC publication years and authors from OpenAlex.
dcc_first_fetched <- extract_openalex_metadata(
  doi_vector = distinct_dcc_dois,
  progress_label = "DCC first attempt"
)

# Re-expand the fetched DCC metadata onto the complete set of requested
# distinct DCC DOI values.
#
# DOIs that failed to produce usable metadata remain in this table with
# NA fields, which makes them eligible for the automatic retry stage.
dcc_metadata_first <- create_metadata_index(
  doi_vector = distinct_dcc_dois,
  fetched_metadata = dcc_first_fetched
)

# Start the first OpenAlex pass for all distinct Charité DOIs.
message(
  "\nStarting first OpenAlex extraction for Charité DOIs."
)

# Fetch Charité publication years and authors from OpenAlex.
charite_first_fetched <-
  extract_openalex_metadata(
    doi_vector = distinct_charite_dois,
    progress_label = "Charité first attempt"
  )

# Re-expand the fetched Charité metadata onto the complete set of requested
# distinct Charité DOI values.
#
# Failed or incomplete DOI metadata remains visible as NA.
charite_metadata_first <-
  create_metadata_index(
    doi_vector = distinct_charite_dois,
    fetched_metadata = charite_first_fetched
  )

# Combine both first-pass metadata tables for checkpointing and inspection.
#
# needs_second_attempt is TRUE whenever a publication has a missing year
# OR missing author metadata after the first OpenAlex pass.
metadata_first_pass <-
  combine_metadata_for_output(
    dcc_metadata = dcc_metadata_first,
    charite_metadata =
      charite_metadata_first
  ) |>
  dplyr::mutate(
    needs_second_attempt =
      is.na(publication_year) |
      is.na(authors)
  )

# CHECKPOINT 01
#
# Save all first-pass OpenAlex metadata before retrying incomplete cases.
#
# This file allows later inspection of:
#
# - which DCC/Charité DOIs were queried;
# - what metadata was obtained on the first attempt;
# - which DOI values were marked for the automatic second attempt.
#
# Filename pattern:
#
# <original_name>_01_openalex_metadata_first_pass.rds
save_checkpoint(
  metadata_first_pass,
  "_01_openalex_metadata_first_pass.rds"
)


# Second metadata extraction ----------------------------------------------

# Check DCC metadata for publications where either year or authors
# remain missing.
message(
  "\nChecking DCC metadata for incomplete records."
)

# Automatically retry incomplete distinct DCC DOI values.
#
# No user interaction is required.
#
# Already-populated first-pass values are preserved.
dcc_metadata_final <- run_second_attempt(
  metadata = dcc_metadata_first,
  progress_label = "DCC"
)

# Check Charité metadata for publications where either year or authors
# remain missing.
message(
  "\nChecking Charité metadata for incomplete records."
)

# Automatically retry incomplete distinct Charité DOI values.
charite_metadata_final <-
  run_second_attempt(
    metadata = charite_metadata_first,
    progress_label = "Charité"
  )

# Combine the finalized DCC and Charité metadata after the automatic retry.
#
# metadata_still_incomplete identifies DOI values for which at least
# one metadata field remains NA even after both possible attempts.
#
# Those cases are NOT removed here.
metadata_final <-
  combine_metadata_for_output(
    dcc_metadata = dcc_metadata_final,
    charite_metadata =
      charite_metadata_final
  ) |>
  dplyr::mutate(
    metadata_still_incomplete =
      is.na(publication_year) |
      is.na(authors)
  )

# CHECKPOINT 02
#
# Save the finalized DOI-level metadata after both extraction attempts.
#
# This is the most complete OpenAlex metadata available to the workflow
# before it is joined back onto the original result table.
#
# Filename pattern:
#
# <original_name>_02_openalex_metadata_final.rds
save_checkpoint(
  metadata_final,
  "_02_openalex_metadata_final.rds"
)


# Join metadata -----------------------------------------------------------

# Rename the generic DCC metadata fields so their source remains clear
# after joining to the original table.
#
# doi_lookup         -> doi_dcc_lookup
# publication_year   -> doi_dcc_year
# authors            -> doi_dcc_authors
dcc_metadata_join <- dcc_metadata_final |>
  dplyr::rename(
    doi_dcc_lookup = doi_lookup,
    doi_dcc_year = publication_year,
    doi_dcc_authors = authors
  )

# Rename the generic Charité metadata fields for the same reason.
#
# doi_lookup         -> doi_charite_lookup
# publication_year   -> doi_charite_year
# authors            -> doi_charite_authors
charite_metadata_join <-
  charite_metadata_final |>
  dplyr::rename(
    doi_charite_lookup = doi_lookup,
    doi_charite_year =
      publication_year,
    doi_charite_authors = authors
  )

# Join the FINALIZED DOI-level metadata back onto every row of the
# ORIGINAL result table.
#
# This join is intentionally performed only after both OpenAlex attempts
# have finished.
#
# Because metadata was extracted using distinct DOI values:
#
# - each DOI was queried only once per extraction pass;
# - if the same DOI occurs in many original rows, the corresponding
#   metadata is copied onto every matching row by left_join().
#
# left_join() preserves all rows from the original input table.
final_results_with_metadata <-
  df_prepared |>
  dplyr::left_join(
    dcc_metadata_join,
    by = "doi_dcc_lookup"
  ) |>
  dplyr::left_join(
    charite_metadata_join,
    by = "doi_charite_lookup"
  ) |>
  # The normalized lookup columns were internal processing keys.
  #
  # Remove them here so the saved result contains the original DOI fields
  # plus the newly attached metadata fields.
  dplyr::select(
    -doi_dcc_lookup,
    -doi_charite_lookup
  )

# CHECKPOINT 03
#
# Save the complete original result table after finalized OpenAlex
# metadata has been joined but BEFORE any rows are excluded.
#
# This is the best checkpoint for examining the complete metadata-enriched
# dataset without author-overlap or same-DOI filtering.
#
# Filename pattern:
#
# <original_name>_03_joined_with_openalex_metadata.rds
save_checkpoint(
  final_results_with_metadata,
  "_03_joined_with_openalex_metadata.rds"
)


# Author overlap exclusion ------------------------------------------------

# Determine whether each row has at least one demonstrated author name
# appearing in both the DCC and Charité author lists.
#
# no_author_overlap() returns:
#
#   TRUE  = no shared author was demonstrated
#   FALSE = at least one author occurs on both sides
#
# The leading "!" reverses this so has_author_overlap becomes:
#
#   TRUE  = shared author detected
#   FALSE = no shared author detected
#
# Importantly, this check occurs only AFTER:
#
# - the first metadata extraction;
# - the automatic second metadata extraction;
# - finalized metadata has been joined.
#
# Missing author information does not itself count as overlap.
has_author_overlap <- !purrr::map2_lgl(
  final_results_with_metadata$doi_dcc_authors,
  final_results_with_metadata$doi_charite_authors,
  no_author_overlap
)

# Collect rows that WILL BE EXCLUDED because at least one normalized
# author occurs on both the DCC and Charité sides.
#
# These rows are saved separately rather than being discarded completely.
removed_shared_authors <-
  final_results_with_metadata[
    has_author_overlap,
    ,
    drop = FALSE
  ]

# Keep all rows where no author overlap was demonstrated.
#
# This retained dataset also includes rows where author metadata remains
# incomplete after both OpenAlex attempts.
#
# For example:
#
# DCC authors       = "Smith"
# Charité authors   = NA
#
# remains here because there is no demonstrated shared author.
final_results_authors_different <-
  final_results_with_metadata[
    !has_author_overlap,
    ,
    drop = FALSE
  ]

# CHECKPOINT 04
#
# Save the rows excluded specifically because shared authors were detected.
#
# Filename pattern:
#
# <original_name>_04_removed_shared_authors.rds
save_checkpoint(
  removed_shared_authors,
  "_04_removed_shared_authors.rds"
)

# CHECKPOINT 05
#
# Save all rows remaining after shared-author exclusion but BEFORE
# identical-DOI exclusion.
#
# This file therefore contains:
#
# - rows with no demonstrated shared authors;
# - rows with incomplete author metadata where no overlap could be shown;
# - potentially some rows where doi_dcc and doi_charite are identical.
#
# Filename pattern:
#
# <original_name>_05_no_shared_authors.rds
save_checkpoint(
  final_results_authors_different,
  "_05_no_shared_authors.rds"
)


# Same DOI exclusion ------------------------------------------------------

# Create temporary normalized DOI lookup columns again for the final
# DCC-vs-Charité DOI equality comparison.
#
# Exactly the same normalize_doi_column() function used for OpenAlex
# extraction and joining is used here.
#
# Therefore values such as:
#
#   10.1000/xyz
#   https://doi.org/10.1000/xyz
#   DOI: 10.1000/XYZ
#
# are treated as the same DOI.
results_with_doi_comparison <-
  final_results_authors_different |>
  dplyr::mutate(
    doi_dcc_lookup =
      normalize_doi_column(doi_dcc),
    doi_charite_lookup =
      normalize_doi_column(doi_charite)
  )

# Identify rows that should be excluded because:
#
# - both DCC and Charité DOI values exist;
# - after normalization, the two DOI values are identical.
#
# Rows where either DOI is missing are NOT placed in this exclusion set
# because equality cannot be demonstrated.
same_doi_rows <-
  results_with_doi_comparison |>
  dplyr::filter(
    !is.na(doi_dcc_lookup),
    !is.na(doi_charite_lookup),
    doi_dcc_lookup ==
      doi_charite_lookup
  ) |>
  # Remove the temporary normalized comparison columns before saving.
  dplyr::select(
    -doi_dcc_lookup,
    -doi_charite_lookup
  )

# Build the final result table.
#
# Keep a row when:
#
# - DCC DOI is missing; OR
# - Charité DOI is missing; OR
# - both exist but the normalized DOI values are different.
#
# At this stage, shared-author rows have already been removed.
final_results <-
  results_with_doi_comparison |>
  dplyr::filter(
    is.na(doi_dcc_lookup) |
      is.na(doi_charite_lookup) |
      doi_dcc_lookup !=
      doi_charite_lookup
  ) |>
  # Remove temporary DOI comparison keys from the final result.
  dplyr::select(
    -doi_dcc_lookup,
    -doi_charite_lookup
  )

# CHECKPOINT 06
#
# Save rows excluded specifically because:
#
#   normalized doi_dcc == normalized doi_charite
#
# These rows had already passed the shared-author exclusion step.
#
# Filename pattern:
#
# <original_name>_06_removed_shared_doi.rds
save_checkpoint(
  same_doi_rows,
  "_06_removed_shared_doi.rds"
)

# Construct the filename of the FINAL result table.
#
# For an input such as:
#
# all_sources_dcc_joined_condensed_dist.rds
#
# the final output becomes:
#
# all_sources_dcc_joined_condensed_dist_w_au_year_au_ov_no_shared_doi.rds
final_save_path <- file.path(
  selected_output_folder,
  paste0(
    base_name,
    "_w_au_year_au_ov_no_shared_doi.rds"
  )
)

# FINAL OUTPUT
#
# Save the final table containing:
#
# - all original input columns;
# - finalized DCC OpenAlex publication year and authors;
# - finalized Charité OpenAlex publication year and authors;
# - no rows with demonstrated shared authors;
# - no rows where normalized doi_dcc == normalized doi_charite.
#
# Rows with unresolved NA metadata remain in the final table unless they
# were excluded for an independently demonstrated reason.
saveRDS(
  final_results,
  final_save_path
)


# Summary -----------------------------------------------------------------

# Count distinct DCC DOI metadata rows where at least one metadata field
# remains missing even after the automatic second attempt.
dcc_still_incomplete <-
  dcc_metadata_final |>
  dplyr::filter(
    is.na(publication_year) |
      is.na(authors)
  ) |>
  nrow()

# Count distinct Charité DOI metadata rows where at least one metadata
# field remains missing even after the automatic second attempt.
charite_still_incomplete <-
  charite_metadata_final |>
  dplyr::filter(
    is.na(publication_year) |
      is.na(authors)
  ) |>
  nrow()

# Print a final console summary so the user can immediately see:
#
# - original table size;
# - number of distinct DCC/Charité DOIs processed;
# - remaining incomplete DOI-level metadata;
# - how many rows were removed for shared authors;
# - how many rows remained after author-overlap exclusion;
# - how many were additionally removed for identical DOIs;
# - final table size;
# - exact location of the final RDS file.
message(
  "\nFinished.",
  "\n",
  "\nInput rows: ",
  nrow(df),
  "\nDistinct DCC DOIs: ",
  length(distinct_dcc_dois),
  "\nDistinct Charité DOIs: ",
  length(distinct_charite_dois),
  "\nDCC DOIs still containing incomplete metadata: ",
  dcc_still_incomplete,
  "\nCharité DOIs still containing incomplete metadata: ",
  charite_still_incomplete,
  "\nRows removed because of shared authors: ",
  nrow(removed_shared_authors),
  "\nRows remaining after author-overlap exclusion: ",
  nrow(final_results_authors_different),
  "\nRows removed because DCC DOI = Charité DOI: ",
  nrow(same_doi_rows),
  "\nFinal rows: ",
  nrow(final_results),
  "\n",
  "\nFinal result saved to:",
  "\n",
  final_save_path
)