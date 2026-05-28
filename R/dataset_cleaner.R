dataset_cleaner <- function(input, dataset_name, output_folder) {
  if (!is.character(dataset_name) || length(dataset_name) != 1 || !nzchar(dataset_name)) {
    stop("`dataset_name` must be one non-empty character string.", call. = FALSE)
  }
  
  if (!is.character(output_folder) || length(output_folder) != 1 || !nzchar(output_folder)) {
    stop("`output_folder` must be one non-empty character string.", call. = FALSE)
  }
  
  dataset_input <- if (inherits(input, "data.frame")) {
    if (ncol(input) != 1) {
      stop("If `input` is a data frame / tibble, it must contain exactly one column.", call. = FALSE)
    }
    
    input[[1]]
  } else {
    input
  }
  
  if (!is.atomic(dataset_input)) {
    stop("`input` must be a one-column data frame / tibble or an atomic vector.", call. = FALSE)
  }
  
  dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
  
  save_step_rds <- function(object, suffix) {
    file_path <- file.path(
      output_folder,
      paste0(dataset_name, suffix, ".rds")
    )
    
    saveRDS(object, file = file_path)
    
    invisible(file_path)
  }
  
  total_steps <- 12L
  
  step_start <- function(step_id, step_name) {
    cat(sprintf("\n[Step %s/%s] %s\n", step_id, total_steps, step_name))
    flush.console()
    invisible(NULL)
  }
  
  step_done <- function(step_id, n_rows = NULL) {
    if (is.null(n_rows)) {
      cat(sprintf("[Step %s/%s] done\n", step_id, total_steps))
    } else {
      cat(sprintf("[Step %s/%s] done (%s rows)\n", step_id, total_steps, format(n_rows, big.mark = ",")))
    }
    flush.console()
    invisible(NULL)
  }
  
  prefixes <- c(
    "sam", "gse", "gsm", "gds", "gpl", "e-mtab-", "egas", "egad", "e-geod",
    "mk", "mh", "phs", "mn", "mw", "pxd", "srr", "prj(eb|na|db|da|ea|sa|ma)",
    "emd-", "gcst", "pdb_", "nm_", "nct", "err", "msv", "mz", "nc_", "np_",
    "sr(p|r|x|s|z)", "pgs", "s-bsst", "mt", "kt", "st", "ol", "op", "or", "oq",
    "scp", "s-biad", "e-tabm", "empiar", "fr-fcm-z", "gca_", "egac", "up", "ng",
    "gcf_", "ensg", "syn", "jpst"
  )
  
  custom_patterns <- c(
    "^lation450",
    "^hiseq",
    "^human",
    "^matlab2019",
    "^[a-zA-Z]{3}$",
    "^mm",
    "^nct",
    "ovaseq6000"
  )
  
  skip_if_included <- function(new_matches, existing_matches) {
    if (length(new_matches) == 0) {
      return(character())
    }
    
    if (length(existing_matches) == 0) {
      return(unique(new_matches))
    }
    
    new_matches[
      !vapply(
        new_matches,
        function(nm) {
          any(
            stringr::str_detect(existing_matches, stringr::fixed(nm)) |
              stringr::str_detect(nm, stringr::fixed(existing_matches))
          )
        },
        logical(1)
      )
    ] |>
      unique()
  }
  
  extract_pdb_ids_from_context <- function(text) {
    pdb_windows <- stringr::str_extract_all(
      text,
      stringr::regex(
        "(?:protein data bank|\\bpdb\\b|\\bpdbs\\b|\\brcsb\\b)[^.;\\n]{0,250}",
        ignore_case = TRUE
      )
    )[[1]]
    
    if (length(pdb_windows) == 0) {
      return(character())
    }
    
    ids <- unlist(
      lapply(
        pdb_windows,
        function(chunk) {
          stringr::str_extract_all(
            chunk,
            stringr::regex("\\b[0-9][A-Za-z0-9]{3}\\b", ignore_case = TRUE)
          )[[1]]
        }
      )
    )
    
    ids |>
      stringr::str_to_lower() |>
      unique()
  }
  
  extract_rcsb_urls <- function(text) {
    stringr::str_extract_all(
      text,
      stringr::regex(
        "(?:https?:)?//(?:www\\.)?rcsb\\.org/structure/[A-Za-z0-9]{4}|https?://(?:www\\.)?rcsb\\.org/structure/[A-Za-z0-9]{4}",
        ignore_case = TRUE
      )
    )[[1]] |>
      unique()
  }
  
  normalize_text <- function(text) {
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    text |>
      iconv(from = "", to = "UTF-8", sub = "") |>
      stringr::str_replace_all("[\r\n\t]+", " ") |>
      stringr::str_squish()
  }
  
  extract_ids <- function(text) {
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    all_matches <- character()
    
    cond1_pattern <- "(?i)(figshare\\.|zenodo\\.|osf\\.io/|mendeley\\.com/datasets/|dryad\\.)(?:[\\s\\da-z._/-]*[\\d][\\s\\da-z._/-]*?)(?=[\\)\\],#';\">:]|$)"
    cond1 <- stringr::str_extract_all(
      text,
      stringr::regex(cond1_pattern, ignore_case = TRUE)
    )[[1]] |>
      unique()
    all_matches <- c(all_matches, cond1)
    
    prefix_pattern <- paste0(
      "(?i)(?<![a-z0-9])(?:",
      paste(prefixes, collapse = "|"),
      ")[a-z0-9._-]*[0-9][a-z0-9._-]*(?=[^a-z0-9._-]|$)"
    )
    cond2 <- stringr::str_extract_all(
      text,
      stringr::regex(prefix_pattern, ignore_case = TRUE)
    )[[1]] |>
      unique() |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, cond2)
    
    cond3_pattern <- "10\\.[0-9]+/[a-z0-9._;()/\\-]+?(?=[\\)\\],#';\">\\s]|$)"
    cond3 <- stringr::str_extract_all(
      text,
      stringr::regex(cond3_pattern, ignore_case = TRUE)
    )[[1]] |>
      unique() |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, cond3)
    
    cond4_pattern <- "(?:https?:)?//[^)\\],#';\">\\s]+"
    cond4 <- stringr::str_extract_all(
      text,
      stringr::regex(cond4_pattern, ignore_case = TRUE)
    )[[1]] |>
      unique() |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, cond4)
    
    other_patterns <- c(
      "fcon_\\s*1000\\.\\s*projects\\.\\s*nitrc\\.\\s*org",
      "dip:[0-9\\s]{3}",
      "fr-fcm-[a-z0-9\\s]{4}",
      "collections?(?:[:/])[0-9\\s]{4}",
      "icpsr\\s*[0-9\\s]{4}",
      "sn\\s*[0-9\\s]{4}",
      "search\\.\\s*kg\\.\\s*ebrains\\.\\s*eu",
      "e\\s*n\\s*c\\s*s\\s*r\\s*0\\s*0\\s*0\\s*[0-9]{3}\\s*[a-z]{3}",
      "\\bjpst[0-9]{6}\\b",
      "\\bpxd[0-9]{6}\\b",
      "\\bemd-[0-9]{4,6}\\b",
      "\\bnc_[0-9]+(?:\\.[0-9]+)?\\b",
      "\\bnp_[0-9]+(?:\\.[0-9]+)?\\b",
      "\\bgcf_[0-9]+(?:\\.[0-9]+)?\\b",
      "\\bgca_[0-9]+(?:\\.[0-9]+)?\\b"
    )
    
    cond5 <- unlist(
      lapply(
        other_patterns,
        function(pat) {
          stringr::str_extract_all(
            text,
            stringr::regex(pat, ignore_case = TRUE)
          )[[1]]
        }
      )
    ) |>
      unique() |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, cond5)
    
    rcsb_urls <- extract_rcsb_urls(text) |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, rcsb_urls)
    
    pdb_ids <- extract_pdb_ids_from_context(text) |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, pdb_ids)
    
    all_matches <- all_matches |>
      stringr::str_trim() |>
      unique()
    
    all_matches <- all_matches[all_matches != ""]
    all_matches <- all_matches[!stringr::str_detect(all_matches, "^\\d+$")]
    
    dplyr::case_when(
      length(all_matches) == 0 ~ NA_character_,
      .default = paste(all_matches, collapse = ";")
    )
  }
  
  expand_id_range <- function(x) {
    if (is.na(x) || !nzchar(x)) {
      return(x)
    }
    
    x <- stringr::str_trim(x)
    
    m1 <- stringr::str_match(x, "^([a-z_]+)([0-9]+)-\\1([0-9]+)$")
    if (!is.na(m1[1, 1])) {
      prefix <- m1[1, 2]
      start_str <- m1[1, 3]
      end_str <- m1[1, 4]
      
      start_num <- as.integer(start_str)
      end_num <- as.integer(end_str)
      width <- max(nchar(start_str), nchar(end_str))
      
      if (!is.na(start_num) && !is.na(end_num) && end_num >= start_num) {
        return(
          paste0(
            prefix,
            stringr::str_pad(seq.int(start_num, end_num), width = width, pad = "0")
          )
        )
      }
    }
    
    m2 <- stringr::str_match(x, "^([a-z_]+)([0-9]+)-([0-9]+)$")
    if (!is.na(m2[1, 1])) {
      prefix <- m2[1, 2]
      start_str <- m2[1, 3]
      end_str_raw <- m2[1, 4]
      
      end_str <- dplyr::case_when(
        nchar(end_str_raw) <= nchar(start_str) ~ paste0(
          substr(start_str, 1, nchar(start_str) - nchar(end_str_raw)),
          end_str_raw
        ),
        .default = end_str_raw
      )
      
      start_num <- as.integer(start_str)
      end_num <- as.integer(end_str)
      width <- max(nchar(start_str), nchar(end_str))
      
      if (!is.na(start_num) && !is.na(end_num) && end_num >= start_num) {
        return(
          paste0(
            prefix,
            stringr::str_pad(seq.int(start_num, end_num), width = width, pad = "0")
          )
        )
      }
    }
    
    m3 <- stringr::str_match(x, "^([a-z_]+)([0-9]+)\\1([0-9]+)$")
    if (!is.na(m3[1, 1])) {
      prefix <- m3[1, 2]
      start_str <- m3[1, 3]
      end_str <- m3[1, 4]
      
      start_num <- as.integer(start_str)
      end_num <- as.integer(end_str)
      width <- max(nchar(start_str), nchar(end_str))
      
      if (!is.na(start_num) && !is.na(end_num) && end_num >= start_num) {
        return(
          paste0(
            prefix,
            stringr::str_pad(seq.int(start_num, end_num), width = width, pad = "0")
          )
        )
      }
    }
    
    x
  }
  
  step_start(1L, "Create initial table")
  
  data_results_1_tolower <- tibble::tibble(dataset = as.character(dataset_input)) |>
    dplyr::mutate(
      dataset = iconv(dataset, from = "", to = "UTF-8", sub = ""),
      dataset = tolower(dataset)
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_1_tolower, "_1_tolower")
  step_done(1L, nrow(data_results_1_tolower))
  
  n_total <- nrow(data_results_1_tolower)
  
  extract_ids_with_progress <- function(text, i) {
    cat(sprintf("[Step 2/%s] %s/%s\r", total_steps, format(i, big.mark = ","), format(n_total, big.mark = ",")))
    flush.console()
    
    text <- normalize_text(text)
    
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    if (!stringr::str_detect(text, "[[:space:];,]") && nchar(text) <= 200) {
      return(text)
    }
    
    extract_ids(text)
  }
  
  step_start(2L, "Extract ids")
  
  data_results_2_ext_ids <- data_results_1_tolower |>
    dplyr::mutate(
      extracted_id = purrr::map2_chr(
        dataset,
        seq_along(dataset),
        extract_ids_with_progress
      )
    )
  
  cat(sprintf("[Step 2/%s] %s/%s\n", total_steps, format(n_total, big.mark = ","), format(n_total, big.mark = ",")))
  flush.console()
  
  save_step_rds(data_results_2_ext_ids, "_2_ext_ids")
  step_done(2L, nrow(data_results_2_ext_ids))
  
  step_start(3L, "Separate rows")
  
  data_results_3_reshaped <- data_results_2_ext_ids |>
    tidyr::separate_rows(extracted_id, sep = ";") |>
    dplyr::transmute(
      dataset,
      extracted_id = stringr::str_trim(extracted_id)
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_3_reshaped, "_3_reshaped")
  step_done(3L, nrow(data_results_3_reshaped))
  
  step_start(4L, "Clean extracted ids")
  
  data_results_4_cleaned <- data_results_3_reshaped |>
    dplyr::filter(!is.na(extracted_id), extracted_id != "") |>
    dplyr::filter(
      !(
        stringr::str_detect(extracted_id, "zenodo|dryad|osf|figshare|harvard|mendeley|github") &
          !stringr::str_detect(extracted_id, "[0-9]")
      )
    ) |>
    dplyr::filter(!stringr::str_detect(extracted_id, "^[a-zA-Z][0-9]{1,2}$")) |>
    dplyr::filter(
      !stringr::str_detect(
        extracted_id,
        "^[-\\sa-zA-Z]+$|^[-\\s0-9]+$"
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_4_cleaned, "_4_cleaned")
  step_done(4L, nrow(data_results_4_cleaned))
  
  step_start(5L, "Standardize datasets")
  
  data_results_5_std <- data_results_4_cleaned |>
    dplyr::mutate(
      extracted_id = stringr::str_trim(extracted_id),
      slug = extracted_id |>
        stringr::str_replace_all("\\s", "") |>
        stringr::str_extract("osf\\.io/([A-Za-z0-9]{4,5})") |>
        stringr::str_remove("osf\\.io/"),
      dataset_clean = dplyr::case_when(
        stringr::str_detect(extracted_id, "zenodo\\.org/record") ~ stringr::str_c(
          "10.5281/zenodo.",
          extracted_id |>
            stringr::str_extract("zenodo\\.org/record/?\\s*([0-9]+)") |>
            stringr::str_remove("^zenodo\\.org/record/?\\s*")
        ),
        stringr::str_detect(extracted_id, "zenodo") ~ stringr::str_c(
          "10.5281/zenodo.",
          extracted_id |>
            stringr::str_extract("zenodo\\.?\\s*([0-9]+)") |>
            stringr::str_remove("^zenodo\\.?\\s*")
        ),
        !is.na(slug) ~ paste0("//osf.io/", slug),
        stringr::str_detect(extracted_id, "figshare") ~ stringr::str_c(
          "10.6084/m9.figshare.",
          extracted_id |>
            stringr::str_extract("figshare\\.?\\s*([0-9]+)") |>
            stringr::str_remove("^figshare\\.?\\s*")
        ),
        stringr::str_detect(extracted_id, "mendeley.*datasets") ~ stringr::str_c(
          "10.17632/",
          extracted_id |>
            stringr::str_replace_all("\\s", "") |>
            stringr::str_extract("datasets/([A-Za-z0-9]+)") |>
            stringr::str_remove("^datasets/")
        ),
        stringr::str_detect(extracted_id, "mendeley") ~ NA_character_,
        stringr::str_detect(extracted_id, "dryad") ~ stringr::str_c(
          "10.5061/dryad.",
          extracted_id |>
            stringr::str_replace_all("\\s", "") |>
            stringr::str_extract("dryad\\.?([A-Za-z0-9]+)") |>
            stringr::str_remove("^dryad\\.?")
        ),
        stringr::str_detect(
          extracted_id,
          stringr::regex("^(?:https?:)?//(?:www\\.)?rcsb\\.org/structure/[A-Za-z0-9]{4}$", ignore_case = TRUE)
        ) ~ extracted_id |>
          stringr::str_extract(stringr::regex("[A-Za-z0-9]{4}$", ignore_case = TRUE)) |>
          stringr::str_to_lower(),
        stringr::str_detect(
          extracted_id,
          stringr::regex("^[0-9][A-Za-z0-9]{3}$", ignore_case = TRUE)
        ) ~ stringr::str_to_lower(extracted_id),
        stringr::str_detect(stringr::str_replace_all(extracted_id, "\\s", ""), stringr::str_c(prefixes, collapse = "|")) &
          !stringr::str_detect(extracted_id, "https?://|^//") &
          !stringr::str_starts(stringr::str_trim(extracted_id), "10\\.") ~ extracted_id |>
          stringr::str_replace_all("\\s+", "") |>
          stringr::str_to_lower(),
        stringr::str_detect(extracted_id, "^10\\.") &
          !stringr::str_detect(extracted_id, "figshare|zenodo|osf|dryad|mendeley") ~ extracted_id |>
          stringr::str_replace_all("\\s+", "") |>
          stringr::str_extract("10\\.[^\\s]+"),
        stringr::str_detect(extracted_id, "id=") ~ extracted_id |>
          stringr::str_extract("id=([^\\s/&]+)") |>
          stringr::str_remove("^id="),
        stringr::str_detect(extracted_id, "addgene\\.org/") ~ extracted_id |>
          stringr::str_extract("addgene\\.org/([0-9 \\t]*)") |>
          stringr::str_remove("addgene\\.org/") |>
          stringr::str_remove_all("\\s"),
        .default = extracted_id
      )
    ) |>
    dplyr::transmute(
      dataset,
      dataset_clean = stringr::str_trim(dataset_clean)
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_5_std, "_5_std")
  step_done(5L, nrow(data_results_5_std))
  
  step_start(6L, "Filter invalid datasets")
  
  data_results_6_filtered <- data_results_5_std |>
    dplyr::filter(
      !(
        stringr::str_starts(dataset_clean, "//") &
          !stringr::str_detect(
            dataset_clean,
            stringr::regex("^//(?:osf\\.io/|(?:www\\.)?rcsb\\.org/structure/)", ignore_case = TRUE)
          )
      )
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        stringr::str_replace_all(dataset_clean, "\\s", ""),
        "^or\\d+$"
      )
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        stringr::str_replace_all(dataset_clean, "\\s", ""),
        "^[a-zA-Z]\\d+$"
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_6_filtered, "_6_filtered")
  step_done(6L, nrow(data_results_6_filtered))
  
  step_start(7L, "Remove trailing dots")
  
  data_results_7_rm_trails <- data_results_6_filtered |>
    dplyr::mutate(
      dataset_clean = stringr::str_remove(dataset_clean, "\\.+$")
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_7_rm_trails, "_7_rm_trails")
  step_done(7L, nrow(data_results_7_rm_trails))
  
  step_start(8L, "Remove custom patterns")
  
  data_results_8_rm_patterns <- data_results_7_rm_trails |>
    dplyr::filter(
      !stringr::str_detect(
        dataset_clean,
        stringr::regex(paste(custom_patterns, collapse = "|"))
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_8_rm_patterns, "_8_rm_patterns")
  step_done(8L, nrow(data_results_8_rm_patterns))
  
  step_start(9L, "Remove raw URLs")
  
  data_results_9_rm_urls <- data_results_8_rm_patterns |>
    dplyr::filter(
      !stringr::str_detect(dataset_clean, "^https?:")
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_9_rm_urls, "_9_rm_urls")
  step_done(9L, nrow(data_results_9_rm_urls))
  
  step_start(10L, "Expand ranges")
  
  data_results_10_ranges_expanded <- data_results_9_rm_urls |>
    dplyr::mutate(
      dataset_clean = purrr::map(dataset_clean, expand_id_range)
    ) |>
    tidyr::unnest(dataset_clean) |>
    dplyr::distinct()
  
  save_step_rds(data_results_10_ranges_expanded, "_10_ranges_expanded")
  step_done(10L, nrow(data_results_10_ranges_expanded))
  
  step_start(11L, "Standardize OpenNeuro")
  
  data_results_11_openneuro_std <- data_results_10_ranges_expanded |>
    dplyr::mutate(
      dataset_clean = dplyr::case_when(
        stringr::str_detect(dataset_clean, "openneuro") ~ stringr::str_c(
          "10.18112/openneuro.ds",
          stringr::str_extract(dataset_clean, "(?<=openneuro\\.ds)\\d+")
        ),
        .default = dataset_clean
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_11_openneuro_std, "_11_openneuro_std")
  step_done(11L, nrow(data_results_11_openneuro_std))
  
  step_start(12L, "Remove versions")
  
  data_results_12_rm_versions <- data_results_11_openneuro_std |>
    dplyr::mutate(
      dataset_clean = stringr::str_trim(dataset_clean),
      dataset_clean = dplyr::case_when(
        stringr::str_detect(dataset_clean, "^10\\.18112/openneuro\\.") ~
          stringr::str_remove(dataset_clean, "\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?$"),
        stringr::str_detect(dataset_clean, "^10\\.17632/") ~
          stringr::str_remove(dataset_clean, "\\.[0-9]+$"),
        !stringr::str_detect(dataset_clean, "/") ~ dataset_clean |>
          stringr::str_remove("\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?$") |>
          stringr::str_remove("\\.[0-9]+$") |>
          stringr::str_remove("http$"),
        .default = dataset_clean
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_12_rm_versions, "_12_rm_versions")
  step_done(12L, nrow(data_results_12_rm_versions))
  
  data_results_12_rm_versions
}