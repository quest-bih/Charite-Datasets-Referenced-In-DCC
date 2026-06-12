# file: dataset_cleaner.R

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
  
  total_steps <- 13L
  
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
    "mk", "mh", "phs", "mn", "mw", "pxd", "pdc", "srr", "prj(eb|na|db|da|ea|sa|ma)",
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
  
  prefix_pattern <- paste0(
    "(?<![a-z0-9])(?:",
    paste(prefixes, collapse = "|"),
    ")[a-z0-9._-]*[0-9][a-z0-9._-]*(?=[^a-z0-9._-]|$)"
  )
  
  simple_identifier_pattern <- paste0(
    "^(?:",
    "(?:", paste(prefixes, collapse = "|"), ")[a-z0-9._-]*[0-9][a-z0-9._-]*",
    "|10\\.[0-9]+/[a-z0-9._;()/\\-]+",
    "|(?:https?:)?//(?:www\\.)?(?:",
    "osf\\.io/[a-z0-9]{4,5}",
    "|zenodo\\.org/record/?[0-9]+",
    "|rcsb\\.org/structure/[a-z0-9]{4}",
    "|ncbi\\.nlm\\.nih\\.gov/[^\\s]+",
    "|doi\\.org/10\\.[0-9]+/[a-z0-9._;()/\\-]+",
    "|openneuro\\.org/datasets/ds[0-9]+",
    "|openneuro\\.ds[0-9]+",
    ")",
    "|openneuro\\.ds[0-9]+",
    ")$"
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
  
  sanitize_text_one <- function(text) {
    if (length(text) == 0 || is.na(text)) {
      return(NA_character_)
    }
    
    text_chr <- as.character(text)
    text_raw <- charToRaw(text_chr)
    
    if (length(text_raw) == 0) {
      return(NA_character_)
    }
    
    text_raw <- text_raw[text_raw != as.raw(0)]
    
    if (length(text_raw) == 0) {
      return(NA_character_)
    }
    
    text_chr <- rawToChar(text_raw)
    text_chr <- iconv(text_chr, from = "", to = "UTF-8", sub = "")
    
    if (is.na(text_chr)) {
      return(NA_character_)
    }
    
    text_chr <- gsub("[\\x01-\\x08\\x0B\\x0C\\x0E-\\x1F\\x7F]+", " ", text_chr, perl = TRUE)
    text_chr <- stringr::str_squish(text_chr)
    
    if (!nzchar(text_chr)) {
      return(NA_character_)
    }
    
    text_chr
  }
  
  sanitize_text <- function(text) {
    vapply(text, sanitize_text_one, character(1), USE.NAMES = FALSE)
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
  
  extract_openneuro_ids <- function(text) {
    openneuro_url_ids <- stringr::str_extract_all(
      text,
      stringr::regex(
        "(?:https?:)?//(?:www\\.)?openneuro\\.org/datasets/(ds[0-9]{6})",
        ignore_case = TRUE
      )
    )[[1]] |>
      stringr::str_extract(stringr::regex("ds[0-9]{6}", ignore_case = TRUE))
    
    openneuro_context_chunks <- stringr::str_extract_all(
      text,
      stringr::regex(
        "open\\s*-?\\s*neuro[^.;\\n]{0,200}\\bds[0-9]{6}\\b",
        ignore_case = TRUE
      )
    )[[1]]
    
    openneuro_context_ids <- unlist(
      lapply(
        openneuro_context_chunks,
        function(chunk) {
          stringr::str_extract_all(
            chunk,
            stringr::regex("\\bds[0-9]{6}\\b", ignore_case = TRUE)
          )[[1]]
        }
      )
    )
    
    c(openneuro_url_ids, openneuro_context_ids) |>
      stringr::str_to_lower() |>
      unique()
  }
  
  extract_spaced_repository_matches <- function(text) {
    patterns <- c(
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?osf\\s*\\.\\s*io\\s*/\\s*[A-Za-z0-9]{4,5}",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?zenodo\\s*\\.\\s*org\\s*/\\s*record\\s*/?\\s*[0-9]+",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?(?:doi\\s*\\.\\s*org\\s*/\\s*)?10\\s*\\.\\s*5281\\s*/\\s*zenodo\\s*\\.\\s*[0-9]+",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?(?:doi\\s*\\.\\s*org\\s*/\\s*)?10\\s*\\.\\s*17632\\s*/\\s*[A-Za-z0-9]+(?:\\s*[A-Za-z0-9]+)*(?:\\s*\\.\\s*[0-9]+)?",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?(?:doi\\s*\\.\\s*org\\s*/\\s*)?10\\s*\\.\\s*17605\\s*/\\s*osf\\s*\\.\\s*io\\s*/\\s*[A-Za-z0-9]{4,5}",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?(?:doi\\s*\\.\\s*org\\s*/\\s*)?10\\s*\\.\\s*6084\\s*/\\s*m9\\s*\\.\\s*figshare\\s*\\.[A-Za-z0-9]+",
      "(?i)(?:https?\\s*:\\s*/\\s*/\\s*)?(?:doi\\s*\\.\\s*org\\s*/\\s*)?10\\s*\\.\\s*5061\\s*/\\s*dryad\\s*\\.[A-Za-z0-9._-]+"
    )
    
    unlist(
      lapply(
        patterns,
        function(pat) {
          stringr::str_extract_all(
            text,
            stringr::regex(pat, ignore_case = TRUE)
          )[[1]]
        }
      )
    ) |>
      stringr::str_replace_all("\\s+", "") |>
      stringr::str_trim() |>
      unique()
  }
  
  normalize_text <- function(text) {
    text <- sanitize_text_one(text)
    
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    text <- gsub("(?i)%(?:0[0-9a-f]|1[0-9a-f]|7f)", " ", text, perl = TRUE)
    
    if (stringr::str_detect(text, "%[0-9A-Fa-f]{2}")) {
      text <- utils::URLdecode(text)
    }
    
    text <- sanitize_text_one(text)
    
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    text <- stringr::str_replace_all(text, "[\r\n\t]+", " ")
    text <- stringr::str_replace_all(text, "(?i)h\\s*t\\s*t\\s*p\\s*s?\\s*:\\s*/\\s*/\\s*", "https://")
    text <- stringr::str_replace_all(text, "(?i)d\\s*o\\s*i\\s*\\.\\s*o\\s*r\\s*g\\s*/\\s*", "doi.org/")
    text <- stringr::str_replace_all(text, "(?i)o\\s*s\\s*f\\s*\\.\\s*i\\s*o\\s*/\\s*", "osf.io/")
    text <- stringr::str_replace_all(text, "(?i)z\\s*e\\s*n\\s*o\\s*d\\s*o\\s*\\.\\s*o\\s*r\\s*g\\s*/\\s*r\\s*e\\s*c\\s*o\\s*r\\s*d\\s*/?\\s*", "zenodo.org/record/")
    text <- stringr::str_squish(text)
    
    if (!nzchar(text)) {
      return(NA_character_)
    }
    
    text
  }
  
  likely_has_identifier <- function(text) {
    text_clean <- sanitize_text(as.character(text))
    text_lower <- stringr::str_to_lower(text_clean)
    text_lower <- stringr::str_replace_all(text_lower, "open\\s*-?\\s*neuro", "openneuro")
    text_trim <- stringr::str_trim(text_lower)
    text_compact <- stringr::str_replace_all(text_lower, "\\s+", "")
    
    dplyr::case_when(
      is.na(text_lower) | text_lower == "" ~ FALSE,
      stringr::str_detect(
        text_lower,
        stringr::regex(prefix_pattern, ignore_case = TRUE)
      ) ~ TRUE,
      stringr::str_detect(
        text_compact,
        stringr::regex("10\\.[0-9]+/[a-z0-9._;()/\\-]+", ignore_case = TRUE)
      ) ~ TRUE,
      stringr::str_detect(
        text_compact,
        stringr::regex(
          "(?:https?:)?//(?:www\\.)?(?:osf\\.io/[a-z0-9]{4,5}|zenodo\\.org/record/?[0-9]+|mendeley\\.com/datasets/[a-z0-9]+|rcsb\\.org/structure/[a-z0-9]{4}|doi\\.org/10\\.[0-9]+/[a-z0-9._;()/\\-]+)",
          ignore_case = TRUE
        )
      ) ~ TRUE,
      stringr::str_detect(
        text_compact,
        stringr::regex(
          "10\\.(?:5281/zenodo\\.[0-9]+|17632/[a-z0-9.]+|17605/osf\\.io/[a-z0-9]{4,5}|6084/m9\\.figshare\\.[a-z0-9]+|5061/dryad\\.[a-z0-9._-]+)",
          ignore_case = TRUE
        )
      ) ~ TRUE,
      stringr::str_detect(
        text_compact,
        stringr::regex(
          "ncbi\\.nlm\\.nih\\.gov/(?:geo/query/acc\\.cgi\\?acc=|nuccore(?:\\?term=|/)|protein/|sra(?:/\\?term=|/))[a-z0-9._-]+",
          ignore_case = TRUE
        )
      ) ~ TRUE,
      stringr::str_detect(
        text_compact,
        stringr::regex("openneuro\\.ds[0-9]+|openneuro\\.org/datasets/ds[0-9]+", ignore_case = TRUE)
      ) ~ TRUE,
      stringr::str_detect(
        text_lower,
        stringr::regex(
          "(?:protein data bank|\\bpdb\\b|\\bpdbs\\b|\\brcsb\\b)[^.;\\n]{0,250}\\b[0-9][A-Za-z0-9]{3}\\b",
          ignore_case = TRUE
        )
      ) ~ TRUE,
      stringr::str_detect(
        text_trim,
        stringr::regex("^[0-9][a-z0-9]{3}$", ignore_case = TRUE)
      ) &
        stringr::str_detect(
          text_trim,
          stringr::regex("[a-z]", ignore_case = TRUE)
        ) ~ TRUE,
        stringr::str_detect(
          text_lower,
          stringr::regex("openneuro[^.;\\n]{0,200}\\bds[0-9]{6}\\b", ignore_case = TRUE)
        ) ~ TRUE,
      .default = FALSE
    )
  }
  
  extract_ids <- function(text) {
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    all_matches <- character()
    
    openneuro_ids <- extract_openneuro_ids(text) |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, openneuro_ids)
    
    spaced_repo_matches <- extract_spaced_repository_matches(text) |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, spaced_repo_matches)
    
    cond1_pattern <- "(?i)(figshare\\.|zenodo\\.|osf\\.io/|mendeley\\.com/datasets/|dryad\\.)(?:[\\s\\da-z._/-]*[\\d][\\s\\da-z._/-]*?)(?=[\\)\\],#';\">:]|$)"
    cond1 <- stringr::str_extract_all(
      text,
      stringr::regex(cond1_pattern, ignore_case = TRUE)
    )[[1]] |>
      stringr::str_replace_all("\\s+", "") |>
      unique() |>
      skip_if_included(all_matches)
    all_matches <- c(all_matches, cond1)
    
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
      "\\bpdc[0-9]{6}\\b",
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
      stringr::str_replace_all("\\s+", "") |>
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
  
  data_results_1_tolower <- tibble::tibble(
    dataset = sanitize_text(as.character(dataset_input))
  ) |>
    dplyr::mutate(
      dataset_1_tolower = sanitize_text(dataset) |>
        stringr::str_squish() |>
        stringr::str_to_lower()
    ) |>
    dplyr::filter(
      !is.na(dataset_1_tolower),
      dataset_1_tolower != "",
      !dataset_1_tolower %in% c("null", "na")
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_1_tolower, "_1_tolower")
  step_done(1L, nrow(data_results_1_tolower))
  
  n_total <- nrow(data_results_1_tolower)
  
  extract_ids_with_progress <- function(text, i, n_to_process) {
    cat(sprintf("[Step 2/%s] %s/%s\r", total_steps, format(i, big.mark = ","), format(n_to_process, big.mark = ",")))
    flush.console()
    
    text <- tryCatch(
      normalize_text(text),
      error = function(e) NA_character_
    )
    
    if (is.na(text) || !nzchar(text)) {
      return(NA_character_)
    }
    
    text_compact <- stringr::str_replace_all(text, "\\s+", "")
    
    is_pdb_like_single_id <- stringr::str_detect(
      text_compact,
      stringr::regex("^[0-9][a-z0-9]{3}$", ignore_case = TRUE)
    ) &&
      stringr::str_detect(
        text_compact,
        stringr::regex("[a-z]", ignore_case = TRUE)
      )
    
    is_simple_identifier <- stringr::str_detect(
      text_compact,
      stringr::regex(simple_identifier_pattern, ignore_case = TRUE)
    ) || is_pdb_like_single_id
    
    if (is_simple_identifier) {
      return(stringr::str_to_lower(text_compact))
    }
    
    extracted <- tryCatch(
      extract_ids(text),
      error = function(e) NA_character_
    )
    
    is_long_text <- dplyr::case_when(
      nchar(text) > 200 ~ TRUE,
      stringr::str_count(text, "\\s+") >= 5 ~ TRUE,
      .default = FALSE
    )
    
    dplyr::case_when(
      is_long_text & !is.na(extracted) & nzchar(extracted) ~ extracted,
      is_long_text ~ NA_character_,
      !is.na(extracted) & nzchar(extracted) ~ extracted,
      .default = text
    )
  }
  
  step_start(2L, "Extract ids")
  
  candidate_rows <- likely_has_identifier(data_results_1_tolower$dataset_1_tolower)
  n_to_process <- sum(candidate_rows, na.rm = TRUE)
  
  data_results_2_ext_ids <- data_results_1_tolower |>
    dplyr::mutate(dataset_2_ext_ids = NA_character_)
  
  if (n_to_process > 0) {
    data_results_2_ext_ids$dataset_2_ext_ids[candidate_rows] <- purrr::map2_chr(
      data_results_1_tolower$dataset_1_tolower[candidate_rows],
      seq_len(n_to_process),
      ~ extract_ids_with_progress(.x, .y, n_to_process)
    )
    
    cat(sprintf("[Step 2/%s] %s/%s\n", total_steps, format(n_to_process, big.mark = ","), format(n_to_process, big.mark = ",")))
  } else {
    cat(sprintf("[Step 2/%s] 0/0\n", total_steps))
  }
  flush.console()
  
  data_results_2_ext_ids <- data_results_2_ext_ids |>
    dplyr::filter(!is.na(dataset_2_ext_ids), dataset_2_ext_ids != "")
  
  save_step_rds(data_results_2_ext_ids, "_2_ext_ids")
  step_done(2L, nrow(data_results_2_ext_ids))
  
  step_start(3L, "Separate rows")
  
  data_results_3_reshaped <- data_results_2_ext_ids |>
    dplyr::mutate(dataset_3_sep_rows = dataset_2_ext_ids) |>
    tidyr::separate_rows(dataset_3_sep_rows, sep = ";") |>
    dplyr::mutate(
      dataset_3_sep_rows = stringr::str_trim(dataset_3_sep_rows)
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_3_reshaped, "_3_reshaped")
  step_done(3L, nrow(data_results_3_reshaped))
  
  step_start(4L, "Clean extracted ids")
  
  data_results_4_cleaned <- data_results_3_reshaped |>
    dplyr::mutate(dataset_4_clean = dataset_3_sep_rows) |>
    dplyr::filter(!is.na(dataset_4_clean), dataset_4_clean != "") |>
    dplyr::filter(
      !(
        stringr::str_detect(dataset_4_clean, "zenodo|dryad|figshare|harvard|mendeley|github") &
          !stringr::str_detect(dataset_4_clean, "[0-9]")
      )
    ) |>
    dplyr::filter(!stringr::str_detect(dataset_4_clean, "^[a-zA-Z][0-9]{1,2}$")) |>
    dplyr::filter(
      !stringr::str_detect(
        dataset_4_clean,
        "^[-\\sa-zA-Z]+$|^[-\\s0-9]+$"
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_4_cleaned, "_4_cleaned")
  step_done(4L, nrow(data_results_4_cleaned))
  
  step_start(5L, "Standardize datasets")
  
  data_results_5_std <- data_results_4_cleaned |>
    dplyr::mutate(
      dataset_4_clean = stringr::str_trim(dataset_4_clean),
      dataset_5_std = dplyr::case_when(
        stringr::str_detect(dataset_4_clean, "osf\\.io/[a-z0-9]{4,5}") ~ stringr::str_c(
          "10.17605/osf.io/",
          dataset_4_clean |>
            stringr::str_replace_all("\\s", "") |>
            stringr::str_extract("osf\\.io/([A-Za-z0-9]{4,5})") |>
            stringr::str_remove("^osf\\.io/") |>
            stringr::str_to_lower()
        ),
        stringr::str_detect(dataset_4_clean, "zenodo\\.org/record") ~ stringr::str_c(
          "10.5281/zenodo.",
          dataset_4_clean |>
            stringr::str_extract("zenodo\\.org/record/?\\s*([0-9]+)") |>
            stringr::str_remove("^zenodo\\.org/record/?\\s*")
        ),
        stringr::str_detect(dataset_4_clean, "zenodo") ~ stringr::str_c(
          "10.5281/zenodo.",
          dataset_4_clean |>
            stringr::str_extract("zenodo\\.?\\s*([0-9]+)") |>
            stringr::str_remove("^zenodo\\.?\\s*")
        ),
        stringr::str_detect(dataset_4_clean, "10\\.6084/m9\\.figshare\\.c\\.") ~ dataset_4_clean |>
          stringr::str_extract("10\\.6084/m9\\.figshare\\.c\\.[0-9]+") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "figshare") ~ stringr::str_c(
          "10.6084/m9.figshare.",
          dataset_4_clean |>
            stringr::str_extract("figshare\\.?\\s*([0-9]+)") |>
            stringr::str_remove("^figshare\\.?\\s*")
        ),
        stringr::str_detect(dataset_4_clean, "mendeley.*datasets") ~ stringr::str_c(
          "10.17632/",
          dataset_4_clean |>
            stringr::str_replace_all("\\s", "") |>
            stringr::str_extract("datasets/([A-Za-z0-9]+)") |>
            stringr::str_remove("^datasets/")
        ),
        stringr::str_detect(dataset_4_clean, "dryad") ~ stringr::str_c(
          "10.5061/dryad.",
          dataset_4_clean |>
            stringr::str_replace_all("\\s", "") |>
            stringr::str_extract("dryad\\.?([A-Za-z0-9]+)") |>
            stringr::str_remove("^dryad\\.?")
        ),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/geo/query/acc\\.cgi\\?acc=") ~ dataset_4_clean |>
          stringr::str_extract("(?<=acc=)[a-z0-9._-]+") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/nuccore\\?term=") ~ dataset_4_clean |>
          stringr::str_extract("(?<=nuccore\\?term=)[a-z0-9._-]+(?:-[a-z0-9._-]+)?") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/nuccore/") ~ dataset_4_clean |>
          stringr::str_extract("(?<=nuccore/)[a-z0-9._-]+") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/protein/") ~ dataset_4_clean |>
          stringr::str_extract("(?<=protein/)[a-z0-9._-]+") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/sra/\\?term=") ~ dataset_4_clean |>
          stringr::str_extract("(?<=sra/\\?term=)[a-z0-9._-]+") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "ncbi\\.nlm\\.nih\\.gov/sra/") ~ dataset_4_clean |>
          stringr::str_extract("(?<=sra/)[a-z0-9._-]+") |>
          stringr::str_remove("\\[accn\\]$") |>
          stringr::str_to_lower(),
        stringr::str_detect(
          dataset_4_clean,
          stringr::regex("^(?:https?:)?//(?:www\\.)?rcsb\\.org/structure/[A-Za-z0-9]{4}$", ignore_case = TRUE)
        ) ~ dataset_4_clean |>
          stringr::str_extract(stringr::regex("[A-Za-z0-9]{4}$", ignore_case = TRUE)) |>
          stringr::str_to_lower(),
        stringr::str_detect(
          dataset_4_clean,
          stringr::regex("^[0-9][A-Za-z0-9]{3}$", ignore_case = TRUE)
        ) ~ stringr::str_to_lower(dataset_4_clean),
        stringr::str_detect(stringr::str_replace_all(dataset_4_clean, "\\s", ""), stringr::str_c(prefixes, collapse = "|")) &
          !stringr::str_detect(dataset_4_clean, "https?://|^//") &
          !stringr::str_starts(stringr::str_trim(dataset_4_clean), "10\\.") ~ dataset_4_clean |>
          stringr::str_replace_all("\\s+", "") |>
          stringr::str_to_lower(),
        stringr::str_detect(dataset_4_clean, "^10\\.") &
          !stringr::str_detect(dataset_4_clean, "figshare|zenodo|osf|dryad|mendeley") ~ dataset_4_clean |>
          stringr::str_replace_all("\\s+", "") |>
          stringr::str_extract("10\\.[^\\s]+"),
        stringr::str_detect(dataset_4_clean, "id=") ~ dataset_4_clean |>
          stringr::str_extract("id=([^\\s/&]+)") |>
          stringr::str_remove("^id="),
        stringr::str_detect(dataset_4_clean, "addgene\\.org/") ~ dataset_4_clean |>
          stringr::str_extract("addgene\\.org/([0-9 \\t]*)") |>
          stringr::str_remove("addgene\\.org/") |>
          stringr::str_remove_all("\\s"),
        .default = dataset_4_clean
      ),
      dataset_5_std = stringr::str_trim(dataset_5_std)
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_5_std, "_5_std")
  step_done(5L, nrow(data_results_5_std))
  
  step_start(6L, "Filter invalid datasets")
  
  data_results_6_filtered <- data_results_5_std |>
    dplyr::mutate(dataset_6_filtered = dataset_5_std) |>
    dplyr::filter(
      !(
        stringr::str_starts(dataset_6_filtered, "//") &
          !stringr::str_detect(
            dataset_6_filtered,
            stringr::regex("^//(?:www\\.)?rcsb\\.org/structure/", ignore_case = TRUE)
          )
      )
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        stringr::str_replace_all(dataset_6_filtered, "\\s", ""),
        "^or\\d+$"
      )
    ) |>
    dplyr::filter(
      !stringr::str_detect(
        stringr::str_replace_all(dataset_6_filtered, "\\s", ""),
        "^[a-zA-Z]\\d+$"
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_6_filtered, "_6_filtered")
  step_done(6L, nrow(data_results_6_filtered))
  
  step_start(7L, "Remove trailing dots")
  
  data_results_7_rm_trails <- data_results_6_filtered |>
    dplyr::mutate(
      dataset_7_rm_trails = stringr::str_remove(dataset_6_filtered, "\\.+$")
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_7_rm_trails, "_7_rm_trails")
  step_done(7L, nrow(data_results_7_rm_trails))
  
  step_start(8L, "Remove custom patterns")
  
  data_results_8_rm_patterns <- data_results_7_rm_trails |>
    dplyr::mutate(dataset_8_rm_patterns = dataset_7_rm_trails) |>
    dplyr::filter(
      !stringr::str_detect(
        dataset_8_rm_patterns,
        stringr::regex(paste(custom_patterns, collapse = "|"))
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_8_rm_patterns, "_8_rm_patterns")
  step_done(8L, nrow(data_results_8_rm_patterns))
  
  step_start(9L, "Remove raw URLs")
  
  data_results_9_rm_urls <- data_results_8_rm_patterns |>
    dplyr::mutate(dataset_9_rm_urls = dataset_8_rm_patterns) |>
    dplyr::filter(
      !stringr::str_detect(dataset_9_rm_urls, "^https?:")
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_9_rm_urls, "_9_rm_urls")
  step_done(9L, nrow(data_results_9_rm_urls))
  
  step_start(10L, "Expand ranges")
  
  data_results_10_ranges_expanded <- data_results_9_rm_urls |>
    dplyr::mutate(
      dataset_10_ranges_expanded = purrr::map(dataset_9_rm_urls, expand_id_range)
    ) |>
    tidyr::unnest(dataset_10_ranges_expanded) |>
    dplyr::distinct()
  
  save_step_rds(data_results_10_ranges_expanded, "_10_ranges_expanded")
  step_done(10L, nrow(data_results_10_ranges_expanded))
  
  step_start(11L, "Standardize OpenNeuro")
  
  data_results_11_openneuro_std <- data_results_10_ranges_expanded |>
    dplyr::mutate(
      dataset_11_openneuro_std = dplyr::case_when(
        stringr::str_detect(
          dataset_10_ranges_expanded,
          stringr::regex(
            "^(?:https?://)?(?:doi\\.org/)?10\\.18112/openneuro\\.ds[0-9]{6}(?:\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?)?$",
            ignore_case = TRUE
          )
        ) ~ dataset_10_ranges_expanded |>
          stringr::str_remove("^(?:https?://)?(?:doi\\.org/)?") |>
          stringr::str_to_lower(),
        stringr::str_detect(
          dataset_10_ranges_expanded,
          stringr::regex("openneuro\\.org/datasets/ds[0-9]{6}", ignore_case = TRUE)
        ) ~ stringr::str_c(
          "10.18112/openneuro.",
          stringr::str_extract(
            dataset_10_ranges_expanded,
            stringr::regex("ds[0-9]{6}", ignore_case = TRUE)
          ) |>
            stringr::str_to_lower()
        ),
        stringr::str_detect(
          dataset_10_ranges_expanded,
          stringr::regex(
            "^openneuro\\.ds[0-9]{6}(?:\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?)?$",
            ignore_case = TRUE
          )
        ) ~ stringr::str_c(
          "10.18112/",
          stringr::str_to_lower(dataset_10_ranges_expanded)
        ),
        stringr::str_detect(
          dataset_10_ranges_expanded,
          stringr::regex(
            "^ds[0-9]{6}(?:\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?)?$",
            ignore_case = TRUE
          )
        ) ~ stringr::str_c(
          "10.18112/openneuro.",
          stringr::str_to_lower(dataset_10_ranges_expanded)
        ),
        .default = dataset_10_ranges_expanded
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_11_openneuro_std, "_11_openneuro_std")
  step_done(11L, nrow(data_results_11_openneuro_std))
  
  step_start(12L, "Remove versions")
  
  data_results_12_rm_versions <- data_results_11_openneuro_std |>
    dplyr::mutate(
      dataset_12_rm_versions = stringr::str_trim(dataset_11_openneuro_std),
      dataset_12_rm_versions = dplyr::case_when(
        stringr::str_detect(dataset_12_rm_versions, "^10\\.18112/openneuro\\.") ~
          stringr::str_remove(dataset_12_rm_versions, "\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?$"),
        stringr::str_detect(dataset_12_rm_versions, "^10\\.17632/") ~
          stringr::str_remove(dataset_12_rm_versions, "\\.[0-9]+$"),
        !stringr::str_detect(dataset_12_rm_versions, "/") ~ dataset_12_rm_versions |>
          stringr::str_remove("\\.v[0-9]+(?:\\.[0-9]+)*(?:\\.p[0-9]*)?$") |>
          stringr::str_remove("\\.[0-9]+$"),
        .default = dataset_12_rm_versions
      )
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_12_rm_versions, "_12_rm_versions")
  step_done(12L, nrow(data_results_12_rm_versions))
  
  data_results_12_rm_versions
  
  step_start(13L, "Remove trailing slashes")
  
  data_results_13_rm_trailing_slash <- data_results_12_rm_versions |>
    dplyr::mutate(
      dataset_13_rm_trailing_slash = stringr::str_remove(dataset_12_rm_versions, "/+$"),
      dataset_clean = dataset_13_rm_trailing_slash
    ) |>
    dplyr::distinct()
  
  save_step_rds(data_results_13_rm_trailing_slash, "_13_rm_trailing_slash")
  step_done(13L, nrow(data_results_13_rm_trailing_slash))
  
}