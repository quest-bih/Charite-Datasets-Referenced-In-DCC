plot_height_from_n_categories <- function(n, min_height = 5, max_height = 14, per_category = 0.35) {
  min(max(min_height, n * per_category), max_height)
}

plot_width_from_n_x_values <- function(n, min_width = 7, max_width = 16, per_value = 0.45) {
  min(max(min_width, n * per_value), max_width)
}

normalize_repository <- function(x) {
  dplyr::case_when(
    x %in% c(
      "ncbi dbgap",
      "ncbi dbgap (database of genotypes and phenotypesgenotypes and phenotypes)"
    ) ~ "NCBI dbGaP",
    x == "figshare" ~ "Figshare",
    x == "the european genome-phenome archive(ega)" ~ "EGA",
    x == "gene expression omnibus (geo)" ~ "GEO",
    x == "pride proteomics identification database" ~ "PRIDE",
    x == "ncbi reference sequence database" ~ "NCBI RefSeq",
    x == "european nucleotide archive" ~ "ENA",
    x == "the protein data bank" ~ "PDB",
    x == "mendeley" ~ "Mendeley",
    x == "bioproject" ~ "Bioproject",
    x == "arrayexpress" ~ "Arrayexpress",
    x == "openneuro" ~ "Openneuro",
    x == "uniprot" ~ "Uniprot",
    x == "zenodo" ~ "Zenodo",
    x == "harvard dataverse" ~ "Harvard Dataverse",
    x == "the electron microscopy data bank (emdb)" ~ "EMDB",
    x == "the international genome sample resource" ~ "IGSR",
    x == "physionet" ~ "Physionet",
    x == "apollo - university of cambridge repository" ~ "Apollo",
    .default = x
  )
}

plot_theme_base <- function() {
  ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(r = 16)),
      axis.title.x = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(t = 16)),
      axis.text.y = ggplot2::element_text(size = 14, color = "black"),
      axis.text.x = ggplot2::element_text(size = 14, color = "black")
    )
}

prep_charite_year_counts <- function(data, year_col = "year") {
  data |>
    dplyr::filter(!is.na(.data[[year_col]])) |>
    dplyr::count(.data[[year_col]], name = "n") |>
    dplyr::rename(year = !!year_col) |>
    dplyr::mutate(year = as.character(year))
}

# Plot title: Charité publication years
# This bar plot shows how many rows fall into each Charité publication year.
plot_charite_year_counts <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(year, as.numeric(year)), y = n)) +
    ggplot2::geom_col(fill = "#B39DDB", color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(y = n, label = n),
      vjust = -0.3,
      size = 4
    ) +
    ggplot2::labs(
      x = "Charité publication year",
      y = "Number of rows"
    ) +
    plot_theme_base()
}

prep_charite_dataset_frequency <- function(data, dataset_col = "dataset_clean") {
  data |>
    dplyr::filter(!is.na(.data[[dataset_col]]), .data[[dataset_col]] != "") |>
    dplyr::count(.data[[dataset_col]], name = "n") |>
    dplyr::rename(dataset_clean = !!dataset_col) |>
    dplyr::arrange(dataset_clean, n)
}

# Plot title: Frequency of Charité datasets
# This histogram shows how often cleaned dataset identifiers appear in Charité,
# using a log-scaled x-axis because dataset frequencies can be highly skewed.
plot_charite_dataset_frequency <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(bins = 10, color = "black", fill = NA) +
    ggplot2::stat_bin(
      bins = 10,
      geom = "text",
      ggplot2::aes(label = after_stat(count)),
      vjust = -0.3,
      size = 5
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Number of times a dataset appears in Charité (log scale)",
      y = "Number of datasets"
    ) +
    plot_theme_base()
}

prep_dcc_reference_frequency <- function(data, dataset_col = "dataset_clean", doi_col = "doi") {
  data |>
    dplyr::filter(
      !is.na(.data[[dataset_col]]),
      .data[[dataset_col]] != "",
      !is.na(.data[[doi_col]])
    ) |>
    dplyr::distinct(.data[[dataset_col]], .data[[doi_col]]) |>
    dplyr::count(.data[[dataset_col]], name = "n") |>
    dplyr::rename(dataset_clean = !!dataset_col) |>
    dplyr::arrange(dataset_clean, n)
}

# Plot title: DCC references per dataset
# This histogram shows how many distinct DCC references each cleaned dataset has,
# again on a log-scaled x-axis to make long-tailed frequencies easier to inspect.
plot_dcc_reference_frequency <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(bins = 10, color = "black", fill = NA) +
    ggplot2::stat_bin(
      bins = 10,
      geom = "text",
      ggplot2::aes(label = after_stat(count)),
      vjust = -0.3,
      size = 5
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Number of references per dataset in DCC (log scale)",
      y = "Number of datasets"
    ) +
    plot_theme_base()
}

prep_dcc_publication_years <- function(data, doi_col = "doi") {
  data |>
    dplyr::mutate(
      doi_chr = as.character(.data[[doi_col]]),
      year = stringr::str_extract(doi_chr, "^[0-9]{4}$|[0-9]{4}$")
    ) |>
    dplyr::filter(!is.na(year)) |>
    dplyr::count(year, name = "n") |>
    dplyr::mutate(year = as.character(year))
}

# Plot title: DCC publication years
# This bar plot shows the number of DCC rows per extracted publication year.
plot_dcc_publication_years <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(year, as.numeric(year)), y = n)) +
    ggplot2::geom_col(fill = "#FFE0B2", color = "black") +
    ggplot2::geom_text(
      ggplot2::aes(y = n, label = n),
      vjust = -0.3,
      size = 4
    ) +
    ggplot2::labs(
      x = "DCC publication year",
      y = "Number of rows"
    ) +
    plot_theme_base()
}

prep_repository_counts <- function(data, repository_col = "repository", dataset_col = "dataset_clean", doi_col = NULL) {
  data_prepped <- data |>
    dplyr::filter(!is.na(.data[[repository_col]]), .data[[repository_col]] != "") |>
    dplyr::mutate(repository = normalize_repository(.data[[repository_col]]))
  
  dplyr::case_when(
    !is.null(doi_col) ~ data_prepped |>
      dplyr::filter(!is.na(.data[[doi_col]]), !is.na(.data[[dataset_col]])) |>
      dplyr::distinct(repository, .data[[doi_col]], .data[[dataset_col]]) |>
      dplyr::count(repository, name = "n", sort = TRUE),
    .default = data_prepped |>
      dplyr::filter(!is.na(.data[[dataset_col]])) |>
      dplyr::distinct(repository, .data[[dataset_col]]) |>
      dplyr::count(repository, name = "n", sort = TRUE)
  )
}

# Plot title: Repository counts
# This horizontal bar plot compares repositories by count.
# The y-axis is log-scaled so both common and rare repositories can be seen clearly.
plot_repository_counts <- function(data, x_label = "Repository", y_label = "Count (log scale)") {
  ggplot2::ggplot(data, ggplot2::aes(x = stats::reorder(repository, n), y = n)) +
    ggplot2::geom_col(fill = "#80CBC4") +
    ggplot2::geom_text(
      ggplot2::aes(y = n, label = n),
      hjust = -0.2,
      size = 4
    ) +
    ggplot2::scale_y_log10(
      labels = scales::label_comma(),
      expand = ggplot2::expansion(mult = c(0.02, 0.15))
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = "none",
      axis.title.y = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(r = 16)),
      axis.title.x = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(t = 16)),
      axis.text.y = ggplot2::element_text(size = 12, color = "black"),
      axis.text.x = ggplot2::element_text(size = 14, color = "black"),
      panel.grid.major.x = ggplot2::element_line(color = "gray80", linewidth = 0.4),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank()
    )
}

prep_matched_reference_frequency <- function(data, dataset_col = "dataset_clean", doi_col = "doi_dcc") {
  data |>
    dplyr::filter(
      !is.na(.data[[dataset_col]]),
      .data[[dataset_col]] != "",
      !is.na(.data[[doi_col]])
    ) |>
    dplyr::distinct(.data[[dataset_col]], .data[[doi_col]]) |>
    dplyr::count(.data[[dataset_col]], name = "n") |>
    dplyr::rename(dataset_clean = !!dataset_col) |>
    dplyr::arrange(dataset_clean, n)
}

# Plot title: Matched DCC references per dataset
# This histogram shows how many matched DCC references each matched dataset has,
# using a log-scaled x-axis to handle large differences in frequency.
plot_matched_reference_frequency <- function(data) {
  ggplot2::ggplot(data, ggplot2::aes(x = n)) +
    ggplot2::geom_histogram(bins = 10, color = "black", fill = NA) +
    ggplot2::stat_bin(
      bins = 10,
      geom = "text",
      ggplot2::aes(label = after_stat(count)),
      vjust = -0.3,
      size = 5
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      x = "Number of matched DCC references per dataset (log scale)",
      y = "Number of matched datasets"
    ) +
    plot_theme_base()
}

prep_matched_reference_years <- function(data, dataset_col = "dataset_clean", year_col = "publication_year_dcc") {
  data |>
    dplyr::filter(!is.na(.data[[dataset_col]]), !is.na(.data[[year_col]])) |>
    dplyr::distinct(.data[[dataset_col]], .data[[year_col]], doi_dcc) |>
    dplyr::count(.data[[dataset_col]], .data[[year_col]], name = "count") |>
    dplyr::rename(
      dataset_clean = !!dataset_col,
      publication_year_dcc = !!year_col
    )
}

# Plot title: Publication years of matched DCC references
# This stacked bar plot shows how matched DCC references are distributed over
# publication years, split by the top datasets and grouped into an "Others" category.
plot_matched_reference_years_top <- function(data, top_n = 7) {
  top_ids <- data |>
    dplyr::group_by(dataset_clean) |>
    dplyr::summarise(total = sum(count), .groups = "drop") |>
    dplyr::slice_max(total, n = top_n, with_ties = FALSE) |>
    dplyr::pull(dataset_clean)
  
  data_for_plot <- data |>
    dplyr::mutate(
      dataset_clean = dplyr::case_when(
        dataset_clean %in% top_ids ~ dataset_clean,
        .default = "Others"
      ),
      publication_year_dcc = as.character(publication_year_dcc),
      publication_year_dcc = dplyr::case_when(
        publication_year_dcc %in% c("2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012") ~ "<=2012",
        .default = publication_year_dcc
      )
    ) |>
    dplyr::group_by(dataset_clean, publication_year_dcc) |>
    dplyr::summarise(count = sum(count), .groups = "drop")
  
  desired_order <- c(sort(setdiff(top_ids, "Others")), "Others")
  
  ggplot2::ggplot(
    data_for_plot |>
      dplyr::mutate(dataset_clean = factor(dataset_clean, levels = desired_order)),
    ggplot2::aes(x = factor(publication_year_dcc), y = count, fill = dataset_clean)
  ) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::labs(
      x = "Publication year of referencing DCC article",
      y = "Number of references",
      fill = "Dataset"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(r = 16)),
      axis.title.x = ggplot2::element_text(size = 20, color = "black", margin = ggplot2::margin(t = 16)),
      axis.text.y = ggplot2::element_text(size = 12, color = "black"),
      axis.text.x = ggplot2::element_text(size = 12, color = "black", angle = 45, hjust = 1),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 11)
    )
}