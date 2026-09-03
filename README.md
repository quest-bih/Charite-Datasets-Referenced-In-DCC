# Detecting Datasets in the Data Citation Corpus (DCC)

This R/Quarto project identifies dataset identifiers from **ODDPub (Numbat)**, **ROAGG**, and **DataStet** that also occur in the **Data Citation Corpus (DCC)**. It standardizes dataset identifiers, matches sources to DCC on `dataset_clean`, enriches matched article DOIs with OpenAlex metadata, and provides a Quarto report with suggested plots.

The matching workflow is intentionally inclusive so that potential matches are not lost. **The final result table should be reviewed manually before analysis.**

## Quick start

1. Clone the repository and open `Charite-Datasets-Referenced-In-DCC.Rproj` in RStudio.
2. Restore the project environment:

   ```r
   renv::restore()
   ```

3. Render `Charite-Datasets-Referenced-In-DCC.qmd`.
4. Review/curate the matched result table.
5. Run `scripts/Openalex_authors_and_years_extract_join_exc_au_ov_and_same_dois.R`.
6. Review the OpenAlex-enriched result and apply any remaining manual exclusions.
7. Render `Results.qmd` to create `Results.html`.

For a screenshot-by-screenshot guide to the interactive prompts, see `Detecting Datasets in the Data Citation Corpus.docx`.

## 1. Match source identifiers to DCC

`Charite-Datasets-Referenced-In-DCC.qmd` is the main workflow. During rendering, it asks you to:

- select one or more sources: ODDPub, ROAGG, and/or DataStet
- choose an output folder
- choose whether each input is raw or already cleaned
- select the input file (or, for DCC CSV input, a folder)
- map the dataset identifier, optional cleaned identifier, and article DOI columns

Supported source inputs are RDS/RDA/RData or a single CSV file. DCC supports RDS/RDA/RData or a folder of CSV files.

⚠️ **Important:** Currently, only the raw RDS/RDA input workflow has been tested and verified. Other input options, including CSV inputs and already-cleaned inputs, are implemented but should be tested before being used for analysis.

Source-specific filtering is applied before matching:

- **ODDPub:** choose the columns and value combinations that define the open/restricted cases to keep.
- **ROAGG:** `resourceType` is restricted to `Dataset` when present, and selected `publicationYear` values are kept when that column is present.
- **DataStet:** no additional source-specific filter is applied.

Dataset values are then normalized for processing and the validated inputs are saved. For raw inputs, dataset identifiers are standardized with `dataset_cleaner()`; already-cleaned inputs reuse their supplied cleaned identifier. The helper scripts in `R/` are sourced automatically, including the functions used for dataset cleaning and DCC CSV loading.

Each source is then matched to DCC with an inner join on `dataset_clean`. The combined result uses the key columns:

- `source`
- `dataset_clean`
- `dataset_source`
- `dataset_dcc`
- `doi_charite`
- `doi_dcc`

### Main outputs

The selected output folder contains `raw/` and `derived/`. The most useful combined tables are in:

`derived/all_sources_dcc_joined/`

- `all_sources_dcc_joined.rds` — full combined matched table.
- `all_sources_dcc_joined_condensed.rds` — analysis-focused columns.
- `all_sources_dcc_joined_dedup.rds` — exact duplicate rows removed within each source match.
- `all_sources_dcc_joined_dedup_condensed.rds` — condensed version of the previous table.

Source overlap is **not** resolved by a fixed hierarchy at this stage; `Results.qmd` includes overlap summaries that can inform later deduplication decisions.

The workflow also saves cleaning/intermediate objects, `derived/input_validation_log.csv`, and `derived/runtime_documentation.csv`.

## 2. Add OpenAlex metadata and remove obvious article overlap

Run:

`scripts/Openalex_authors_and_years_extract_join_exc_au_ov_and_same_dois.R`

Select a CSV/RDS result table containing `doi_dcc` and `doi_charite`, then choose an output folder. It should be the result table: `all_sources_dcc_joined_dedup.rds`.

The script:

1. normalizes and deduplicates DOI values for lookup;
2. retrieves publication years and authors from OpenAlex;
3. automatically retries DOIs with incomplete author/year metadata once;
4. joins the finalized metadata back to the original rows;
5. removes rows with demonstrated shared authors;
6. removes rows where normalized `doi_dcc` and `doi_charite` are identical;
7. saves intermediate checkpoints and the final RDS file.

The final filename is based on the selected input and ends with:

`_w_au_year_au_ov_no_shared_doi.rds`

Missing OpenAlex metadata is retained rather than treated as evidence of overlap.

## 3. Manual review

Before using the result analytically, inspect it for cases such as:

- irrelevant publication years or preprints;
- ROAGG/DataStet matches that are not open/restricted data;
- values that are not actually datasets;
- multiple identifiers representing the same dataset or other identifier edge cases.

Identifier cleaning is deliberately inclusive. For example, identifier ranges such as `gse005-gse015` may be expanded into the identifiers in that range.

For incremental updates, curate the newly generated rows before appending them to an existing clean result table.

## 4. Plot results

⚠️ **Important:** `Results.qmd` expects the following files:

  - `data/output/openalex_script_output/all_sources_dcc_joined_condensed_dist_w_au_year_au_ov_no_shared_doi.rds`
  - `data/output/derived/all_sources_dcc_joined/all_sources_dcc_joined_condensed.rds`
  - `data/output/derived/oddpub/cleaning_steps/oddpub_cleaning_steps_13_rm_trailing_slash.rds`
  - `data/output/derived/roagg/cleaning_steps/roagg_cleaning_steps_13_rm_trailing_slash.rds`
  - `data/output/derived/datastet/cleaning_steps/datastet_cleaning_steps_13_rm_trailing_slash.rds`

**Therefore, before rendering `Results.qmd`, all cases that were manually removed from the final result table should also be removed from these tables, preferably using a script.**

Render `Results.qmd` from the project root. The current file is configured for the repository output folder `data/output` and loads:

- the curated OpenAlex-enriched result from `data/output/openalex_script_output/`;
- `derived/all_sources_dcc_joined/all_sources_dcc_joined_condensed.rds`;
- the final cleaning-step RDS files for ODDPub, ROAGG, and DataStet.

It produces suggested plots for matched datasets, associated articles, mentions, repository distributions, reference frequency, yearly top identifiers, and overlap among sources.

If your curated/OpenAlex output has a different filename or location, update the `paths` object near the top of `Results.qmd` before rendering. The plotting file is configured for a full three-source run, so its required input paths must exist.

## Key files

- `Charite-Datasets-Referenced-In-DCC.qmd` — interactive cleaning and DCC matching workflow.
- `R/` — dataset-cleaning and input helper functions sourced by the main workflow.
- `scripts/Openalex_authors_and_years_extract_join_exc_au_ov_and_same_dois.R` — OpenAlex enrichment and article-overlap exclusions.
- `Results.qmd` — analysis summaries and suggested plots.
- `Detecting Datasets in the Data Citation Corpus.docx` — detailed illustrated operating instructions.
