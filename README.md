---
title: "README"
author: "Avihay Cohen"
date: "2026-06-24"
output: html_document
---

# Dataset Identifiers Mentioned in DCC

## Overview

This Quarto document loads dataset-identifier lists from several sources, standardizes them, and matches them against DCC dataset identifiers.

The supported source inputs are:

* ODDPub
* ROAGG
* DataStet
* DCC

The document is interactive. During the run, the user is asked to:

* choose which sources should be matched with DCC
* choose the output folder
* choose the storage type of each input
* choose the file or folder for each input
* choose which columns should be used as dataset identifier, cleaned dataset identifier (if relevant), and article DOI
* choose source-specific filters where applicable

## Purpose

The goal of the workflow is to:

1. load dataset identifier lists from multiple tools or sources
2. validate them before further processing
3. clean and standardize dataset identifiers
4. match source identifiers to DCC identifiers using a common cleaned identifier
5. save all important intermediate and final outputs for later inspection

## Supported Input Types

### ODDPub, ROAGG, and DataStet

These inputs can be provided as:

* raw RDS / RDA / RData
* already-cleaned RDS / RDA / RData
* raw CSV file
* already-cleaned CSV file

### DCC

DCC can be provided as:

* raw RDS / RDA / RData
* already-cleaned RDS / RDA / RData
* folder with raw CSV files
* folder with already-cleaned CSV files

If a CSV input is used, the document converts it to RDS and saves the converted file in the derived output folder.

## Input Validation

Each loaded input is validated before further processing.

The validation step:

* checks that the loaded object is a data frame / tibble
* checks character and factor columns for text/encoding problems
* removes rows containing values that cannot be converted safely to UTF-8
* logs removed or problematic values
* saves the validated input as an RDS file for downstream processing

If an input cannot be validated or becomes empty after validation, it is skipped.

This means the workflow is designed to continue even when one or more inputs are invalid or corrupted for input-related reasons.

## Column Mapping

For every input, the user is asked to map columns to the internal analysis structure.

### For raw inputs

The user maps:

* original dataset identifier column
* article DOI column, or **No article DOI column**

### For already-cleaned inputs

The user maps:

* original dataset identifier column
* cleaned dataset identifier column
* article DOI column, or **No article DOI column**

If **No article DOI column** is selected, the workflow creates an empty DOI column filled with missing values.

## Source-Specific Filtering

### ODDPub

The user is asked to define which columns determine whether a dataset is considered open.

The user then selects which combinations of values should be kept.

### ROAGG

If the input contains a `resourceType` column, only rows with `resourceType = Dataset` are kept, case-insensitively.

If the input contains a `publicationYear` column, the user can choose which `publicationYear` values to keep.

### DataStet

No additional source-specific filter is applied.

## Dataset Cleaning

After all inputs are loaded and validated, dataset identifiers are prepared for matching.

### If the input is raw

* the dataset identifier column is cleaned using `dataset_cleaner()`
* the cleaned identifier is stored in a new column called `dataset_clean`

### If the input is already cleaned

* the existing cleaned identifier column is used directly
* `dataset_cleaner()` is not run

### DCC-specific rule

For DCC only:

* rows where DOI equals `dataset` or `dataset_clean` are removed after preparation

## Matching Logic

Each selected source is matched separately against DCC.

Matching is performed by:

* joining the source data and the DCC data on `dataset_clean`

For each matched table, the output contains:

* `source`
* `dataset_clean`
* `dataset_source`
* `dataset_dcc`
* `doi_source`
* `doi_dcc`
* all remaining columns from both sides

Separate matched tables are created for:

* ODDPub vs DCC
* ROAGG vs DCC
* DataStet vs DCC

In addition, all per-source matched tables are combined into one appended table.

## Outputs

All major outputs are saved as RDS files in the derived output folder.

Examples include:

* validated raw inputs
* renamed raw inputs
* cleaned inputs
* per-source matched tables
* deduplicated matched tables
* combined matched tables
* validation log

A CSV file named `input_validation_log.csv` is also written to the derived output folder.

This file documents removed invalid rows and validation issues detected during input checking.

## Summary Table

At the end of the run, the document displays a summary table with counts for:

* ODDPub Articles
* ODDPub Datasets
* DataStet Articles
* DataStet Datasets
* ROAGG Articles
* ROAGG Datasets
* DCC Mentioning Articles
* DCC Mentions
* DCC Datasets

Only the selected and successfully processed source inputs are included in the summary.

## Runtime Documentation

At the beginning of the run, the document records the render start time.

At the end of the run, it writes a CSV file named `runtime_documentation.csv`.

This file contains:

* `render_start`
* `render_end`
* `duration_seconds`

## Important Notes

* The workflow is designed to be robust to input-level problems.
* Invalid rows with encoding problems are removed automatically.
* Invalid or unusable inputs are skipped rather than causing the entire document to fail, as far as input-related issues are concerned.
* The workflow still depends on required packages, sourced helper scripts, and working GUI prompts being available.
* The external functions `dataset_cleaner()` and `dcc_csv_to_rds()` must exist in the `R` folder and be sourceable at runtime.

## Expected External Helpers

The qmd expects the following helper functions to exist in external R scripts:

* `dataset_cleaner()`
* `dcc_csv_to_rds()`

These scripts are sourced automatically from the local `R` folder at the start of the document.

## Recommended Usage

1. Start the qmd render.
2. Select which source inputs should be matched with DCC.
3. Choose the output folder.
4. For each selected source, choose the input type and input file.
5. For DCC, choose the input type and the file or folder.
6. Review source-specific filters when prompted.
7. Map the relevant columns for each input.
8. Let the workflow validate, clean, match, and save the outputs.

## End
