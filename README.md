# Internet-scientific-integration

This repository contains the data and analytical code used to study the evolution of global scientific integration and the role of digital connectivity at both the country and institutional levels.

The repository is organized into two main directories: `code/` and `data/raw/`. The structure reflects the two empirical components of the study: (i) country-level analysis and (ii) institutional-level analysis.

---

## Repository Structure

```
Internet-scientific-integration/

├── README.md
├── LICENSE
│
├── code/
│   ├── Institutions/
│   │   ├── 00 Full script institutional level
│   │   ├── 1. Script to read and combine ...
│   │   ├── 2. Script to get first instance ...
│   │   ├── 3. Script to get JSI matrices ...
│   │   ├── 4. Script to compute edge pagerank ...
│   │   ├── 5. Script to combine data and ...
│   │   └── utilities.R
│   │
│   └── country_level/
│       ├── 00 Full script country level ...
│     
│
└── data/
    └── raw/
        ├── Institutions/
        ├── Centrality measures for modeling BP ...
        ├── Core groups KX10 ...
        ├── Data modeling 2507 links balanced panel ...
        ├── Internet access compliance.xlsx
        ├── JSI_comp_USAKX10_v2.zip
        ├── country-list.csv
        ├── data_tot_npub_USAagg_kx10 ...
        └── global-south-countries-2024.csv
```

---

## Data

### Country-level data

The country-level analysis relies on bibliometric data derived from **SciSciNet**. Due to data use restrictions, raw bibliometric records are not redistributed in this repository.

The `data/raw/` directory contains the processed and intermediate datasets required to reproduce the empirical models, including:

* Harmonized country lists and classifications (`country-list.csv`, `global-south-countries-2024.csv`)
* Balanced dyadic panel used for regression modeling
* Network centrality measures
* Core–periphery group assignments
* Constructed Jaccard similarity indices
* Aggregated publication counts
* Internet adoption compliance documentation

Users who wish to reconstruct the pipeline starting from raw bibliometric records must obtain independent access to SciSciNet.

---

### Institutional-level data

The `data/raw/Institutions/` directory contains the datasets used for the institutional-level analysis, including harmonized institutional identifiers, collaboration matrices, and derived network measures.

These data support the analysis of institutional email adoption timing and within-country heterogeneity in scientific integration.

---

## Code

All analytical scripts are located in the `code/` directory.

### Institutional-level analysis (`code/Institutions/`)

The scripts in this folder implement:

* Reading and combining institutional datasets
* Identification of first email-domain appearance in publications
* Construction of institutional Jaccard similarity matrices
* Computation of edge-level PageRank and related network measures
* Assembly of regression-ready panels

The file `00 Full script institutional level` reproduces the full institutional pipeline.

---

### Country-level analysis (`code/country_level/`)

The country-level folder contains the master script required to:

* Construct dyadic collaboration measures
* Merge collaboration data with internet adoption timing
* Estimate regression models, including the two-stage difference-in-differences specification
* Generate the figures reported in the manuscript

The file `00 Full script country level` runs the complete country-level analysis.

---

## Reproducibility

The analysis is implemented in R. Required packages are listed at the beginning of each script.

To reproduce results:

1. Keep the datasets in `data/raw/` as structured.
2. Run the master script in `code/country_level/` for country-level results.
3. Run the master script in `code/Institutions/` for institutional-level results.

All regression outputs and figures are generated directly from these scripts.

---

## Data Availability

Because the country-level analysis relies on SciSciNet, which is subject to data access conditions, raw bibliometric records are not publicly redistributed. This repository provides the processed datasets and full analytical code necessary to reproduce the empirical results conditional on data access.

