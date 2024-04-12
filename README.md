---
title: "Code and Data for: Reproduce to Validate: a Comprehensive Study on the Reproducibility of Economics Research"
author:
  - Sylverie Herbert
  - Hautahi Kingi
  - Flavio Stanchi
  - Lars Vilhuber
date: "2024-04-12"
output:
  html_document: 
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_depth: 1
  word_document: default
  pdf_document: 
    toc: true
    number_sections: true
    toc_depth: 1
editor_options: 
  chunk_output_type: console
bibliography: 
  - text/data.bib
  - text/includes/paper.bib
  - grateful-refs.bib
csl: _readme/chicago-author-date.csl
---

> File prepared for **Canadian Journal of Economics**.




# Overview

The code in this replication package constructs the analysis file from the four data sources [@raw-data-2019; @openalex-data; @webofscience2018; @crossref-data]. The code is in R, but also uses public APIs to download some data. The replication package also includes PDF copies the survey used to query the replicators. The original Google Forms surveys cannot be exported. 

# Data Availability and Provenance Statements

All data are public, and can be redistributed. The key data [@raw-data-2019] are provided in a separate repository ([https://doi.org/10.5281/zenodo.2639920](https://doi.org/10.5281/zenodo.2639920)), but are downloaded automatically. Data pulled via APIs are provided as of the date last downloaded, with no guarantees that the code to access the API still works.

## Statement about Rights

- [x] I certify that the author(s) of the manuscript have legitimate access to and permission to use the data used in this manuscript. 
- [x] I certify that the author(s) of the manuscript have documented permission to redistribute/publish the data contained within this replication package. Appropriate permission are documented in the [LICENSE.txt](LICENSE.txt) file.


## License for Data

- Original data [@raw-data-2019] (not included) are licensed as [CC-BY-NC-4.0](https://creativecommons.org/licenses/by-nc/4.0/legalcode).
- Data from @openalex-data are obtained under a [CC0](https://creativecommons.org/publicdomain/zero/1.0/) Public Domain attribution.
- Data from @webofscience2018 are proprietary, but redistribution of small extracts is permitted.
- Data fom @crossref-data is "is open and available for reuse without restriction" ([https://www.crossref.org/documentation/retrieve-metadata/](https://www.crossref.org/documentation/retrieve-metadata/))

All derivative data contained herein, if not otherwise encumbered, is available under a [CC-BY-NC-4.0](https://creativecommons.org/licenses/by-nc/4.0/legalcode) license. Usage by commercial entities is permitted, reselling the data is not.


## Summary of Availability

- [x] All data **are** publicly available.
- [ ] Some data **cannot be made** publicly available.
- [ ] **No data can be made** publicly available.








|Data Source                                                       |Filename                                          |Provided |
|:-----------------------------------------------------------------|:-------------------------------------------------|:--------|
|Kingi et al (2019)                                                |./data/replication_data/entryQ_pub.Rds            |FALSE    |
|Kingi et al (2019)                                                |./data/replication_data/exitQ_pub.Rds             |FALSE    |
|Kingi et al (2019)                                                |./data/replication_data/replication_list_pub.Rds  |FALSE    |
|Crossref (2023)                                                   |./data/crossref/crossref_aejdois.Rds              |TRUE     |
|Crossref (2023)                                                   |./data/crossref/crossref_info.csv                 |TRUE     |
|Crossref (2023)                                                   |./data/crossref/crossref_info.Rds                 |TRUE     |
|OurResearch (2023)                                                |./data/openalex/citations-per-paper.Rds           |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae-authors.Rds        |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae.Rds                |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-hindex.Rds               |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-institutions-aejae.Rds   |TRUE     |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.csv  |TRUE     |
|Reuters (2016), Clarivate (2018) and manual edits                 |./data/h_index_data/h-index-assignment1.2019.xlsx |TRUE     |
|Generated during acquisition of Crossref data                     |./data/crossref/crossref_timing.Rds               |TRUE     |
|Generated for data quality audit purposes                         |./data/crossref/audit-exp.xlsx                    |TRUE     |
|Hand-generated by authors based on AEA websites                   |./data/crossref/issns.Rds                         |TRUE     |
|Generated from openAlex data                                      |./data/openalex/affiliations.csv                  |TRUE     |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.csv            |TRUE     |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.xlsx           |TRUE     |
|Machine-imputed institutions using auxiliary openAlex information |./data/openalex/affiliations-imputed.Rds          |TRUE     |
|Records to remove from extracted openAlex data                    |./data/openalex/blacklist.xlsx                    |TRUE     |
|Procedural file to map short names to long variable names         |./data/auxiliary/mainOA-mapping.xlsx              |TRUE     |



## Details on each Data Source

### @raw-data-2019 

The data were collected through the methods described in the paper. Data were collected over several years, and deposited at @raw-data-2019 in 2019, with replicator names replaced by random identifiers. 


|Data Source        |Filename                                         |Provided |
|:------------------|:------------------------------------------------|:--------|
|Kingi et al (2019) |./data/replication_data/entryQ_pub.Rds           |FALSE    |
|Kingi et al (2019) |./data/replication_data/exitQ_pub.Rds            |FALSE    |
|Kingi et al (2019) |./data/replication_data/replication_list_pub.Rds |FALSE    |


### Crossref data

Crossref were extracted as needed to obtain bibliographic information (author names, article titles, publication dates) [@crossref-data], using the `rcrossref` package to query the API `. The database itself is free to access. More information about it can be read in @crossref-paper . Note that data can and is updated, so running the query again 


|Data Source                                     |Filename                             |Provided |
|:-----------------------------------------------|:------------------------------------|:--------|
|Crossref (2023)                                 |./data/crossref/crossref_aejdois.Rds |TRUE     |
|Crossref (2023)                                 |./data/crossref/crossref_info.csv    |TRUE     |
|Crossref (2023)                                 |./data/crossref/crossref_info.Rds    |TRUE     |
|Generated during acquisition of Crossref data   |./data/crossref/crossref_timing.Rds  |TRUE     |
|Generated for data quality audit purposes       |./data/crossref/audit-exp.xlsx       |TRUE     |
|Hand-generated by authors based on AEA websites |./data/crossref/issns.Rds            |TRUE     |


### openAlex data

Data were accessed in 2023 to increase the time series covered by the bibliometric analysis [@openalex-data]. The openAlex database is described in @openalex2022. OpenAlex data are accessed via an API, and computed statistics, such as the h-index, are limited to the past 10 years. We therefore had to recompute some numbers. We also saved our extract, as future extracts may have different numbers, due to improvements in entity disambiguation (author names) and other factors outside of our control.

Not all data elements are complete in the openAlex data. We output data for various quality checks, and did manual research to "impute" attributes such as affiliations. All "overrides" are captured in a separate file. 

Thus, the openAlex data has (a) the raw data as downloaded; (b) the problematic data, as output for manual review (c) the edited/imputed data as used to complement the downloaded data.


|Data Source                                                       |Filename                                        |Provided |
|:-----------------------------------------------------------------|:-----------------------------------------------|:--------|
|OurResearch (2023)                                                |./data/openalex/citations-per-paper.Rds         |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae-authors.Rds      |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-aejae.Rds              |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-hindex.Rds             |TRUE     |
|OurResearch (2023)                                                |./data/openalex/openalex-institutions-aejae.Rds |TRUE     |
|Generated from openAlex data                                      |./data/openalex/affiliations.csv                |TRUE     |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.csv          |TRUE     |
|Hand-edited data to override or complement openAlex data          |./data/openalex/affiliation-impute.xlsx         |TRUE     |
|Machine-imputed institutions using auxiliary openAlex information |./data/openalex/affiliations-imputed.Rds        |TRUE     |
|Records to remove from extracted openAlex data                    |./data/openalex/blacklist.xlsx                  |TRUE     |

### Web of Science data

The bibliometric information, in particular on h-index, was originally extracted from Web of Science [@WebofScience;@webofscience2018], and was used in the early working papers [@kingi2018;@herbert2021]. In response to referee comments, the primary bibliometric analysis shifted to the use of openAlex data (described next). OpenAlex data is compared to WoS data in the appendix.

WoS is proprietary data and requires a subscription. We accessed the interface thanks to Cornell's subscription.

Data were manually extracted from the WoS web interface, with RAs following guidance on what data to extract (see [`data/h_index_data/README.md`](data/h_index_data/README.md)). The collated data is provided in XLSX and CSV formats.



|Data Source                                               |Filename                                          |Provided |
|:---------------------------------------------------------|:-------------------------------------------------|:--------|
|Reuters (2016), Clarivate (2018) and manual edits         |./data/h_index_data/h-index-assignment1.2019.csv  |TRUE     |
|Reuters (2016), Clarivate (2018) and manual edits         |./data/h_index_data/h-index-assignment1.2019.xlsx |TRUE     |
|Procedural file to map short names to long variable names |./data/auxiliary/mainOA-mapping.xlsx              |TRUE     |


# Computational requirements


## Software Requirements



- [x] The replication package contains one or more programs to install all dependencies and set up the necessary directory structure. 

- R version 4.2.2 (2022-10-31) on x86_64, linux-gnu
  - Docker image is used (see appendix), with system libraries defined by the relevant image (rocker/verse:4.2.2) (optional, but recommended)
  - RSPM (now [Posit Package Manager, PPM](https://packagemanager.posit.co/client/)) is used, set to 2022-11-22. All libraries are installed from that time-stamped repository.



|Libraries are defined in |
|:------------------------|
|global-libraries.R       |
|programs/libraries.R     |
|readme-libraries.R       |
|text/libraries.R         |

### Software citations

Only directly loaded libraries are cited. For all libraries used in the runtime environment, see the Appendix.


|Package     |Version    |Citation                                       |
|:-----------|:----------|:----------------------------------------------|
|base        |4.2.2      |@base                                          |
|devtools    |2.4.5      |@devtools                                      |
|fastDummies |1.6.3      |@fastDummies                                   |
|knitr       |1.42       |@knitr2014; @knitr2015; @knitr2023             |
|openalexR   |1.2.2.9999 |@openalexR                                     |
|rcrossref   |1.2.0      |@rcrossref                                     |
|remotes     |2.4.2      |@remotes                                       |
|reshape2    |1.4.4      |@reshape2                                      |
|rmarkdown   |2.20       |@rmarkdown2018; @rmarkdown2020; @rmarkdown2023 |
|rprojroot   |2.0.3      |@rprojroot                                     |
|tidyverse   |2.0.0      |@tidyverse                                     |


## Controlled Randomness



- [ ] Random seed is set at line _____ of program ______
- [x] No Pseudo random generator is used in the analysis described here.

Note that re-running the API queries (turned off by default) **will** generate different data, which is likely to affect the regression output.

## Memory, Runtime, Storage Requirements

### Summary

Approximate time needed to reproduce the analyses on a standard (CURRENT YEAR) desktop machine (when not running API queries):

- [ ] <10 minutes
- [x] 10-60 minutes
- [ ] 1-2 hours
- [ ] 2-8 hours
- [ ] 8-24 hours
- [ ] 1-3 days
- [ ] 3-14 days
- [ ] > 14 days

API queries can take a long time, are not guaranteed to work, and are not guaranteed to return the same results. All API queries are stored as of the last run, and made available in the replication package.

Approximate storage space needed:

- [ ] < 25 MBytes
- [x] 25 MB - 250 MB
- [ ] 250 MB - 2 GB
- [ ] 2 GB - 25 GB
- [ ] 25 GB - 250 GB
- [ ] > 250 GB

- [ ] Not feasible to run on a desktop machine, as described below.

### Details

The code was last run on 

- OS: "openSUSE Leap 15.5"
- Processor:  AMD Ryzen 9 3900X 12-Core Processor, 24 cores
- Memory available: 31GB memory
- Docker version 24.0.7-ce, build 311b9ff0aa93
- R version 4.2.2 (2022-10-31) on x86_64, linux-gnu

as well as a Macbook with 16GB of memory. At least one program failed when run with less than 16GB of memory.


# Description of programs/code

Each numbered R program can be run independently, in the sequence implied by the numbering scheme. 
A convenience bash main script (`run.sh`) to run all programs is provided in the root of the project, and will run all data cleaning and analysis programs.

Of note, several programs leverage APIs, which can yield different results, and might take  long time. The programs will detect previously downloaded data files, and skip the download part if those files are present. To start with a fresh download, delete the following files:

- /home/rstudio/data/crossref/crossref_info.Rds - Crossref extract.
- /home/rstudio/text/includes/Replication_write.bib - bibtex file of articles analyzed
- /home/rstudio/data/openalex/openalex-aejae.Rds - extract of articles analyzed from openAlex
- /home/rstudio/data/openalex/openalex-aejae-authors.Rds - bibliometric information from openAlex on all the authors of articles analyzed
- /home/rstudio/data/crossref/crossref_aejdois.Rds - bibliographic information from Crossref for later analysis

Internet access is required to run the programs, 

- to download the key input files [@raw-data-2019]
- when re-running queries to the APIs

The programs can be run without internet access if manually downloaded files from [https://doi.org/10.5281/zenodo.2639920](https://doi.org/10.5281/zenodo.2639920) are placed into `dataloc`.

##  License for Code


The code (all files ending in `.R`, `.Rmd`, and `.sh`) is licensed under a BSD license. See [LICENSE.txt](LICENSE.txt) for details.

# Instructions to Replicators

If using Docker image on Linux or macOS system:

- run `start_rstudio.sh` and connect to [https://localhost:8787](https://localhost:8787)
- in the "Terminal" of the RStudio app, run `bash ./run.sh`

or equivalently,

- in the terminal of a computer with Docker installed, run `bash ./run_docker.sh ./run.sh`

Alternative ways to run this (these were not tested):

- optionally, before running project code, run `Rscript -e "renv::init()"` (on Windows, `Rscript.exe -e "renv::init()"` ) to isolate the project libraries from your system (assumes `renv` is installed, see [renv](https://rstudio.github.io/renv/articles/renv.html)).
- Using the same R version as described above, run each program individually as desired, in the order indicated above.

## Details


|Filename                                  |Note                                                                                              |
|:-----------------------------------------|:-------------------------------------------------------------------------------------------------|
|./global-libraries.R                      |Defines libraries used globally in various pieces (text, analysis, and documentation)             |
|./pathconfig.R                            |Defines various relative paths                                                                    |
|./programs/_footnotes.R                   |Functions for displaying various table footnotes.                                                 |
|./programs/_read_analysis_data.R          |Function to read analysis data used in various programs                                           |
|./programs/01_download_replication_data.R |Download the Replication Lab data from Zenodo.                                                    |
|./programs/02_get_crossref.R              |Old program to get Crossref info on articles. May no longer work.                                 |
|./programs/04_clean_replicationlist.R     |Various cleaning steps applied to the Replication Lab data.                                       |
|./programs/06_gen_hindex_list.R           |Used to generate the task list to query Web of Science. Used once, for reference only.            |
|./programs/07_readclean_hindex_list.R     |Read the generated and hand-collated data from Web of Science.                                    |
|./programs/08_get_crossref_bibs.R         |Old program to get Crossref info for bib files for all the articles. May no longer be functional. |
|./programs/25_prepare_sample.R            |Uses downloaded data and generates the analysis sample                                            |
|./programs/30_results1.R                  |Original analysis, now only used for appendix tables.                                             |
|./programs/31_results2.R                  |Original analysis, now only used for appendix tables.                                             |
|./programs/40_supplementary1.R            |New query to Crossref, creation of table_article_selection.tex.                                   |
|./programs/41_supplementary2.R            |Query to openAlex for all articles and their subsequent citations.                                |
|./programs/42_prepare_authors.R           |Query to openAlex for author and institution information                                          |
|./programs/43_supplementary4.R            |Processes the author and institution information, completing missing information                  |
|./programs/44_supplementary_hindex.R      |Re-computes the h-index per author and year                                                       |
|./programs/45_supplementary_authexp.R     |This program primarily handles computing academic age of author                                   |
|./programs/46_prepare_analysis.R          |Combine the various pieces into a single analysis file for the bibliometric analysis.             |
|./programs/47_prepare_mainOA.R            |Add additional pieces to the analysis                                                             |
|./programs/48_mainOA_authorpaper_stats.R  |Summary statistics: Papers and authors                                                            |
|./programs/49_assignments.R               |Summary statistics: Assignments                                                                   |
|./programs/50_analysis_openAlex.R         |Core analysis                                                                                     |
|./programs/51_analysis_Poisson.R          |Poisson models                                                                                    |
|./programs/52_robustess_standarderrors.R  |Robust standard errors                                                                            |
|./programs/99_zz_info.R                   |System information                                                                                |
|./programs/config.R                       |Basic configuration                                                                               |
|./programs/libraries.R                    |All libraries used                                                                                |
|./text/libraries.R                        |Additional libraries (possibly no longer relevant, kept for robustness)                           |


# List of tables and programs


The provided code reproduces:

- [x] All numbers provided in text in the paper
- [x] All tables and figures in the paper
- [ ] Selected tables and figures in the paper, as explained and justified below.

The code also produces numerous tables which were not included in the paper. Note that following a request from copy-editors, all tables used in the paper were subsumed into the main document. Earlier versions used `\input` statements in LaTeX.


|Table number |Program                                |LaTeX file                                       |
|:------------|:--------------------------------------|:------------------------------------------------|
|1            |programs/40_supplementary1.R           |text/includes/table_article_selection.tex        |
|2            |NA                                     |No code                                          |
|3            |programs/30_results1.R                 |text/includes/table_data_availability.tex        |
|4            |programs/30_results1.R                 |text/includes/table_data_prog.tex                |
|5            |programs/30_results1.R                 |text/includes/table_difficult.tex                |
|6            |programs/30_results1.R                 |text/includes/table_doc.tex                      |
|7            |programs/31_results2.R                 |text/includes/table_results_both.tex             |
|8            |programs/31_results2.R                 |text/includes/table_reason.tex                   |
|9            |programs/31_results2.R                 |text/analysis/table_code.tex                     |
|10           |programs/31_results2.R                 |text/analysis/table_reg1.tex                     |
|11           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_metrics_OA.tex               |
|12           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_reg_probit_0_full.tex        |
|13           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_ratios.tex                   |
|14           |programs/50_analysis_openAlex.R        |text/analysis/table_reg2OA.tex                   |
|15           |programs/50_analysis_openAlex.R        |text/analysis/table_arcreg3OA.tex                |
|16           |programs/50_analysis_openAlex.R        |text/analysis/table_arcregpost12OA.tex           |
|17           |programs/50_analysis_openAlex.R        |text/analysis/table_arcreg3OA4.tex               |
|A1           |programs/49_assignments.R              |text/includes/table_assignments.tex              |
|A2           |programs/30_results1.R                 |text/includes/table_absence.tex                  |
|A3           |programs/31_results2.R                 |text/includes/table_results_by_year.tex          |
|A4           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_reg_probit_0_fullpartial.tex |
|A5           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_metrics_OA_app0.tex          |
|A6           |programs/31_results2.R                 |text/analysis/table_metrics.tex                  |
|A7           |programs/48_mainOA_authorpaper_stats.R |text/analysis/table_metrics_OA_wos.tex           |
|A8           |programs/50_analysis_openAlex.R        |text/analysis/table_stassessed.tex               |
|A9           |programs/50_analysis_openAlex.R        |text/analysis/table_stattempted.tex              |
|A10          |programs/50_analysis_openAlex.R        |text/analysis/table_reg3OA.tex                   |
|A11          |programs/50_analysis_openAlex.R        |text/analysis/table_logreg3OA.tex                |
|A12          |programs/51_analysis_Poisson.R         |text/analysis/table_poissonreg.tex               |
|A13          |programs/50_analysis_openAlex.R        |text/analysis/table_arcreg3OA_partial.tex        |
|A14          |programs/50_analysis_openAlex.R        |text/analysis/table_reg3OA_partial.tex           |
|A15          |programs/50_analysis_openAlex.R        |text/analysis/table_logreg3OA_partial.tex        |
|A16          |programs/51_analysis_Poisson.R         |text/analysis/table_poissonreg_partial.tex       |
|A17          |programs/50_analysis_openAlex.R        |text/analysis/table_arcregpost12OA_partial.tex   |

---

# Acknowledgements

This README based on the template created by @template-readme. 

# References


<div id="refs"></div>

#  Appendix {-} 

## Appendix: System and package info





```
## ─ Session info ───────────────────────────────────────────────────────────────────────────────────────────────────────
##  setting  value
##  version  R version 4.2.2 (2022-10-31)
##  os       Ubuntu 22.04.2 LTS
##  system   x86_64, linux-gnu
##  ui       X11
##  language (EN)
##  collate  en_US.UTF-8
##  ctype    en_US.UTF-8
##  tz       Etc/UTC
##  date     2024-03-16
##  pandoc   2.19.2 @ /usr/local/bin/ (via rmarkdown)
## 
## ─ Packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────
##  package     * version date (UTC) lib source
##  base64enc     0.1-3   2015-07-28 [1] RSPM (R 4.2.0)
##  bindr         0.1.1   2018-03-13 [1] RSPM (R 4.2.0)
##  bindrcpp    * 0.2.2   2018-03-29 [1] RSPM (R 4.2.0)
##  boot          1.3-28  2021-05-03 [2] CRAN (R 4.2.2)
##  cachem        1.0.7   2023-02-24 [1] RSPM (R 4.2.0)
##  callr         3.7.3   2022-11-02 [1] RSPM (R 4.2.0)
##  cellranger    1.1.0   2016-07-27 [1] RSPM (R 4.2.0)
##  cli           3.6.0   2023-01-09 [1] RSPM (R 4.2.0)
##  colorspace    2.1-0   2023-01-23 [1] RSPM (R 4.2.0)
##  crayon        1.5.2   2022-09-29 [1] RSPM (R 4.2.0)
##  crul          1.3     2022-09-03 [1] RSPM (R 4.2.0)
##  curl          5.0.0   2023-01-12 [1] RSPM (R 4.2.0)
##  data.table  * 1.14.8  2023-02-17 [1] RSPM (R 4.2.0)
##  devtools    * 2.4.5   2022-10-11 [1] RSPM (R 4.2.0)
##  digest        0.6.31  2022-12-11 [1] RSPM (R 4.2.0)
##  dplyr       * 1.1.0   2023-01-29 [1] RSPM (R 4.2.0)
##  DT            0.26    2022-10-19 [1] RSPM (R 4.2.0)
##  ellipsis      0.3.2   2021-04-29 [1] RSPM (R 4.2.0)
##  evaluate      0.20    2023-01-17 [1] RSPM (R 4.2.0)
##  fansi         1.0.4   2023-01-22 [1] RSPM (R 4.2.0)
##  fastDummies * 1.6.3   2020-11-29 [1] RSPM (R 4.2.0)
##  fastmap       1.1.1   2023-02-24 [1] RSPM (R 4.2.0)
##  formattable * 0.2.1   2021-01-07 [1] RSPM (R 4.2.0)
##  fs            1.6.1   2023-02-06 [1] RSPM (R 4.2.0)
##  generics      0.1.3   2022-07-05 [1] RSPM (R 4.2.0)
##  ggplot2     * 3.4.1   2023-02-10 [1] RSPM (R 4.2.0)
##  glue          1.6.2   2022-02-24 [1] RSPM (R 4.2.0)
##  grateful    * 0.2.6   2024-03-14 [1] Github (Pakillo/grateful@cdaaa90)
##  gtable        0.3.1   2022-09-01 [1] RSPM (R 4.2.0)
##  hms           1.1.2   2022-08-19 [1] RSPM (R 4.2.0)
##  htmltools     0.5.4   2022-12-07 [1] RSPM (R 4.2.0)
##  htmlwidgets   1.6.1   2023-01-07 [1] RSPM (R 4.2.0)
##  httpcode      0.3.0   2020-04-10 [1] RSPM (R 4.2.0)
##  httpuv        1.6.9   2023-02-14 [1] RSPM (R 4.2.0)
##  jsonlite      1.8.4   2022-12-06 [1] RSPM (R 4.2.0)
##  knitr       * 1.42    2023-01-25 [1] RSPM (R 4.2.0)
##  later         1.3.0   2021-08-18 [1] RSPM (R 4.2.0)
##  lattice       0.20-45 2021-09-22 [2] CRAN (R 4.2.2)
##  lifecycle     1.0.3   2022-10-07 [1] RSPM (R 4.2.0)
##  magrittr      2.0.3   2022-03-30 [1] RSPM (R 4.2.0)
##  markdown    * 1.4     2022-11-16 [1] RSPM (R 4.2.0)
##  memoise       2.0.1   2021-11-26 [1] RSPM (R 4.2.0)
##  mime          0.12    2021-09-28 [1] RSPM (R 4.2.0)
##  miniUI        0.1.1.1 2018-05-18 [1] RSPM (R 4.2.0)
##  munsell       0.5.0   2018-06-12 [1] RSPM (R 4.2.0)
##  pastecs     * 1.3.21  2018-03-15 [1] RSPM (R 4.2.0)
##  pillar        1.8.1   2022-08-19 [1] RSPM (R 4.2.0)
##  pkgbuild      1.4.0   2022-11-27 [1] RSPM (R 4.2.0)
##  pkgconfig     2.0.3   2019-09-22 [1] RSPM (R 4.2.0)
##  pkgload       1.3.2   2022-11-16 [1] RSPM (R 4.2.0)
##  plyr          1.8.8   2022-11-11 [1] RSPM (R 4.2.0)
##  prettyunits   1.1.1   2020-01-24 [1] RSPM (R 4.2.0)
##  processx      3.8.0   2022-10-26 [1] RSPM (R 4.2.0)
##  profvis       0.3.7   2020-11-02 [1] RSPM (R 4.2.0)
##  promises      1.2.0.1 2021-02-11 [1] RSPM (R 4.2.0)
##  ps            1.7.2   2022-10-26 [1] RSPM (R 4.2.0)
##  purrr         1.0.1   2023-01-10 [1] RSPM (R 4.2.0)
##  R6            2.5.1   2021-08-19 [1] RSPM (R 4.2.0)
##  Rcpp        * 1.0.10  2023-01-22 [1] RSPM (R 4.2.0)
##  rcrossref   * 1.2.0   2022-11-11 [1] RSPM (R 4.2.0)
##  readr       * 2.1.4   2023-02-10 [1] RSPM (R 4.2.0)
##  readxl      * 1.4.2   2023-02-09 [1] RSPM (R 4.2.0)
##  remotes       2.4.2   2021-11-30 [1] RSPM (R 4.2.0)
##  repr          1.1.4   2022-01-04 [1] RSPM (R 4.2.0)
##  reshape2    * 1.4.4   2020-04-09 [1] RSPM (R 4.2.0)
##  rjson       * 0.2.21  2022-01-09 [1] RSPM (R 4.2.0)
##  rlang         1.0.6   2022-09-24 [1] RSPM (R 4.2.0)
##  rmarkdown     2.20    2023-01-19 [1] RSPM (R 4.2.0)
##  rprojroot   * 2.0.3   2022-04-02 [1] RSPM (R 4.2.0)
##  sandwich    * 3.0-2   2022-06-15 [1] RSPM (R 4.2.0)
##  scales        1.2.1   2022-08-20 [1] RSPM (R 4.2.0)
##  sessioninfo   1.2.2   2021-12-06 [1] RSPM (R 4.2.0)
##  shiny         1.7.4   2022-12-15 [1] RSPM (R 4.2.0)
##  skimr       * 2.1.4   2022-04-15 [1] RSPM (R 4.2.0)
##  stargazer   * 5.2.3   2022-03-04 [1] RSPM (R 4.2.0)
##  stringi       1.7.12  2023-01-11 [1] RSPM (R 4.2.0)
##  stringr     * 1.5.0   2022-12-02 [1] RSPM (R 4.2.0)
##  tibble        3.2.0   2023-03-08 [1] RSPM (R 4.2.0)
##  tictoc      * 1.1     2022-09-03 [1] RSPM (R 4.2.0)
##  tidyr       * 1.3.0   2023-01-24 [1] RSPM (R 4.2.0)
##  tidyselect    1.2.0   2022-10-10 [1] RSPM (R 4.2.0)
##  tzdb          0.3.0   2022-03-28 [1] RSPM (R 4.2.0)
##  urlchecker    1.0.1   2021-11-30 [1] RSPM (R 4.2.0)
##  usethis     * 2.1.6   2022-05-25 [1] RSPM (R 4.2.0)
##  utf8          1.2.3   2023-01-31 [1] RSPM (R 4.2.0)
##  vctrs         0.5.2   2023-01-23 [1] RSPM (R 4.2.0)
##  withr         2.5.0   2022-03-03 [1] RSPM (R 4.2.0)
##  xfun          0.37    2023-01-31 [1] RSPM (R 4.2.0)
##  xml2          1.3.3   2021-11-30 [1] RSPM (R 4.2.0)
##  xtable      * 1.8-4   2019-04-21 [1] RSPM (R 4.2.0)
##  zoo           1.8-11  2022-09-17 [1] RSPM (R 4.2.0)
## 
##  [1] /usr/local/lib/R/site-library
##  [2] /usr/local/lib/R/library
## 
## ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
##                _                           
## platform       x86_64-pc-linux-gnu         
## arch           x86_64                      
## os             linux-gnu                   
## system         x86_64, linux-gnu           
## status                                     
## major          4                           
## minor          2.2                         
## year           2022                        
## month          10                          
## day            31                          
## svn rev        83211                       
## language       R                           
## version.string R version 4.2.2 (2022-10-31)
## nickname       Innocent and Trusting
```

## Appendix: Dockerfile



```
## FROM rocker/verse:4.2.2
## 
## RUN apt-get update \
##     && DEBIAN_FRONTEND=noninteractive apt-get install -y \
##          locales \
##          libcurl4-openssl-dev \
##          libssl-dev \
##         imagemagick \
##         libmagick++-dev \
##         gsfonts \
##         pandoc \
##         libicu-dev \
##         libtcl8.6 \
##         libtk8.6 \
##     && rm -rf /var/lib/apt/lists/* \
##     && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
## RUN apt-get update \
##     && DEBIAN_FRONTEND=noninteractive apt-get install -y \
##         texlive-latex-recommended texlive-latex-base texlive-binaries texlive-latex-extra\
##         texlive-humanities texlive-bibtex-extra biber \
##     && rm -rf /var/lib/apt/lists/* \
##     && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
## 
## COPY global-libraries.R install1.R
## COPY programs/libraries.R install2.R
## COPY text/libraries.R install3.R
## COPY readme-libraries.R install4.R
## 
## RUN cat install?.R > install.R && Rscript install.R
```
