# aej-applied-replications

## Structure

- [data](data/) contains the data, both acquired and generated.
  - [citation_data](data/citation_data) contains the data extracted from Web of Science on citations of articles. See [README.md](data/citation_data/README.md) for instructions how to manually extract the data. The directory contains the results of various extracts, described in the article.
  - [h_index_data](data/h_index_data) contains data extracted from Web of Science to compute the h-index of all the authors of the article. The h-index is recomputed to be as-of date of publication of the article. See [README.md](data/h_index_data/README.md) for instructions how to manually extract the data. Data were extracted at various times.
  - [replication_data](data/replication_data) contains the cleaned and anonymized data on replicators' assessment, and outcome of the replication attempts. Code to anonymize is in the [programs](programs/) directory. Raw data is not made available due to privacy concerns, but the code will reveal that only person IDs were modified.
  - [interwrk](data/interwrk) is only used for processing while article tables are generated, and contains no permanent data. In the replication archive, this directory is empty, and will be created if absent.
- [programs](programs/) contains all programs to clean and process the data outlined above. It separates code that was run by the authors (anonymizing), and code that can be run by anybody replicating the paper given the data. Code that cannot be executed by third parties (anonymizing code) is marked "eval=false" in the [README](programs/README.Rmd). All code is executed from within this directory - load the  [README](programs/README.Rmd) in Rstudio or R, and evaluate (knit); alternatively, run `R CMD BATCH 00_main.R`, which will do the knitting for you.
- [text](text/) contains the LaTeX file to generate the article.
  - [text/analysis](text/analysis) contains figures and tables written out by the R programs, which are included in LaTeX.
  - [text/includes](text/includes) contains other files written out by the R programs, to be included in LaTeX. In particular, single numbers that are derived from the analysis.

## Additional files

- [pathconfig.R]() defines common paths used by all programs and is referenced from within the [programs](programs/) and [text](text/) directories. It should not be executed separately.
- [global-libraries.R]() defines libraries used by all programs and is referenced from within the [programs](programs/) and [text](text/) directories. It should not be executed separately.
- README.md - this file
- replication-paper.Rproj - a configuration file for Rstudio. Only used if using Rstudio (optional).

## Requirements

- R
  - the only package that needs to be present before starting is `rprojroot`. 
  - All others are dynamically installed. Complete lists are 
    - [global-libraries.R]()
    - [programs/libraries.R]()
    - [text/libraries.R]()
- (optional but useful) Rstudio
- a compatible LaTeX installation
