---
title: "Programs"
author:
  '1':
    name: Lars Vilhuber
  '2':
    name: Flavio Stanchi
  '3':
    name: Hautahi Kingi
  '4':
    name: Sylverie Herbert
date: "2024-03-02"
output:
  html_document: 
    keep_md: yes
    number_sections: yes
    toc: yes
  word_document: default
  pdf_document: default
editor_options: 
  chunk_output_type: console
---



This README provides details on how to run (or not) the programs associated with the paper. For details on the environment in which they are run, see the main README.

# Programs

All programs can be found in the `programs` folder, except for some configuration parameters which (by necessity) are stored in the root folder. 

> No foldername should be hard-coded.



## Setup

Every program contains the following lines, and can be run independently:



```r
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
```

```
## Loading required package: dplyr
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Loading required package: devtools
```

```
## Loading required package: usethis
```

```
## Loading required package: rprojroot
```

```
## Loading required package: tictoc
```

```
## Loading required package: ggplot2
```

```
## Loading required package: bindrcpp
```

```
## Loading required package: Rcpp
```

```
## Loading required package: markdown
```

```r
source(file.path(programs,"libraries.R"), echo=FALSE)
```

```
## Loading required package: rcrossref
```

```
## Loading required package: readr
```

```
## Loading required package: tidyr
```

```
## Loading required package: data.table
```

```
## 
## Attaching package: 'data.table'
```

```
## The following object is masked from 'package:tictoc':
## 
##     shift
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## Loading required package: xtable
```

```
## Loading required package: rjson
```

```
## Loading required package: stargazer
```

```
## 
## Please cite as:
```

```
##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer
```

```
## Loading required package: knitr
```

```
## Loading required package: stringr
```

```
## Loading required package: readxl
```

```
## Loading required package: fastDummies
```

```
## Loading required package: skimr
```

```
## Loading required package: sandwich
```

```
## Loading required package: pastecs
```

```
## 
## Attaching package: 'pastecs'
```

```
## The following objects are masked from 'package:data.table':
## 
##     first, last
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```
## The following objects are masked from 'package:dplyr':
## 
##     first, last
```

```
## Loading required package: formattable
```

```
## 
## Attaching package: 'formattable'
```

```
## The following object is masked from 'package:xtable':
## 
##     digits
```

```
## Skipping install of 'openalexR' from a github remote, the SHA1 (558581c6) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```r
source(file.path(programs,"config.R"), echo=FALSE)
```

Note that the path `interwrk` is transitory, and is only kept during processing. It will be empty in the replication archive.

Any libraries needed are called and if necessary installed through `libraries.R`, though this is generally handled by the Docker image at build time.

Most parameters are set in `config.R`:


```r
source(file.path(programs,"config.R"), echo=TRUE)
```

```
## 
## > HindexRaw <- "h-index-assignment1.2019.csv"
## 
## > HindexClean <- "hindex.csv"
## 
## > zenodo.id <- "2639919"
## 
## > zenodo.id <- "2639920"
## 
## > zenodo.api = "https://zenodo.org/api/records/"
## 
## > readRenviron(file.path(basepath, ".Renviron"))
## 
## > issns.file <- file.path(crossrefloc, paste0("issns.Rds"))
## 
## > doi.file <- file.path(dataloc, "crossref_aejdois")
## 
## > doi.file.Rds <- paste(doi.file, "Rds", sep = ".")
## 
## > doi.file.csv <- paste(doi.file, "csv", sep = ".")
## 
## > openalex.file <- file.path(openalexloc, "openalex-aejae")
## 
## > openalex.Rds <- paste0(openalex.file, ".Rds")
## 
## > citations.latest <- file.path(openalexloc, "citations-per-paper.Rds")
## 
## > openalex.authors <- file.path(openalexloc, "openalex-aejae-authors")
## 
## > openalex.authors.Rds <- paste0(openalex.authors, ".Rds")
## 
## > openalex.hindex <- file.path(openalexloc, "openalex-hindex.Rds")
## 
## > bibtex_all = file.path(TexIncludes, "Replication_write.bib")
## 
## > config.tex <- tibble(String = "% Automatically created, do not edit")
## 
## > config.tex[2, ] <- ("String" = paste("\\newcommand{\\TexIncludes}{", 
## +     TexIncludes, "}", sep = ""))
## 
## > config.tex[3, ] <- ("String" = paste("\\newcommand{\\ROutputs}{", 
## +     Outputs, "}", sep = ""))
## 
## > config.tex[4, ] <- ("String" = paste("\\newcommand{\\Rprograms}{", 
## +     programs, "}", sep = ""))
## 
## > write.table(config.tex, file = file.path(TexIncludes, 
## +     "config.tex"), quote = FALSE, col.names = FALSE, row.names = F)
## 
## > options(scipen = 100, digits = 3, width = 120)
## 
## > cw = "0.4pt"
## 
## > fs = "footnotesize"
```

## All programs



|Program name                   |
|:------------------------------|
|_footnotes.R                   |
|_read_analysis_data.R          |
|00_main.R                      |
|01_download_replication_data.R |
|02_get_crossref.R              |
|04_clean_replicationlist.R     |
|06_gen_hindex_list.R           |
|07_readclean_hindex_list.R     |
|08_get_crossref_bibs.R         |
|20_analytics.R                 |
|25_prepare_sample.R            |
|30_results1.R                  |
|31_results2.R                  |
|32_conclusion.R                |
|35_appendix.R                  |
|36_list-articles.R             |
|40_supplementary1.R            |
|41_supplementary2.R            |
|42_prepare_authors.R           |
|43_supplementary4.R            |
|44_supplementary_authexp.R     |
|45_supplementary_hindex.R      |
|46_prepare_analysis.R          |
|47_prepare_mainOA.R            |
|48_mainOA_authorpaper_stats.R  |
|49_assignments.R               |
|50_analysis_openAlex.R         |
|51_analysis_Poisson.R          |
|52_robustess_standarderrors.R  |
|99_write_nums.R                |
|config-private.R               |
|config.R                       |
|function_latexnums.R           |
|libraries.R                    |


## Data cleaning and merging

We combine our collected data with bibliometric data, both manually extracted from "Web of Science" and collected from CrossRef. We also download the cleaned data for both the replication and the Web of Science data here. These programs should be runnable by anybody.



## Download the replication data from Zenodo
The responses to the replication attempts are stored on Google Sheets, and considered private. We have separately cleaned the data,  anonymized it, and uploaded to Zenodo (see https://www.github.com/labordynamicsinstitute/ldi-replication-dataprep). Here, we simply download the data, with a bit of additional data cleaning. 

 - Input data: On Zenodo
 - Output data: path `interwrk',"repllist2.Rds"
 

```r
source(file.path(programs,"01_download_replication_data.R"),echo=TRUE)
```

At the end of this step, the `interwrk` directory  should have the following data files:

- entryQ_pub.Rds - the main data from the "Entry" questionnaire (assessment) 
- exitQ_pub.Rds - the main data from the post-replication "Exit" questionnaire (assessment)
- replication_list_pub.Rds -  the assignment spreadsheet. 


## Get CrossRef information
The master replication list has all the DOIs. We   look up the DOI at CrossRef.


Note: downloading references from CrossRef can take a while. The code contains a fallback - if the file `crossref_info.Rds`  exists, no new pull will be computed. To override, delete the file. 

 - inputs: entryQ_pub, exitQ_pub, replication list (in `dataloc`)
 - outputs: crossref_info.Rds (in `crossrefloc`) and intermediate raw data in `interwrk`


```r
source(file.path(programs,"02_get_crossref.R"),echo=TRUE)
```

### Some diagnostics

When finding DOIs, some articles might not be found. When that is the case, they are reported here.


```r
if ( file.exists(file.path(interwrk,"crossref.diagnostics.Rds"))) {
  crossref.diagnostics <- readRDS(file=file.path(interwrk,"crossref.diagnostics.Rds"))
} else {
  crossref.diagnostics <- data.frame()
}
```

###- DOIs to download (unique DOIs in all replication files): **r NROW(dois.df) **
###- DOIs successfully looked up on CrossRef: **r nrow(bibinfo.df)**
###- DOIs not found: **r nrow(crossref.diagnostics)**   (should be ZERO)


|DOI                  |
|:--------------------|
|10.1257              |
|10.1257/mac.4.2..218 |
|aej-policy-2         |
|10.1257/mic.6.4.237  |
|10.1257/mic.6.4.362  |
|10.1257/mic.6.1.182  |
|AEJPOLICY-10         |
|10.1257/app.20150057 |
|NA                   |



```r
source(file.path(programs,"04_clean_replicationlist.R"),echo=TRUE)
```



## Download the h-index information


## Collect Bibtex info on the articles


```r
# This program collects bibtex information for all the DOIs read by the replicators from crossref
# It will skip processing if the previously collected data is there.
# Relies on CrossRef API, which can be fragile.
source(file.path(programs,"08_get_crossref_bibs.R"),echo=TRUE)
```


## Paper analysis

The paper analysis is run in the `3x*.R`  programs.


```r
source(file.path(programs,'25_prepare_sample.R'),echo=TRUE)
source(file.path(programs,'30_results1.R'),echo=TRUE)
source(file.path(programs,'31_results2.R'),echo=TRUE)
#source(file.path(programs,'20_analytics.R'),echo=TRUE) 
source(file.path(programs,'32_conclusion.R'),echo=TRUE)
#source(file.path(programs,'33_tables.R'),echo=TRUE)   # incorporated into 20_analytics.R
#source(file.path(programs,'34_figures.R'),echo=TRUE)  # incorporated into 20_analytics.R
#source(file.path(programs,'35_appendix.R'),echo=TRUE)
source(file.path(programs,'36_list-articles.R'),echo=TRUE)
```

Note that the path `interwrk` is transitory, and is only kept during processing. It will be empty in the replication archive.

## Outputs

Some numbers are written out to `TexIncludes` (here: /home/rstudio/text/includes) as LaTeX files with a single number. Tables and figures are written out to `Outputs` (here: /home/rstudio/text/analysis). 

Key datasets:

- Complete sample: /home/rstudio/data/replication_data/00_complete_sample.Rds, keyed by DOI.

## Revision 1

After referee reports, the following supplementary analysis has been added:

### Adding a clean count of the number of articles in the universe


```r
source(file.path(programs,'40_supplementary1.R'),echo=TRUE)
```


### Update citations with the latest

Referees wanted to see an updated citation analysis. While we keep the original analysis as-is, the following extracts newer per-year citation counts from openAlex. 




```r
source(file.path(programs,'41_supplementary2.R'),echo=TRUE)
```


The first output file is /home/rstudio/data/openalex/openalex-aejae.Rds, an extract of OpenAlex for all articles in the sample, with



```r
names(readRDS(openalex.Rds))
```

```
##  [1] "id"                          "display_name"                "author"                     
##  [4] "ab"                          "publication_date"            "so"                         
##  [7] "so_id"                       "host_organization"           "issn_l"                     
## [10] "url"                         "pdf_url"                     "license"                    
## [13] "version"                     "first_page"                  "last_page"                  
## [16] "volume"                      "issue"                       "is_oa"                      
## [19] "is_oa_anywhere"              "oa_status"                   "oa_url"                     
## [22] "any_repository_has_fulltext" "language"                    "grants"                     
## [25] "cited_by_count"              "counts_by_year"              "publication_year"           
## [28] "cited_by_api_url"            "ids"                         "doi.url"                    
## [31] "type"                        "referenced_works"            "related_works"              
## [34] "is_paratext"                 "is_retracted"                "concepts"                   
## [37] "DOI"
```

Key to match back to the internal database is `DOI`, a transformation of the OpenAlex-provided `doi` (renamed to `doi.url`. Note that the field `author` contains a list of author ids, as follows:


```r
readRDS(openalex.Rds) %>% select(author) %>% tidyr::unnest(author) %>% names()
```

```
##  [1] "au_id"                    "au_display_name"          "au_orcid"                 "author_position"         
##  [5] "au_affiliation_raw"       "institution_id"           "institution_display_name" "institution_ror"         
##  [9] "institution_country_code" "institution_type"         "institution_lineage"
```

The column `au_id` is key to matching in later OpenAlex information computed. 

The final output file (/home/rstudio/data/openalex/citations-per-paper.Rds) is keyed on `DOI` per article, and has both within-year citations, and cumulative year-to-date citations. These must be recomputed based on the available data, because the OpenAlex API only has 10 years worth of data, but the max citations is (?) accurate.



```r
names(readRDS(citations.latest))
```

```
## [1] "DOI"                "doi.url"            "max_cited_by_count" "year"               "cited_by_count"    
## [6] "ytd_cited_by_count"
```

### Get information on all authors, so we can compute h-index

This is used in several of the following programs. It can take up to 1 hour to run.



```r
source(file.path(programs,'42_prepare_authors.R'),echo=TRUE)
```


Output files:

- openalex.authors.Rds - all works for in-scope authors (authors having published during the time period in AEJ:AE)

Intermediate files

- file.path(interwrk,"authors.df.Rds")) - these are all the *works* by the authors in the sample

Input files:

- file.path(openalexloc,"blacklist.xlsx") - an edited list of author ids to remove, since impossible to find proper data on OpenAlex (usually, because of very very bad name disambiguation)


### Institutional affiliations, mapped to regions, also productivity of those institutions 

There are some authors for whom we did not have institutions; we hand-edited some of those. These can be found in

- /home/rstudio/data/crossref/affiliation-impute.xlsx


```r
source(file.path(programs,'43_supplementary4.R'),echo=TRUE)
```



### Getting at experience

In order to get publication experience, we use the first observed publications for an author. This hinges on entity disambiguation by OpenAlex, which does not always get it right. We audited some extreme values, and manually searched for authors' first publication or obtention of Ph.D. 



```r
source(file.path(programs,'44_supplementary_authexp.R'),echo=TRUE)
```

- Output: /home/rstudio/data/interwrk/authors.exp.df.Rds
- Audit file: /home/rstudio/data/crossref/audit-exp.xlsx


### Recomputing h-index

In order to recompute the h-index, we pull ALL works by each of the authors in our set, compute year-to-date citations, and compute a as-of-year h-index.

- At least one author was mis-matched by OpenAlex, and the mismatched entity is on a blacklist.


```r
source(file.path(programs,'45_supplementary_hindex.R'),echo=TRUE)
```

Output files:

- openalex.hindex - per-author-year hindex

Intermediate files:

- file.path(interwrk,"citations.df.Rds")) - these are the citations of those works. It can be expanded from the authors.df.

Auxiliary files:

- At least one author was mis-matched by OpenAlex, and the mismatched entity is on a blacklist. /home/rstudio/data/crossref/blacklist.xlsx

The final output file `openalex.hindex` (/home/rstudio/data/openalex/openalex-hindex.Rds) is keyed on the OpenAlex author id `au_id`


```r
names(hindex.by.year)
```

Here's an example:

```r
# test whether this seems reasonable

#citations.df %>% filter(au_id=="https://openalex.org/A5027614993") %>% kable()
hindex.by.year %>% filter(au_id=="https://openalex.org/A5027614993") %>% kable()

authors.df %>% filter(article_id=="https://openalex.org/W2577227262") %>% kable()
citations.df %>% filter(article_id=="https://openalex.org/W2577227262") %>% kable()
```
