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
date: "2023-10-18"
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


# Programs

## Setup



```r
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=TRUE)
```

```
## 
## > basepath <- rprojroot::find_rstudio_root_file()
## 
## > dataloc <- file.path(basepath, "data", "replication_data")
## 
## > interwrk <- file.path(basepath, "data", "interwrk")
## 
## > hindexloc <- file.path(basepath, "data", "h_index_data")
## 
## > crossrefloc <- file.path(basepath, "data", "crossref")
## 
## > TexBase <- file.path(basepath, "text")
## 
## > TexIncludes <- file.path(basepath, "text", "includes")
## 
## > Outputs <- file.path(basepath, "text", "analysis")
## 
## > notes <- file.path(basepath, "text", "hautahi_notes")
## 
## > programs <- file.path(basepath, "programs")
## 
## > for (dir in list(dataloc, interwrk, hindexloc, crossrefloc, 
## +     TexIncludes, Outputs)) {
## +     if (file.exists(dir)) {
## +     }
## +     else {
## +     .... [TRUNCATED]
```

Note that the path `interwrk` is transitory, and is only kept during processing. It will be empty in the replication archive.

Any libraries needed are called and if necessary installed through `libraries.R`:


```r
source(file.path(basepath,"global-libraries.R"),echo=TRUE)
```

```
## 
## > mran.date <- "2022-11-22"
## 
## > get_os <- function() {
## +     sysinf <- Sys.info()
## +     if (!is.null(sysinf)) {
## +         os <- sysinf["sysname"]
## +         if (os == "Darwin") 
## +   .... [TRUNCATED] 
## 
## > if (get_os() == "linux") {
## +     options(repos = c(REPO_NAME = paste0("https://packagemanager.posit.co/cran/__linux__/focal/", 
## +         mran.date) .... [TRUNCATED] 
## 
## > getOption("repos")["CRAN"]
## <NA> 
##   NA 
## 
## > pkgTest <- function(x) {
## +     if (!require(x, character.only = TRUE)) {
## +         install.packages(x, dep = TRUE)
## +         if (!require(x, charact .... [TRUNCATED] 
## 
## > global.libraries <- c("dplyr", "devtools", "rprojroot", 
## +     "tictoc", "ggplot2", "bindrcpp", "Rcpp")
## 
## > results <- sapply(as.list(global.libraries), pkgTest)
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

```r
source(file.path(programs,"libraries.R"), echo=TRUE)
```

```
## 
## > libraries <- c("dplyr", "devtools", "rcrossref", "readr", 
## +     "tidyr", "data.table", "rjson", "ggplot2", "Rcpp", "stargazer", 
## +     "knitr", "st ..." ... [TRUNCATED] 
## 
## > results <- sapply(as.list(libraries), pkgTest)
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
## Loading required package: openalexR
```

```
## Thank you for using openalexR!
## To acknowledge our work, please cite the package by calling
## `citation("openalexR")`.
```

```
## 
## > cbind(libraries, results)
##       libraries    results
##  [1,] "dplyr"      "OK"   
##  [2,] "devtools"   "OK"   
##  [3,] "rcrossref"  "OK"   
##  [4,] "readr"      "OK"   
##  [5,] "tidyr"      "OK"   
##  [6,] "data.table" "OK"   
##  [7,] "rjson"      "OK"   
##  [8,] "ggplot2"    "OK"   
##  [9,] "Rcpp"       "OK"   
## [10,] "stargazer"  "OK"   
## [11,] "knitr"      "OK"   
## [12,] "stringr"    "OK"   
## [13,] "readxl"     "OK"   
## [14,] "openalexR"  "OK"   
## 
## > libraries3 <- c("magick", "summarytools")
## 
## > pkgTestSrc <- function(x) {
## +     if (!require(x, character.only = TRUE)) {
## +         install.packages(x, repos = paste0("https://packagemanager.pos ..." ... [TRUNCATED] 
## 
## > if (get_os() == "linux") {
## +     libraries2 <- c("Rcpp")
## +     results2 <- sapply(as.list(libraries2), pkgTest)
## + } else {
## +     sapply(as.list(libr .... [TRUNCATED] 
## 
## > remotes::install_github("ropensci/openalexR")
```

```
## Skipping install of 'openalexR' from a github remote, the SHA1 (51340446) has not changed since last install.
##   Use `force = TRUE` to force installation
```

Most parameters are set in the `config.R`:


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
## > openalex.file <- file.path(crossrefloc, "openalex-aejae")
## 
## > openalex.Rds <- paste0(openalex.file, ".Rds")
## 
## > citations.latest <- file.path(hindexloc, "citations-per-paper.Rds")
## 
## > openalex.authors <- file.path(crossrefloc, "openalex-aejae-authors")
## 
## > openalex.authors.Rds <- paste0(openalex.authors, ".Rds")
## 
## > openalex.hindex <- file.path(hindexloc, "openalex-hindex.Rds")
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
## > options(scipen = 1, digits = 1, width = 50)
## 
## > cw = "0.4pt"
## 
## > fs = "footnotesize"
```


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

```
## 
## > repllist.doi <- readRDS(file = file.path(dataloc, 
## +     "replication_list_pub.Rds")) %>% select(DOI)
## 
## > exit.doi <- readRDS(file = file.path(dataloc, "exitQ_pub.Rds")) %>% 
## +     select(DOI)
## 
## > entry.doi <- readRDS(file = file.path(dataloc, "entryQ_pub.Rds")) %>% 
## +     select(DOI)
## 
## > crossref.file <- file.path(crossrefloc, "crossref_info")
## 
## > dois_toget <- unique(c(repllist.doi$DOI, exit.doi$DOI, 
## +     entry.doi$DOI))
## 
## > assessment_data1 <- unique(repllist.doi)
## 
## > assessment_data2 <- unique(exit.doi)
## 
## > assessment_data3 <- unique(entry.doi)
## 
## > if (file.exists(paste0(crossref.file, ".Rds"))) {
## +     message(paste0("File already exists: ", crossref.file))
## +     message("Loading file from pre ..." ... [TRUNCATED]
```

```
## File already exists: /home/rstudio/data/crossref/crossref_info
```

```
## Loading file from previous version.
```

```
## 
## > dois.df <- as.data.frame(dois_toget)
## 
## > names(dois.df) <- "DOI"
## 
## > crossref.diagnostics <- anti_join(as.data.frame(dois.df), 
## +     bibinfo.df)
```

```
## Joining with `by = join_by(DOI)`
```

```
## 
## > saveRDS(crossref.diagnostics, file = file.path(interwrk, 
## +     "crossref.diagnostics.Rds"))
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


```r
source(file.path(programs,"06_gen_hindex_list.R"),echo=TRUE)
```

```
## 
## > hindex_request.df <- readRDS(file = file.path(interwrk, 
## +     "replication_list_clean.Rds")) %>% select(DOI, "title", "author", 
## +     "year", "vol ..." ... [TRUNCATED] 
## 
## > saveRDS(hindex_request.df, file = file.path(interwrk, 
## +     "hindex_request.Rds"))
## 
## > write.csv(hindex_request.df, file = file.path(interwrk, 
## +     "hindex_request.csv"))
```

```r
source(file.path(programs,"07_readclean_hindex_list.R"),echo=TRUE)
```

```
## 
## > h.index <- read.csv(paste(hindexloc, HindexRaw, sep = "/"), 
## +     header = TRUE)
## 
## > h.index.melt <- reshape2::melt(data = h.index, id.vars = c("DOI", 
## +     "Total.Citations", "Average.per.Year"), measure.vars = c("Author.1", 
## +     .... [TRUNCATED] 
## 
## > names(h.index.melt)[4:5] <- c("Author.Order", "Author.Name")
## 
## > h.index.melt$Author.Order <- sub("Author.([1-6])", 
## +     "\\1", h.index.melt$Author.Order, perl = TRUE)
## 
## > h.index.melt2 <- reshape2::melt(data = h.index, id.vars = c("Title", 
## +     "DOI", "Total.Citations", "Average.per.Year"), measure.vars = c("h_index ..." ... [TRUNCATED] 
## 
## > names(h.index.melt2)[5:6] <- c("Author.Order", "h-index")
## 
## > h.index.melt2$Author.Order <- sub("h_index..Author.([1-6]).", 
## +     "\\1", h.index.melt2$Author.Order, perl = TRUE)
## 
## > h.index.clean <- merge(h.index.melt[which(h.index.melt$DOI != 
## +     "" & h.index.melt$Author.Name != ""), ], h.index.melt2[which(h.index.melt2$DOI  .... [TRUNCATED] 
## 
## > nrow(h.index.clean)
## [1] 796
## 
## > authors.publications <- as.data.frame(table(h.index.clean$Author.Name))
## 
## > names(authors.publications)[1] <- "Author.Name"
## 
## > nrow(authors.publications)
## [1] 582
## 
## > multiply.published <- authors.publications[which(authors.publications[, 
## +     2] > 1), ]
## 
## > data.authors <- h.index.clean[!duplicated(h.index.clean$Author.Name), 
## +     c("Author.Name", "h-index")]
## 
## > summary(data.authors[, 2])[c(3, 4, 6)]
## Median   Mean   Max. 
##      6      7     40 
## 
## > hist(data.authors[, 2], main = "h-index", xlab = "", 
## +     breaks = max(data.authors[, 2], na.rm = TRUE))
```

![](README_files/figure-html/clean_hindex-1.png)<!-- -->

```
## 
## > summary(h.index$Total.Citations, na.rm = TRUE)[c(3, 
## +     4, 6)]
## Median   Mean   Max. 
##     17     25    209 
## 
## > hist(h.index$Total.Citations, main = "Total number of citations", 
## +     xlab = "", max(h.index$Total.Citations, na.rm = TRUE))
```

![](README_files/figure-html/clean_hindex-2.png)<!-- -->

```
## 
## > write.csv(h.index.clean, paste(Outputs, HindexClean, 
## +     sep = "/"), row.names = FALSE)
```

```r
#save.image("./data/interwrk/my_work_space.RData")
```

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
```

```
## 
## > source(file.path(rprojroot::find_rstudio_root_file(), 
## +     "pathconfig.R"), echo = FALSE)
## 
## > source(file.path(basepath, "global-libraries.R"), 
## +     echo = FALSE)
## 
## > source(file.path(programs, "libraries.R"), echo = FALSE)
```

```
## Skipping install of 'openalexR' from a github remote, the SHA1 (51340446) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```
## 
## > source(file.path(programs, "config.R"), echo = FALSE)
## 
## > source(file.path(programs, "_read_analysis_data.R"), 
## +     echo = TRUE)
## 
## > repllist4 <- readRDS(file = file.path(interwrk, "replication_list_clean.Rds"))
## 
## > d <- repllist4 %>% mutate(replicated_clean = replicated1_clean)
## 
## > exit <- readRDS(file = file.path(dataloc, "exitQ.Rds"))
## 
## > entry <- readRDS(file = file.path(dataloc, "entryQ.Rds"))
## 
## > bibinfo.df <- readRDS(file = file.path(interwrk, "crossref_info.Rds")) %>% 
## +     select(DOI, year, journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Applied Economics", 
## +     "AEJ:AE", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Macroeconomics", 
## +     "AEJ:Mac", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Microeconomics", 
## +     "AEJ:Mic", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Review", 
## +     "AER", bibinfo.df$journal)
## 
## > d <- d %>% filter(journal == "American Economic Journal: Applied Economics")
## 
## > exit <- exit %>% left_join(bibinfo.df, by = "DOI") %>% 
## +     filter(journal == "AEJ:AE")
## 
## > entry <- entry %>% left_join(bibinfo.df, by = "DOI") %>% 
## +     filter(journal == "AEJ:AE")
## 
## > data_assessment4 <- unique(d$DOI)
## 
## > data_assessment5 <- unique(exit$DOI)
## 
## > d_entry <- entry %>% filter(!is.na(DOI))
## 
## > uniquedoi <- length(unique(d_entry$DOI))
## 
## > coalesce_by_column <- function(df) {
## +     return(coalesce(df[1], df[2]))
## + }
## 
## > entry_merge <- d_entry %>% group_by(DOI) %>% summarise_all(coalesce_by_column) %>% 
## +     rename(difficult = `How difficult do you think replicating .... [TRUNCATED] 
## 
## > eligible <- entry_merge %>% filter(TypeOfArticle == 
## +     "Yes")
## 
## > sample_confdata <- eligible %>% filter(absence == 
## +     "Confidential Data")
## 
## > sample_nodata <- eligible %>% filter(absence == "No Data or Reason")
## 
## > eligible_DOIs <- unique(eligible$DOI[eligible$absence == 
## +     "Data was Provided"])
## 
## > exit_d <- exit %>% filter(DOI %in% eligible_DOIs)
## 
## > temp <- exit_d %>% mutate(dup = paste(DOI, Replication_Success)) %>% 
## +     distinct(dup, .keep_all = TRUE) %>% group_by(DOI) %>% mutate(id = 1:n()) .... [TRUNCATED] 
## 
## > temp_num <- length(temp$DOI)
## 
## > cat(temp_num, file = file.path(TexIncludes, "temp_num.tex"))
## 
## > dup_num <- sum(duplicated(exit_d$DOI))
## 
## > temp_num <- length(temp$DOI)
## 
## > cat(dup_num, file = file.path(TexIncludes, "dup_num.tex"))
## 
## > cat1 <- c("no readme file was provided.")
## 
## > cat2 <- c("complete. provided all information required to run the programs.")
## 
## > cat3 <- c("incomplete. was ambiguous or left out crucial steps.")
## 
## > exit_d <- exit_d %>% mutate(X18 = if_else(DOI == "10.1257/app.3.2.1", 
## +     "No Info", X18)) %>% mutate(X18 = if_else(DOI == "10.1257/app.20160089" .... [TRUNCATED] 
## 
## > exit_merge <- exit_d %>% mutate(dup = paste(DOI, Replication_Success)) %>% 
## +     distinct(dup, .keep_all = TRUE) %>% group_by(DOI) %>% mutate(lab = .... [TRUNCATED] 
## 
## > df <- d %>% mutate(replicated = ifelse(replicated1_clean %in% 
## +     c("yes") | replicated2_clean %in% c("yes"), "yes", "no"), 
## +     replicated = i .... [TRUNCATED] 
## 
## > df <- df %>% mutate(dup = paste(DOI, replicated)) %>% 
## +     distinct(dup, .keep_all = TRUE)
## 
## > df <- df %>% group_by(DOI) %>% mutate(lab = paste(replicated, 
## +     collapse = " "), n = n()) %>% mutate(rep_list = ifelse(grepl("partially", 
## +    .... [TRUNCATED] 
## 
## > exit_merge <- exit_merge %>% left_join(df, by = "DOI") %>% 
## +     mutate(rep = ifelse(replicated == "NA", rep_list, replicated), 
## +         replicat .... [TRUNCATED] 
## 
## > exit_merge <- exit_merge %>% filter(replicated != 
## +     "NA")
## 
## > exit_all <- merge(x = entry_merge, y = exit_merge, 
## +     by = "DOI") %>% rename(year = year.x)
## 
## > complete_sample <- bind_rows(exit_all, sample_confdata, 
## +     sample_nodata)
## 
## > saveRDS(complete_sample, file = file.path(dataloc, 
## +     "00_complete_sample.Rds"))
## 
## > saveRDS(exit_all, file = file.path(dataloc, "00_exit_all.Rds"))
## 
## > articlesdoi <- length(unique(complete_sample$DOI))
## 
## > cat(articlesdoi, file = file.path(TexIncludes, "articlesdoi.tex"))
## 
## > articles_confdata <- length(unique(sample_confdata$DOI))
## 
## > cat(articles_confdata, file = file.path(TexIncludes, 
## +     "articles_confdata.tex"))
## 
## > articles_nodata <- length(unique(sample_nodata$DOI))
## 
## > cat(articles_nodata, file = file.path(TexIncludes, 
## +     "articles_nodata.tex"))
## 
## > articles_data <- length(unique(exit_all$DOI))
## 
## > cat(articles_data, file = file.path(TexIncludes, "articles_data.tex"))
## 
## > articles_exit <- length(unique(exit_d))
## 
## > cat(articles_exit, file = file.path(TexIncludes, "articles_exit.tex"))
```

```r
source(file.path(programs,'30_results1.R'),echo=TRUE)
```

```
## 
## > source(file.path(rprojroot::find_rstudio_root_file(), 
## +     "pathconfig.R"), echo = FALSE)
## 
## > source(file.path(basepath, "global-libraries.R"), 
## +     echo = FALSE)
## 
## > source(file.path(programs, "libraries.R"), echo = FALSE)
```

```
## Skipping install of 'openalexR' from a github remote, the SHA1 (51340446) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```
## 
## > source(file.path(programs, "config.R"), echo = FALSE)
## 
## > complete_sample <- readRDS(file = file.path(dataloc, 
## +     "00_complete_sample.Rds"))
## 
## > t_entry <- complete_sample %>% group_by(year) %>% 
## +     summarize(n = n())
## 
## > t_entry[11, 1] = c("Total")
## 
## > t_entry[11, 2] = colSums(t_entry[, 2], na.rm = TRUE)
## 
## > t_entry_t <- transpose(t_entry)
## 
## > colnames(t_entry_t) <- t_entry_t[1, ]
## 
## > t_entry_t = t_entry_t[-1, ]
## 
## > stargazer(t_entry_t, type = "latex", title = "Articles by Year", 
## +     label = "tab:EntryJournalSumOld", style = "aer", flip = FALSE, 
## +     summar .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:19 PM
## \begin{table}[!htbp] \centering 
##   \caption{Articles by Year} 
##   \label{tab:EntryJournalSumOld} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & Total \\ 
## \hline \\[-1.8ex] 
## 21 & 23 & 32 & 39 & 8 & 40 & 21 & 33 & 39 & 18 & 274 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > trial <- complete_sample[, c("year", "absence")]
## 
## > tab_absence <- complete_sample %>% group_by(absence, 
## +     year) %>% summarize(n = n()) %>% spread(year, n) %>% rename(Reason = absence)
```

```
## `summarise()` has grouped output by 'absence'.
## You can override using the `.groups` argument.
```

```
## 
## > tab_absence$Total <- rowSums(tab_absence[, 2:ncol(tab_absence)], 
## +     na.rm = T)
## 
## > tab_absence$Percent <- round(tab_absence$Total * 100/sum(tab_absence$Total), 
## +     2)
## 
## > row_sum <- colSums(tab_absence[, 2:13], na.rm = TRUE)
## 
## > tab_absence <- bind_rows(tab_absence, row_sum)
## 
## > tab_absence[4, 1] = c("Total")
## 
## > tab_absence[is.na(tab_absence)] <- 0
## 
## > stargazer(tab_absence, type = "latex", title = "Was Data Provided?", 
## +     label = "tab:absence", style = "aer", flip = FALSE, summary = FALSE, 
## +  .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:20 PM
## \begin{table}[!htbp] \centering 
##   \caption{Was Data Provided?} 
##   \label{tab:absence} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Reason & 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & Total & Percent \\ 
## \hline \\[-1.8ex] 
## Confidential Data & 5 & 10 & 8 & 10 & 1 & 15 & 2 & 11 & 11 & 7 & 80 & 29.2 \\ 
## Data was Provided & 14 & 13 & 24 & 26 & 7 & 23 & 18 & 20 & 26 & 9 & 180 & 65.69 \\ 
## No Data or Reason & 2 & 0 & 0 & 3 & 0 & 2 & 1 & 2 & 2 & 2 & 14 & 5.11 \\ 
## Total & 21 & 23 & 32 & 39 & 8 & 40 & 21 & 33 & 39 & 18 & 274 & 100 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > t_difficult <- complete_sample %>% group_by(difficult) %>% 
## +     summarize(n = n()) %>% mutate(n = as.numeric(n), difficult = ifelse(is.na(difficul .... [TRUNCATED] 
## 
## > t_difficult[is.na(t_difficult)] <- 0
## 
## > stargazer(t_difficult, type = "latex", title = "Replication Difficulty Assessment", 
## +     label = "tab:difficult", style = "aer", flip = FALSE, sum .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:20 PM
## \begin{table}[!htbp] \centering 
##   \caption{Replication Difficulty Assessment} 
##   \label{tab:difficult} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Difficulty Rating & Number of Articles & Percent \\ 
## \hline \\[-1.8ex] 
## 1 & 57 & 20.8 \\ 
## 2 & 56 & 20.44 \\ 
## 3 & 56 & 20.44 \\ 
## 4 & 35 & 12.77 \\ 
## 5 & 70 & 25.55 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > entry_merge_prog <- complete_sample %>% select(journal, 
## +     ProgramFormat, year) %>% mutate(Stata = grepl("stata", tolower(ProgramFormat)), 
## +    .... [TRUNCATED] 
## 
## > tab_prog <- entry_merge_prog %>% summarize(Stata = sum(Stata), 
## +     Matlab = sum(Matlab), R = sum(R), SAS = sum(SAS), Fortran = sum(Fortran), 
## +   .... [TRUNCATED] 
## 
## > entry_merge_data <- complete_sample %>% select(journal, 
## +     OnlineDataFormat1, ProgramFormat, year) %>% mutate(Stata = grepl("stata", 
## +     tolo .... [TRUNCATED] 
## 
## > tab_data <- entry_merge_data %>% summarize(Stata = sum(Stata), 
## +     Matlab = sum(Matlab), RDS = sum(R), Fortran = sum(Fortran), 
## +     SPSS = sum( .... [TRUNCATED] 
## 
## > tab_prog_data <- merge(tab_prog, tab_data, by = c("Software"), 
## +     all = TRUE) %>% rename(`Programming Language` = Count.x, 
## +     `Data Format`  .... [TRUNCATED] 
## 
## > stargazer(tab_prog_data, type = "latex", title = "Programming Languages and Data Formats", 
## +     label = "tab:prog", style = "aer", flip = FALSE, s .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:20 PM
## \begin{table}[!htbp] \centering 
##   \caption{Programming Languages and Data Formats} 
##   \label{tab:prog} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Software & Programming Language & Data Format \\ 
## \hline \\[-1.8ex] 
## Stata & $252$ & $174$ \\ 
## Not Reported & $14$ & $76$ \\ 
## Matlab & $11$ & $3$ \\ 
## SAS & $8$ & $0$ \\ 
## R & $6$ & $0$ \\ 
## SPSS & $2$ & $1$ \\ 
## Excel & $1$ & $7$ \\ 
## CSV & $0$ & $10$ \\ 
## RDS & $0$ & $2$ \\ 
## txt & $0$ & $2$ \\ 
## \hline \\[-1.8ex] 
## \multicolumn{3}{l}{Column sums are greater than the number of articles} \\ 
## \multicolumn{3}{l}{because articles can use more than one} \\ 
## \multicolumn{3}{l}{programming language or data format.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > exit_all <- readRDS(file = file.path(dataloc, "00_exit_all.Rds"))
## 
## > tab_doc <- exit_all %>% group_by(clarity, year) %>% 
## +     summarize(n = n()) %>% spread(year, n) %>% rename(` ` = clarity)
```

```
## `summarise()` has grouped output by 'clarity'.
## You can override using the `.groups` argument.
```

```
## 
## > tab_doc$Total <- rowSums(tab_doc[, 2:11], na.rm = T)
## 
## > tab_doc$Percent <- round(tab_doc$Total * 100/sum(tab_doc$Total), 
## +     2)
## 
## > row_sum <- colSums(tab_doc[, 2:13], na.rm = TRUE)
## 
## > tab_doc <- bind_rows(tab_doc, row_sum)
## 
## > tab_doc[4, 1] = c("Total")
## 
## > td.total <- tab_doc$Total[tab_doc$` ` == "Total"]
## 
## > td.complete <- tab_doc$Total[tab_doc$` ` == "Complete"]
## 
## > td.incomplete <- tab_doc$Total[tab_doc$` ` == "Incomplete"]
## 
## > td.noinfo <- tab_doc$Total[tab_doc$` ` == "No Info"]
## 
## > cat(td.total, file = file.path(TexIncludes, "tdtotal.tex"))
## 
## > cat(td.complete, file = file.path(TexIncludes, "tdcomplete.tex"))
## 
## > cat(td.incomplete, file = file.path(TexIncludes, "tdincomplete.tex"))
## 
## > cat(td.noinfo, file = file.path(TexIncludes, "tdnoinfo.tex"))
## 
## > td.completepct <- td.complete * 100/td.total
## 
## > td.incompletepct <- td.incomplete * 100/td.total
## 
## > cat(td.completepct, file = file.path(TexIncludes, 
## +     "tdcompletepct.tex"))
## 
## > cat(td.incompletepct, file = file.path(TexIncludes, 
## +     "tdincompletepct.tex"))
## 
## > tab_doc[is.na(tab_doc)] <- 0
## 
## > stargazer(tab_doc, type = "latex", title = "Documentation Clarity", 
## +     label = "tab:doc", style = "aer", flip = FALSE, summary = FALSE, 
## +     r .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:20 PM
## \begin{table}[!htbp] \centering 
##   \caption{Documentation Clarity} 
##   \label{tab:doc} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
##   & 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & Total & Percent \\ 
## \hline \\[-1.8ex] 
## Complete & 10 & 8 & 17 & 21 & 4 & 18 & 15 & 15 & 19 & 6 & 133 & 73.89 \\ 
## Incomplete & 4 & 5 & 6 & 5 & 3 & 5 & 3 & 5 & 7 & 2 & 45 & 25 \\ 
## No Info & 0 & 0 & 1 & 0 & 0 & 0 & 0 & 0 & 0 & 1 & 2 & 1.11 \\ 
## Total & 14 & 13 & 24 & 26 & 7 & 23 & 18 & 20 & 26 & 9 & 180 & 100 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > rm(td.total)
## 
## > rm(td.complete)
## 
## > rm(td.incomplete)
## 
## > rm(td.noinfo)
## 
## > rm(td.completepct)
## 
## > rm(td.incompletepct)
```

```r
source(file.path(programs,'31_results2.R'),echo=TRUE)
```

```
## 
## > source(file.path(rprojroot::find_rstudio_root_file(), 
## +     "pathconfig.R"), echo = FALSE)
## 
## > source(file.path(basepath, "global-libraries.R"), 
## +     echo = FALSE)
## 
## > source(file.path(programs, "libraries.R"), echo = FALSE)
```

```
## Skipping install of 'openalexR' from a github remote, the SHA1 (51340446) has not changed since last install.
##   Use `force = TRUE` to force installation
```

```
## 
## > source(file.path(programs, "config.R"), echo = FALSE)
## 
## > source(file.path(programs, "_read_analysis_data.R"), 
## +     echo = TRUE)
## 
## > repllist4 <- readRDS(file = file.path(interwrk, "replication_list_clean.Rds"))
## 
## > d <- repllist4 %>% mutate(replicated_clean = replicated1_clean)
## 
## > exit <- readRDS(file = file.path(dataloc, "exitQ.Rds"))
## 
## > entry <- readRDS(file = file.path(dataloc, "entryQ.Rds"))
## 
## > bibinfo.df <- readRDS(file = file.path(interwrk, "crossref_info.Rds")) %>% 
## +     select(DOI, year, journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Applied Economics", 
## +     "AEJ:AE", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Macroeconomics", 
## +     "AEJ:Mac", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Journal: Microeconomics", 
## +     "AEJ:Mic", bibinfo.df$journal)
## 
## > bibinfo.df$journal <- gsub("American Economic Review", 
## +     "AER", bibinfo.df$journal)
## 
## > d <- d %>% filter(journal == "American Economic Journal: Applied Economics")
## 
## > exit <- exit %>% left_join(bibinfo.df, by = "DOI") %>% 
## +     filter(journal == "AEJ:AE")
## 
## > entry <- entry %>% left_join(bibinfo.df, by = "DOI") %>% 
## +     filter(journal == "AEJ:AE")
## 
## > data_assessment4 <- unique(d$DOI)
## 
## > data_assessment5 <- unique(exit$DOI)
## 
## > exit_merge <- exit_merge %>% mutate(confdata = ifelse(grepl("confidential", 
## +     tolower(Main_Issue)) | grepl("proprietary", tolower(Main_Issue)), .... [TRUNCATED] 
## 
## > con_df <- exit_merge %>% group_by(year, confdata) %>% 
## +     summarise(n = n()) %>% spread(confdata, n) %>% select(year, 
## +     `Confidential Data`  .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > tab_results <- exit_merge %>% filter(confdata == "Other") %>% 
## +     group_by(year, replicated) %>% summarise(n = n()) %>% spread(replicated, 
## +     .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > tab_results$Total <- rowSums(tab_results[, 2:5], na.rm = T)
## 
## > row_sum <- colSums(tab_results[, 2:6], na.rm = TRUE)
## 
## > tab_results <- bind_rows(tab_results, row_sum)
## 
## > tab_results[11, 1] = c("Total")
## 
## > col_pct <- round(tab_results[11, 2:6]) * 100/as.numeric(tab_results[11, 
## +     6], 2)
## 
## > tab_results <- bind_rows(tab_results, col_pct)
## 
## > tab_results[12, 1] = c("Percent")
## 
## > conf.absent <- as.numeric(tab_absence$Total[tab_absence$Reason == 
## +     "Confidential Data"])
## 
## > missing <- as.numeric(tab_absence$Total[tab_absence$Reason == 
## +     "No Data or Reason"])
## 
## > conf.failed <- as.numeric(tab_results$`Confidential Data`[tab_results$Year == 
## +     "Total"])
## 
## > conf.total <- as.numeric(conf.absent) + as.numeric(conf.failed)
## 
## > total.assessed <- as.numeric(tab_absence[4, ncol(tab_absence) - 
## +     1])
## 
## > total.attempted <- as.numeric(tab_results$Total[tab_results$Year == 
## +     "Total"])
## 
## > total.attempted.nonconf <- total.attempted - conf.failed
## 
## > attempted.success <- as.numeric(tab_results$Successful[tab_results$Year == 
## +     "Total"])
## 
## > attempted.partial <- as.numeric(tab_results$Partial[tab_results$Year == 
## +     "Total"])
## 
## > attempted.fail <- as.numeric(tab_results$Unsuccessful[tab_results$Year == 
## +     "Total"])
## 
## > successrate <- round(attempted.success/total.attempted.nonconf * 
## +     100, 2)
## 
## > partialrate <- round(attempted.partial/total.attempted.nonconf * 
## +     100, 2)
## 
## > confrate <- round(conf.total/total.assessed * 100, 
## +     2)
## 
## > missingrate <- round(missing/total.assessed * 100, 
## +     2)
## 
## > failrate <- round(attempted.fail/total.attempted.nonconf * 
## +     100, 2)
## 
## > totalsuccess <- round(attempted.success/total.attempted * 
## +     100, 2)
## 
## > totalfail <- round(attempted.fail/total.attempted * 
## +     100, 2)
## 
## > totalpartial <- round(attempted.partial/total.attempted * 
## +     100, 2)
## 
## > total_report_nondouble <- missing + conf.total + attempted.success + 
## +     attempted.partial + attempted.fail
## 
## > totalsuccessoverall <- round(attempted.success/total_report_nondouble * 
## +     100, 2)
## 
## > totalpartialoverall <- round(attempted.partial/total_report_nondouble * 
## +     100, 2)
## 
## > totalfailoverall <- round(attempted.fail/total_report_nondouble * 
## +     100, 2)
## 
## > confassessed <- round(conf.total/total.assessed * 
## +     100, 2)
## 
## > successcat <- c(totalsuccess, successrate)
## 
## > partialcat <- c(totalpartial, partialrate)
## 
## > failcat <- c(totalfail, failrate)
## 
## > mysummary1 = cbind(missingrate, confrate)
## 
## > mysummary2 = cbind(failcat, partialcat, successcat)
## 
## > png(file = "../text/figure/summarysuccess.png")
## 
## > par(mfrow = c(1, 2))
## 
## > barcenter1 <- barplot(mysummary1, beside = T, names.arg = c("Missing", 
## +     "Confidential"), col = c("cadetblue4", "cadetblue3"), font = 1, 
## +     .... [TRUNCATED]
```

```
## Warning in plot.window(xlim, ylim, log = log,
## ...): "cwidth" is not a graphical parameter
```

```
## Warning in axis(if (horiz) 2 else 1, at = at.l,
## labels = names.arg, lty = axis.lty, : "cwidth" is
## not a graphical parameter
```

```
## Warning in title(main = main, sub = sub, xlab =
## xlab, ylab = ylab, ...): "cwidth" is not a
## graphical parameter
```

```
## Warning in axis(if (horiz) 1 else 2, cex.axis =
## cex.axis, ...): "cwidth" is not a graphical
## parameter
```

```
## 
## > mtext("Percent", side = 2, line = 2, font = 0.8)
## 
## > barcenter2 <- barplot(mysummary2, beside = T, names.arg = c("Fail", 
## +     "Partial", "Success"), col = c("#56B4E9", "aliceblue"), font = 1, 
## +      .... [TRUNCATED]
```

```
## 
## > mtext("Percent", side = 2, line = 2, font = 0.8)
## 
## > dev.off()
## png 
##   2 
## 
## > cat(conf.total, file = file.path(TexIncludes, "conf.total.tex"))
## 
## > cat(total.assessed, file = file.path(TexIncludes, 
## +     "total.assessed.tex"))
## 
## > cat(total.attempted.nonconf, file = file.path(TexIncludes, 
## +     "total.attempted.nonconf.tex"))
## 
## > cat(attempted.partial, file = file.path(TexIncludes, 
## +     "attempted.partial.tex"))
## 
## > cat(total.attempted, file = file.path(TexIncludes, 
## +     "total.attempted.tex"))
## 
## > cat(attempted.success, file = file.path(TexIncludes, 
## +     "attempted.success.tex"))
## 
## > cat(attempted.fail, file = file.path(TexIncludes, 
## +     "attempted.fail.tex"))
## 
## > cat(successrate, file = file.path(TexIncludes, "successrate.tex"))
## 
## > cat(partialrate, file = file.path(TexIncludes, "partialrate.tex"))
## 
## > cat(confrate, file = file.path(TexIncludes, "confrate.tex"))
## 
## > cat(totalsuccess, file = file.path(TexIncludes, "totalsuccess2.tex"))
## 
## > cat(missing, file = file.path(TexIncludes, "missing.tex"))
## 
## > cat(conf.failed, file = file.path(TexIncludes, "conffailed.tex"))
## 
## > cat(conf.absent, file = file.path(TexIncludes, "confabsent.tex"))
## 
## > cat(confassessed, file = file.path(TexIncludes, "confassessed.tex"))
## 
## > cat(totalsuccessoverall, file = file.path(TexIncludes, 
## +     "totalsuccessoverall.tex"))
## 
## > tab_results[is.na(tab_results)] <- 0
## 
## > tab_results[12, 3] <- round(as.numeric(tab_results[12, 
## +     3]))
## 
## > tab_results[12, 4] <- round(as.numeric(tab_results[12, 
## +     4]))
## 
## > tab_results[12, 5] <- round(as.numeric(tab_results[12, 
## +     5]))
## 
## > stargazer(tab_results, digits = 2, out = file.path(Outputs, 
## +     "table_results.tex"), type = "latex", title = "Reproduction Results", 
## +     labe .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:20 PM
## \begin{table}[!htbp] \centering 
##   \caption{Reproduction Results} 
##   \label{tab:results} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} cccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Year & Confidential Data & Unsuccessful & Successful & Partial & Total \\ 
## \hline \\[-1.8ex] 
## 2009 & 5 & 1 & 4 & 4 & 14 \\ 
## 2010 & 0 & 3 & 7 & 3 & 13 \\ 
## 2011 & 0 & 12 & 10 & 2 & 24 \\ 
## 2012 & 2 & 3 & 8 & 13 & 26 \\ 
## 2013 & 0 & 0 & 4 & 3 & 7 \\ 
## 2014 & 2 & 2 & 7 & 12 & 23 \\ 
## 2015 & 2 & 0 & 4 & 12 & 18 \\ 
## 2016 & 3 & 1 & 8 & 8 & 20 \\ 
## 2017 & 2 & 2 & 13 & 9 & 26 \\ 
## 2018 & 2 & 1 & 3 & 3 & 9 \\ 
## Total & 18 & 25 & 68 & 69 & 180 \\ 
## Percent & 10 & 14 & 38 & 38 & 100 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > cat1 <- c("no data available", "missing dataset (publically available)", 
## +     "broken link missing data", "one variable could not be found", 
## +    .... [TRUNCATED] 
## 
## > cat2 <- c("missing code", "missing code, data not provided")
## 
## > cat3 <- c("no code provided", "no programs")
## 
## > cat4 <- c("code error")
## 
## > cat5 <- c("given my ciser License, i could not run the provided mathematica code, and therefore it was impossible to replicate this article", 
## +     .... [TRUNCATED] 
## 
## > exit_merge <- exit_merge %>% mutate(reason = ifelse(tolower(Main_Issue) %in% 
## +     cat1, "Missing Data", "Other"), reason = ifelse(tolower(Main_Iss .... [TRUNCATED] 
## 
## > tab_reason <- exit_merge %>% filter(replicated %in% 
## +     c("No") & confdata == "Other") %>% group_by(year, reason) %>% 
## +     summarise(n = n()) % .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > tab_reason$Total <- rowSums(tab_reason[, 2:6], na.rm = T)
## 
## > row_sum <- colSums(tab_reason[, 2:7], na.rm = TRUE)
## 
## > tab_reason <- bind_rows(tab_reason, row_sum)
## 
## > tab_reason[9, 1] = c("Total")
## 
## > tab_reason[is.na(tab_reason)] <- 0
## 
## > faildata <- as.numeric(tab_reason$`Missing Data`[tab_reason$Year == 
## +     "Total"])
## 
## > cat(faildata, file = file.path(TexIncludes, "faildata.tex"))
## 
## > failcode <- as.numeric(tab_reason$`Code Error`[tab_reason$Year == 
## +     "Total"])
## 
## > cat(failcode, file = file.path(TexIncludes, "failcode.tex"))
## 
## > failcorrupted <- as.numeric(tab_reason$`Corrupted Data`[tab_reason$Year == 
## +     "Total"])
## 
## > cat(failcorrupted, file = file.path(TexIncludes, "failcorrupted.tex"))
## 
## > failsoftware <- as.numeric(tab_reason$`Software Unavailable`[tab_reason$Year == 
## +     "Total"])
## 
## > cat(failsoftware, file = file.path(TexIncludes, "failsoftware.tex"))
## 
## > failother <- as.numeric(tab_reason$Other[tab_reason$Year == 
## +     "Total"])
## 
## > cat(failother, file = file.path(TexIncludes, "failother.tex"))
## 
## > stargazer(tab_reason, out = file.path(Outputs, "table_reason.tex"), 
## +     type = "latex", title = "Reason for Unsuccessful Reproduction", 
## +     la .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## \begin{table}[!htbp] \centering 
##   \caption{Reason for Unsuccessful Reproduction} 
##   \label{tab:reason} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Year & Missing Data & Corrupted Data & Code Error & Software Unavailable & Other & Total \\ 
## \hline \\[-1.8ex] 
## 2009 & 0 & 1 & 0 & 0 & 0 & 1 \\ 
## 2010 & 0 & 0 & 0 & 0 & 3 & 3 \\ 
## 2011 & 0 & 0 & 0 & 0 & 12 & 12 \\ 
## 2012 & 1 & 1 & 1 & 0 & 0 & 3 \\ 
## 2014 & 1 & 0 & 0 & 1 & 0 & 2 \\ 
## 2016 & 1 & 0 & 0 & 0 & 0 & 1 \\ 
## 2017 & 1 & 0 & 0 & 0 & 1 & 2 \\ 
## 2018 & 0 & 1 & 0 & 0 & 0 & 1 \\ 
## Total & 4 & 3 & 1 & 1 & 16 & 25 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > tab_reason2 <- exit_merge %>% filter(replicated %in% 
## +     c("No", "Partial") & confdata == "Other") %>% group_by(year, 
## +     reason) %>% summaris .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > tab_reason2$Total <- rowSums(tab_reason2[, 2:6], na.rm = T)
## 
## > row_sum <- colSums(tab_reason2[, 2:6], na.rm = TRUE)
## 
## > tab_reason2 <- bind_rows(tab_reason2, row_sum)
## 
## > tab_reason2[11, 1] = c("Total")
## 
## > tab_reason2[is.na(tab_reason2)] <- 0
## 
## > stargazer(tab_reason2, out = file.path(Outputs, "table_reason2.tex"), 
## +     type = "latex", title = "Reason for Unsuccessful or Partial Reproductio ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## \begin{table}[!htbp] \centering 
##   \caption{Reason for Unsuccessful or Partial Reproduction} 
##   \label{tab:reason2} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Year & Missing Data & Corrupted Data & Code Error & Software Unavailable & Other & Total \\ 
## \hline \\[-1.8ex] 
## 2009 & 0 & 1 & 0 & 0 & 4 & 5 \\ 
## 2010 & 0 & 0 & 0 & 0 & 6 & 6 \\ 
## 2011 & 0 & 0 & 0 & 0 & 14 & 14 \\ 
## 2012 & 1 & 1 & 1 & 0 & 13 & 16 \\ 
## 2013 & 0 & 0 & 0 & 0 & 3 & 3 \\ 
## 2014 & 1 & 0 & 0 & 1 & 12 & 14 \\ 
## 2015 & 0 & 0 & 0 & 0 & 12 & 12 \\ 
## 2016 & 1 & 0 & 0 & 0 & 8 & 9 \\ 
## 2017 & 1 & 0 & 0 & 0 & 10 & 11 \\ 
## 2018 & 0 & 1 & 0 & 0 & 3 & 4 \\ 
## Total & 4 & 3 & 1 & 1 & 85 & 0 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > cat1 <- c("yes, no changes were necessary.")
## 
## > cat2 <- c("yes")
## 
## > cat3 <- c("no, the changes to the code were more involved.")
## 
## > exit_merge <- exit_merge %>% mutate(change = ifelse(tolower(Directory_Change) %in% 
## +     cat2, "Directory Change", "No Change"), change = ifelse(to .... [TRUNCATED] 
## 
## > tab_code <- exit_merge %>% filter(replicated %in% 
## +     c("Yes")) %>% group_by(year, change) %>% summarize(n = n()) %>% 
## +     spread(change, n) %> .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > row_sum <- colSums(tab_code[, 2:5], na.rm = TRUE)
## 
## > tab_code <- bind_rows(tab_code, row_sum)
## 
## > tab_code[nrow(tab_code), 1] = c("Total")
## 
## > col_pct <- round(tab_code[nrow(tab_code), 2:5]) * 
## +     100/as.numeric(tab_code[nrow(tab_code), 5], 2)
## 
## > tab_code <- bind_rows(tab_code, col_pct)
## 
## > tab_code[nrow(tab_code), 1] = c("Percent")
## 
## > tab_code_partial <- exit_merge %>% filter(replicated %in% 
## +     c("Partial")) %>% group_by(year, change) %>% summarize(n = n()) %>% 
## +     spread(c .... [TRUNCATED]
```

```
## `summarise()` has grouped output by 'year'. You
## can override using the `.groups` argument.
```

```
## 
## > row_sum <- colSums(tab_code_partial[, 2:5], na.rm = TRUE)
## 
## > tab_code_partial <- bind_rows(tab_code_partial, row_sum)
## 
## > tab_code_partial[nrow(tab_code_partial), 1] = c("Total")
## 
## > col_pct <- round(tab_code_partial[nrow(tab_code_partial), 
## +     2:5]) * 100/as.numeric(tab_code_partial[nrow(tab_code_partial), 
## +     5], 2)
## 
## > tab_code_partial <- bind_rows(tab_code_partial, col_pct)
## 
## > tab_code_partial[nrow(tab_code_partial), 1] = c("Percent")
## 
## > nochange <- as.numeric(tab_code$`No Change`[tab_code$Year == 
## +     "Total"])
## 
## > directorychange <- as.numeric(tab_code$`Directory Change`[tab_code$Year == 
## +     "Total"])
## 
## > change <- nochange + directorychange
## 
## > cat(change, file = file.path(TexIncludes, "change.tex"))
## 
## > complex <- as.numeric(tab_code$`Complex Change`[tab_code$Year == 
## +     "Total"])
## 
## > cat(complex, file = file.path(TexIncludes, "complex.tex"))
## 
## > ratiocomplex <- round(complex/attempted.success, 2)
## 
## > cat(ratiocomplex, file = file.path(TexIncludes, "ratiocomplex.tex"))
## 
## > tab_code[is.na(tab_code)] <- 0
## 
## > stargazer(tab_code, out = file.path(Outputs, "table_code.tex"), 
## +     type = "latex", title = "Manipulation of Code Required for Successful Reprodu ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## \begin{table}[!htbp] \centering 
##   \caption{Manipulation of Code Required for Successful Reproductions} 
##   \label{tab:code} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Year & Complex Change & Directory Change & No Change & Total \\ 
## \hline \\[-1.8ex] 
## 2009 & 1 & 1 & 2 & 4 \\ 
## 2010 & 1 & 3 & 3 & 7 \\ 
## 2011 & 4 & 2 & 4 & 10 \\ 
## 2012 & 2 & 3 & 3 & 8 \\ 
## 2013 & 0 & 1 & 3 & 4 \\ 
## 2014 & 2 & 1 & 4 & 7 \\ 
## 2015 & 0 & 3 & 1 & 4 \\ 
## 2016 & 3 & 3 & 2 & 8 \\ 
## 2017 & 9 & 2 & 2 & 13 \\ 
## 2018 & 2 & 0 & 1 & 3 \\ 
## Total & 24 & 19 & 25 & 68 \\ 
## Percent & 35.2941176470588 & 27.9411764705882 & 36.7647058823529 & 100 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > main <- exit_merge %>% mutate(doc_qual = ifelse(clarity == 
## +     "Complete", 1, 0), `Fully reproduced` = ifelse(replicated == 
## +     "Yes", 1, 0))
## 
## > ols.docqual <- lm(`Fully reproduced` ~ doc_qual, main)
## 
## > resmatrix <- cor.test(main$`Fully reproduced`, main$doc_qual, 
## +     method = "pearson")
## 
## > resmatrix2 <- cor.test(main$`Fully reproduced`, main$doc_qual, 
## +     method = "kendall")
## 
## > resmatrix3 <- cbind(resmatrix$estimate, resmatrix$p.value)
## 
## > resmatrix4 <- cbind(resmatrix2$estimate, resmatrix2$p.value)
## 
## > resmatrix3 <- rbind(resmatrix3, resmatrix4)
## 
## > rownames(resmatrix3) <- c("Pearson", "Kendall")
## 
## > colnames(resmatrix3) <- c("Estimate", "p-value")
## 
## > stargazer(resmatrix3, out = file.path(Outputs, "table_reg1.tex"), 
## +     column.labels = c("test", "estimate", "p-value"), title = "Correlation of R ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{Correlation of Reproduction Success vs Documentation Quality} 
##   \label{reg1} 
## \begin{tabular}{@{\extracolsep{0.4pt}} D{.}{.}{-5} D{.}{.}{-5} D{.}{.}{-5} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \multicolumn{1}{c}{} & \multicolumn{1}{c}{Estimate} & \multicolumn{1}{c}{p-value} \\ 
## \hline \\[-1.8ex] 
## \multicolumn{1}{c}{Pearson} & 0.30000 & 0.00060 \\ 
## \multicolumn{1}{c}{Kendall} & 0.30000 & 0.00070 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > hindex <- read.csv(paste(Outputs, HindexClean, sep = "/"), 
## +     header = TRUE, stringsAsFactors = F)
## 
## > hindex$article <- sub(pattern = "10.1257/app.", replacement = "", 
## +     hindex$DOI)
## 
## > hindex$pubyear <- sapply(strsplit(hindex$article, 
## +     "[.]"), function(x) x[1])
## 
## > hindex$pubyear <- as.numeric(hindex$pubyear) + 2008
## 
## > hindex$Average.per.Year <- as.numeric(as.character(sub(",", 
## +     ".", hindex$Average.per.Year, fixed = TRUE)))
## 
## > hindex$Average.per.Year <- as.numeric(hindex$Average.per.Year)
## 
## > hindex$Average.per.Year[hindex$pubyear == 2009] <- hindex$Average.per.Year[hindex$pubyear == 
## +     2009] * 8/7
## 
## > hindex$Average.per.Year[hindex$pubyear == 2011] <- hindex$Average.per.Year[hindex$pubyear == 
## +     2011] * 6/5
## 
## > hindex$Average.per.Year[hindex$pubyear == 2012] <- hindex$Average.per.Year[hindex$pubyear == 
## +     2012] * 5/4
## 
## > hindex$Average.per.Year[hindex$pubyear == 2013] <- hindex$Average.per.Year[hindex$pubyear == 
## +     2013] * 4/3
## 
## > hindex <- hindex %>% group_by(DOI) %>% summarise(cite = mean(Total.Citations), 
## +     avgcite = mean(Average.per.Year), avghindex = mean(h.index, 
## + .... [TRUNCATED] 
## 
## > hindex$tophindex[!is.finite(hindex$tophindex)] <- NA
## 
## > hindex$lowhindex[!is.finite(hindex$lowhindex)] <- NA
## 
## > main <- exit_merge %>% left_join(hindex, by = "DOI")
## 
## > author <- main %>% filter(!is.na(avghindex)) %>% group_by(replicated) %>% 
## +     summarise(`Number of Articles` = n(), `Avg h-index` = mean(avghinde .... [TRUNCATED] 
## 
## > nauthor <- main %>% filter(!is.na(avghindex)) %>% 
## +     group_by(replicated) %>% summarise(`Number of Articles` = n(), 
## +     `Avg h-index` = mean( .... [TRUNCATED] 
## 
## > cat1 <- c("no readme file was provided.")
## 
## > cat2 <- c("complete. provided all information required to run the programs.")
## 
## > cat3 <- c("incomplete. was ambiguous or left out crucial steps.")
## 
## > main2 <- main %>% mutate(clarity = ifelse(tolower(X18) %in% 
## +     cat1, "No ReadMe Provided", "No Info"), clarity = ifelse(tolower(X18) %in% 
## +     .... [TRUNCATED] 
## 
## > author2 <- main2 %>% filter((replicated != "NA")) %>% 
## +     group_by(clarity) %>% summarise(`Number of Articles` = n(), 
## +     `Number of Authors`  .... [TRUNCATED] 
## 
## > stargazer(author2, out = file.path(Outputs, "table_authormetrics.tex"), 
## +     type = "latex", title = "Clarity and Author Metrics", label = "tab:au ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## \begin{table}[!htbp] \centering 
##   \caption{Clarity and Author Metrics} 
##   \label{tab:authormetrics} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## clarity & Number of Articles & Number of Authors \\ 
## \hline \\[-1.8ex] 
## Complete & 133 & 2.4 \\ 
## Incomplete & 45 & 2.3 \\ 
## No Info & 2 & 2 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > stargazer(author, out = file.path(Outputs, "table_metrics.tex"), 
## +     type = "latex", title = "Publication and Author Metrics", 
## +     label = "ta ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## \begin{table}[!htbp] \centering 
##   \caption{Publication and Author Metrics} 
##   \label{tab:metrics} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} cccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## Outcome & Number of Articles & Avg h-index & Lowest h-index & Number of Authors & Avg Annual Citations \\ 
## \hline \\[-1.8ex] 
## Unsuccessful & 32 & 6.8 & 4.5 & 2.1 & 4.6 \\ 
## Partial & 53 & 7.1 & 4.2 & 2.4 & 3.5 \\ 
## Successful & 45 & 7.8 & 4.5 & 2.6 & 5.3 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > main <- entry_merge %>% select(DOI, absence) %>% left_join(exit_merge) %>% 
## +     left_join(hindex, by = "DOI") %>% mutate(confidential_data = absen .... [TRUNCATED]
```

```
## Joining with `by = join_by(DOI)`
```

```
## 
## > main <- main %>% mutate(`Fully reproduced` = replicated, 
## +     `Fully reproduced` = ifelse(replicated %in% c("No", "Partial"), 
## +         0, `Fully .... [TRUNCATED] 
## 
## > ols.cite.avgh <- lm(cite ~ avghindex * confidential_data, 
## +     main)
## 
## > ols.cite.toph <- lm(cite ~ tophindex * confidential_data, 
## +     main)
## 
## > ols.cite.lowh <- lm(cite ~ lowhindex * confidential_data, 
## +     main)
## 
## > stargazer(ols.cite.avgh, ols.cite.toph, ols.cite.lowh, 
## +     title = "OLS: Citations vs Confidential Data", align = TRUE, 
## +     no.space = FALSE,  .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:21 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Citations vs Confidential Data} 
##   \label{reg2} 
## \begin{tabular}{@{\extracolsep{-15pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 3.000^{***} &  &  \\ 
##   & (0.500) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 1.000^{***} &  \\ 
##   &  & (0.200) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 2.000^{**} \\ 
##   &  &  & (0.800) \\ 
##   & & & \\ 
##  confidential\_data & 18.000^{***} & 14.000^{**} & 6.000 \\ 
##   & (7.000) & (6.000) & (7.000) \\ 
##   & & & \\ 
##  avghindex:confidential\_data & -2.000^{**} &  &  \\ 
##   & (0.800) &  &  \\ 
##   & & & \\ 
##  tophindex:confidential\_data &  & -0.900^{**} &  \\ 
##   &  & (0.500) &  \\ 
##   & & & \\ 
##  lowhindex:confidential\_data &  &  & -0.900 \\ 
##   &  &  & (1.000) \\ 
##   & & & \\ 
##  Constant & 5.000 & 11.000^{***} & 17.000^{***} \\ 
##   & (4.000) & (3.000) & (4.000) \\ 
##   & & & \\ 
## \textit{N} & \multicolumn{1}{c}{180} & \multicolumn{1}{c}{180} & \multicolumn{1}{c}{180} \\ 
## \hline 
## \hline \\[-1.8ex] 
## \multicolumn{4}{l}{Notes: *** Significant at the 1 percent level, ** Significant at the 5 percent level} \\ 
## \multicolumn{4}{l}{* Significant at the 10 percent level.} \\ 
## \multicolumn{4}{l}{Avghindex is the average hindex about the article's authors,  tophindex the maximum} \\ 
## \multicolumn{4}{l}{ hindex among them and lowhindex the lowest. Avghindex:confidential stands for} \\ 
## \multicolumn{4}{l}{the interaction between a dummy taking value 1 if the article uses confidential  } \\ 
## \multicolumn{4}{l}{data, and the aforementioned average hindex of authors.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > ols.cite.avgh <- lm(cite ~ avghindex, main)
## 
## > ols.cite.toph <- lm(cite ~ tophindex, main)
## 
## > ols.cite.lowh <- lm(cite ~ lowhindex, main)
## 
## > print(mean(main$cite, na.rm = T))
## [1] 26
## 
## > print(mean(main$avghindex, na.rm = T))
## [1] 7
## 
## > print(mean(main$tophindex, na.rm = T))
## [1] 11
## 
## > print(mean(main$lowhindex, na.rm = T))
## [1] 4
## 
## > stargazer(ols.cite.avgh, ols.cite.toph, ols.cite.lowh, 
## +     out = file.path(Outputs, "table_reg2test.tex"), title = "OLS: Citations vs Confidentia ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:22 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Citations vs Confidential Data Testing} 
##   \label{reg2test} 
## \begin{tabular}{@{\extracolsep{-15pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 2.000^{***} &  &  \\ 
##   & (0.400) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 1.000^{***} &  \\ 
##   &  & (0.200) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 1.000^{**} \\ 
##   &  &  & (0.600) \\ 
##   & & & \\ 
##  Constant & 11.000^{***} & 15.000^{***} & 20.000^{***} \\ 
##   & (3.000) & (3.000) & (3.000) \\ 
##   & & & \\ 
## \textit{N} & \multicolumn{1}{c}{210} & \multicolumn{1}{c}{210} & \multicolumn{1}{c}{210} \\ 
## \hline 
## \hline \\[-1.8ex] 
## \textit{Notes:} & \multicolumn{3}{l}{$^{***}$Significant at the 1 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{**}$Significant at the 5 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{*}$Significant at the 10 percent level.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > main <- exit_merge %>% filter(confdata == "Other") %>% 
## +     mutate(`Fully reproduced` = ifelse(replicated %in% c("No", 
## +         "Partial"), 0, 1 .... [TRUNCATED] 
## 
## > ols.cite.avgh <- lm(cite ~ avghindex * `Fully reproduced`, 
## +     main)
## 
## > ols.cite.toph <- lm(cite ~ tophindex * `Fully reproduced`, 
## +     main)
## 
## > ols.cite.lowh <- lm(cite ~ lowhindex * `Fully reproduced`, 
## +     main)
## 
## > ols.cite.avghp <- lm(cite ~ avghindex * `Full or Partial`, 
## +     main)
## 
## > ols.cite.tophp <- lm(cite ~ tophindex * `Full or Partial`, 
## +     main)
## 
## > ols.cite.lowhp <- lm(cite ~ lowhindex * `Full or Partial`, 
## +     main)
## 
## > stargazer(ols.cite.avgh, ols.cite.toph, ols.cite.lowh, 
## +     title = "OLS: Citations vs Reproduction Success (A)", align = TRUE, 
## +     no.space =  .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:22 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Citations vs Reproduction Success (A)} 
##   \label{reg3} 
## \begin{tabular}{@{\extracolsep{-20pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 3.000^{***} &  &  \\ 
##   & (0.600) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 1.000^{***} &  \\ 
##   &  & (0.300) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 2.000^{*} \\ 
##   &  &  & (1.000) \\ 
##   & & & \\ 
##  `Fully reproduced` & 2.000 & 1.000 & 9.000 \\ 
##   & (8.000) & (7.000) & (8.000) \\ 
##   & & & \\ 
##  avghindex:`Fully reproduced` & 0.600 &  &  \\ 
##   & (0.900) &  &  \\ 
##   & & & \\ 
##  tophindex:`Fully reproduced` &  & 0.500 &  \\ 
##   &  & (0.500) &  \\ 
##   & & & \\ 
##  lowhindex:`Fully reproduced` &  &  & -0.200 \\ 
##   &  &  & (1.000) \\ 
##   & & & \\ 
##  Constant & 4.000 & 10.000^{***} & 14.000^{***} \\ 
##   & (5.000) & (4.000) & (5.000) \\ 
##   & & & \\ 
## Observations & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} \\ 
## \hline \\[-1.8ex] 
## \textit{Notes:} & \multicolumn{3}{l}{$^{***}$Significant at the 1 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{**}$Significant at the 5 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{*}$Significant at the 10 percent level.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > stargazer(ols.cite.avghp, ols.cite.tophp, ols.cite.lowhp, 
## +     title = "OLS: Citations vs Reproduction Success (B)", align = TRUE, 
## +     no.space .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:22 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Citations vs Reproduction Success (B)} 
##   \label{reg3} 
## \begin{tabular}{@{\extracolsep{-20pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 4.000^{**} &  &  \\ 
##   & (2.000) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 1.000^{*} &  \\ 
##   &  & (0.800) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 2.000 \\ 
##   &  &  & (2.000) \\ 
##   & & & \\ 
##  `Full or Partial` & 3.000 & -2.000 & -4.000 \\ 
##   & (12.000) & (10.000) & (11.000) \\ 
##   & & & \\ 
##  avghindex:`Full or Partial` & -0.800 &  &  \\ 
##   & (2.000) &  &  \\ 
##   & & & \\ 
##  tophindex:`Full or Partial` &  & -0.100 &  \\ 
##   &  & (0.800) &  \\ 
##   & & & \\ 
##  lowhindex:`Full or Partial` &  &  & 0.200 \\ 
##   &  &  & (2.000) \\ 
##   & & & \\ 
##  Constant & 2.000 & 12.000 & 20.000^{*} \\ 
##   & (12.000) & (9.000) & (10.000) \\ 
##   & & & \\ 
## Observations & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} \\ 
## \hline \\[-1.8ex] 
## \textit{Notes:} & \multicolumn{3}{l}{$^{***}$Significant at the 1 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{**}$Significant at the 5 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{*}$Significant at the 10 percent level.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > logmain <- exit_merge %>% filter(confdata == "Other") %>% 
## +     mutate(`Fully reproduced` = ifelse(replicated %in% c("No", 
## +         "Partial"), 0 .... [TRUNCATED] 
## 
## > ols.lcite.avgh <- lm(logcite ~ avghindex * `Fully reproduced`, 
## +     logmain)
## 
## > ols.lcite.toph <- lm(logcite ~ tophindex * `Fully reproduced`, 
## +     logmain)
## 
## > ols.lcite.lowh <- lm(logcite ~ lowhindex * `Fully reproduced`, 
## +     logmain)
## 
## > ols.lcite.avghp <- lm(logcite ~ avghindex * `Full or Partial`, 
## +     logmain)
## 
## > ols.lcite.tophp <- lm(logcite ~ tophindex * `Full or Partial`, 
## +     logmain)
## 
## > ols.lcite.lowhp <- lm(logcite ~ lowhindex * `Full or Partial`, 
## +     logmain)
## 
## > stargazer(ols.lcite.avgh, ols.lcite.toph, ols.lcite.lowh, 
## +     out = file.path(Outputs, "table_logreg3a.tex"), title = "OLS: Log Citations vs Repr ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:23 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Log Citations vs Reproduction Success} 
##   \label{logreg3} 
## \begin{tabular}{@{\extracolsep{-20pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 0.100^{***} &  &  \\ 
##   & (0.020) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 0.060^{***} &  \\ 
##   &  & (0.010) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 0.100^{**} \\ 
##   &  &  & (0.050) \\ 
##   & & & \\ 
##  `Fully reproduced` & 0.600^{*} & 0.500^{*} & 0.600^{*} \\ 
##   & (0.300) & (0.300) & (0.300) \\ 
##   & & & \\ 
##  avghindex:`Fully reproduced` & -0.040 &  &  \\ 
##   & (0.040) &  &  \\ 
##   & & & \\ 
##  tophindex:`Fully reproduced` &  & -0.010 &  \\ 
##   &  & (0.020) &  \\ 
##   & & & \\ 
##  lowhindex:`Fully reproduced` &  &  & -0.040 \\ 
##   &  &  & (0.060) \\ 
##   & & & \\ 
##  Constant & 2.000^{***} & 2.000^{***} & 2.000^{***} \\ 
##   & (0.200) & (0.200) & (0.200) \\ 
##   & & & \\ 
## Observations & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} \\ 
## \hline \\[-1.8ex] 
## \textit{Notes:} & \multicolumn{3}{l}{$^{***}$Significant at the 1 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{**}$Significant at the 5 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{*}$Significant at the 10 percent level.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > stargazer(ols.lcite.avghp, ols.lcite.tophp, ols.lcite.lowhp, 
## +     out = file.path(Outputs, "table_logreg3b.tex"), title = "OLS: Log Citations vs R ..." ... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:23 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{OLS: Log Citations vs Reproduction Success} 
##   \label{logreg3b} 
## \begin{tabular}{@{\extracolsep{-20pt}}lD{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \\[-1.8ex] & \multicolumn{3}{c}{Annual Citations} \\ 
## \\[-1.8ex] & \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)}\\ 
## \hline \\[-1.8ex] 
##  avghindex & 0.200^{**} &  &  \\ 
##   & (0.070) &  &  \\ 
##   & & & \\ 
##  tophindex &  & 0.060^{*} &  \\ 
##   &  & (0.030) &  \\ 
##   & & & \\ 
##  lowhindex &  &  & 0.080 \\ 
##   &  &  & (0.090) \\ 
##   & & & \\ 
##  `Full or Partial` & 0.100 & -0.100 & -0.200 \\ 
##   & (0.500) & (0.400) & (0.500) \\ 
##   & & & \\ 
##  avghindex:`Full or Partial` & -0.040 &  &  \\ 
##   & (0.070) &  &  \\ 
##   & & & \\ 
##  tophindex:`Full or Partial` &  & -0.009 &  \\ 
##   &  & (0.040) &  \\ 
##   & & & \\ 
##  lowhindex:`Full or Partial` &  &  & 0.010 \\ 
##   &  &  & (0.100) \\ 
##   & & & \\ 
##  Constant & 2.000^{***} & 2.000^{***} & 3.000^{***} \\ 
##   & (0.500) & (0.400) & (0.400) \\ 
##   & & & \\ 
## Observations & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} & \multicolumn{1}{c}{119} \\ 
## \hline \\[-1.8ex] 
## \textit{Notes:} & \multicolumn{3}{l}{$^{***}$Significant at the 1 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{**}$Significant at the 5 percent level.} \\ 
##  & \multicolumn{3}{l}{$^{*}$Significant at the 10 percent level.} \\ 
## \end{tabular} 
## \end{table} 
## 
## > count <- c(uniquedoi, total.assessed, total.attempted, 
## +     total.attempted.nonconf)
## 
## > sample <- matrix(c("Assessed articles", "Assessed with complete records", 
## +     "Eligible articles with non confidential data, complete and unique  ..." ... [TRUNCATED] 
## 
## > sample_doc <- data.frame(sample, count)
## 
## > stargazer(sample_doc, type = "latex", title = "Summary of data", 
## +     label = "tab:appendix", out = file.path(Outputs, "table_appendix.tex"), 
## +   .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:23 PM
## % Requires LaTeX packages: dcolumn 
## \begin{table}[!htbp] \centering 
##   \caption{Summary of data} 
##   \label{tab:appendix} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} D{.}{.}{-3} D{.}{.}{-3} D{.}{.}{-3} } 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## \multicolumn{1}{c}{} & \multicolumn{1}{c}{sample} & \multicolumn{1}{c}{count} \\ 
## \hline \\[-1.8ex] 
## \multicolumn{1}{c}{1} & \multicolumn{1}{c}{Assessed articles} & 303 \\ 
## \multicolumn{1}{c}{2} & \multicolumn{1}{c}{Assessed with complete records} & 274 \\ 
## \multicolumn{1}{c}{3} & \multicolumn{1}{c}{Eligible articles with non confidential data, complete and unique records} & 180 \\ 
## \multicolumn{1}{c}{4} & \multicolumn{1}{c}{Amenable for replication, after removing confidential data articles identified during replication} & 162 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table} 
## 
## > nauthors <- round(mean(author$`Number of Authors`), 
## +     2)
## 
## > cat(nauthors, file = file.path(TexIncludes, "nauthors.tex"))
## 
## > ncite <- round(mean(author$`Avg Annual Citations`), 
## +     2)
## 
## > cat(ncite, file = file.path(TexIncludes, "ncite.tex"))
```

```r
#source(file.path(programs,'20_analytics.R'),echo=TRUE) 
source(file.path(programs,'32_conclusion.R'),echo=TRUE)
```

```
## 
## > PctDeWald1986 <- round(7/54 * 100, 2)
## 
## > PctMcCullough2006 <- round(14/186 * 100, 2)
## 
## > PctChangLi2015 <- round(22/67 * 100, 2)
```

```r
#source(file.path(programs,'33_tables.R'),echo=TRUE)   # incorporated into 20_analytics.R
#source(file.path(programs,'34_figures.R'),echo=TRUE)  # incorporated into 20_analytics.R
#source(file.path(programs,'35_appendix.R'),echo=TRUE)
source(file.path(programs,'36_list-articles.R'),echo=TRUE)
```

```
## 
## > dois <- unique(entry_merge$DOI)
## 
## > for (i in 1:length(dois)) {
## +     if (i == length(dois)) {
## +         citestring = paste0("\\textcite{", dois[i], "}. ")
## +     }
## +     else {
## +       .... [TRUNCATED] 
## \textcite{10.1257/app.1.1.1}; \textcite{10.1257/app.1.1.136}; \textcite{10.1257/app.1.1.164}; \textcite{10.1257/app.1.1.183}; \textcite{10.1257/app.1.1.219}; \textcite{10.1257/app.1.1.22}; \textcite{10.1257/app.1.1.49}; \textcite{10.1257/app.1.1.86}; \textcite{10.1257/app.1.2.1}; \textcite{10.1257/app.1.2.128}; \textcite{10.1257/app.1.2.176}; \textcite{10.1257/app.1.2.35}; \textcite{10.1257/app.1.2.53}; \textcite{10.1257/app.1.2.88}; \textcite{10.1257/app.1.3.1}; \textcite{10.1257/app.1.3.135}; \textcite{10.1257/app.1.3.33}; \textcite{10.1257/app.1.3.59}; \textcite{10.1257/app.1.3.97}; \textcite{10.1257/app.1.4.1}; \textcite{10.1257/app.1.4.109}; \textcite{10.1257/app.1.4.140}; \textcite{10.1257/app.1.4.34}; \textcite{10.1257/app.2.1.116}; \textcite{10.1257/app.2.1.149}; \textcite{10.1257/app.2.1.165}; \textcite{10.1257/app.2.1.193}; \textcite{10.1257/app.2.1.211}; \textcite{10.1257/app.2.1.33}; \textcite{10.1257/app.2.1.62}; \textcite{10.1257/app.2.1.86}; \textcite{10.1257/app.2.2.1}; \textcite{10.1257/app.2.2.118}; \textcite{10.1257/app.2.2.147}; \textcite{10.1257/app.2.2.164}; \textcite{10.1257/app.2.2.179}; \textcite{10.1257/app.2.2.241}; \textcite{10.1257/app.2.2.72}; \textcite{10.1257/app.2.2.95}; \textcite{10.1257/app.2.3.129}; \textcite{10.1257/app.2.3.190}; \textcite{10.1257/app.2.3.205}; \textcite{10.1257/app.2.3.22}; \textcite{10.1257/app.2.3.228}; \textcite{10.1257/app.2.3.256}; \textcite{10.1257/app.2.3.46}; \textcite{10.1257/app.2.3.60}; \textcite{10.1257/app.2.3.96}; \textcite{10.1257/app.2.4.150}; \textcite{10.1257/app.2.4.177}; \textcite{10.1257/app.2.4.200}; \textcite{10.1257/app.2.4.213}; \textcite{10.1257/app.2.4.236}; \textcite{10.1257/app.2.4.42}; \textcite{10.1257/app.2.4.76}; \textcite{10.1257/app.20120337}; \textcite{10.1257/app.20120359}; \textcite{10.1257/app.20130034}; \textcite{10.1257/app.20130104}; \textcite{10.1257/app.20130126}; \textcite{10.1257/app.20130169}; \textcite{10.1257/app.20130170}; \textcite{10.1257/app.20130267}; \textcite{10.1257/app.20130272}; \textcite{10.1257/app.20130327}; \textcite{10.1257/app.20130346}; \textcite{10.1257/app.20130359}; \textcite{10.1257/app.20130369}; \textcite{10.1257/app.20130399}; \textcite{10.1257/app.20130401}; \textcite{10.1257/app.20130423}; \textcite{10.1257/app.20130442}; \textcite{10.1257/app.20130472}; \textcite{10.1257/app.20130475}; \textcite{10.1257/app.20130480}; \textcite{10.1257/app.20130489}; \textcite{10.1257/app.20130505}; \textcite{10.1257/app.20130533}; \textcite{10.1257/app.20130535}; \textcite{10.1257/app.20130537}; \textcite{10.1257/app.20140010}; \textcite{10.1257/app.20140039}; \textcite{10.1257/app.20140073}; \textcite{10.1257/app.20140095}; \textcite{10.1257/app.20140100}; \textcite{10.1257/app.20140111}; \textcite{10.1257/app.20140135}; \textcite{10.1257/app.20140162}; \textcite{10.1257/app.20140185}; \textcite{10.1257/app.20140201}; \textcite{10.1257/app.20140262}; \textcite{10.1257/app.20140287}; \textcite{10.1257/app.20140323}; \textcite{10.1257/app.20140362}; \textcite{10.1257/app.20140379}; \textcite{10.1257/app.20140404}; \textcite{10.1257/app.20140405}; \textcite{10.1257/app.20140430}; \textcite{10.1257/app.20140468}; \textcite{10.1257/app.20140473}; \textcite{10.1257/app.20140494}; \textcite{10.1257/app.20140495}; \textcite{10.1257/app.20140533}; \textcite{10.1257/app.20150004}; \textcite{10.1257/app.20150015}; \textcite{10.1257/app.20150023}; \textcite{10.1257/app.20150027}; \textcite{10.1257/app.20150042}; \textcite{10.1257/app.20150043}; \textcite{10.1257/app.20150044}; \textcite{10.1257/app.20150048}; \textcite{10.1257/app.20150059}; \textcite{10.1257/app.20150076}; \textcite{10.1257/app.20150083}; \textcite{10.1257/app.20150087}; \textcite{10.1257/app.20150090}; \textcite{10.1257/app.20150112}; \textcite{10.1257/app.20150114}; \textcite{10.1257/app.20150131}; \textcite{10.1257/app.20150149}; \textcite{10.1257/app.20150158}; \textcite{10.1257/app.20150172}; \textcite{10.1257/app.20150213}; \textcite{10.1257/app.20150234}; \textcite{10.1257/app.20150241}; \textcite{10.1257/app.20150245}; \textcite{10.1257/app.20150295}; \textcite{10.1257/app.20150314}; \textcite{10.1257/app.20150342}; \textcite{10.1257/app.20150365}; \textcite{10.1257/app.20150369}; \textcite{10.1257/app.20150373}; \textcite{10.1257/app.20150385}; \textcite{10.1257/app.20150390}; \textcite{10.1257/app.20150397}; \textcite{10.1257/app.20150405}; \textcite{10.1257/app.20150411}; \textcite{10.1257/app.20150421}; \textcite{10.1257/app.20150431}; \textcite{10.1257/app.20150447}; \textcite{10.1257/app.20150476}; \textcite{10.1257/app.20150509}; \textcite{10.1257/app.20150512}; \textcite{10.1257/app.20150517}; \textcite{10.1257/app.20150530}; \textcite{10.1257/app.20150532}; \textcite{10.1257/app.20150540}; \textcite{10.1257/app.20150548}; \textcite{10.1257/app.20150554}; \textcite{10.1257/app.20150576}; \textcite{10.1257/app.20150581}; \textcite{10.1257/app.20160004}; \textcite{10.1257/app.20160008}; \textcite{10.1257/app.20160030}; \textcite{10.1257/app.20160031}; \textcite{10.1257/app.20160055}; \textcite{10.1257/app.20160079}; \textcite{10.1257/app.20160089}; \textcite{10.1257/app.20160121}; \textcite{10.1257/app.20160126}; \textcite{10.1257/app.20160127}; \textcite{10.1257/app.20160140}; \textcite{10.1257/app.20160150}; \textcite{10.1257/app.20160179}; \textcite{10.1257/app.20160180}; \textcite{10.1257/app.20160213}; \textcite{10.1257/app.20160214}; \textcite{10.1257/app.20160220}; \textcite{10.1257/app.20160247}; \textcite{10.1257/app.20160267}; \textcite{10.1257/app.20160278}; \textcite{10.1257/app.20160307}; \textcite{10.1257/app.20160404}; \textcite{10.1257/app.20160567}; \textcite{10.1257/app.20160597}; \textcite{10.1257/app.20160634}; \textcite{10.1257/app.20170048}; \textcite{10.1257/app.3.1.1}; \textcite{10.1257/app.3.1.101}; \textcite{10.1257/app.3.1.129}; \textcite{10.1257/app.3.1.152}; \textcite{10.1257/app.3.1.189}; \textcite{10.1257/app.3.1.224}; \textcite{10.1257/app.3.1.239}; \textcite{10.1257/app.3.1.35}; \textcite{10.1257/app.3.1.65}; \textcite{10.1257/app.3.1.91}; \textcite{10.1257/app.3.2.1}; \textcite{10.1257/app.3.2.119}; \textcite{10.1257/app.3.2.137}; \textcite{10.1257/app.3.2.167}; \textcite{10.1257/app.3.2.196}; \textcite{10.1257/app.3.2.34}; \textcite{10.1257/app.3.2.67}; \textcite{10.1257/app.3.2.96}; \textcite{10.1257/app.3.3.1}; \textcite{10.1257/app.3.3.124}; \textcite{10.1257/app.3.3.158}; \textcite{10.1257/app.3.3.188}; \textcite{10.1257/app.3.3.221}; \textcite{10.1257/app.3.3.244}; \textcite{10.1257/app.3.3.29}; \textcite{10.1257/app.3.3.55}; \textcite{10.1257/app.3.3.88}; \textcite{10.1257/app.3.4.1}; \textcite{10.1257/app.3.4.119}; \textcite{10.1257/app.3.4.152}; \textcite{10.1257/app.3.4.186}; \textcite{10.1257/app.3.4.215}; \textcite{10.1257/app.3.4.228}; \textcite{10.1257/app.3.4.29}; \textcite{10.1257/app.3.4.56}; \textcite{10.1257/app.3.4.86}; \textcite{10.1257/app.4.1.1}; \textcite{10.1257/app.4.1.109}; \textcite{10.1257/app.4.1.136}; \textcite{10.1257/app.4.1.164}; \textcite{10.1257/app.4.1.193}; \textcite{10.1257/app.4.1.212}; \textcite{10.1257/app.4.1.245}; \textcite{10.1257/app.4.1.30}; \textcite{10.1257/app.4.1.49}; \textcite{10.1257/app.4.1.85}; \textcite{10.1257/app.4.2.1}; \textcite{10.1257/app.4.2.134}; \textcite{10.1257/app.4.2.168}; \textcite{10.1257/app.4.2.199}; \textcite{10.1257/app.4.2.219}; \textcite{10.1257/app.4.2.247}; \textcite{10.1257/app.4.2.274}; \textcite{10.1257/app.4.2.36}; \textcite{10.1257/app.4.2.62}; \textcite{10.1257/app.4.2.98}; \textcite{10.1257/app.4.3.1}; \textcite{10.1257/app.4.3.116}; \textcite{10.1257/app.4.3.138}; \textcite{10.1257/app.4.3.167}; \textcite{10.1257/app.4.3.190}; \textcite{10.1257/app.4.3.225}; \textcite{10.1257/app.4.3.28}; \textcite{10.1257/app.4.3.43}; \textcite{10.1257/app.4.3.64}; \textcite{10.1257/app.4.3.90}; \textcite{10.1257/app.4.4.1}; \textcite{10.1257/app.4.4.121}; \textcite{10.1257/app.4.4.140}; \textcite{10.1257/app.4.4.165}; \textcite{10.1257/app.4.4.194}; \textcite{10.1257/app.4.4.226}; \textcite{10.1257/app.4.4.254}; \textcite{10.1257/app.4.4.32}; \textcite{10.1257/app.4.4.57}; \textcite{10.1257/app.4.4.94}; \textcite{10.1257/app.5.1.104}; \textcite{10.1257/app.5.1.193}; \textcite{10.1257/app.5.2.151}; \textcite{10.1257/app.5.2.86}; \textcite{10.1257/app.5.3.189}; \textcite{10.1257/app.5.3.41}; \textcite{10.1257/app.5.3.91}; \textcite{10.1257/app.5.4.111}; \textcite{10.1257/app.5.4.206}; \textcite{10.1257/app.5.4.92}; \textcite{10.1257/app.6.1.1}; \textcite{10.1257/app.6.1.108}; \textcite{10.1257/app.6.1.133}; \textcite{10.1257/app.6.1.157}; \textcite{10.1257/app.6.1.190}; \textcite{10.1257/app.6.1.220}; \textcite{10.1257/app.6.1.253}; \textcite{10.1257/app.6.1.38}; \textcite{10.1257/app.6.1.73}; \textcite{10.1257/app.6.1.91}; \textcite{10.1257/app.6.2.1}; \textcite{10.1257/app.6.2.105}; \textcite{10.1257/app.6.2.127}; \textcite{10.1257/app.6.2.152}; \textcite{10.1257/app.6.2.178}; \textcite{10.1257/app.6.2.195}; \textcite{10.1257/app.6.2.231}; \textcite{10.1257/app.6.2.32}; \textcite{10.1257/app.6.2.49}; \textcite{10.1257/app.6.2.76}; \textcite{10.1257/app.6.3.1}; \textcite{10.1257/app.6.3.103}; \textcite{10.1257/app.6.3.133}; \textcite{10.1257/app.6.3.159}; \textcite{10.1257/app.6.3.189}; \textcite{10.1257/app.6.3.20}; \textcite{10.1257/app.6.3.206}; \textcite{10.1257/app.6.3.234}; \textcite{10.1257/app.6.3.58}; \textcite{10.1257/app.6.3.76}; \textcite{10.1257/app.6.4.1}; \textcite{10.1257/app.6.4.110}; \textcite{10.1257/app.6.4.142}; \textcite{10.1257/app.6.4.175}; \textcite{10.1257/app.6.4.197}; \textcite{10.1257/app.6.4.226}; \textcite{10.1257/app.6.4.251}; \textcite{10.1257/app.6.4.35}; \textcite{10.1257/app.6.4.66}; \textcite{10.1257/app.6.4.90}.
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

```
## 
## > library(rcrossref)
## 
## > if (!file.exists(issns.file)) {
## +     issns <- data.frame(matrix(ncol = 3, nrow = 1))
## +     names(issns) <- c("journal", "issn", "lastdate")
## +     t .... [TRUNCATED] 
## 
## > issns <- readRDS(file = issns.file)
## 
## > if (!file.exists(doi.file.Rds)) {
## +     new.df <- NA
## +     for (x in 1) {
## +         new <- cr_journals(issn = issns[x, "issn"], works = TRUE, 
## +     .... [TRUNCATED] 
## 
## > aejdois <- readRDS(file = doi.file.Rds)
## 
## > nrow(aejdois)
## [1] 633
## 
## > aejdois.by.year <- aejdois %>% mutate(year = substr(published.print, 
## +     1, 4)) %>% filter(year < 2019) %>% filter(published.print < 
## +     "2018 ..." ... [TRUNCATED] 
## 
## > aejdois.by.year
## # A tibble: 10 × 2
##    year  Published
##    <chr>     <int>
##  1 2009         36
##  2 2010         41
##  3 2011         37
##  4 2012         40
##  5 2013         38
##  6 2014         40
##  7 2015         35
##  8 2016         37
##  9 2017         42
## 10 2018         20
## 
## > aejdois.total <- aejdois.by.year %>% ungroup() %>% 
## +     summarize(n = sum(Published))
## 
## > aejdois.total
## # A tibble: 1 × 1
##       n
##   <int>
## 1   366
## 
## > cat(as.numeric(aejdois.total), file = file.path(TexIncludes, 
## +     "aejdoistotal.tex"))
## 
## > complete_sample <- readRDS(file = file.path(dataloc, 
## +     "00_complete_sample.Rds"))
## 
## > t_entry <- complete_sample %>% group_by(year) %>% 
## +     summarize(Selected = n())
## 
## > t_entry <- aejdois.by.year %>% left_join(t_entry)
```

```
## Joining with `by = join_by(year)`
```

```
## 
## > t_entry[11, 1] = c("Total")
## 
## > t_entry[11, 2] = colSums(t_entry[, 2], na.rm = TRUE)
## 
## > t_entry[11, 3] = colSums(t_entry[, 3], na.rm = TRUE)
## 
## > t_entry_t <- t_entry %>% mutate(Percent = round(Selected/Published * 
## +     100, 2)) %>% pivot_longer(cols = -1) %>% pivot_wider(names_from = "year" .... [TRUNCATED] 
## 
## > t_entry
## # A tibble: 11 × 3
##    year  Published Selected
##    <chr>     <int>    <int>
##  1 2009         36       21
##  2 2010         41       23
##  3 2011         37       32
##  4 2012         40       39
##  5 2013         38        8
##  6 2014         40       40
##  7 2015         35       21
##  8 2016         37       33
##  9 2017         42       39
## 10 2018         20       18
## 11 Total       366      274
## 
## > t_entry_t <- transpose(t_entry)
## 
## > colnames(t_entry_t) <- t_entry_t[1, ]
## 
## > t_entry_t = t_entry_t[-1, ]
## 
## > stargazer(t_entry_t, type = "latex", title = "Articles Published and Selected by Year", 
## +     label = "tab:Selection", style = "aer", flip = FALSE, .... [TRUNCATED] 
## 
## % Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
## % Date and time: Wed, Oct 18, 2023 - 06:45:24 PM
## \begin{table}[!htbp] \centering 
##   \caption{Articles Published and Selected by Year} 
##   \label{tab:Selection} 
## \footnotesize 
## \begin{tabular}{@{\extracolsep{0.4pt}} ccccccccccc} 
## \\[-1.8ex]\hline 
## \hline \\[-1.8ex] 
## 2009 & 2010 & 2011 & 2012 & 2013 & 2014 & 2015 & 2016 & 2017 & 2018 & Total \\ 
## \hline \\[-1.8ex] 
## 36 & 41 & 37 & 40 & 38 & 40 & 35 & 37 & 42 & 20 & 366 \\ 
## 21 & 23 & 32 & 39 & 8 & 40 & 21 & 33 & 39 & 18 & 274 \\ 
## \hline \\[-1.8ex] 
## \end{tabular} 
## \end{table}
```


### Update citations with the latest

Referees wanted to see an updated citation analysis. While we keep the original analysis as-is, the following extracts newer per-year citation counts from openAlex. 



```r
source(file.path(programs,'41_supplementary2.R'),echo=TRUE)
```

```
## 
## > library(openalexR)
## 
## > if (file.exists(doi.file.Rds)) {
## +     filtered.df <- readRDS(file = doi.file.Rds)
## +     nrow(filtered.df)
## + } else {
## +     stop(paste0("File missin ..." ... [TRUNCATED] 
## [1] 633
## 
## > if (file.exists(openalex.Rds)) {
## +     message(paste0("File already exists: ", openalex.Rds))
## +     message("Loading file from previous version.")
## + .... [TRUNCATED]
```

```
## File already exists: /home/rstudio/data/crossref/openalex-aejae.Rds
```

```
## Loading file from previous version.
```

```
## 
## > works_from_dois <- readRDS(openalex.Rds)
## 
## > works.df <- works_from_dois %>% rename(max_cited_by_count = cited_by_count) %>% 
## +     select(DOI, doi.url, max_cited_by_count, counts_by_year) %>%  .... [TRUNCATED] 
## 
## > names(works.df)
## [1] "DOI"                "doi.url"           
## [3] "max_cited_by_count" "year"              
## [5] "cited_by_count"     "ytd_cited_by_count"
## 
## > saveRDS(works.df, citations.latest)
```


The first output file is /home/rstudio/data/crossref/openalex-aejae.Rds, with



```r
names(readRDS(openalex.Rds))
```

Key to match back to the internal database is `DOI`, a transformation of the OpenAlex-provided `doi` (renamed to `doi.url`. Note that the field `author` contains a list of author ids, as follows:


```r
readRDS(openalex.Rds) %>% select(author) %>% tidyr::unnest(author) %>% names()
```

The column `au_id` is key to matching in later OpenAlex information computed. 

The final output file (/home/rstudio/data/h_index_data/citations-per-paper.Rds) is keyed on `DOI` per article, and has both within-year citations, and cumulative year-to-date citations. These must be recomputed based on the available data, because the OpenAlex API only has 10 years worth of data, but the max citations is (?) accurate.



```r
names(readRDS(citations.latest))
```

In order to recompute the h-index, we pull ALL works by each of the authors in our set, compute year-to-date citations, and compute a as-of-year h-index.




```r
source(file.path(programs,'42_supplementary3.R'),echo=TRUE)
```

```
## 
## > library(openalexR)
## 
## > if (file.exists(doi.file.Rds)) {
## +     filtered.df <- readRDS(file = doi.file.Rds)
## +     nrow(filtered.df)
## + } else {
## +     stop(paste0("File missin ..." ... [TRUNCATED] 
## [1] 633
## 
## > get_works <- function(list, filter = "doi") {
## +     if (filter == "doi") {
## +         works <- oa_fetch(entity = "works", doi = list, verbose = FALSE .... [TRUNCATED] 
## 
## > if (file.exists(openalex.Rds)) {
## +     message(paste0("File already exists: ", openalex.Rds))
## +     message("Loading file from previous version.")
## + .... [TRUNCATED]
```

```
## File already exists: /home/rstudio/data/crossref/openalex-aejae.Rds
```

```
## Loading file from previous version.
```

```
## 
## > works_from_dois <- readRDS(file = openalex.Rds)
## 
## > nrow(works_from_dois)
## [1] 633
## 
## > authorlist.aej.df <- works_from_dois %>% select(DOI, 
## +     doi.url, author) %>% tidyr::unnest(author) %>% filter(!is.na(au_id)) %>% 
## +     distinct .... [TRUNCATED] 
## 
## > nrow(authorlist.aej.df)
## [1] 1138
## 
## > author_ids <- unique(c(authorlist.aej.df$au_id))
## 
## > if (file.exists(openalex.authors.Rds)) {
## +     message(paste0("File already exists: ", openalex.authors.Rds))
## +     message("Loading file from previ ..." ... [TRUNCATED]
```

```
## File already exists: /home/rstudio/data/crossref/openalex-aejae-authors.Rds
## Loading file from previous version.
```

```
## 
## > works_list <- readRDS(file = openalex.authors.Rds)
## 
## > authors.df.all <- works_list %>% tidyr::unnest(author) %>% 
## +     select(article_id = id, au_id, au_display_name, institution_id, 
## +         institu .... [TRUNCATED] 
## 
## > nrow(authors.df.all)
## [1] 779935
## 
## > nrow(authors.df.all %>% distinct(au_id))
## [1] 231886
## 
## > authors.df <- right_join(authors.df.all, authorlist.aej.df, 
## +     by = "au_id")
## 
## > authors.df %>% distinct(au_id) %>% nrow()
## [1] 1138
## 
## > citations.df <- authors.df %>% select(article_id, 
## +     au_id, au_display_name, publication_year, max_citations = cited_by_count, 
## +     counts_by_ .... [TRUNCATED] 
## 
## > nrow(citations.df)
## [1] 573215
## 
## > names(citations.df)
## [1] "au_id"            "au_display_name" 
## [3] "year"             "article_id"      
## [5] "publication_year" "max_citations"   
## [7] "cited_by_count"  
## 
## > hindex.by.year <- citations.df %>% arrange(au_id, 
## +     article_id, desc(year)) %>% group_by(au_id, article_id) %>% 
## +     mutate(neg_cum_citations .... [TRUNCATED] 
## 
## > nrow(hindex.by.year)
## [1] 12627
## 
## > names(hindex.by.year)
## [1] "au_id"           "au_display_name"
## [3] "year"            "hindex"         
## 
## > saveRDS(hindex.by.year, openalex.hindex)
```

The final output file (/home/rstudio/data/h_index_data/openalex-hindex.Rds) is keyed on the OpenAlex author id `au_id`

```r
names(hindex.by.year)
```

Here's an example:

```r
# test whether this seems reasonable

#citations.df %>% filter(au_id=="https://openalex.org/A5027614993") %>% kable()
hindex.by.year %>% filter(au_id=="https://openalex.org/A5027614993") %>% kable()
```



|au_id                            |au_display_name   | year| hindex|
|:--------------------------------|:-----------------|----:|------:|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2012|     51|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2013|     55|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2014|     57|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2015|     59|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2016|     61|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2017|     62|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2018|     64|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2019|     65|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2020|     66|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2021|     67|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2022|     66|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2023|     64|

```r
authors.df %>% filter(article_id=="https://openalex.org/W2577227262") %>% kable()
```



|article_id                       |au_id                            |au_display_name   |institution_id |institution_display_name |institution_country_code | cited_by_count| publication_year|counts_by_year                                                                                                                     |
|:--------------------------------|:--------------------------------|:-----------------|:--------------|:------------------------|:------------------------|--------------:|----------------:|:----------------------------------------------------------------------------------------------------------------------------------|
|https://openalex.org/W2577227262 |https://openalex.org/A5027614993 |Joshua D. Angrist |NA             |NA                       |NA                       |           5228|             2009|2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013, 2012, 423, 503, 525, 500, 470, 475, 381, 355, 384, 346, 296, 223 |

```r
citations.df %>% filter(article_id=="https://openalex.org/W2577227262") %>% kable()
```



|au_id                            |au_display_name   | year|article_id                       | publication_year| max_citations| cited_by_count|
|:--------------------------------|:-----------------|----:|:--------------------------------|----------------:|-------------:|--------------:|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2023|https://openalex.org/W2577227262 |             2009|          5228|            423|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2022|https://openalex.org/W2577227262 |             2009|          5228|            503|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2021|https://openalex.org/W2577227262 |             2009|          5228|            525|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2020|https://openalex.org/W2577227262 |             2009|          5228|            500|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2019|https://openalex.org/W2577227262 |             2009|          5228|            470|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2018|https://openalex.org/W2577227262 |             2009|          5228|            475|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2017|https://openalex.org/W2577227262 |             2009|          5228|            381|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2016|https://openalex.org/W2577227262 |             2009|          5228|            355|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2015|https://openalex.org/W2577227262 |             2009|          5228|            384|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2014|https://openalex.org/W2577227262 |             2009|          5228|            346|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2013|https://openalex.org/W2577227262 |             2009|          5228|            296|
|https://openalex.org/A5027614993 |Joshua D. Angrist | 2012|https://openalex.org/W2577227262 |             2009|          5228|            223|

