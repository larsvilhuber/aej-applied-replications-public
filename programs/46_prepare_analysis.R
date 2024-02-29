# Combine the various pieces into a single analysis file for the bibliometric analysis.
# LHS:
# - citations (from 41) citations.latest
# RHS
# - reproducibility outcomes
# - openalex.hindex (from 42) per-author-year hindex
# - authors.completed.df.rds (from 43) - institution characteristics (region, output)
# - authors.exp.df.rds (from 44) - author publishing experience
# Auxiliary:
# - file.path(interwrk,"authors.df.Rds")) - list of authors (and other stuff) that appear in the journal

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# Unpack the citations data for the authors field -> au_id
# These files come from 42-44
authors.df.rds   <- file.path(interwrk,"authors.df.Rds")
citations.df.rds <- file.path(interwrk,"citations.df.Rds")
authors.completed.df.rds <- file.path(interwrk,"authors.completed.df.Rds")
authors.exp.df.rds <- file.path(interwrk,"authors.exp.df.Rds")

# Some problematic author names might creep back in
blacklist.xlsx <- file.path(openalexloc,"blacklist.xlsx")

# Regression input: our final analysis file for the bibliometric stuff:

bibliometric.rds <- file.path(dataloc,"46_biblio_ols.Rds")

# We use new names here.

## CITATIONS
## Keys: DOI=doi.url, year
## Vars:
## - cited_by_count = within-year citations
## - ytd_cited_by_count = cumulative year-to-date citations
## NOTE: this has citations for ALL AEJ:AE records, through the time this code is run
##       Thus, it has extra records.
##       We filter here by

lhs <- readRDS(citations.latest) %>% ungroup()
# > names(lhs)
# [1] "DOI"                "doi.url"            "max_cited_by_count" "year"               "cited_by_count"
# [6] "ytd_cited_by_count"
nrow(lhs)
# [1] 3623
nrow(lhs %>% distinct(DOI))
# [1] 363
nrow(lhs %>% distinct(DOI,year))
# [1] 3623
skim(lhs)

## HINDEX
## Keys: au_id,year
## Vars: hindex (based on cumulative citations for author)

rhs.a <- readRDS(openalex.hindex) %>%
  ungroup() %>%
  select(au_id,year,hindex)
# > names(rhs.a)
# [1] "au_id"           "au_display_name" "year"            "hindex"
nrow(rhs.a)
# [1] 13012
nrow(rhs.a %>% distinct(au_id))
# [1] 1112
nrow(rhs.a %>% distinct(au_id,year))
# should be same as nrow()
skim(rhs.a)
# should have complete_rate =1 everywhere

## AUTHOR INSTITUTION INFORMATION
## Keys: article_id,au_id
## Info on institutions (note: time-invariant! First institution observed for the author!

authors.df <- readRDS(authors.df.rds)
nrow(authors.df)
nrow(authors.df %>% distinct(au_id))
# [1] 1114 - has a few more

aej_dois <- readRDS(file=openalex.Rds)
nrow(aej_dois)

blacklist <- read_excel(blacklist.xlsx) %>%
  select(au_id)
# how many
nrow(blacklist %>% distinct(au_id))

rhs.b <- readRDS(authors.completed.df.rds) %>%
  # Restrict to those in scope for the analysis
  inner_join(rhs.a %>% distinct(au_id))  %>%
  # Remove any on the blacklist - problematic names for name searches
  anti_join(blacklist) %>%
  select(au_id,
         institution_type,institution_works_count,institution_country_code,
         metaregion) %>%
  ungroup()
skim(rhs.b)
# > names(readRDS(authors.completed.df.rds))
# [1] "article_id"                 "au_id"                      "au_display_name"
# [5] "au_affiliation_raw"         "publication_year"           "is_nber"                    "institution_type"
# [9] "institution_works_count"    "institution_cited_by_count" "imputed"                    "institution_id"
# [13] "institution_display_name"   "institution_ror"            "institution_country_code"   "metaregion"
nrow(rhs.b)
# [1] 1091
nrow(rhs.b %>% distinct(au_id))
# [1] 1091

skim(rhs.b)

## AUTHOR OWN INFORMATION: Publication experience
## Keys: au_id
## Vars: earliest_pub (a year value)

rhs.c <- readRDS(authors.exp.df.rds) %>%
  select(au_id,earliest_pub)
# [1] "au_id"           "au_display_name" "earliest_pub"    "latest_pub"
nrow(rhs.c)


# We need a crosswalk between DOI and article_id

# First, expand LHS to DOI,year,au_id

step1 <-
  left_join(lhs,
          aej_dois %>% select(DOI,author,article_id=id,publication_year),
          by=c("DOI")) %>%
  tidyr::unnest(author) %>%
  select(DOI,year,au_id,article_id,publication_year,
         max_cited_by_count,cited_by_count,ytd_cited_by_count)
nrow(step1)
nrow(step1 %>% distinct(DOI,year,au_id))
nrow(step1 %>% distinct(DOI))
# [1] 363
nrow(step1 %>% distinct(au_id))
# [1] 658
skim(step1)

# Add h-index
## Keys: au_id,year

step2 <-
  left_join(step1,rhs.a,by=c("au_id","year")) %>%
  # a few hindex are missing, we set them to zero
  mutate(hindex = replace_na(hindex,0))
skim(step2)
nrow(step2 %>% distinct(au_id))
# [1] 658
nrow(step2 %>% distinct(article_id))
# [1] 363

## AUTHOR INSTITUTION INFORMATION
## Keys: article_id,au_id

step3 <-
  left_join(step2,
            rhs.b ,
            by=c("au_id"))
skim(step3)
nrow(step3 %>% distinct(au_id))
# [1] 658
nrow(step3 %>% distinct(article_id))
# [1] 363

## Author self information
## Keys: au_id

step4 <-
  left_join(step3,rhs.c,by="au_id")
skim(step4)
nrow(step4 %>% distinct(au_id))
# [1] 658
nrow(step4 %>% distinct(article_id))
# [1] 363

names(step4)

saveRDS(step4,bibliometric.rds)

## OLS REGRESSIONS: next program

