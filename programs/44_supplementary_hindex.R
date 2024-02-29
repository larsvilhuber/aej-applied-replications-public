# Get author and institution information
# Output files:
#  - openalex.hindex - per-author-year hindex
# Intermediate files:
# - file.path(interwrk,"citations.df.Rds"))
# Inputs:
# - file.path(crossrefloc,"audit-exp.xlsx")
# - file.path(interwrk,"authors.df.Rds")) (from 42)
# Requires 16GB of memory at least.

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


if ( file.exists(openalex.authors.Rds) ) {
  message(paste0("File already exists: ",openalex.authors.Rds))
  message("Loading file from previous version.")
} else {

  stop(paste0("File missing: ",openalex.authors.Rds))
}

# We now have authors data, with all citations

works_list <- readRDS(file=openalex.authors.Rds)
nrow(works_list)
names(works_list)
table(works_list$type)


# Read audits back in.

exp_audit <- read_excel(file.path(crossrefloc,"audit-exp.xlsx")) %>%
  select(au_id,verified_first) %>%
  filter(!is.na(verified_first)) %>%
  group_by(au_id) %>%
  mutate(verified_first=min(verified_first,na.rm=TRUE)) %>%
  distinct()

nrow(exp_audit)
# merge the audited data back on. Also apply conditions.

# we have a blacklist of known bad matches
blacklist.au_id <- read_excel(file.path(openalexloc,"blacklist.xlsx")) %>%
  select(au_id)
nrow(blacklist.au_id %>% distinct(au_id))

# expanded list of authors in the AEJ:AE

authors.in.sample <- readRDS(file.path(interwrk,"authors.df.Rds")) %>%
  distinct(au_id)
nrow(authors.in.sample)
# [1] 1114

# Now let's look at citations for these folks

# we start again with works_list, because we need to expand it to full years.

works_list %>%
  select(article_id=id,counts_by_year) %>%
  tidyr::unnest(counts_by_year) %>%
  select(article_id,year) %>%
  expand(article_id,year) %>%
  filter(!is.na(year)) -> expanded

min.year <- min(expanded$year)

# we now have all years, all articles

# We correct for some obvious issues:
# - incorrectly attributed articles are removed, when publication_year < 1950

nrow(works_list)
# 166051
works_list %>%
  ############ article level #################################################
  #filter(id=="https://openalex.org/W2114926243") %>%
  select(article_id=id,max_citations = cited_by_count,publication_year,
         counts_by_year,author,type) %>%
  # remove a few types we don't want to use here
  # This leaves 165060
  filter(! type %in% c("other","paratext","peer-review","reference-entry")) %>%
  tidyr::unnest(counts_by_year) %>%
  ########### article - year level, sparse ###################################
  # some articles have no citations at all
  mutate(year = replace_na(year,min.year)) %>%
  # we now have all articles, all authors, select years
  right_join(expanded,by=c("article_id","year")) %>%
  ########### article - year level, full, missing ############################
  # fill in values
  group_by(article_id) %>%
  fill(author,.direction = "updown") %>%
  fill(max_citations,.direction = "updown") %>%
  fill(publication_year,.direction = "updown") %>%
  mutate(cited_by_count = replace_na(cited_by_count,0)) %>%
  ########### article - year level, full, filled #############################
  filter(year >= publication_year) %>%
  ########### article - year level, reduced to post-publication ##############
  # now expand the authors again
  tidyr::unnest(author) -> TMP
  ########### article - year - author level  #################################
message(paste0("Author-article-year: ",nrow(TMP)))
gc()
TMP %>%
  # We only want the data for the authors in our sample.
  right_join(authors.in.sample %>% select(au_id),
             by="au_id") -> TMP
########### article - year - author, limited  #################################
message(paste0(" - limited to AEJ:AE: ",nrow(TMP)))
message(paste0("   Authors: ",nrow(TMP %>% distinct(au_id))))

gc()
TMP %>%
  # But this gets us all the data for ALL the co-authors on papers that are out of scope.
  # Kenneth J Arrow rule...
  filter(publication_year > 1950) %>%
  group_by(au_id) %>%
  mutate(earliest_pub = min(publication_year)) %>%
  ungroup() %>%
  # bring in audited file
  left_join(exp_audit,by=c("au_id")) %>%
  mutate(earliest_pub = max(earliest_pub,verified_first,na.rm=TRUE)) %>%
  filter(publication_year <= earliest_pub) -> TMP
########### article - year - author, cleaned  #################################
message(paste0(" - removing implausible records: ",nrow(TMP)))
message(paste0("   Authors: ",nrow(TMP %>% distinct(au_id))))

TMP %>%
  # remove blacklist authors
  # note that this COMPLETE removes them, because of full bad match
  anti_join(blacklist.au_id) -> TMP
message(paste0(" - after blacklist removal: ",nrow(TMP)))
message(paste0("   Authors: ",nrow(TMP %>% distinct(au_id))))

# we now have all articles, all authors, all years
TMP %>%
  select(au_id,au_display_name,year,article_id,publication_year,
         max_citations,cited_by_count,type) %>%
  ungroup() -> citations.df
rm(TMP)
gc()


nrow(citations.df)
# [1] 1343406
nrow(citations.df %>% distinct(au_id))
# [1] 1137
names(citations.df)
# [1] "au_id"            "au_display_name"
# [3] "year"             "article_id"
# [5] "publication_year" "max_citations"
# [7] "cited_by_count"   "type"

## quality control
citations.df %>% select(au_id) %>% skim() %>% filter(n_missing>0)
# A tibble: 0 × 9
citations.df %>% select(publication_year,max_citations,cited_by_count,year) %>% skim()
# ── Variable type: numeric ────────────────────────
# skim_variable    n_missing complete_rate    mean
# 1 publication_year         0             1 2009.
# 2 max_citations            0             1   35.5
# 3 cited_by_count           0             1    2.40
# 4 year                     0             1 2018.

saveRDS(citations.df,file=file.path(interwrk,"citations.df.Rds"))

# We first tried out straighforward by-year
# result:
# - cumulative does not seem to correspond to reported cited_by_count
# - cumulative h-index is different than computed off the article-level cited_by_count.
# - this is likely due to the fact that we are using the API, which is limited to 10 years.
#
# So let's try another thing
# use the reported total and compute the cumulative-by-year as the remainder going backward

message("Preparing H-index file")
citations.df %>%
  #filter(au_id=="https://openalex.org/A5027614993") %>%
  arrange(au_id,article_id,desc(year)) %>%
  group_by(au_id,article_id) %>%
  mutate(max_citations = max(max_citations,na.rm = TRUE),
         neg_cum_citations = cumsum(replace_na(cited_by_count,0)),
         ytd_citations = max_citations - neg_cum_citations + cited_by_count) %>%
  arrange(au_id,year,desc(ytd_citations)) %>%
  group_by(au_id,year) %>%
  mutate(position=row_number(),
         flag = position<=ytd_citations,
         n_pubs=n())  %>%
  ungroup() -> hindex.tmp
  # we still have 1114 here
nrow(hindex.tmp %>% distinct(au_id))
# [1] 1114
message(paste0("   Authors: ",nrow(hindex.tmp %>% distinct(au_id))))

# this drops folks with no citations = h-index =0
hindex.tmp2 <- hindex.tmp %>%
  filter(flag) %>%
  group_by(au_id,au_display_name,year) %>%
  summarize(hindex=max(position),.groups="keep") %>%
  ungroup()

message(paste0("   - after filter(flag): ",nrow(hindex.tmp2 %>% distinct(au_id))))

# lets get the others, who never have citations.
hindex.tmp3 <- hindex.tmp %>%
  filter(!flag) %>%
  distinct(au_id,au_display_name,year) %>%
  mutate(hindex=0)

message(paste0("   - never/not yet cited (no flag): ",nrow(hindex.tmp3 %>% distinct(au_id))))


hindex.by.year.tmp <-
  full_join(hindex.tmp2,hindex.tmp3,by=c("au_id","au_display_name","year")) %>%
  left_join(hindex.tmp %>% distinct(au_id,year,n_pubs)) %>%
  mutate(hindex = coalesce(hindex.x,hindex.y)) %>%
  select(-hindex.x,-hindex.y)

message(paste0("   - after joining back: ",nrow(hindex.by.year.tmp %>% distinct(au_id))))
skim(hindex.by.year.tmp)

# Removing outliers
message(paste0(".   Outliers: ",max(hindex.by.year.tmp$n_pubs)))

# we filter all individuals who ever have too many pubs, indicative of a bad match
hindex.by.year.tmp %>%
  # Richard Freeman Rule: Cumulative publication less than Freeman or Acemoglu.
  # Acemoglu is not in this sample
  filter(n_pubs > 1500) %>% distinct(au_id,au_display_name) -> outliers

print(outliers)

hindex.by.year <-
  hindex.by.year.tmp %>%
  anti_join(outliers %>% select(au_id))

# who is now max
hindex.by.year %>% filter(n_pubs == max(n_pubs)) %>% select(au_id,au_display_name,year,n_pubs)
message(paste0("   - after filtering outliers: ",nrow(hindex.by.year %>% distinct(au_id))))
skim(hindex.by.year)


nrow(hindex.by.year)
nrow(hindex.by.year %>% distinct(au_id))
# Should be: 1112
names(hindex.by.year)
# > names(hindex.by.year)
# [1] "au_id"           "au_display_name" "year"            "hindex"


saveRDS(hindex.by.year,openalex.hindex)


