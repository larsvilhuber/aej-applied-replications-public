# This program primarily handles computing academic age of author
#  = time since first publication
# Output (intermediate files)
# - authors.exp.df.rds <- file.path(interwrk,"authors.exp.df.Rds")
# Auxiliary files:
# - file.path(interwrk,"audit-exp.csv")) (output to prepare edits)
# - file.path(crossrefloc,"audit-exp.xlsx")) (input after edits)


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# Unpack the citations data for the authors field -> au_id
# These files come from 42
authors.df.rds   <- file.path(interwrk,"authors.df.Rds")
citations.df.rds <- file.path(interwrk,"citations.df.Rds")
authors.completed.df.rds <- file.path(interwrk,"authors.completed.df.Rds")
authors.exp.df.rds <- file.path(interwrk,"authors.exp.df.Rds")


if ( file.exists(authors.df.rds) ) {
  message(paste0("File already exists: ",authors.df.rds))
  message("Loading file from previous version.")
} else {
  stop(paste0("File should have been present: ",authors.df.rds))
}

## load the file from disk
authors.df <- readRDS(authors.df.rds)

if ( file.exists(citations.df.rds) ) {
  message(paste0("File already exists: ",citations.df.rds))
  message("Loading file from previous version.")
} else {
  stop(paste0("File should have been present: ",citations.df.rds))
}

## load the file from disk
citations.df <- readRDS(citations.df.rds)

# there are citations that are wrong
message(paste0(" Unique authors in citations.df: ",nrow(citations.df %>% distinct(au_id))))


# names(citations.df)
citations.df %>%
  # Kenneth J Arrow rule...
  filter(publication_year > 1950) %>%
  select(starts_with("au"),article_id,publication_year) %>%
  distinct(au_id,article_id,.keep_all = TRUE) %>%
  group_by(au_id) %>%
  mutate(earliest_pub = min(publication_year),
         latest_pub = max(publication_year)) %>%
  select(starts_with("au"),earliest_pub,latest_pub) %>%
  distinct(au_id,.keep_all = TRUE) -> author_exp.early

message(paste0(" Unique authors in author_exp.early: ",nrow(author_exp.early %>% distinct(au_id))))

# Verify plausibility

author_exp.early %>%
  mutate(exp=latest_pub-earliest_pub) %>%
  filter(exp > 50 | earliest_pub < 1950) -> audit

audit <- left_join(audit %>% select(au_id,au_display_name,earliest_pub) %>% distinct(),
                   authors.df %>% select(article_id,au_id,publication_year,doi.url),
                   by=c("au_id","earliest_pub"="publication_year"),multiple="all")


write_excel_csv(audit,file=file.path(interwrk,"audit-exp.csv"))

# Edit instructions
# - Open the CSV
# - Search for information on the named author. The spreadsheet has the early publications.
#   These MIGHT be legit (Kenneth J Arrow is legit!) but mostly not.
# - Find the year of Ph.D., or if not available, via Google Scholar, the observed earliest year of publication.
# - Save the CSV-imported file with the SAME name, but as XLSX in crossrefloc!

# Read them back in.

exp_audit <- read_excel(file.path(crossrefloc,"audit-exp.xlsx")) %>%
  select(au_id,verified_first) %>%
  filter(!is.na(verified_first)) %>%
  group_by(au_id) %>%
  mutate(verified_first=min(verified_first,na.rm=TRUE)) %>%
  distinct()

nrow(exp_audit)
# merge the audited data back on. Also apply conditions.

citations.df %>%
  # Kenneth J Arrow rule...
  filter(publication_year > 1950) %>%
  select(starts_with("au"),article_id,publication_year) %>%
  distinct(au_id,article_id,.keep_all = TRUE) %>%
  group_by(au_id) %>%
  mutate(earliest_pub = min(publication_year),
         latest_pub = max(publication_year)) %>%
  # bring in audited file
  left_join(exp_audit,by=c("au_id")) %>%
  mutate(earliest_pub = max(earliest_pub,verified_first,na.rm=TRUE)) %>%
  select(starts_with("au"),earliest_pub,latest_pub) %>%
  distinct(au_id,.keep_all = TRUE) %>%
  ungroup() -> author_exp.final

message(paste0(" Unique authors in author_exp.final: ",nrow(author_exp.final %>% distinct(au_id))))

saveRDS(author_exp.final,file=authors.exp.df.rds)
nrow(author_exp.final %>% distinct(au_id))
skim(author_exp.final)

# merge the h-index data by year of publication, do aggregations across authors
# Run the same regressions as with WoS data
