## OLS REGRESSIONS:
#### Create variables for similar OLS regressions as the ones before
### Reproduce the regressions with web of Sciences
### Add author information such as institution and region


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# Some problematic author names might creep back in
blacklist.xlsx <- file.path(openalexloc,"blacklist.xlsx")
blacklist <- read_excel(blacklist.xlsx)
# how many
nrow(blacklist %>% distinct(au_id))
nrow(blacklist %>% distinct(DOI))

# footnote text
source(file.path(programs,"_footnotes.R"))


# =======================   Read analysis data
# exit_all = any article that reproduction was attempted
exit_all <- readRDS(file=file.path(dataloc,"00_exit_all.Rds"))
nrow(exit_all %>% distinct(DOI))
skim(exit_all)

#
entry_merge <- readRDS(file=file.path(dataloc,"00_entry_merge.Rds"))
skim(entry_merge)

#====================================================================================
# complete_sample = any article assigned and with complete initial assessment, as well as outcomes
# We do a bit of data cleaning here
complete_sample <- readRDS(file=file.path(dataloc,"00_complete_sample.Rds"))
nrow(complete_sample %>% distinct(DOI))

#============================== prepare data

# Note here:
# We remove ONE DOI from the sample, because we are unable to unambiguously identify
# the author in the bibliometric databases.
# > aejdois %>% filter(doi=="10.1257/app.6.1.1")
# container.title published.print               doi issue
# 1 American Economic Journal: Applied Economics      2014-01-01 10.1257/app.6.1.1     1
# title volume
# 1 Dynamic Implications of Subjective Expectations: Evidence from Adult Smokers      6
# author      issn
# 1 Yang, Wang, first, Department of Economics, 213 Simon Center, Lafayette College, Easton, PA, 18042 (e-mail: ) 1945-7790
#
# Our apologies to the author.


complete_sample %>%
  inner_join(blacklist %>% select(DOI)) %>%
  select(DOI,difficult,replicated)
# DOI difficult replicated
# 1 10.1257/app.6.1.1         5       <NA>


complete_sample %>%
  select(DOI, clarity, replicated,difficult,absence,confdata) %>%
  anti_join(blacklist %>% select(DOI)) %>%
  mutate(flag_main_issue_confdata = ifelse(confdata=="Confidential",TRUE,FALSE)) %>%
  mutate(flag_entry_confdata = absence,
         flag_entry_confdata = ifelse(absence %in% c("Confidential Data"),1,flag_entry_confdata),
         flag_entry_confdata = ifelse(absence %in% c("Data was Provided"),0,flag_entry_confdata),
         flag_entry_confdata = ifelse(absence %in% c("No Data or Reason"),1,flag_entry_confdata),
         flag_entry_confdata = as.numeric(flag_entry_confdata)) %>%
         #flag_entry_confdata = ifelse(flag_entry_confdata == 0 &
          #                             flag_main_issue_confdata,1,flag_entry_confdata)) %>%
  mutate(`Fully reproduced` = if_else(is.na(replicated),NA,
                                      if_else(replicated %in% c("No","Partial"),0,1)),
         `Full or Partial`  = if_else(is.na(replicated),NA,
                                      if_else(replicated %in% c("No"),0,1))
         )->
  mainOA

#  mainOA$confidential_data[grepl("No Data or Reason",mainOA$absence)] <- 0
skim(mainOA)
names(mainOA)
# > names(mainOA)
# [1] "DOI"
# [2] "clarity"
# [3] "replicated"
# [4] "difficult"
# [5] "absence"
# [6] "flag_main_issue_confdata"
# [7] "flag_entry_confdata"
# [8] "Fully reproduced"
nrow(mainOA)
# [1] 273

saveRDS(mainOA,file.path(interwrk,"47_mainOA_complete.Rds"))

#===================================================================================
# data is by au_id (author), article_id (=DOI), year (of observation of citations)
bibliometric.rds <- readRDS(file.path(dataloc,"46_biblio_ols.Rds"))
nrow(bibliometric.rds %>% distinct(DOI))
# [1] 363
nrow(bibliometric.rds %>% distinct(au_id))
# [1] 658
nrow(bibliometric.rds %>% distinct(year))
# [1] 12
#
skim(bibliometric.rds)


# subset to our sample
bibliometricdata <-
  bibliometric.rds %>%
  inner_join(complete_sample %>% distinct(DOI)) %>%
  anti_join(blacklist %>% select(DOI)) %>%
  # OpenAlex has citations before year of publication!
  mutate(years_since_publication = year - publication_year) %>%
  filter(years_since_publication >=0)

nrow(bibliometricdata %>% distinct(DOI))
# [1] 273
nrow(bibliometricdata %>% distinct(au_id))
# [1] 533
nrow(bibliometricdata %>% distinct(year))
# [1] 12
nrow(bibliometricdata %>% distinct(year,DOI))
# 2545 different frm 12*274, unbalanced panel!
table(bibliometricdata$years_since_publication,useNA = "always")
skim(bibliometricdata)

# double-check
nrow(bibliometricdata %>% distinct(au_id,article_id,year))
# [1] 6163
nrow(bibliometricdata)
# [1] 6163

saveRDS(bibliometricdata,file=file.path(interwrk,"47_bibliometricdata.Rds"))

# names
# > names(bibliometricdata)
# [1] "DOI"
# [2] "year"
# [3] "au_id"
# [4] "article_id"
# [5] "publication_year"
# [6] "max_cited_by_count"
# [7] "cited_by_count"
# [8] "ytd_cited_by_count"
# [9] "hindex"
# [10] "institution_type"
# [11] "institution_works_count"
# [12] "institution_country_code"
# [13] "metaregion"
# [14] "earliest_pub"
# [15] "years_since_publication"


# average hindex by DOI, top and low, citations by year and total citations, keep data to 4th year after publi
biblio.derived <- fastDummies::dummy_cols(bibliometricdata,  select_columns = "metaregion") %>%
  ungroup() %>% group_by(DOI,year) %>%
  summarise(publication_year = max(publication_year),
            ytd_cites = mean(ytd_cited_by_count),
            new_cites = mean(cited_by_count),
            avghindex = mean(hindex,na.rm=TRUE),
            tophindex=max(hindex,na.rm=TRUE),
            lowhindex = min(hindex,na.rm=TRUE),
            metaregion_US=max(metaregion_US,na.rm=TRUE),
            max_institution_works=max(institution_works_count,na.rm=TRUE),
            max_experience=max(year-earliest_pub,na.rm=TRUE),
            nauthors=n()) %>%
  mutate(years_since_publication = year - publication_year) %>%
  ungroup()

nrow(biblio.derived)
# [1] 2692
nrow(biblio.derived %>% distinct(DOI))
# [1] 273
nrow(biblio.derived %>% distinct(year))
# [1] 12

saveRDS(biblio.derived,file=file.path(interwrk,"47_hindex_allyears.Rds"))





# Use same exit-all to bring reproducibility results
# Define Confidential Data Variable
#exit_all2 <- exit_all %>%
#  mutate(confdata = ifelse(grepl("confidential",tolower(Main_Issue)) |
#                             grepl("proprietary",tolower(Main_Issue)),"Confidential","Other"))


##### Replication of previous tables
# First creating selections of our samples for the two main regression samples
#exit_all3 <- exit_all2 %>% select(DOI, confdata, clarity)
#exit_all4 <- exit_all2 %>% select(DOI, confdata, clarity, replicated)
#complete_sample2 <- complete_sample %>% select(DOI,difficult, replicated, absence)
#skim(complete_sample2)


### Replicate table 2 (not in paper): confidential data and citations
# mainOA <- complete_sample2 %>% select(DOI,absence, replicated) %>% left_join(exit_all3) %>%
#   left_join(biblio.derived,by="DOI") %>%
#   mutate(confidential_data = absence,
#          confidential_data = ifelse(absence %in% c("Confidential Data"),1,confidential_data),
#          confidential_data = ifelse(absence %in% c("Data was Provided"),0,confidential_data),
#          confidential_data = ifelse(absence %in% c("No Data or Reason"),0,confidential_data),
#          confidential_data = as.numeric(confidential_data),
#          confidential_data = ifelse(confidential_data == 0 & confdata == "Confidential",1,confidential_data))

# mainOA$confidential_data[grepl("No Data or Reason",mainOA$absence)] <- 0

# it's missing info about 10 total out  of 274

# Create numeric replication variable
# mainOA <- mainOA %>% mutate(`Fully reproduced` = replicated,
#                             `Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,`Fully reproduced`),
#                             `Fully reproduced` = ifelse(replicated %in% c("Yes"),1,`Fully reproduced`),
#                             `Fully reproduced` = as.numeric(`Fully reproduced`))

mainOAnoconf <- mainOA %>% filter(flag_main_issue_confdata==FALSE) %>%
  mutate(`Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,1)) %>%
  mutate(`Full or Partial`= ifelse(replicated %in% c("No"),0,1)) %>%
  mutate(doc_qual = ifelse(clarity == "Complete",1,0),
         `Fully reproduced` = ifelse(replicated == "Yes",1,0)) %>%
  filter(!is.na(replicated)) %>%
  left_join(biblio.derived %>% filter(years_since_publication==4),by="DOI")
nrow(mainOAnoconf %>% distinct(DOI))
# [1] 152

saveRDS(mainOAnoconf,file.path(interwrk,"47_mainOA_noconf.Rds"))

