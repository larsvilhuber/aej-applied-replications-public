# Read analysis dataset
repllist4 <- readRDS(file=file.path(interwrk,"replication_list_clean.Rds"))
d <- repllist4 %>% mutate(replicated_clean=replicated1_clean)

# Read exit questionnaire
exit <- readRDS(file=file.path(dataloc,"exitQ_pub.Rds"))

# Read entry questionnaire
entry <- readRDS(file=file.path(dataloc,"entryQ_pub.Rds"))

# Read crossref data
bibinfo.df <- readRDS(file=file.path(crossrefloc,"crossref_info.Rds")) %>% select(DOI, year,journal)
bibinfo.df$journal <- gsub("American Economic Journal: Applied Economics","AEJ:AE",bibinfo.df$journal)
bibinfo.df$journal <- gsub("American Economic Journal: Macroeconomics","AEJ:Mac",bibinfo.df$journal)
bibinfo.df$journal <- gsub("American Economic Journal: Microeconomics","AEJ:Mic",bibinfo.df$journal)
bibinfo.df$journal <- gsub("American Economic Review","AER",bibinfo.df$journal)

# Filter dataframes by journal
d <- d %>% filter(journal == "American Economic Journal: Applied Economics")
exit <- exit  %>% left_join(bibinfo.df,by="DOI") %>% filter(journal == "AEJ:AE")
entry <- entry  %>% left_join(bibinfo.df,by="DOI") %>% filter(journal == "AEJ:AE")

# This is the number of published articles in our time frame: we will differ from those as we did not cover all.
data_assessment4<- unique(d$DOI)
data_assessment5<- unique(exit$DOI)
