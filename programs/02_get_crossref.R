# This program collects bibliographic information for all the DOIs read by the replicators from crossref

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# Read dataframes of the 3 datasets created in previous files
repllist.doi <- readRDS(file=file.path(dataloc,"replication_list_pub.Rds")) %>% select(DOI)
exit.doi <- readRDS(file=file.path(dataloc,"exitQ_pub.Rds")) %>% select(DOI)
entry.doi <- readRDS(file=file.path(dataloc,"entryQ_pub.Rds")) %>% select(DOI)

# Gather DOIs
dois_toget <- unique(c(repllist.doi$DOI,exit.doi$DOI,entry.doi$DOI))

############ UNDERSTANDING DATA
assessment_data1 <- unique(repllist.doi)
assessment_data2 <- unique(exit.doi) 
assessment_data3 <- unique(entry.doi)
# we have 1231 papers in  the replication list (because it covers not only applied, we assign after), 
# during the years 2009 2018 we should have 390 papers to be replicated (published effectively)
# exit: 514, but 451 unique
# entry: 912 but 802 unique
# We have 1125 unique DOI in replication list, 
# 1136 doi_toget total: recall that replication has too many listed (not selective on published AEJ applied, binding constraint 
# should be 451 from exit)
#################
# Get citation information from Crossref (this can take a while).

if ( file.exists(paste0(crossref.file,".Rds"))) {
    message(paste0("File already exists: ",crossref.file))
    message("Loading file from previous version.")
    bibinfo.df <- readRDS(file=paste0(crossref.file,".Rds"))
} else {

tic.clear()
tic("Query to CrossRef")
bibinfo <- cr_cn(dois_toget, format = "bibentry")
toc(log=TRUE)

# Save bibliographic information object
saveRDS(bibinfo,file=file.path(interwrk,"crossref_query.Rds"))

# Convert to data frame
bibinfo.df <- lapply(bibinfo,function(x) as.data.frame(x)) %>% bind_rows() %>% distinct()
names(bibinfo.df)[1] <- "DOI"
# the url field is not well constructed, we just rebuild it from scratch
bibinfo.df$url = paste0("https://doi.org/",bibinfo.df$DOI)

# Save the information as dataframes
saveRDS(bibinfo.df,file=paste0(crossref.file,".Rds"))
write.csv(bibinfo.df,file=paste0(crossref.file,".csv"))

# Store log of time required to run
bibinfo.log <- tic.log(format=FALSE)
saveRDS(bibinfo.log,file=file.path(crossrefloc,"crossref_timing.Rds"))
}


# Prepare diagnostics
dois.df <- as.data.frame(dois_toget)
names(dois.df) <- "DOI"
crossref.diagnostics <- anti_join(as.data.frame(dois.df),bibinfo.df)
saveRDS(crossref.diagnostics,file=file.path(interwrk,"crossref.diagnostics.Rds"))
