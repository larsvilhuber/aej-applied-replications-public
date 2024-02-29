# This program creates the list of files for which to capture hindex data

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# Read the compiled replication lists
hindex_request.df <- readRDS(file=file.path(interwrk,"replication_list_clean.Rds")) %>%
  select(DOI,"title","author","year","volume","number","url","journal")

# Need to add columns for each article in the desired format
#
# Need to split out authors and create entry for each of them

# Save the information as dataframes
saveRDS(hindex_request.df,file=file.path(interwrk,"hindex_request.Rds"))
write.csv(hindex_request.df,file=file.path(interwrk,"hindex_request.csv"))

