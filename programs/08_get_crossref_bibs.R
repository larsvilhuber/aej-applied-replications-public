# This program collects bibtex information for all the DOIs read by the replicators from crossref


if ( file.exists(bibtex_all) ) {
  message("Skipping processing of ",bibtex_all," as it already exists")
} else {
library(rcrossref)

# Read dataframes of the 3 datasets created in previous files
repllist2 <- readRDS(file=file.path(dataloc,"replication_list_2.Rds")) %>% select(DOI)
exit <- readRDS(file=file.path(dataloc,"exitQ_pub.Rds"))
entry <- readRDS(file=file.path(dataloc,"entryQ_pub.Rds"))
# Check with Lars, it was  exitQ, entryQ but I did not see them created before only the _pub version
# Gather DOIs
dois_toget <- unique(c(repllist2$DOI,exit$DOI,entry$DOI))

# Get citation information from Crossref (this can take a while).
tic.clear()
tic("Query to CrossRef")
bibinfo <- cr_cn(dois_toget, format = "bibtex")
toc(log=TRUE)

# Format some of the bibtex
for (i in 1:length(bibinfo)){
  if ( DEBUG ) { print(i) }
  # Get the whole bibtex string
  bibtex_str <- bibinfo[[i]]

  # Get first part
  to_remove = strsplit(bibtex_str,",")

  # Get length of first part
  len_str <- nchar(to_remove[[1]][1]) + 1

  # Get string to keep
  bibtex_end <- substring(bibtex_str,len_str)

  # Add string
  bibtex_new <- paste0("@article{",dois_toget[i],bibtex_end)

  # Print to file
  cat(bibtex_new,file=bibtex_all,sep="\n",append=TRUE)
  cat("",file=bibtex_all,sep="\n",append=TRUE)

}

}
