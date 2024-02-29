# We clean up additional variables on repllist (which is manually coded)

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# Read the compiled replication lists
repllist2 <- readRDS(file=file.path(dataloc,"replication_list_pub.Rds"))

# Merge with bibliographic information and save new dataframe
bibinfo.df <- readRDS(file=file.path(crossrefloc,"crossref_info.Rds"))
repllist3 <- left_join(repllist2,bibinfo.df,by="DOI")
saveRDS(repllist3,file=file.path(interwrk,"replication_list_3.Rds"))

# -------------------------------------
# Manually tidy the Replicated variable, which is a bit noisy
# -------------------------------------

# Check to see the various options used
table(repllist3$Replicated1)
table(repllist3$Replicated2)

# Classify the various options
val.partial <- c("partial","partially","partly","yes(?) table 3 still unable to replicate","mostly",
                 "partial (sas files couldn't run)","y (mostly)","y (unavailable data for one table)")
val.yes <- c("yes","y")
val.no  <- c("no","n","n (code errors)")

# Recode
repllist4 <- repllist3 %>%
	mutate(replicated1_clean = ifelse(tolower(Replicated1) %in% val.partial,"partially",
	                                  ifelse(tolower(Replicated1) %in% val.yes, "yes",
	                                         ifelse(tolower(Replicated1) %in% val.no,"no",
	                                                tolower(Replicated1)
	                                                )
	                                         )
	                                  )
	       ) %>%
  mutate(replicated2_clean = ifelse(tolower(Replicated2) %in% val.partial,"partially",
                                    ifelse(tolower(Replicated2) %in% val.yes, "yes",
                                           ifelse(tolower(Replicated2) %in% val.no,"no",
                                                  tolower(Replicated2)
                                                  )
                                           )
                                    )
         )

# Save master replication list to be used for analysis
saveRDS(repllist4,file=file.path(interwrk,"replication_list_clean.Rds"))
