# needs rjson, tidyr, dplyr

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# we combine the generic Zenodo API with the file identifier
# 2024-02-29: This needed to be updated to the new Zenodo API.

download.file(paste0(zenodo.api,zenodo.id),destfile=file.path(dataloc,"metadata.json"))
latest <- fromJSON(file=file.path(dataloc,"metadata.json"))

print(paste0("DOI: ",latest$links$doi))
print(paste0("Current: ",latest$links$html))
print(paste0("Latest: ",latest$links$latest_html))

# we download all the Rds files
file.list <- as.data.frame(latest$files) %>% select(starts_with("self")) %>% gather()

for ( value in file.list$value ) {
	print(value)
	if ( grepl("Rds",value ) ) {
	    file.name <- basename(value %>% str_remove("/content"))
	    message(paste0("Downloading... ",file.name))
	    download.file(value,destfile=file.path(dataloc,file.name))
	}
}

# A bit of cleanup in EntryQ
# This needs a bit of manual adjustment when the DOI was miscoded in the online form
# 2                 10.1257/mac.4.2..218
# 3                         aej-policy-2
# 4  https://doi.org/10.1257/mic.6.3.227
# 5             DOI: 10.1257/mic.6.4.237
# 6             DOI: 10.1257/mic.6.4.362
# 7                 10.1257/mic.20130268
# 8             DOI: 10.1257/mic.6.1.182
# 9                         AEJPOLICY-10
# 10        doi.org/10.1257/pol.20150299
# 11           DOI: 10.1257/app.20150057
# 12                 0.1257/mic.20150240
#


entryQ <- readRDS(file.path(dataloc,"entryQ_pub.Rds")) %>%
  mutate(DOI = gsub("..",".",DOI,fixed=TRUE),
         DOI = gsub("https://doi.org/","",DOI,fixed=TRUE),
         DOI = gsub("DOI:","",DOI, fixed=TRUE),
         DOI = gsub("doi.org/","",DOI,fixed=TRUE)) %>%
  mutate(DOI = if_else(DOI=="0.1257/mic.20150240","10.1257/mic.20150240",DOI))  %>%
  mutate(DOI = if_else(DOI=="10.1257/mic.20130268","10.1257/mic.20130146",DOI))

saveRDS(entryQ,file.path(dataloc,"entryQ_pub.Rds"))
print("Modified entryQ_pub.")

