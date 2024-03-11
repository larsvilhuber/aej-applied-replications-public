# write out the numbers collected


### Load libraries 
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# Make available the latexnums dataset

latexnums <- readRDS(latexnums.Rda) %>%
  mutate(pre="\\newcommand{\\",mid="}{",end="}") %>%
  unite(latexcode,c("pre","field","mid","value","end"),sep = "")
write(latexnums$latexcode,file=file.path(tables,"latexnums.tex"))

