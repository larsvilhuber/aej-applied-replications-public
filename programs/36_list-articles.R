### R code from vignette source 'list-articles.Rnw'

###################################################
### code chunk number 1: list-articles.Rnw:3-13
###################################################
dois <- unique(entry_merge$DOI)

for (i in 1:length(dois)) {
  if (i==length(dois)) {
    citestring = paste0("\\textcite{",dois[i],"}. ")
  } else {
    citestring = paste0("\\textcite{",dois[i],"}; ")
  }
  cat(citestring)
}


