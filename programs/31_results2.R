### R code from vignette source 'results2.Rnw'

###################################################
### code chunk number 1: set-parent-results
###################################################
#set_parent('Replication_aejae.Rnw')
#set_parent('results1.Rnw')
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
#source(file.path(TexBase,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

# Read analysis data
exit_all <- readRDS(file=file.path(dataloc,"00_exit_all.Rds"))
complete_sample <- readRDS(file=file.path(dataloc,"00_complete_sample.Rds"))

###################################################
### code chunk number 2: results
###################################################

# Create confidential data breakdown
con_df <- exit_all %>% group_by(year,confdata) %>% summarise(n=n()) %>%
  spread(confdata,n) %>% select(year,`Confidential Data` = Confidential)

# Create table
tab_results.raw <- exit_all %>% filter(confdata=="Other") %>% group_by(year,replicated) %>%
  summarise(n=n()) %>% spread(replicated,n) %>%
  left_join(con_df,by="year") %>% ungroup() %>%
  #select(Year=year,`Confidential Data`,Unsuccessful=No,Successful=Yes,Partial,`No Recorded Result`=`<NA>`)
  select(Year=year,Successful=Yes,Partial,`Confidential Data`,`Other failure`=No)

# Add sum rows/cols
tab_results <- tab_results.raw
tab_results$Total <- rowSums(tab_results[,2:5],na.rm = T)


row_sum <- colSums(tab_results[,2:6],na.rm=TRUE)

tab_results <- bind_rows(tab_results,row_sum)
tab_results[11,1] = c("Total")

# Add col percent
col_pct <- round(tab_results[11,2:6]*100/as.numeric(tab_results[11,6],2),1)
tab_results <- bind_rows(tab_results,col_pct)

tab_results[12,1]=c("Percent")

total.attempted <- as.numeric(tab_results$Total[tab_results$Year=="Total"])


###################################################
### code chunk number 4: tab1
###################################################

#!!!!!!!!!!!!!!!! CAUTION: We are playing around here with variable names that
#!!!!!!!!!!!!!!!!  explicitly differ only by some spaces at the end!!!!
#!!!!!!!!!!!!!!!! There probably is a better way to do this...

# Print table
tab_results %>% mutate_all(.funs = as.character) -> tab_results.char

tab_results.char[is.na(tab_results.char)] <- ""
tab_results.char[12,2] <-paste0(tab_results.char[12,2],"%")
tab_results.char[12,3] <-paste0(tab_results.char[12,3],"%")
tab_results.char[12,4] <-paste0(tab_results.char[12,4],"%")
tab_results.char[12,5] <-paste0(tab_results.char[12,5],"%")
tab_results.char[12,6] <-paste0(tab_results.char[12,6],"%")

stargazer(tab_results.char, digits=2,
          out=file.path(TexIncludes,"table_results_by_year.tex"),
          type="latex",title = "Reproduction Results",
          label="tab:results:year",
          style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          column.sep.width = cw)


### Alternate table

tab_results.summary <- tab_results %>%
  ungroup() %>%
  filter(Year == "Total") %>%
  select(-Year) %>%
  pivot_longer(cols = everything()) %>%
  mutate(Percent = round(100*value/ total.attempted,2),
         ` ` = "%") %>%
  rename(Outcome = name,`No. of Articles`= value)


stargazer(tab_results.summary, digits=2,
          out=file.path(TexIncludes,"table_results.tex"),
          type="latex",title = "Reproduction Results",
          label="tab:results",
          style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          font.size = fs,column.sep.width = cw)

### Alternate table with both attempted and data available cases

tab_results.summary   %>%
  filter(Outcome!="Total") %>%
  filter(Outcome!="Confidential Data") %>%
  mutate(`Cond. on Data` = round(100*`No. of Articles`/sum(`No. of Articles`),2)) %>%
  select(-Percent,-` `) %>%
  rename(`No. of Articles ` = `No. of Articles`) -> tmp

tmp %>% bind_rows(tmp %>% summarize(`Cond. on Data` = sum(`Cond. on Data`),
                                    `No. of Articles `=sum(`No. of Articles `)) %>%
                    mutate(Outcome = "Total")) %>%
  mutate('  ' = "%") -> tmp2

left_join(tab_results.summary,tmp2) %>%
  mutate_all(.funs = as.character) -> tmp3
tmp3[is.na(tmp3)]  <- ""

stargazer(tmp3, digits=2,
          out=file.path(TexIncludes,"table_results_both.tex"),
          type="latex",title = "Reproduction Results",
          label="tab:results",
          style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          column.sep.width = cw)

# some numbers.

tab_absence <- readRDS(file=file.path(interwrk,"00_tab_absence.Rds"))

# Calculating -
conf.absent   <- as.numeric(tab_absence$Total[tab_absence$Reason == "Confidential Data"])
missing       <- as.numeric(tab_absence$Total[tab_absence$Reason == "No Data or Reason"])
conf.failed   <- as.numeric(tab_results$`Confidential Data`[tab_results$Year=="Total"])
conf.total    <- as.numeric(conf.absent) + as.numeric(conf.failed)

total.assessed <- as.numeric(tab_absence[4,ncol(tab_absence)-1])
total.attempted.nonconf <- total.attempted - conf.failed
total.excluded <- as.numeric(nrow(complete_sample) - total.attempted.nonconf)

attempted.success <- as.numeric(tab_results$Successful[tab_results$Year=="Total"])
attempted.partial <- as.numeric(tab_results$Partial[tab_results$Year=="Total"])
# conf.failed is already computed above
attempted.other <- as.numeric(tab_results$`Other failure`[tab_results$Year=="Total"])
attempted.fail <- attempted.other + conf.failed
attemptrate    <- round(total.attempted/nrow(complete_sample) * 100,2)
successrate    <- round(attempted.success/total.attempted.nonconf * 100,2)
partialrate    <- round(attempted.partial/total.attempted.nonconf*100,2)
fullpartialrate    <- round((attempted.success + attempted.partial)/total.attempted.nonconf*100,2)

confrate       <- round(conf.total/total.assessed*100,2)
missingrate    <- round(missing/total.assessed*100,2)
failrate       <- round(attempted.fail/total.attempted.nonconf*100,2)
excluderate    <- round(total.excluded/total.assessed*100,2)

attempted.success.rate   <-round(attempted.success/total.attempted * 100,2)
assessed.success.rate   <-round(attempted.success/total.assessed * 100,2)
totalfail      <-round(attempted.fail/total.attempted * 100,2)

attempted.partial.rate   <-round(attempted.partial/total.attempted * 100,2)

total_report_nondouble <- missing+conf.total+attempted.success+attempted.partial+attempted.fail
totalsuccessoverall<-round(attempted.success/total_report_nondouble * 100,2)
totalpartialoverall<-round(attempted.partial/total_report_nondouble * 100,2)
totalfailoverall<-round(attempted.fail/total_report_nondouble * 100,2)
confassessed  <-round(conf.total/total.assessed * 100,2)

successcat    <- c(attempted.success.rate,successrate)
partialcat    <-c(attempted.partial.rate,partialrate)
failcat       <-c(totalfail,failrate)


###################################################
### code chunk number 3: figsum
###################################################
# mysummary1=cbind(missingrate,confrate)
# mysummary2=cbind(failcat,partialcat,successcat)
# png(file="../text/figure/summarysuccess.png")
# par(mfrow = c(1, 2))
#     barcenter1 <- barplot(mysummary1,beside=T,names.arg=c("Missing","Confidential"), col=c("cadetblue4", "cadetblue3"),font = 1, font.lab = 1, font.axis = 1,cex.names = 0.8,cwidth=0.4,ylim=range(pretty(c(0, mysummary1))))
#     mtext("Percent", side = 2, line = 2,font=0.8)
#     barcenter2 <- barplot(mysummary2,beside=T,names.arg=c("Fail","Partial","Success"),col=c("#56B4E9","aliceblue"),font = 1, font.lab = 1, font.axis = 1,cex.names = 0.8, legend.text = c("Over eligible articles","Over non-confidential articles"), args.legend = list(x = "topright",
#                            inset = c(-0.15, 0), cex=0.8),ylim=range(pretty(c(0, mysummary2))))
#     mtext("Percent", side = 2, line = 2,font=0.8)
# dev.off()

# mydata <- data.frame(Barplot1=missingrate, Barplot2=confrate,
#                      Barplot3=failcat, Barplot4=partialcat, Barplot5=successcat)
# barplot(as.matrix(mydata), main="Interesting", ylab="Total", beside=TRUE,
#         col=terrain.colors(5))
# legend(13, 12, c("Label1","Label2","Label3","Label4","Label5"), cex=0.6,
#        fill=terrain.colors(5))


# write tex to load in abstract and intro
cat(conf.total, 						file=file.path(TexIncludes,"conf.total.tex"))
cat(total.assessed, 				file=file.path(TexIncludes,"total.assessed.tex"))
cat(total.attempted.nonconf,file=file.path(TexIncludes,"total.attempted.nonconf.tex"))
cat(attempted.partial, 			file=file.path(TexIncludes,"attempted.partial.tex"))
cat(total.attempted, 				file=file.path(TexIncludes,"total.attempted.tex"))
cat(total.excluded,         file = file.path(TexIncludes,"total.excluded.tex"))

cat(attempted.success, 			file=file.path(TexIncludes,"attempted.success.tex"))
cat(attempted.fail, 			file=file.path(TexIncludes,"attempted.fail.tex"))
cat(attemptrate, 						file=file.path(TexIncludes,"attemptrate.tex"))
cat(successrate, 						file=file.path(TexIncludes,"successrate.tex"))
cat(partialrate, 						file=file.path(TexIncludes,"partialrate.tex"))
cat(fullpartialrate, 						file=file.path(TexIncludes,"fullpartialrate.tex"))
cat(confrate, 							file=file.path(TexIncludes,"confrate.tex"))
cat(excluderate,            file=file.path(TexIncludes,"excluderate.tex"))

cat(attempted.success.rate, 					file=file.path(TexIncludes,"attempted.success.rate.tex"))
cat(assessed.success.rate, 					file=file.path(TexIncludes,"assessed.success.rate.tex"))
cat(attempted.partial.rate, 					file=file.path(TexIncludes,"attempted.partial.rate.tex"))
cat(missing, 					file=file.path(TexIncludes,"missing.tex"))
cat(conf.failed, 					file=file.path(TexIncludes,"conffailed.tex"))
cat(conf.absent, 					file=file.path(TexIncludes,"confabsent.tex"))
cat(confassessed, 					file=file.path(TexIncludes,"confassessed.tex"))
cat(totalsuccessoverall, 					file=file.path(TexIncludes,"totalsuccessoverall.tex"))


# Create confidential data breakdown
#con_df <- exit_merge %>% group_by(year,confdata) %>% summarise(n = n()) %>%
#  spread(confdata,n) %>% select(year,`Confidential Data` = Confidential)

# Create table
tab_reason <- exit_all %>%
  filter(replicated %in% c("No") & confdata == "Other") %>%
  group_by(year,reason) %>%
  summarise(n=n()) %>% spread(reason,n) %>%
  select(Year=year,`Most Numbers Differ`,`Missing Data`,`Corrupted Data`,
         `Missing Code`,`Code Error`,`Software Unavailable`,Other)

tab_reason.summary <- tab_reason %>%
  ungroup() %>%
  select(-Year) %>%
  mutate_all(replace_na, replace = 0) %>%
  summarise_all(.funs=sum) %>%
  pivot_longer(cols = everything()) %>%
  rename(Cause = name,`No. of Articles`= value)

# Add sum row and col
tab_reason$Total <- rowSums(tab_reason[,2:ncol(tab_reason)],na.rm = T)


row_sum <- colSums(tab_reason[,2:ncol(tab_reason)],na.rm=TRUE)

tab_reason <- bind_rows(tab_reason,row_sum)
tab_reason[nrow(tab_reason),1] = c("Total")

#  Some nnumbers for the text
faildata <- as.numeric(tab_reason$`Missing Data`[tab_reason$Year == "Total"])
cat(faildata, 					file=file.path(TexIncludes,"faildata.tex"))
failcode<-as.numeric(tab_reason$`Code Error`[tab_reason$Year == "Total"])
cat(failcode, 					file=file.path(TexIncludes,"failcode.tex"))
failcorrupted<-as.numeric(tab_reason$`Corrupted Data`[tab_reason$Year == "Total"])
cat(failcorrupted, 					file=file.path(TexIncludes,"failcorrupted.tex"))
failsoftware<-as.numeric(tab_reason$`Software Unavailable`[tab_reason$Year == "Total"])
cat(failsoftware, 					file=file.path(TexIncludes,"failsoftware.tex"))
failother<-as.numeric(tab_reason$Other[tab_reason$Year == "Total"])
cat(failother, 					file=file.path(TexIncludes,"failother.tex"))
failall <- as.numeric(tab_reason$Total[tab_reason$Year == "Total"])
cat(failall, 					file=file.path(TexIncludes,"failall.tex"))


###################################################
### code chunk number 6: tab3
###################################################
# Print table

tab_reason.char <- tab_reason %>% mutate_all(.funs = as.character)
tab_reason.char[is.na(tab_reason.char)]  <- ""

stargazer(tab_reason.char,
          out=file.path(TexIncludes,"table_reason_year.tex"),
          type="latex",title = "Reason for Unsuccessful Reproduction",
          label="tab:reason:year",style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          font.size = fs,
          column.sep.width = cw,
          notes=c("Note: Articles for which a reproduction was attempted,",
                  "the reproduction was unsuccessful, with valid Exit Q4",
                  "(see Appendix)."))



stargazer(tab_reason.summary,
          out=file.path(TexIncludes,"table_reason.tex"),
          type="latex",title = "Reason for Unsuccessful Reproduction",
          label="tab:reason",style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          #font.size = fs,
          column.sep.width = cw,
          notes=c("Note: Articles for which a reproduction was attempted,",
                        "the reproduction was unsuccessful, with valid Exit Q4",
                        "(see Appendix). Question was not asked for articles",
                        "with partial reproduction success."))
###################################################
### code chunk number 7: tab4
###################################################
# Create table
tab_reason2 <- exit_all %>%
  filter(replicated %in% c("No","Partial") & confdata == "Other") %>%
  group_by(year,reason) %>%
  summarise(n=n()) %>% 
  spread(reason,n) %>%
  ungroup() %>%
  mutate_all(replace_na, replace = 0) %>%
  select(Year=year,`Missing Data`,`Corrupted Data`,`Code Error`,`Software Unavailable`,Other)


# Add sum row and col
tab_reason2$Total <- rowSums(tab_reason2[,2:6],na.rm = T)


row_sum <- colSums(tab_reason2[,2:7],na.rm=TRUE)

tab_reason2 <- bind_rows(tab_reason2,row_sum)
tab_reason2[nrow(tab_reason2),1] = c("Total")

tab_reason2[is.na(tab_reason2)] <- 0
# Print table
stargazer(tab_reason2,
          out=file.path(TexIncludes,"table_reason2.tex"),
          type="latex",title = "Reason for Unsuccessful or Partial Reproduction",
          label="tab:reason2",style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          font.size = fs,column.sep.width = cw,
          notes=c("Note: Sample of reproduced and scored papers, i.e, assessed papers for which we attempted reproduction."))


###################################################
### code chunk number 8: tab5
###################################################
#unique(main$Program_Run_Clean[main$replicated=="yes"])
#unique(main$Directory_Change[main$replicated=="yes"])

table(exit_all$Directory_Change,useNA = "always")
table(exit_all$Program_Run_Clean,useNA = "always")

table(exit_all$Directory_Change,exit_all$Program_Run_Clean,useNA = "always")

table(exit_all$change,exit_all$replicated,useNA = "always")



### New code table: simpler

table_dir <- exit_all %>%
  filter(!is.na(replicated)) %>%
  filter(replicated != "No") %>%
  filter(confdata == "Other") %>%
  mutate(replicated = if_else(replicated=="Yes","Fully",replicated),
         change = replace_na(change,"Missing"),
         change = if_else(change=="None","No Change",change))
nrow(table_dir)

table(table_dir$change,table_dir$replicated,useNA = "ifany") %>%
  xtable(caption="Code Changes for Successful Reproductions",
         label="tab:code") -> tab_code.summary

notes <- list(pos=list(0), command="")
notes$pos <- list(-1,nrow(tab_code.summary))
notes$command <- c("\\\\","\\hline\\multicolumn{3}{p{0.4\\textwidth}}{\\footnotesize{ Note: Partially or fully reproduced articles.
                   Answers to questions Exit Q8 and Q9. These questions were skipped when
                   reproduction was not successful.}}\\\\")
print(tab_code.summary,
      file=file.path(Outputs,"table_code.tex"),
      add.to.row = notes,
      caption.placement = "top",
      sanitize.rownames.function=function(x)gsub("\\."," ",x))


### Old code Create table
tab_code.raw <- exit_all %>% filter(replicated %in% c("Yes")) %>%
  group_by(year,change) %>% summarize(n=n()) %>% spread(change,n) %>%
  mutate(Total = sum(`None`,`Complex Change`,`Directory Change`,na.rm = TRUE)) %>%
  rename(Year = year)



## Add sum row

row_sum <- colSums(tab_code.raw[,2:5],na.rm=TRUE)
tab_code <- bind_rows(tab_code.raw,row_sum)
tab_code[nrow(tab_code),1] = c("Total")

## Add col percent
col_pct <- round(tab_code[nrow(tab_code),2:5])*100/as.numeric(tab_code[nrow(tab_code),5],2)
tab_code <- bind_rows(tab_code,col_pct)
tab_code[nrow(tab_code),1]=c("Percent")

# Same for partial

tab_code_partial <- exit_all %>% filter(replicated %in% c("Partial")) %>%
  group_by(year,change) %>% summarize(n=n()) %>% spread(change,n) %>%
  mutate(Total = sum(`None`,`Complex Change`,`Directory Change`,na.rm = TRUE)) %>%
  rename(Year = year)

## Add sum row

row_sum <- colSums(tab_code_partial[,2:5],na.rm=TRUE)
tab_code_partial <- bind_rows(tab_code_partial,row_sum)
tab_code_partial[nrow(tab_code_partial),1] = c("Total")

## Add col percent
col_pct <- round(tab_code_partial[nrow(tab_code_partial),2:5])*100/
                  as.numeric(tab_code_partial[nrow(tab_code_partial),5],
                             2)
tab_code_partial <- bind_rows(tab_code_partial,col_pct)
tab_code_partial[nrow(tab_code_partial),1]=c("Percent")

# Number for tex
nochange        <-as.numeric(tab_code$`None`[tab_code$Year == "Total"])
directorychange <-as.numeric(tab_code$`Directory Change`[tab_code$Year == "Total"])
change          <-nochange+directorychange
cat(change, 					file=file.path(TexIncludes,"change.tex"))
complex         <-as.numeric(tab_code$`Complex Change`[tab_code$Year == "Total"])
cat(complex, 					file=file.path(TexIncludes,"complex.tex"))
ratiocomplex    <-100*round(complex/attempted.success,2)
cat(ratiocomplex, 					file=file.path(TexIncludes,"ratiocomplex.tex"))

###################################################
### code chunk number 9: results2.Rnw:255-259
###################################################
# Print table
tab_code[is.na(tab_code)] <- 0
#tab_code[12,2]<- round(as.numeric(tab_code[12,2]))
#tab_code[12,3]<- round(as.numeric(tab_code[12,3]))
#tab_code[12,4]<- round(as.numeric(tab_code[12,4]))
#tab_code[12,1]=c("Percent")

stargazer(tab_code,
          out=file.path(Outputs,"table_code_year.tex"),
          type="latex",
          title = "Manipulation of Code Required for Successful Reproductions",
          label="tab:code",
          style="aer",flip=F, summary=F, rownames = F,
          font.size = fs,column.sep.width = cw,
          notes=c("Note: Articles for which reproduction was attempted."))



###################################################
### code chunk number 10: results2.Rnw:265-289
###################################################
main <- exit_all %>% mutate(doc_qual = ifelse(clarity == "Complete",1,0),
                              `Fully reproduced` = ifelse(replicated == "Yes",1,0))


# Run Regression: citations on h-indices and confidential data
ols.docqual <- lm(`Fully reproduced`~doc_qual,main)

# stargazer(ols.docqual,
#            title="OLS: Reproduction Success vs Documentation Quality",
#           align=TRUE,no.space=FALSE,label = "reg1",
#            dep.var.labels=c('Successful Reproduction'),
#           covariate.labels = c("Documentation Clarity = Complete","Constant"),
#           notes.align = "l", notes.append = FALSE,
#           style="qje", type="latex",notes = c("Dependent variable = 1 if fully replicated."),
#            keep.stat = c("n"))

resmatrix<-cor.test(main$`Fully reproduced`,main$doc_qual,method="pearson")
resmatrix2<-cor.test(main$`Fully reproduced`,main$doc_qual,method="kendall")
resmatrix3<-cbind(resmatrix$estimate,resmatrix$p.value)
resmatrix4<-cbind(resmatrix2$estimate,resmatrix2$p.value)
resmatrix3<-rbind(resmatrix3,resmatrix4)
rownames(resmatrix3) <- c("Pearson","Kendall")
colnames(resmatrix3) <- c("Estimate", "p-value")
stargazer(resmatrix3,
          out=file.path(Outputs,"table_reg1.tex"),
          column.labels = c("test","estimate", "p-value"),
          title="Correlation of Reproduction Success vs Documentation Quality",
          align=TRUE, no.space=FALSE, label = "reg1",
          notes.align = "l", column.sep.width = cw,
          type="latex",single.row = TRUE,digits = 5,
          notes=c("Notes: Sample of reproduced and scored papers, i.e, assessed",
"papers for which we attempted reproduction."))



###################################################
### code chunk number 11: results2.Rnw:299-363
###################################################
# TODO: correct the average citation - it is not corrected in the raw data

# Load h-index data and citation data
hindex <- read.csv(paste(Outputs,HindexClean,sep="/"), header=TRUE,stringsAsFactors = F)

# Use DOI to identify year
hindex$article <- sub(pattern = "10.1257/app.",replacement = "",hindex$DOI)
hindex$pubyear <- sapply(strsplit(hindex$article,'[.]'),function(x) x[1])
hindex$pubyear <- as.numeric(hindex$pubyear)+2008

# Fix the formatting on citations per year number
hindex$Average.per.Year <- as.numeric(as.character(sub(",",".", hindex$Average.per.Year, fixed = TRUE)))

# as downloaded, but  need to convert to numeric first
# 2010 have 6 possible publication years divided by 6 years : are correctly computed
# 2011 have 5 possible publication years divided by 6 years
# 2013 have 3 possible publication years divided by 4 years
hindex$Average.per.Year <- as.numeric(hindex$Average.per.Year)
hindex$Average.per.Year[hindex$pubyear==2009] <- hindex$Average.per.Year[hindex$pubyear==2009] * 8/7
hindex$Average.per.Year[hindex$pubyear==2011] <- hindex$Average.per.Year[hindex$pubyear==2011] * 6/5
hindex$Average.per.Year[hindex$pubyear==2012] <- hindex$Average.per.Year[hindex$pubyear==2012] * 5/4
hindex$Average.per.Year[hindex$pubyear==2013] <- hindex$Average.per.Year[hindex$pubyear==2013] * 4/3

# Summarise stats
hindex <- hindex %>% group_by(DOI) %>%
  summarise(cite = mean(Total.Citations),
            avgcite = mean(Average.per.Year),
            avghindex = mean(h.index,na.rm=TRUE),
            tophindex=max(h.index,na.rm=TRUE),
            pubyear = mean(pubyear),
            lowhindex = min(h.index,na.rm=TRUE),
            authnum=max(Author.Order))
hindex$tophindex[!is.finite(hindex$tophindex)] <- NA
hindex$lowhindex[!is.finite(hindex$lowhindex)] <- NA

# save for future re-use
saveRDS(hindex,file=file.path(interwrk,"31_hindex_merged_wos.Rds"))

# This operation creates problem with  -Inf and Inf later, need to replace with Nan?
# Merge with exit Q
main <- exit_all %>% left_join(hindex,by="DOI")

# Summary Stats by Replication Success
authorstats.doi <- main %>% filter(!is.na(avghindex))

author <- authorstats.doi %>%
  # coherence with later selection criteria
  filter(!is.na(replicated)) %>%
  filter(confdata!="Confidential")%>%
  group_by(replicated) %>%
  summarise(`Number of Articles` = n(),
            `Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(authnum,na.rm=TRUE),
            `Avg Annual Citations`=mean(avgcite,na.rm=TRUE)) %>%
  mutate(replicated = ifelse(replicated == "No","Unsuccessful",replicated),
         replicated = ifelse(replicated == "Yes","Successful",replicated)) %>%
  rename(Outcome = replicated) %>%
   mutate_if(is.numeric, round, digits=2)

saveRDS(authorstats.doi,file=file.path(interwrk,"31_authorstats_doi.Rds"))

# Run Regression: number of authors and replication success
nauthor <- main %>% filter(!is.na(avghindex)) %>%
  group_by(replicated) %>%
  summarise(`Number of Articles` = n(),
            `Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(authnum,na.rm=TRUE),
            `Avg Annual Citations`=mean(avgcite,na.rm=TRUE)) %>%
  mutate(replicated = ifelse(replicated == "No","Unsuccessful",replicated),
         replicated = ifelse(replicated == "Yes","Successful",replicated)) %>%
  rename(Outcome = replicated) %>%
   mutate_if(is.numeric, round, digits=2)


###################################################
### code chunk number 12: results2.Rnw:369-397
###################################################
# Clarity and number of co-authors
cat1 <- c("no readme file was provided.")
cat2 <- c("complete. provided all information required to run the programs.")
cat3 <- c("incomplete. was ambiguous or left out crucial steps.")

main2 <- main %>%
  mutate(clarity = ifelse(tolower(README_Quality) %in% cat1,"No ReadMe Provided","No Info"),
         clarity = ifelse(tolower(README_Quality) %in% cat2,"Complete",clarity),
         clarity = ifelse(tolower(README_Quality) %in% cat3,"Incomplete",clarity))

#author2 <- main2 %>% filter(!is.na(avghindex)) %>% filter(replicated != "NA")
author2 <- main2 %>% filter((replicated != "NA"))   %>%
  #filter((clarity != "No ReadMe Provided")) %>%
  group_by(clarity) %>%
  summarise(`Number of Articles` = n(),
            `Number of Authors`=mean(authnum,na.rm=TRUE))%>%
   mutate_if(is.numeric, round, digits=2)

# Metrics clarity and number of authors
stargazer(author2,
          out=file.path(Outputs,"table_authormetrics.tex"),
          type="latex",title = "Clarity and Author Metrics",
          label="tab:authormetrics",
          style="aer",flip=F, summary=F, rownames = F,
          font.size = fs,column.sep.width = cw,
          notes=c("Notes: Sample of reproduced and scored papers, i.e, assessed",
                  "papers for which we attempted reproduction."))

# Print table
# Flip table

author %>%
  select(Outcome,`Avg h-index`, `Lowest h-index`, `Number of Authors`  ,
         Citations=`Avg Annual Citations`,N=`Number of Articles`  ) %>%
  transpose(keep.names = "Name") -> tmp
names(tmp) <- as.character(unlist(tmp[1,]))
tmp[2:nrow(tmp),] %>%
  select(Outcome,"Unsuccessful","Partial","Successful") %>%
  rename(` ` = Outcome) -> author.t
rm(tmp)

stargazer(author.t,
          out=file.path(Outputs,"table_metrics.tex"),
          type="latex",title = "Publication and Author Metrics, WoS",
          label="tab:metrics",
          style="aer",flip=F, summary=F, rownames = F,
          #font.size = fs,
          column.sep.width = cw,
          digits=2,
          notes=c("Notes: Articles with attempted reproduction.",
                  "Bibliometric data manually queried from WoS."))



###################################################
### code chunk number 13: results2.Rnw:403-446
###################################################

# Merge with entry Q
# main_entry <- entry_merge %>% left_join(hindex,by="DOI")
#
# # Merge with exit Q
# main_exit <- exit_merge %>% left_join(hindex,by="DOI")
#
# # Create a binary confidential variable
# main_entry <- main_entry %>%   mutate(confidential_data = absence, confidential_data = ifelse(absence %in% c("Confidential Data"),1,confidential_data), confidential_data = ifelse(absence %in%
#  c("No Data or Reason","Data was Provided"),0,confidential_data), #
#  confidential_data = as.numeric(confidential_data))
#
#  main_exit <- main_exit %>% mutate(confidential_data = ifelse(confdata =="Confidential",1,0))

# Create dataset with total confidential data variable from entry and exit
main <- complete_sample %>% select(DOI,absence) %>% left_join(exit_all) %>%
  left_join(hindex,by="DOI") %>%
  mutate(confidential_data = absence,
         confidential_data = ifelse(absence %in% c("Confidential Data"),1,confidential_data),
         confidential_data = ifelse(absence %in% c("No Data or Reason","Data was Provided"),0,confidential_data),
         confidential_data = as.numeric(confidential_data),
         confidential_data = ifelse(confidential_data == 0 & confdata == "Confidential",1,confidential_data))
main$confidential_data[grepl("No Data or Reason",main$absence)] <- 0


# Create numeric replication variable
main <- main %>% mutate(`Fully reproduced` = replicated,
                        `Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,`Fully reproduced`),
                        `Fully reproduced` = ifelse(replicated %in% c("Yes"),1,`Fully reproduced`),
                        `Fully reproduced` = as.numeric(`Fully reproduced`))

# Run Regression: citations on h-indices and confidential data
ols.cite.avgh <- lm(cite~avghindex* confidential_data,main)
ols.cite.toph <- lm(cite~tophindex* confidential_data,main)
ols.cite.lowh <- lm(cite~lowhindex* confidential_data,main)

# Make table
stargazer(ols.cite.avgh,ols.cite.toph,ols.cite.lowh,
          title="OLS: Citations vs Confidential Data",
          align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="reg2",
          dep.var.labels = "Total Citations",
          style="qje", type="latex",
          column.sep.width = "-15pt",
          keep.stat = c("n"), out=file.path(Outputs,"table_reg2.tex"),
          notes.label = "",
          notes = c("Notes: *** Significant at the 1 percent level, ** Significant at the 5 percent level",
                    "* Significant at the 10 percent level.","Avghindex is the average hindex about the article's authors,  tophindex the maximum",
                    " hindex among them and lowhindex the lowest. Avghindex:confidential stands for","the interaction between a dummy taking value 1 if the article uses confidential  ","data, and the aforementioned average hindex of authors.")
)






###################################################
### code chunk number 14: results2.Rnw:450-468
###################################################
# Run Regression: citations on h-indices and confidential data
ols.cite.avgh <- lm(cite~avghindex,main)
ols.cite.toph <- lm(cite~tophindex,main)
ols.cite.lowh <- lm(cite~lowhindex,main)

print(mean(main$cite,na.rm = T))
print(mean(main$avghindex,na.rm = T))
print(mean(main$tophindex,na.rm = T))
print(mean(main$lowhindex,na.rm = T))

# Make table
stargazer(ols.cite.avgh,ols.cite.toph,ols.cite.lowh,
          out=file.path(Outputs,"table_reg2test.tex"),
          title="OLS: Citations vs Confidential Data Testing",
          align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,label="reg2test",
          dep.var.labels = "Annual Citations",
          style="qje", type="latex",
          column.sep.width = "-15pt",
          keep.stat = c("n"))


###################################################
### code chunk number 15: ols1
###################################################

# Get exit data without confidential data
main <- exit_all %>% filter(confdata=="Other") %>%
  mutate(`Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,1)) %>%
  mutate(`Full or Partial`= ifelse(replicated %in% c("No"),0,1)) %>%
  mutate(doc_qual = ifelse(clarity == "Complete",1,0),
                              `Fully reproduced` = ifelse(replicated == "Yes",1,0)) %>%
  left_join(hindex,by="DOI")


# Run Regression: citations on h-indices and replication success
avgh <- lm(cite~avghindex* `Fully reproduced`,main)
toph <- lm(cite~tophindex* `Fully reproduced`,main)
lowh <- lm(cite~lowhindex* `Fully reproduced`,main)

avghp <- lm(cite~avghindex* `Full or Partial`,main)
tophp <- lm(cite~tophindex* `Full or Partial`,main)
lowhp <- lm(cite~lowhindex* `Full or Partial`,main)


###################################################
### code chunk number 16: ols1table
###################################################

# Make table
# stargazer(ols.cite.avgh,ols.cite.toph,
#           ols.cite.lowh,
#           title="OLS: Citations vs Reproduction Success (A)",
#           align=TRUE,
#           no.space=FALSE,
#           notes.align = "l", notes.append = FALSE,
#           label="reg3",
#           dep.var.labels = "Annual Citations",
#           style="aer", type="latex",
#            column.sep.width = "-20pt",
#            keep.stat = c("n"),
#           out=file.path(Outputs,"table_reg3a.tex")
# )

stargazer(avgh, toph, lowh, avghp, tophp,lowhp,
          title="OLS: Citations vs Reproduction Success (B)",
          align=TRUE,
          no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="reg3",
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"),
          out=file.path(Outputs,"table_reg3b.tex")
)
# Merged
# DOES NOT WORK
# stargazer(ols.cite.avgh,ols.cite.toph,
#           ols.cite.lowh, ols.cite.avghp,
#           ols.cite.tophp,ols.cite.lowhp, out=file.path(Outputs,"table_reg3.tex"),
#           title="OLS: Citations vs Reproduction Success",
#           align=TRUE,
#           no.space=FALSE,
#           notes.align = "l", notes.append = FALSE,
#           label="reg3",
#           dep.var.labels = "Annual Citations",
#           style="aer", type="latex",
#           column.sep.width = "-20pt",
#           keep.stat = c("n"),
#           notes.label = "",
#           notes = c("Notes: *** Significant at the 1 percent level, ** Significant at the 5 percent level",
#                     "* Significant at the 10 percent level.","Avghindex, lowhindex and tophindex stand respectively for the average, minimum","and maximum hindex across the article's authors. Fully reproduced takes value 1 if there",
#                     "is full reproducibility. Full or partial is either full or partial reproducibility."
#           )
#)


###################################################
### code chunk number 17: logols1
###################################################

# Get exit data without confidential data
logmain <- exit_all %>%
  filter(confdata=="Other") %>%
  mutate(`Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,1)) %>%
  mutate(`Full or Partial`= ifelse(replicated %in% c("No"),0,1)) %>%
  mutate(doc_qual = ifelse(clarity == "Complete",1,0),
                              `Fully reproduced` = ifelse(replicated == "Yes",1,0)) %>%
  left_join(hindex,by="DOI") %>%
  mutate(logcite = log(cite))


# Run Regression: citations on h-indices and replication success
lavgh <- lm(logcite~avghindex* `Fully reproduced`,logmain)
ltoph <- lm(logcite~tophindex* `Fully reproduced`,logmain)
llowh <- lm(logcite~lowhindex* `Fully reproduced`,logmain)

lavghp <- lm(logcite~avghindex* `Full or Partial`,logmain)
ltophp <- lm(logcite~tophindex* `Full or Partial`,logmain)
llowhp <- lm(logcite~lowhindex* `Full or Partial`,logmain)



###################################################
### code chunk number 18: logols1table
###################################################

# Make table
stargazer(lavgh,ltoph, llowh,lavghp, ltophp, llowhp,
          out=file.path(Outputs,"table_logreg3b.tex"),
          title="OLS: Log Citations vs Reproduction Success", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,label="logreg3b",
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
           column.sep.width = "-20pt",
           keep.stat = c("n"))

# stargazer(ols.lcite.avghp,
#           ols.lcite.tophp,ols.lcite.lowhp,
#           out=file.path(Outputs,"table_logreg3b.tex"),
#           title="OLS: Log Citations vs Reproduction Success", align=TRUE,no.space=FALSE,
#           notes.align = "l", notes.append = FALSE,label="logreg3b",
#           dep.var.labels = "Annual Citations",
#           style="aer", type="latex",
#           column.sep.width = "-20pt",
#           keep.stat = c("n"))
# Merged table
# stargazer(ols.lcite.avgh,ols.lcite.toph,
#           ols.lcite.lowh, ols.lcite.avghp,
#           ols.lcite.tophp,ols.lcite.lowhp,
#           out=file.path(Outputs,"table_logreg3.tex"),
#           title="OLS: Log Citations vs Reproduction Success", align=TRUE,no.space=FALSE,
#           notes.align = "l", notes.append = FALSE,label="logreg3b",
#           dep.var.labels = "Annual Citations",
#           style="aer", type="latex",
#           column.sep.width = "-20pt",
#           keep.stat = c("n"))

###################################################
###################################################
### code chunk number 18: arcsins1table
###################################################
arcsinmain <- exit_all %>%
  filter(confdata=="Other") %>%
  mutate(`Fully reproduced` = ifelse(replicated %in% c("No","Partial"),0,1)) %>%
  mutate(`Full or Partial`= ifelse(replicated %in% c("No"),0,1)) %>%
  mutate(doc_qual = ifelse(clarity == "Complete",1,0),
         `Fully reproduced` = ifelse(replicated == "Yes",1,0)) %>%
  left_join(hindex,by="DOI") %>%
  mutate(ascite = asinh(cite))


# Run Regression: citations on h-indices and replication success
asavgh <- lm(ascite~avghindex* `Fully reproduced`,arcsinmain)
astoph <- lm(ascite~tophindex* `Fully reproduced`,arcsinmain)
aslowh <- lm(ascite~lowhindex* `Fully reproduced`,arcsinmain)

asavghp <- lm(ascite~avghindex* `Full or Partial`,arcsinmain)
astophp <- lm(ascite~tophindex* `Full or Partial`,arcsinmain)
aslowhp <- lm(ascite~lowhindex* `Full or Partial`,arcsinmain)

# Make table
stargazer(asavgh,astoph, aslowh,asavghp, astophp, aslowhp,
          out=file.path(Outputs,"table_asreg3b.tex"),
          title="OLS: Log Citations vs Reproduction Success", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,label="asreg3b",
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"))

###################################################
### Explanation of sample
###################################################
uniquedoi <- readRDS(file=file.path(dataloc,"00_uniquedoi.Rds"))

count <- c(uniquedoi,total.assessed,total.attempted[1],total.attempted.nonconf[1])
sample <-matrix(c("Assessed articles","Assessed with complete records", "Eligible articles with non confidential data, complete and unique records","Amenable for replication, after removing confidential data articles identified during replication"))
sample_doc <- data.frame(sample, count)


###################################################
### code chunk number 3: tabappeendix
###################################################
# Print table
stargazer(sample_doc,
          type="latex",title = "Summary of data",
          label="tab:appendix",
          out=file.path(Outputs,"table_appendix.tex"),
          style="aer",align = TRUE,
          flip=FALSE,summary=FALSE,
          rownames = TRUE,
          font.size = fs,column.sep.width = cw)

#### Some extra numbers
nauthors<-round(mean(author$`Number of Authors`),2)
cat(nauthors, 					file=file.path(TexIncludes,"nauthors.tex"))
ncite<-round(mean(author$`Avg Annual Citations`),2)
cat(ncite, 					file=file.path(TexIncludes,"ncite.tex"))

