### R code from vignette source 'results1.Rnw'

###################################################
### code chunk number 1: setparent
###################################################
#knitr::set_parent('Replication_aejae.Rnw')
#load("../data/interwrk/my_work_space.RData")
source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
#source(file.path(TexBase,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)

### Now, can do stats on effective sample

###################################################
### code chunk number 3: t_entry table 2 articles per year
###################################################
# Summarize number of articles by year

complete_sample <- readRDS(file=file.path(dataloc,"00_complete_sample.Rds"))


# raw info on absence when no justification

complete_sample %>% filter(absence=="No Data or Reason") %>% select(DataAbsence)


###################################################
### code chunk number 4: tababs table 3: decomposion missing per year
###################################################
#unique(entry_merge$DataAbsence)
trial <- complete_sample[,c("year","absence")]
# Here need to change eligible for sth else
# should be 180 + 80 + 14 so 274

# Summarize number of articles by data presence
tab_absence <- complete_sample %>%
  group_by(absence,year) %>%
  summarize(n = n()) %>% spread(year,n) %>%
  rename(Reason = absence)

# Add sum col/row
tab_absence$Total <- rowSums(tab_absence[,2:ncol(tab_absence)],na.rm = T)
tab_absence$Percent<- round(tab_absence$Total*100/sum(tab_absence$Total),2)


row_sum <- colSums(tab_absence[,2:13],na.rm=TRUE)

tab_absence <- bind_rows(tab_absence,row_sum)
tab_absence[4,1] = c("Total")
tab_absence[is.na(tab_absence)] <- 0

# save tab_absence to be used in 31
saveRDS(tab_absence,file=file.path(interwrk,"00_tab_absence.Rds"))

# some raw output for absence of data reasons




###################################################
### code chunk number 6: tab4_1
###################################################
# Print table
stargazer(tab_absence,
          type="latex",title = "Assessment of Data Availability, By Year",label="tab:absence",
          style="aer",
          flip=FALSE,summary=FALSE, rownames = FALSE,#font.size = fs,
          column.sep.width = cw,out=file.path(TexIncludes,"table_absence.tex"),
          notes=c("\\footnotesize Notes: Assessments made  by replicators using the entry questionnaire, prior to attempting reproduction."))


## Compute presence of confidential data at initial assessment

tab_absence.raw <- tab_absence %>%
  select(Reason,Total) %>% filter(! Reason %in% c("Percent")) %>%
  mutate(Reason = if_else(Reason == "Data was Provided","Some Data Provided",Reason)) %>%
  arrange(Reason) %>% rename(Assessment = Reason, Articles = Total)


# Print table: assessments

stargazer(tab_absence.raw, digits=2,
          out=file.path(TexIncludes,"table_data_availability.tex"),
          type="latex",title = "Assessment of Data Availability",
          label="tab:data_availability",
          style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          # font.size = fs,
          column.sep.width = cw,
          notes=c("\\footnotesize Notes: Assessments made using the entry questionnaire by replicators,",
                  "\\footnotesize prior to attempting reproduction. Entry Q14, Q15."))



###################################################
### code chunk number 7: t_difficult table 4
###################################################

# Summarize number of articles by difficulty
t_difficult <- complete_sample %>% group_by(difficult) %>%
  summarize(n = n()) %>%
  mutate(n = as.numeric(n), difficult = ifelse(is.na(difficult),"No Rating Assigned",difficult)) %>%
  mutate(`Percent` = round(n/sum(n)*100,2),
         difficult = as.character(difficult)) %>%
  rename(`Difficulty Rating` = difficult, `Number of Articles` = n)
median_difficulty <- t_difficult %>%
  ungroup() %>%
  mutate(cumpct=cumsum(Percent)) %>%
  filter(cumpct>=50) %>%
  summarize(median=min(as.numeric(`Difficulty Rating`))) %>%
  pull()

# replace NA
t_difficult[is.na(t_difficult)] <- 0

# Add sum row
t_difficult[nrow(t_difficult)+1,1] = c("Total")
t_difficult[nrow(t_difficult),2] = colSums(t_difficult[,2],na.rm=TRUE)
t_difficult[nrow(t_difficult),3] = colSums(t_difficult[,3],na.rm=TRUE)

t_difficult_attempted <- complete_sample %>%
  filter(!is.na(replicated)) %>%
  group_by(difficult) %>%
  summarize(n = n()) %>%
  mutate(n = as.numeric(n), difficult = ifelse(is.na(difficult),"No Rating Assigned",difficult)) %>%
  mutate(`Percent` = round(n/sum(n)*100,2),
         difficult = as.character(difficult)) %>%
  rename(`Difficulty Rating` = difficult, `Number of Articles` = n)
median_difficulty_attempted <- t_difficult_attempted %>%
  ungroup() %>%
  mutate(cumpct=cumsum(Percent)) %>%
  filter(cumpct>=50) %>%
  summarize(median=min(as.numeric(`Difficulty Rating`))) %>%
  pull()
# replace NA

t_difficult_attempted[is.na(t_difficult_attempted)] <- 0

# Add sum row
t_difficult_attempted[nrow(t_difficult_attempted)+1,1] = c("Total")
t_difficult_attempted[nrow(t_difficult_attempted),2] = colSums(t_difficult_attempted[,2],na.rm=TRUE)
t_difficult_attempted[nrow(t_difficult_attempted),3] = colSums(t_difficult_attempted[,3],na.rm=TRUE)

# joint

t_difficult_joint <- left_join(t_difficult,t_difficult_attempted,by=c("Difficulty Rating")) %>%
  rename(`Assessed articles` = `Number of Articles.x`,
         `Percent ` = `Percent.x`,
         `Attempted articles` = `Number of Articles.y`,
         `Percent`  = `Percent.y`)


###################################################
### code chunk number 8: tab2_1
###################################################
# Print article count by difficulty table
stargazer(t_difficult_attempted,
          type="latex",title = "Difficulty of Reproduction",
          label="tab:difficult:attempted",style="aer",
          flip=FALSE,summary=FALSE, rownames = FALSE,
          #font.size = fs,
          column.sep.width = cw,
          out=file.path(TexIncludes,"table_difficult_attempted.tex"),
          notes=c("\\footnotesize Notes: Assessments made using the entry questionnaire by replicators,",
                  "\\footnotesize prior to attempting to reproduce any tables or figures. Assessments",
                  "\\footnotesize reported only for articles for which reproduction was attempted."))

stargazer(t_difficult_joint,
          type="latex",title = "Difficulty of Reproduction",
          label="tab:difficult:joint",style="aer",
          flip=FALSE,summary=FALSE, rownames = FALSE,
          font.size = fs,column.sep.width = cw,
          out=file.path(TexIncludes,"table_difficult.tex"),
          notes=c("Notes: Entry Q60. Assessments made prior",
                  " to attempting to reproduce any tables or figures. "))

stargazer(t_difficult,
          type="latex",title = "Difficulty of Reproduction (all)",
          label="tab:difficult:all",style="aer",
          flip=FALSE,summary=FALSE, rownames = FALSE,
          #font.size = fs,
          column.sep.width = cw,
          out=file.path(TexIncludes,"table_difficult_assessed.tex"),
          notes=c("\\footnotesize Notes: Assessments made using the entry questionnaire by replicators,",
                  "\\footnotesize prior to attempting to reproduce any tables or figures. Assessments",
                  "\\footnotesize reported for all articles with complete assessments."))

cat(median_difficulty, 					file=file.path(TexIncludes,"median.difficulty.all.tex"))
cat(median_difficulty_attempted,file=file.path(TexIncludes,"median.difficulty.attempted.tex"))


###################################################
### code chunk number 9: data_prog
###################################################
# Define program format variable
entry_merge_prog <- complete_sample %>%
  select(journal,ProgramFormat,year) %>%
  mutate(Stata = grepl("stata", tolower(ProgramFormat)),
         Matlab = grepl("matlab", tolower(ProgramFormat)),
         R = grepl(", r", tolower(ProgramFormat)) | tolower(ProgramFormat) %in% c("r"),
         SAS = grepl("sas", tolower(ProgramFormat)),
         Fortran = grepl("fortran", tolower(ProgramFormat)),
         Mathematica = grepl("mathematica", tolower(ProgramFormat)),
         SPSS = grepl("spss", tolower(ProgramFormat)),
         Eviews = grepl("eviews", tolower(ProgramFormat)),
         Excel = grepl("excel", tolower(ProgramFormat)),
         `Not Reported` = is.na(ProgramFormat))


# Make program format table
tab_prog <- entry_merge_prog %>%
  summarize(Stata = sum(Stata),
            Matlab = sum(Matlab),
            R = sum(R),
            SAS = sum(SAS),
            Fortran = sum(Fortran),
            Mathematica = sum(Mathematica),
            SPSS = sum(SPSS),
            Eviews = sum(Eviews),
            Excel = sum(Excel),
            `Not Reported` = sum(`Not Reported`)) %>%

  gather(Software,Count)

# Define data format variable
entry_merge_data <- complete_sample %>%
  select(journal,OnlineDataFormat1,ProgramFormat,year) %>%
  mutate(Stata = grepl("stata", tolower(OnlineDataFormat1)),
         Matlab = grepl("matlab", tolower(OnlineDataFormat1)),
         R = grepl(", r", tolower(OnlineDataFormat1)) | tolower(ProgramFormat) %in% c("r"),
         Fortran = grepl("fortran", tolower(OnlineDataFormat1)),
         SPSS = grepl("spss", tolower(OnlineDataFormat1)),
         Excel = grepl("excel", tolower(OnlineDataFormat1)),
         CSV = grepl("csv", tolower(OnlineDataFormat1)),
         txt = grepl("txt", tolower(OnlineDataFormat1)),
         `Not Reported` = is.na(OnlineDataFormat1))

# Make data format table
tab_data <- entry_merge_data %>%
  summarize(Stata = sum(Stata),
            Matlab = sum(Matlab),
            R = sum(R),
            Fortran = sum(Fortran),
            SPSS = sum(SPSS),
            Excel = sum(Excel),
            CSV = sum(CSV),
            txt = sum(txt),
            `Not Reported` = sum(`Not Reported`)) %>%
  gather(Software,Count)

# Combine the two tables
tab_prog_data <- merge(tab_prog,tab_data,by = c("Software"),all = TRUE) %>%
  rename(`Programming Language` = Count.x,`Data Format` = Count.y) %>%
  arrange(desc(`Programming Language`)) %>%
  mutate(`Programming Language`=replace_na(`Programming Language`,0),
         `Data Format` = replace_na(`Data Format`,0),
         sum=`Programming Language` + `Data Format`) %>%
  # filter
  filter(sum>0) %>%
  select(-sum)


###################################################
### code chunk number 10: tab3_1
###################################################
# Print table
stargazer(tab_prog_data,
          type="latex",title = "Programming Languages and Data Formats",label="tab:prog",style="aer",
          flip=FALSE, summary=FALSE, rownames = F,
          #font.size = fs,
          column.sep.width = cw,
          notes = c("\\footnotesize Notes: Combination of answers from Entry Q8, Q21, Q28, Q36, Q44." ,
                    "\\footnotesize Column sums are greater than the number of articles",
                    "\\footnotesize because articles can use more than one  programming",
                    "\\footnotesize language or data format. Sample of assessments made",
                    "\\footnotesize using the entry questionnaire by replicators prior to ",
                      "\\footnotesize attempting to reproduce any tables or figures."),
          out=file.path(TexIncludes,"table_data_prog.tex"))


###################################################
### code chunk number 11: clarity
###################################################
# Make table

exit_all <- readRDS(file=file.path(dataloc,"00_exit_all.Rds"))
nrow(exit_all)

tab_doc <- exit_all %>%
  group_by(clarity,year) %>% summarize(n=n()) %>% spread(year,n) %>%
  rename(` ` = clarity)


# Add sum col/row
tab_doc$Total <- rowSums(tab_doc[,2:11],na.rm = T)
tab_doc$Percent<- round(tab_doc$Total*100/sum(tab_doc$Total),2)

row_sum <- colSums(tab_doc[,2:13],na.rm=TRUE)

tab_doc <- bind_rows(tab_doc,row_sum)
tab_doc[4,1] = c("Total")



###################################################
### code chunk number 14: convenience
###################################################
td.total <- tab_doc$Total[tab_doc$` `=="Total"]
td.complete <- tab_doc$Total[tab_doc$` `=="Complete"]
td.incomplete <- tab_doc$Total[tab_doc$` `=="Incomplete"]
td.noinfo <- tab_doc$Total[tab_doc$` `=="No Info"]
cat(td.total, 					file=file.path(TexIncludes,"tdtotal.tex"))
cat(td.complete, 					file=file.path(TexIncludes,"tdcomplete.tex"))
cat(td.incomplete, 					file=file.path(TexIncludes,"tdincomplete.tex"))
cat(td.noinfo , 					file=file.path(TexIncludes,"tdnoinfo.tex"))
td.completepct<-td.complete*100/td.total
td.incompletepct<-td.incomplete*100/td.total
cat(td.completepct, 					file=file.path(TexIncludes,"tdcompletepct.tex"))
cat(td.incompletepct, 					file=file.path(TexIncludes,"tdincompletepct.tex"))



###################################################
### code chunk number 16: tab5_1
###################################################
# Print documentation clarity table

tab_doc[is.na(tab_doc)] <- 0

stargazer(tab_doc,
          type="latex",title = "Ex-Post Assessment of Documentation Clarity",label="tab:doc:yearly",
          style="aer",flip=FALSE,
          summary=FALSE, rownames = FALSE, font.size = fs,
          column.sep.width = cw,
          out=file.path(TexIncludes,"table_doc_clarity_by_year.tex"),
          notes=c("\\footnotesize Notes: Articles with attempted reproduction. Exit Q22.",
                  "\\footnotesize Complete = Provided all information required to run the programs.",
                  "\\footnotesize Incomplete = Was ambiguous or left out crucial steps."))




## Compute presence of confidential data at initial assessment


tab_doc.raw <- exit_all %>%
  group_by(clarity) %>%
  summarize(n=n()) %>%
  ungroup() %>%
  mutate(Percent = round(100*n/sum(n),2)) %>%
  rename(` ` = clarity)


# Print table: assessments

stargazer(tab_doc.raw, digits=2,
          out=file.path(TexIncludes,"table_doc.tex"),
          type="latex",title = "Ex-Post Assessment of Documentation Clarity",
          label="tab:doc",
          style="aer",
          flip=FALSE, summary=FALSE, rownames = FALSE,
          column.sep.width = cw,
          notes=c("\\footnotesize Notes: Articles with attempted reproduction. Exit Q22. ",
                  "\\footnotesize Complete = Provided all information required to run the programs.",
                  "\\footnotesize Incomplete = Was ambiguous or left out crucial steps."))



# how to add notes
# table <- stargazer(...,notes="This will be replaced")
# table[grepl("Note",table)] <- "replacement note"
# write(table,file.path(TexIncludes,"doc.tex"))


###################################################
### code chunk number 15: conveniencecleanup
###################################################
rm(td.total)
rm(td.complete)
rm(td.incomplete)
rm(td.noinfo)
rm(td.completepct)
rm(td.incompletepct)
