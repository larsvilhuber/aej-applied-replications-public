## @knitr main
# This program first consolidates datasources into the main dataset
# It then analyzes this dataset


# ###########################
# SECTION A: Consolidate datasources
# ###########################

# where does DF come from?
# HYPOTHESIS: entry_merge -> see 30_results1.R

# Collapse  categories
df$Replicated.[df$Replicated.=="Partial"]="No"
df$Replicated.[df$Replicated.=="Mostly"]="Yes"
df$Replicated.[df$Replicated.=="partly"]="No"
df$Replicated.[df$Replicated.=="Partly"]="No"
df$Replicated.[df$Replicated.=="Partially"]="No"
df$Replicated.[df$Replicated.=="partially"]="No"
df$Replicated.[df$Replicated.=="yes"]="Yes"
df$Replicated.[df$Replicated.=="no"]="No"

# Consolidate into "main" dataset
main <- df %>%
  select(DOI,Title,Year,Replicated.,Main.Issue,Data.Type,Data.Access.Type,
         Program_Run_Clean,Directory_Change,instructions,Data.URL,Data.Contact) %>%
  mutate(ConfData=ifelse(Main.Issue=="Confidential Data",1,0),
         rep=ifelse(Replicated.=="Yes",1,0))

# construct a useful analysis variable
# 2 = replicated, public-use
# 1 = not replicated, public -use
# 0 = not replicated, confidential
main$ConfDataRep <- main$rep - main$ConfData*(1-main$rep) + 1
main$ConfDataRepFactor <- as.factor(main$ConfDataRep)
levels(main$ConfDataRepFactor) <- c("Confidential data","Unsuccessful","Successful")

# Add h-index and citation data
hindex <- read.csv(paste(Outputs,HindexClean,sep="/"), header=TRUE)
# TODO: correct the average citation - it is not corrected in the raw data
# use DOI to identify year
hindex$article <- sub(pattern = "10.1257/app.",replacement = "",hindex$DOI)
hindex$pubyear <- sapply(strsplit(hindex$article,'[.]'),function(x) x[1])
hindex$pubyear <- as.numeric(hindex$pubyear)+2008
hindex$Average.per.Year <- as.numeric(as.character(sub(",",".", hindex$Average.per.Year, fixed = TRUE)))

# as downloaded,
# 2010 have 6 possible publication years divided by 6 years : are correctly computed
# 2011 have 5 possible publication years divided by 6 years
# 2013 have 3 possible publication years divided by 4 years
hindex$Average.per.Year[hindex$pubyear==2009] <- hindex$Average.per.Year[hindex$pubyear==2009] * 8/7
hindex$Average.per.Year[hindex$pubyear==2011] <- hindex$Average.per.Year[hindex$pubyear==2011] * 6/5
hindex$Average.per.Year[hindex$pubyear==2012] <- hindex$Average.per.Year[hindex$pubyear==2012] * 5/4
hindex$Average.per.Year[hindex$pubyear==2013] <- hindex$Average.per.Year[hindex$pubyear==2013] * 4/3

main <- hindex %>% group_by(DOI) %>%
  summarise(cite=mean(Total.Citations),avgcite=mean(Average.per.Year),
            avghindex = mean(h.index,na.rm=TRUE), tophindex=max(h.index,na.rm=TRUE),
            pubyear = mean(pubyear),
            lowhindex=min(h.index,na.rm=TRUE),authnum=max(Author.Order)) %>%
  merge(main, by="DOI", all.y=TRUE)


# Add program and data format
temp <- select(de1,DOI,ProgramFormat,DataFormat=DataFormatAnalysis)
temp1 <- select(de2,DOI,ProgramFormat,DataFormat=OnlineDataFormat1)
temp2 <- rbind(temp,temp1)
# some cleaning
temp2$DOI <- sub("\t","",temp2$DOI)
# Note: there are multiple values in some fields, we'll worry about it later.

main <- merge(temp2,main,by="DOI",all.y=TRUE)
main <- main[!duplicated(main$DOI),]

# ###########################
# SECTION B: Perform Analyses
# ###########################

# ###########################
# 1. Replication Success by Year
# ###########################

mainbyyear <- main %>% group_by(Year,Replicated.) %>% summarise(count=n()) %>%
  dcast(Year~Replicated.,sum) %>% rbind(apply(., 2, sum)) %>%
  mutate(Unsuccessful = No,Successful=Yes,Total= Yes + No)

names = mainbyyear$Year

mainbyyear <- mainbyyear %>% select(Unsuccessful, Successful,Total)

rownames(mainbyyear) = names
rownames(mainbyyear)[nrow(mainbyyear)] = "Total"

mainbyyear1 <- main %>% mutate(odie=ifelse(Replicated.=="Yes", 1, ifelse(ConfData==1,3,2))) %>%
  group_by(Year,odie) %>% summarise(count=n()) %>%
  dcast(Year~odie,sum) %>% rbind(apply(., 2, sum)) %>%
  mutate(`Confidential Data` = `3`,Successful=`1`,Unsuccessful=`2`,
         Total=`1`+`2`+`3`) %>%
  select(`Confidential Data`,Unsuccessful, Successful,Total)

rownames(mainbyyear1) = names
rownames(mainbyyear1)[nrow(mainbyyear1)] = "Total"

# ###########################
# 2. Unsuccessful breakdown by year
# ###########################

Unsuc <- main %>% group_by(Year,Main.Issue) %>%
  filter(Replicated.=="No") %>%
  summarise(count=n()) %>% dcast(Year~Main.Issue,sum) %>%
  rbind(apply(., 2, sum)) %>% mutate(Other=Var.2)

names = Unsuc$Year
Unsuc <- Unsuc %>% select(`Missing Data`,`Corrupted Data`, `Code Error`,
                          `Missing Code`, `Other`) %>%
  mutate(Total = rowSums(.))

# break lines
rownames(Unsuc) = names
rownames(Unsuc)[nrow(Unsuc)] = "Total"

# ###########################
# 3. DOI by replication success
# ###########################

SucDOI <- main$DOI[main$Replicated.=="Yes"]

ConfidDOI <- main$DOI[(main$Replicated.=="No") & main$Main.Issue=="Confidential Data"]

UnsucDOI <- main$DOI[(df$Replicated.=="No") & main$Main.Issue!="Confidential Data"]

# ###########################
# 4.1 Breakdown of Articles with Confidential Data by Type of Data
# ###########################

Miss <- main %>% group_by(Year,Data.Type) %>%
  filter(Main.Issue == "Confidential Data") %>% summarise(count=n()) %>%
  dcast(Year~Data.Type,sum) %>% rbind(apply(., 2, sum))

names = Miss$Year

Miss <- Miss %>% select(-Year) %>% mutate(Total = rowSums(.))

names(Miss) <- c("Admin local","Admin National","Admin Regional",
                 "Private Commercial","Private Other","Total")

rownames(Miss) = names
rownames(Miss)[nrow(Miss)] = "Total"

# ###########################
# 4.2 Breakdown of Articles with Confidential Data by URL or Contact provided
# ###########################
prov_url <- main %>% group_by(Year,Data.URL) %>%
  filter(Main.Issue == "Confidential Data") %>% summarise(count=n()) %>%
  dcast(Year~Data.URL,sum) %>% rbind(apply(., 2, sum))
names = prov_url$Year
prov_url <- prov_url %>% select(-Year) %>% mutate(Total = rowSums(.))

names(prov_url) <- c("No","Yes","Total")

rownames(prov_url) = names
rownames(prov_url)[nrow(prov_url)] = "Total"

#### URL=YES, decomposition if contact or not
prov_UY_cont <- main %>% group_by(Year,Data.Contact) %>%
  filter(Main.Issue == "Confidential Data" & Data.URL=="Y") %>% summarise(count=n()) %>%
  dcast(Year~Data.Contact,sum) %>% rbind(apply(., 2, sum))
names = prov_UY_cont$Year

prov_UY_cont <- prov_UY_cont %>% select(-Year) %>% mutate(Total = rowSums(.))
names(prov_UY_cont) <- c("No","Yes","Total")

rownames(prov_UY_cont) = names
rownames(prov_UY_cont)[nrow(prov_UY_cont)] = "Total"

#######
prov_cont <- main %>% group_by(Year,Data.Contact) %>%
  filter(Main.Issue == "Confidential Data") %>% summarise(count=n()) %>%
  dcast(Year~Data.Contact,sum) %>% rbind(apply(., 2, sum))
names = prov_cont$Year
prov_cont <- prov_cont %>% select(-Year) %>% mutate(Total = rowSums(.))

names(prov_cont) <- c("No","Yes","Total")

rownames(prov_cont) = names
rownames(prov_cont)[nrow(prov_cont)] = "Total"


# ###########################
# 5. Breakdown of Articles with Confidential Data by Access Type
# ###########################

Acc <- main %>% group_by(Year,Data.Access.Type) %>%
  filter(Main.Issue == "Confidential Data") %>% summarise(count=n()) %>%
  dcast(Year~Data.Access.Type,sum) %>% rbind(apply(., 2, sum))

names = Acc$Year

Acc <- Acc %>% select(-Year) %>% mutate(Total = rowSums(.))

names(Acc) <- c("Formal","Informal Commitment","Informal No Commitment","No Info","Total")

rownames(Acc) = names
rownames(Acc)[nrow(Acc)] = "Total"

# ###########################
# 6. Code Changes Required?
# ###########################

Clean <- main %>% group_by(Year,Program_Run_Clean) %>%
  filter(Replicated.=="Yes") %>% summarise(count=n()) %>%
  dcast(Year~Program_Run_Clean,sum) %>% rbind(apply(., 2, sum))

names = Clean$Year

Clean <- Clean %>% select(-Year) %>%
  mutate(Total = rowSums(.),`No Change`=Total-`No, I needed to make changes in the code.`) %>%
  select(`No Change`, Total)

code <- main %>% group_by(Year,Directory_Change) %>%
  filter(Replicated.=="Yes", Program_Run_Clean=="No, I needed to make changes in the code.") %>%
  summarise(count=n()) %>%
  dcast(Year~Directory_Change,sum) %>% rbind(apply(., 2, sum)) %>% select(-Year) %>%
  cbind(Clean,.)

oldnames = c('No, the changes to the code were more involved.', "Yes")
newnames = c('Complex Change',"Directory Change")
setnames(code, old = oldnames, new = newnames)

code<-code[c(1,4,3,2)]

rownames(code) = names
rownames(code)[nrow(code)] = "Total"

# ###########################
# 7. Documentation Clarity
# ###########################

# Successful Replications
Instruct <- main %>% group_by(Year,instructions) %>%
  filter(Replicated.=="Yes") %>% summarise(count=n()) %>%
  dcast(Year~instructions,sum) %>% rbind(apply(., 2, sum))

names = Instruct$Year

Instruct <- Instruct %>% select(-Year) %>%
  mutate(Total = rowSums(.),
         Complete = `Complete. Provided all information required to run the programs.` + `NA`,
         Incomplete = `Incomplete. Was ambiguous or left out crucial steps.`,
         "No ReadMe Provided" = Total - Complete - Incomplete) %>%
  select(Complete, Incomplete,`No ReadMe Provided`,Total)

rownames(Instruct) = names
rownames(Instruct)[nrow(Instruct)] = "Total"

# Unsuccessful Replications
In <- main %>% group_by(Year,instructions) %>%
  filter(Replicated.=="No" & ConfData==0) %>% summarise(count=n()) %>%
  dcast(Year~instructions,sum) %>% rbind(apply(., 2, sum))

names = In$Year

In <- In %>% select(-Year) %>%
  mutate(Total = rowSums(.),
         Complete = `Complete. Provided all information required to run the programs.` + `NA`,
         Incomplete = `Incomplete. Was ambiguous or left out crucial steps.`,
         "No ReadMe Provided" = Total - Complete - Incomplete) %>%
  select(Complete, Incomplete,`No ReadMe Provided`,Total)

rownames(In) = names
rownames(In)[nrow(In)] = "Total"

# Confidential Data
Instruct3 <- main %>% group_by(Year,instructions) %>%
  filter(ConfData==1) %>% summarise(count=n()) %>%
  dcast(Year~instructions,sum) %>% rbind(apply(., 2, sum))

names = Instruct3$Year

Instruct3 <- Instruct3 %>% select(-Year) %>%
  mutate(Total = rowSums(.),
         Complete = `Complete. Provided all information required to run the programs.` + `NA`,
         Incomplete = `Incomplete. Was ambiguous or left out crucial steps.`,
         "No ReadMe Provided" = Total - Complete - Incomplete) %>%
  select(Complete, Incomplete,`No ReadMe Provided`,Total)

rownames(Instruct3) = names
rownames(Instruct3)[nrow(In)] = "Total"

# ###########################
# 8. Citations/h-index Analysis
# ###########################
names = c("Unsuccessful Replications","Successful Replications")

author <- main %>% group_by(Replicated.) %>%
  summarise(`Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(authnum,na.rm=TRUE)) %>% mutate(names=names)

names1=author$names
author <- author %>% select(-Replicated., -names)
# these generate warnings due to some transition of codes in dplyr/tibble. For now, we suppress the warning
suppressWarnings(rownames(author) <- names1)

#######

author1 <- main %>% filter(Main.Issue!="Confidential Data") %>%
  group_by(Replicated.) %>%
  summarise(`Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(authnum,na.rm=TRUE)) %>%
  select(-Replicated.)
suppressWarnings(rownames(author1) <- names1)

author_avgh <- main %>% mutate(odie=ifelse(Replicated.=="Yes", 1, ifelse(ConfData==1,3,2))) %>%
  select(Year,odie,avghindex) %>% group_by(Year,odie) %>%  dcast(Year~odie,mean) %>%
  mutate(`Confidential Data` = `3`,Successful=`1`,Unsuccessful=`2`)

namesyear = author_avgh$Year
author_avgh <- author_avgh %>% select(`Confidential Data`,Unsuccessful, Successful)
rownames(author_avgh) = namesyear

author_toph <- main %>% mutate(odie=ifelse(Replicated.=="Yes", 1, ifelse(ConfData==1,3,2))) %>%
  select(Year,odie,tophindex) %>% group_by(Year,odie) %>%  dcast(Year~odie,mean) %>%
  mutate(`Confidential Data` = `3`,Successful=`1`,Unsuccessful=`2`) %>%
  select(`Confidential Data`,Unsuccessful, Successful)
rownames(author_toph) = namesyear

author_lowh <- main %>% mutate(odie=ifelse(Replicated.=="Yes", 1, ifelse(ConfData==1,3,2))) %>%
  select(Year,odie,lowhindex) %>% group_by(Year,odie) %>%  dcast(Year~odie,mean) %>%
  mutate(`Confidential Data` = `3`,Successful=`1`,Unsuccessful=`2`) %>%
  select(`Confidential Data`,Unsuccessful, Successful)
rownames(author_lowh) = namesyear

citations <- main %>% mutate(odie=ifelse(Replicated.=="Yes", 1, ifelse(ConfData==1,3,2))) %>%
  select(Year,odie,cite) %>% group_by(Year,odie) %>%  dcast(Year~odie,mean) %>%
  mutate(`Confidential Data` = `3`,Successful=`1`,Unsuccessful=`2`) %>%
  select(`Confidential Data`,Unsuccessful, Successful)
rownames(citations) = namesyear

# ###########################
# 9. Program Formats
# ###########################
# could have used unlist(strsplit(as.vector(temp2$ProgFormat),","))
ProgFormat <- main %>% group_by(Year,ProgramFormat) %>% filter(!is.na(ProgramFormat)) %>%
  summarise(count=n()) %>%
  dcast(Year~ProgramFormat,sum) %>% rbind(apply(., 2, sum)) %>%
  mutate(SPSS = `SPSS`,
         Stata=Stata+`Stata, SAS`,#`Stata, R`,
         SAS = SAS + `Stata, SAS`,
         `Not Reported` = Missing)

names = ProgFormat$Year

ProgFormat <- ProgFormat %>% select(Stata,SPSS,SAS,`Not Reported`)

rownames(ProgFormat) = names
rownames(ProgFormat)[nrow(ProgFormat)] = "Total"

# ###########################
# 9. Data Formats
# ###########################
# could have used unlist(strsplit(as.vector(temp2$DataFormat),","))
DataFormat <- main %>% group_by(Year,DataFormat) %>%
  filter(!is.na(DataFormat),ConfData==0) %>% summarise(count=n()) %>%
  dcast(Year~DataFormat,sum) %>% rbind(apply(., 2, sum)) %>%
  mutate(Stata= Stata + `Stata, CSV, SAS`+`Stata, CSV`,
         SAS = `Stata, CSV, SAS`,
         CSV = `Stata, CSV, SAS`+`Stata, CSV`,
         SPSS=`SPSS`,
         Excel=`Excel`)

names = DataFormat$Year

DataFormat <- DataFormat %>% select(Stata,SPSS,SAS,CSV)

rownames(DataFormat) = names
rownames(DataFormat)[nrow(DataFormat)] = "Total"

# ###########################
# 8. Regressions
# ###########################

ols.cite.avgh <- lm(cite~avghindex*ConfData,main)
ols.cite.toph <- lm(cite~tophindex*ConfData,main)
ols.cite.lowh <- lm(cite~lowhindex*ConfData,main)

log.rep.avgh <- main %>% filter(ConfData==0) %>%
  glm(rep ~ avghindex,., family = "binomial")

log.rep.toph <- main %>% filter(ConfData==0) %>%
  glm(rep ~ tophindex,., family = "binomial")

log.rep.lowh <- main %>% filter(ConfData==0) %>%
  glm(rep ~ lowhindex,., family = "binomial")

log.avail.avgh <- main %>%
  glm(ConfData ~ avghindex,., family = "binomial")

log.avail.toph <- main %>%
  glm(ConfData ~ tophindex,., family = "binomial")

log.avail.lowh <- main %>%
  glm(ConfData ~ lowhindex,., family = "binomial")


 stargazer(ols.cite.avgh,ols.cite.toph,ols.cite.lowh,log.rep.avgh,
           log.rep.toph,log.rep.lowh,
           title="Logit Models", align=TRUE,no.space=FALSE,
           notes = c("Column 1 presents results from the estimation with all data pooled.",
                    "Columns 2 - 5 control for the indicated fixed effects."),
          notes.align = "l", notes.append = FALSE,
          style="qje", type="text",
           keep.stat = c("n"))


### R code from vignette source 'tables.Rnw'

###################################################
### code chunk number 1: tables.Rnw:6-7
###################################################


###################################################
### code chunk number 2: tables.Rnw:11-14
###################################################
stargazer(author_avgh,type="latex",
          title = "Average Author h-indices",
          label="tab:author1",style="aer",
          flip=FALSE,summary=FALSE,
          out=file.path(Outputs,"table_author1.tex")
          #notes = c("Notes: Calculated for articles with accessible data.")
)


###################################################
### code chunk number 3: tables.Rnw:17-20
###################################################
stargazer(author_toph,type="latex",
          title = "Average h-index for leading author",
          label="tab:author2",
          style="aer",flip=FALSE,summary=FALSE,
          out=file.path(Outputs,"table_author2.tex")
          #notes = c("Notes: Leading authors are those with the highest h-index")
)


###################################################
### code chunk number 4: tables.Rnw:23-26
###################################################
stargazer(author_lowh,type="latex",
          title = "Average h-index for junior author",
          label="tab:author3",
          style="aer",flip=FALSE,summary=FALSE,
          out=file.path(Outputs,"table_author3.tex")
          #notes = c("Notes: Calculated for articles with accessible data.")
)


###################################################
### code chunk number 5: tables.Rnw:29-32
###################################################
stargazer(citations,type="latex",
          title = "Average article citation count",
          label="tab:citation",style="aer",
          flip=FALSE,summary=FALSE,
          out=file.path(Outputs,"table_author3.tex")
          # notes = c("Notes: Calculated for articles with accessible data.")
)




# ###########################
# 9. Figures
# ###########################

# Convert to long format. Prepare for line graph of results by year
mby.m <- melt(as.matrix(mainbyyear1))
names(mby.m) <- c("years", "Type", "articles")

# Convert to long format. Prepare for pie chart of causes for unsuccess, all years
unc.m <- melt(as.matrix(Unsuc))
names(unc.m) <- c("years", "Type", "articles")


### R code from vignette source 'figures.Rnw'

###################################################
### code chunk number 1: breakdown_year
###################################################
mbyplot1 <- mby.m %>% filter(years!="Total" & Type != "Total") %>%
  ggplot(aes(x=years,y=articles, group=Type, colour=Type)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") +
  ylab("Number of articles") +
  xlab("Publication year") +
  ylim(0,max(mby.m$articles[mby.m$years!="Total" & mby.m$Type != "Total"]))

ggsave(filename=file.path(Outputs,"figure1.png"),mbyplot1)

