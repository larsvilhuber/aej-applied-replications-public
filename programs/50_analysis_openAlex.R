## OLS REGRESSIONS:
#### Create variables for similar OLS regressions as the ones before
### Reproduce the regressions with web of Sciences
### Add author information such as institution and region


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# data is by au_id (author), article_id (=DOI), year (of observation of citations)

mainOA           <- readRDS(file=file.path(interwrk,"47_mainOA_complete.Rds"))

hindex.allyears  <- readRDS(file=file.path(interwrk,"47_hindex_allyears.Rds")) %>%
  mutate(max_institution_works = max_institution_works/10000)
name.mapping     <- read_excel(file.path(auxilloc,"mainOA-mapping.xlsx")) %>%
  # one fix
  mutate(model.varnames = case_when(
    model.varnames == "Data absent: confidential"~ "Confidential data",
    .default = model.varnames
    ))

# > names(hindex.allyears)
# [1] "DOI"
# [2] "year"
# [3] "publication_year"
# [4] "ytd_cites"
# [5] "new_cites"
# [6] "avghindex"
# [7] "`Max H-index`"
# [8] "`Min H-index`"
# [9] "`Author at US university`"
# [10] "max_institution_works"
# [11] "max_experience"
# [12] "nauthors"
# [13] "years_since_publication"

# Combine mainOA with 4th-year ytd citations
#
# Changes from earlier: confidential_data -> flag_entry_confdata
#

mainOA.4yr <- mainOA %>%
  # first, get the citations in Year 4
  left_join(hindex.allyears %>%
              filter(years_since_publication == 4) %>%
              select(DOI,ytd_cites,new_cites)) %>%
  # next, get the other data as of year=(4-1)=3
  left_join(hindex.allyears %>%
              filter(years_since_publication == 3) %>%
              select(-ytd_cites,-new_cites)) %>%
  mutate(log_ytd_cites = log(ytd_cites+1),
         asin_ytd_cites = asinh(ytd_cites),
         `Solo-authored` = (nauthors==1))

mainOA.4yrpost2012 <- mainOA %>%
  # first, get the citations in Year 4
  left_join(hindex.allyears %>%
              filter(years_since_publication == 4) %>%
              select(DOI,ytd_cites,new_cites)) %>%
  # next, get the other data as of year=(4-1)=3
  left_join(hindex.allyears %>%
              filter(years_since_publication == 0) %>%
              select(-ytd_cites,-new_cites)) %>%
  filter(publication_year >=2012) %>%
  mutate(log_ytd_cites = log(ytd_cites+1),
         asin_ytd_cites = asinh(ytd_cites),
         `Solo-authored` = (nauthors==1))

mainOA.allyears <- mainOA %>%
  # first, get the citations in Year x = same
  left_join(hindex.allyears %>%
              select(DOI,year,ytd_cites,new_cites,years_since_publication)) %>%
  filter(years_since_publication > 0) %>%
  filter(years_since_publication <=5 ) %>%
  # we lose year = 2012 because OA data only goes back 10 years
  filter(year > 2012) %>%
  # next, get the other data as of year=(4-1)=3, so we add a year for merge
  left_join(hindex.allyears %>%
              mutate(year = year +1) %>%
              select(-ytd_cites,-new_cites,-years_since_publication)) %>%
  mutate(log_ytd_cites = log(ytd_cites+1),
         asin_ytd_cites = asinh(ytd_cites),
         asin_new_cites=asinh(new_cites),
         dummy2019 = (year >=2019),
         `Solo-authored` = (nauthors==1),
         `Years squared` = years_since_publication*years_since_publication)
nrow(mainOA.allyears %>% distinct(DOI))
# [1] 273
skim(mainOA.allyears) %>% filter(n_missing > 0)
# should only have clarity, replicated, and "Fully Reproduced" with missing.

# do nice labeling
# Update the master spreadsheet if necessary!
names.main.4yr <- names(mainOA.4yr) %>%
  as_tibble() %>%
  rename(names=value)
left_join(names.main.4yr,name.mapping) %>%
  mutate(model.varnames = if_else(is.na(model.varnames),names,model.varnames)) ->
  mainOA.mapping.4yr
mainOA.mapping.4yr %>% filter(is.na(model.varnames)) -> test.na
if ( nrow(test.na) > 0 ) {
  print(test.na)
  stop("Error in variable name mapping")
  }

# assign varnames by renaming
names(mainOA.4yr) <- mainOA.mapping.4yr$model.varnames
names(mainOA.4yr)
# > names(mainOA.4yr)
# [1] "DOI"
# [2] "Clarify of documentation"
# [3] "Reproduced"
# [4] "Difficulty of reproduction"
# [5] "Was data absent"
# [6] "Reproduction failed: confidential data"
# [7] "Confidential data"
# [8] "Fully reproduced"
# [9] "YTD citations"
# [10] "New citations"
# [11] "Year"
# [12] "Year of publication"
# [13] "Avg. H-index"
# [14] "Max H-index"
# [15] "Min H-index"
# [16] "Author at US university"
# [17] "Highest Institution Publications"
# [18] "Highest Co-author Experience"
# [19] "Number of authors"
# [20] "Years since publication"
# [21] "Log(YTD citations)"
# [22] "AsinH(YTD citations)"

# same for post2012
names.main.4yrpost2012 <- names(mainOA.4yrpost2012) %>%
  as_tibble() %>%
  rename(names=value)
left_join(names.main.4yrpost2012,name.mapping) %>%
  mutate(model.varnames = if_else(is.na(model.varnames),names,model.varnames)) ->
  mainOA.mapping.4yrpost2012
mainOA.mapping.4yrpost2012 %>% filter(is.na(model.varnames)) -> test.na
if ( nrow(test.na) > 0 ) {
  print(test.na)
  stop("Error in variable name mapping")
}

# assign varnames by renaming
names(mainOA.4yrpost2012) <- mainOA.mapping.4yrpost2012$model.varnames
names(mainOA.4yrpost2012)


# Same for the allyear one
names.main.allyears <- names(mainOA.allyears) %>%
  as_tibble() %>%
  rename(names=value)
left_join(names.main.allyears,name.mapping) %>%
  mutate(model.varnames = case_when(
    names == "dummy2019" ~ "Year >= 2019",
    names=="asin_new_cites"  ~ "AsinH(new citations)",
    .default = model.varnames )) %>%
  mutate(model.varnames = if_else(is.na(model.varnames),names,model.varnames)) ->
  mainOA.mapping.allyears
# add dummy


mainOA.mapping.allyears %>% filter(is.na(model.varnames)) -> test.na
if ( nrow(test.na) > 0 ) {
  print(test.na)
  stop("Error in variable name mapping")
}

# assign varnames by renaming
names(mainOA.allyears) <- mainOA.mapping.allyears$model.varnames
names(mainOA.allyears)


### we are done preparing analysis data. we need them again in 52.

saveRDS(mainOA.4yr,file.path(interwrk,"50_mainOA_4yr.Rds"))
saveRDS(mainOA.4yrpost2012,file.path(interwrk,"50_mainOA_4yr_p2012.Rds"))
saveRDS(mainOA.allyears,file.path(interwrk,"50_mainOA_allyears.Rds"))

### repeated footnotes ============================================================

source(file.path(programs,"_footnotes.R"))



# Run Regression: total citations on h-indices and confidential data
ols.cite.avgh <- lm(`YTD citations`~`Avg. H-index`* `Confidential data` +
                      `Highest Institution Publications` +
                      `Highest Co-author Experience`    +
                      `Number of authors` +
                      `Author at US university` +
                      `Solo-authored`
                      ,mainOA.4yr)
ols.cite.toph <- lm(`YTD citations`~`Max H-index`* `Confidential data` +
                    `Highest Institution Publications` +
                      `Highest Co-author Experience`    +
                      `Number of authors` +
                      `Author at US university`+
                      `Solo-authored`
                    ,mainOA.4yr)
ols.cite.lowh <- lm(`YTD citations`~`Min H-index`* `Confidential data` +
                      `Highest Institution Publications` +
                      `Highest Co-author Experience`    +
                      `Number of authors` +
                      `Author at US university`+
                      `Solo-authored`
                    ,mainOA.4yr)

ols.cite.kitchen.sink <- lm(`YTD citations`~`Avg. H-index`* `Confidential data` +
                              `Max H-index`* `Confidential data` +
                              `Min H-index`* `Confidential data` +
                              `Highest Institution Publications` +
                              `Highest Co-author Experience`    +
                              `Number of authors` +
                              `Author at US university`+
                              `Solo-authored`
                            ,mainOA.4yr)

# weird stuff

names(ols.cite.kitchen.sink$coefficients)[names(ols.cite.kitchen.sink$coefficients) ==
                                            "`Confidential data`:`Max H-index`"] <- "`Max H-index`:`Confidential data`"
names(ols.cite.kitchen.sink$coefficients)[names(ols.cite.kitchen.sink$coefficients) ==
                                            "`Confidential data`:`Min H-index`"] <- "`Min H-index`:`Confidential data`"


stargazer(ols.cite.avgh,ols.cite.toph,ols.cite.lowh,ols.cite.kitchen.sink,
          title="OLS: Citations and Confidential Data",
          notes.append = FALSE,
          label="reg2:OA",
          dep.var.labels = "Total Citations",
          align=TRUE,no.space=TRUE, style="qje", type="latex",column.sep.width = "-15pt",
          keep.stat = c("adj.rsq","n"),
          out=file.path(Outputs,"table_reg2OA.tex"),
          notes.label = "",
          notes.align="l",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    "\"Confidential data\" identifies if the article was assessed to have confidential data.",
                    footnote.sample.assessed)
)

# add.lines = list(c("Fixed effects?", "No", "No"),
# c("Results believable?", "Maybe", "Try again later")))
#           omit        = "quarter",
# omit.labels = "Quarter dummies?")

### Replicate table 13: Run Regression: citations on h-indices and replication success

#mainOAexit <- saveRDS(file.path(interwrk,"47_mainOA_exit.Rds"))

lhs.level = "`YTD citations`"
lhs.log   = "`Log(YTD citations)`"
lhs.arcsin= "`AsinH(YTD citations)`"
kitchensink.spec = "`Highest Institution Publications` +  `Highest Co-author Experience` +
             `Number of authors` + `Author at US university` +`Solo-authored`"

# Regressions

avgh <- lm(as.formula(paste0(lhs.level,"~ `Avg. H-index`* `Fully reproduced` +",
                            kitchensink.spec)),
           mainOA.4yr)
toph <- lm(as.formula(paste0(lhs.level,"~`Max H-index`* `Fully reproduced`+",
                             kitchensink.spec)),
           mainOA.4yr)
lowh <- lm(as.formula(paste0(lhs.level,"~`Min H-index`* `Fully reproduced`+",
                             kitchensink.spec)),
           mainOA.4yr)
kitchen.sink <- lm(as.formula(paste0(lhs.level,"~ `Avg. H-index`* `Fully reproduced`+
                     `Max H-index`* `Full or Partial`+
                     `Min H-index`* `Full or Partial`+",
                            kitchensink.spec)),
           mainOA.4yr)

avghp <- lm(as.formula(paste0(lhs.level,"~`Avg. H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
tophp <- lm(as.formula(paste0(lhs.level,"~`Max H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
lowhp <- lm(as.formula(paste0(lhs.level,"~`Min H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
kitchen.sink.p <- lm(as.formula(paste0(lhs.level,"~ `Avg. H-index`* `Full or Partial`+
                     `Max H-index`* `Full or Partial`+
                     `Min H-index`* `Full or Partial`+",
                                       kitchensink.spec)),
                   mainOA.4yr)

# weird stuff

names(kitchen.sink$coefficients)[names(kitchen.sink$coefficients) ==
                                            "`Fully reproduced`:`Max H-index`"] <- "`Max H-index`:`Fully reproduced`"
names(kitchen.sink$coefficients)[names(kitchen.sink$coefficients) ==
                                            "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

# weird stuff

names(kitchen.sink.p$coefficients)[names(kitchen.sink.p$coefficients) ==
                                            "`Full or partial`:`Max H-index`"] <- "`Max H-index`:`Full or partial`"
names(kitchen.sink.p$coefficients)[names(kitchen.sink.p$coefficients) ==
                                            "`Full or partial`:`Min H-index`"] <- "`Min H-index`:`Full or partial`"


 # Make table
stargazer(avgh,toph,lowh, kitchen.sink,
          title="OLS: YTD Citations on Reproduction Outcomes ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="reg3:OA",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial, footnote.sample.attempted),
          out=file.path(Outputs,"table_reg3OA.tex")
)

stargazer(avghp, tophp,lowhp, kitchen.sink.p,
          title="OLS: YTD Citations on Reproduction Outcomes ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="reg3:OA:partial",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial, footnote.sample.attempted),
          out=file.path(Outputs,"table_reg3OA_partial.tex")
)



##### Same but with log: table 14

# Run Regression: citations on h-indices and replication success

l.avgh <- lm(as.formula(paste0(lhs.log,"~ `Avg. H-index`* `Fully reproduced` +",
                             kitchensink.spec)),
           mainOA.4yr)
l.toph <- lm(as.formula(paste0(lhs.log,"~`Max H-index`* `Fully reproduced`+",
                             kitchensink.spec)),
           mainOA.4yr)
l.lowh <- lm(as.formula(paste0(lhs.log,"~`Min H-index`* `Fully reproduced`+",
                             kitchensink.spec)),
           mainOA.4yr)
l.kitchen.sink <- lm(as.formula(paste0(lhs.log,"~ `Avg. H-index`* `Fully reproduced`+
                     `Max H-index`* `Fully reproduced`+
                     `Min H-index`* `Fully reproduced`+",
                                     kitchensink.spec)),
                   mainOA.4yr)

l.avghp <- lm(as.formula(paste0(lhs.log,"~`Avg. H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
l.tophp <- lm(as.formula(paste0(lhs.log,"~`Max H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
l.lowhp <- lm(as.formula(paste0(lhs.log,"~`Min H-index`* `Full or Partial`+",
                              kitchensink.spec)),
            mainOA.4yr)
l.kitchen.sink.p <- lm(as.formula(paste0(lhs.log,"~ `Avg. H-index`* `Full or Partial`+
                     `Max H-index`* `Full or Partial`+
                     `Min H-index`* `Full or Partial`+",
                                       kitchensink.spec)),
                     mainOA.4yr)

# weird stuff

names(l.kitchen.sink$coefficients)[names(l.kitchen.sink$coefficients) ==
                                   "`Fully reproduced`:`Max H-index`"] <- "`Max H-index`:`Fully reproduced`"
names(l.kitchen.sink$coefficients)[names(l.kitchen.sink$coefficients) ==
                                   "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

# weird stuff

names(l.kitchen.sink.p$coefficients)[names(l.kitchen.sink.p$coefficients) ==
                                     "`Full or Partial`:`Max H-index`"] <- "`Max H-index`:`Full or Partial`"
names(l.kitchen.sink.p$coefficients)[names(l.kitchen.sink.p$coefficients) ==
                                     "`Full or Partial`:`Min H-index`"] <- "`Min H-index`:`Full or Partial`"



# Print table
stargazer(l.avgh,l.toph, l.lowh, l.kitchen.sink,
          title="OLS: Log Citations on Reproduction Outcomes ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="logreg3alt:OA",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Log Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,
                    "Log(YTD citations) is computed as log(YTD Citations + 1).",
                    footnote.lag.hindex, footnote.full.partial,
                    footnote.sample.attempted),
          out=file.path(Outputs,"table_logreg3OA.tex"))

stargazer(l.avghp, l.tophp, l.lowhp,l.kitchen.sink.p,
          title="OLS: Log Citations on Reproduction Outcomes ",
          align=TRUE,no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="logreg3alt:OA:partial",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Log Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,
                    "Log(YTD citations) is computed as log(YTD Citations + 1).",
                    footnote.lag.hindex, footnote.full.partial,
                    footnote.sample.attempted),
          out=file.path(Outputs,"table_logreg3OA_partial.tex"))

##### Same but with arcsin table  13 and 14 alt

# Run Regression: citations on h-indices and replication success

a.avgh <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Fully reproduced` +",
                               kitchensink.spec)),
             mainOA.4yr)
a.toph <- lm(as.formula(paste0(lhs.arcsin,"~`Max H-index`* `Fully reproduced`+",
                               kitchensink.spec)),
             mainOA.4yr)
a.lowh <- lm(as.formula(paste0(lhs.arcsin,"~`Min H-index`* `Fully reproduced`+",
                               kitchensink.spec)),
             mainOA.4yr)
a.kitchen.sink <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Fully reproduced`+
                     `Max H-index`* `Fully reproduced`+
                     `Min H-index`* `Fully reproduced`+",
                                       kitchensink.spec)),
                     mainOA.4yr)

a.avghp <- lm(as.formula(paste0(lhs.arcsin,"~`Avg. H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yr)
a.tophp <- lm(as.formula(paste0(lhs.arcsin,"~`Max H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yr)
a.lowhp <- lm(as.formula(paste0(lhs.arcsin,"~`Min H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yr)
a.kitchen.sink.p <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Full or Partial`+
                     `Max H-index`* `Full or Partial`+
                     `Min H-index`* `Full or Partial`+",
                                         kitchensink.spec)),
                       mainOA.4yr)

# weird stuff

names(a.kitchen.sink$coefficients)[names(a.kitchen.sink$coefficients) ==
                                     "`Fully reproduced`:`Max H-index`"] <- "`Max H-index`:`Fully reproduced`"
names(a.kitchen.sink$coefficients)[names(a.kitchen.sink$coefficients) ==
                                     "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

# weird stuff

names(a.kitchen.sink.p$coefficients)[names(a.kitchen.sink.p$coefficients) ==
                                       "`Full or Partial`:`Max H-index`"] <- "`Max H-index`:`Full or Partial`"
names(a.kitchen.sink.p$coefficients)[names(a.kitchen.sink.p$coefficients) ==
                                       "`Full or Partial`:`Min H-index`"] <- "`Min H-index`:`Full or Partial`"


# Print table
stargazer(a.avgh,a.toph,a.lowh, a.kitchen.sink,
          title="OLS: Arcsin Citations on Reproduction Outcomes ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="arcreg3alt:OA",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "ArcSinH of Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial,footnote.sample.attempted),
          out=file.path(Outputs,"table_arcreg3OA.tex"))

stargazer(a.avghp,a.tophp,a.lowhp, a.kitchen.sink.p,
          title="OLS: Arcsin Citations on Reproduction Outcomes ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="arcreg3alt:OA:partial",
          #dep.var.labels = "ArcSinH of Total Citations",
          dep.var.labels.include = FALSE,
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial,footnote.sample.attempted),
          out=file.path(Outputs,"table_arcreg3OA_partial.tex"))


# ##### Now, bring in new information we have
# # Run Regression: citations on h-indices and replication success, control for US region
# avgh2 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Fully reproduced` * `Author at US university`,mainOA.4yr)
# toph2 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Fully reproduced` * `Author at US university`,mainOA.4yr)
# lowh2 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Fully reproduced` * `Author at US university`,mainOA.4yr)
#
# avghp2 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Full or Partial` * `Author at US university`,mainOA.4yr)
# tophp2 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Full or Partial` * `Author at US university`,mainOA.4yr)
# lowhp2 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Full or Partial` * `Author at US university`,mainOA.4yr)
#
#
# stargazer(avgh2,toph2, lowh2,avghp2, tophp2, lowhp2,
#           title="OLS: Arcsin Citations on Reproduction Outcomes, control for region ", align=TRUE,no.space=TRUE,
#           notes.align = "l", notes.append = FALSE,
#           label="arcreg3cont:a:OA",
#           dep.var.labels = "Total Citations",
#           style="aer", type="latex",
#           column.sep.width = "-20pt",
#           keep.stat = c("adj.rsq","n"),
#           notes.label = "",
#           omit=c(".+:.+:.+",  # matches all 3-way interactions
#                  ".+:.+" # all two- and 3-way interactions
#                  ),
#           notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
#                     footnote.full.partial,
#                         footnote.region.us,footnote.nway,
#                     footnote.sample.attempted),
#           font.size = "small",
#           out=file.path(Outputs,"table_arcreg3OA2.tex"))
#
#
# # Run Regression: citations on h-indices and replication success, control for US region
# avgh3 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Fully reproduced` * `Author at US university`*
#                                     `Highest Institution Publications` ,mainOA.4yr)
# toph3 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Fully reproduced` *  `Author at US university` *
#               `Highest Institution Publications`,mainOA.4yr)
# lowh3 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Fully reproduced` * `Author at US university` *
#               `Highest Institution Publications`,mainOA.4yr)
#
# avghp3 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Fully reproduced` * `Author at US university` *
#                `Highest Institution Publications` * `Highest Co-author Experience`,mainOA.4yr)
# tophp3 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Fully reproduced` * `Author at US university` *
#                `Highest Institution Publications` * `Highest Co-author Experience`,mainOA.4yr)
# lowhp3 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Fully reproduced` * `Author at US university` *
#                `Highest Institution Publications` * `Highest Co-author Experience`,mainOA.4yr)
#
# stargazer(avgh3,avghp3,
#           title="OLS: Arcsin Citations on Reproduction Outcomes, control for institution productivity ",
#           align=TRUE,no.space=TRUE,
#           notes.align = "l", notes.append = FALSE,
#           label="arcreg3prod:a:OA",
#           dep.var.labels = "Total Citations",
#           style="aer", type="latex",
#           column.sep.width = "-20pt",
#           keep.stat = c("adj.rsq","n"),
#           notes.label = "",
#           omit=c(".+:.+" # all two-way and higher interactions
#           ),
#           notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
#                     footnote.full.partial,
#                     footnote.region.us,footnote.productivity,footnote.experience,
#                     footnote.nway,footnote.sample.attempted),
#           out=file.path(Outputs,"table_arcreg3OA3.tex"))
#

# Do year since publication, allow for several years

# RHS reproducible confidential dummy for "2019 or later" (année où des data editor ont été nommés)

sparse = paste0(lhs.arcsin, "~`Avg. H-index`*`Fully reproduced`")
sparse.time = paste0(sparse,"*`Years since publication`")
sparse.y2019 = paste0(sparse.time,"*`Year >= 2019`")

t.mainOA.allyears <- mainOA.allyears %>%
  filter(!is.na(Reproduced))

t.articles <- t.mainOA.allyears %>%
  distinct(DOI) %>% nrow()

t.avgh       <- lm(as.formula(sparse),t.mainOA.allyears)
t.avghY      <- lm(as.formula(paste0(sparse.time,"+`Years squared`")),t.mainOA.allyears)
t.avghD2019Y  <- lm(as.formula(paste0(sparse.y2019,"+`Years squared`")),t.mainOA.allyears)
#t.avghD2019Y <- lm(`AsinH(new citations)`~`Avg. H-index`* `Fully reproduced`*`Year >= 2019` *
#                   `Years since publication`,t.mainOA.allyears)
# rename coefficients for interactions


# names(t.avghD2019Y$coefficients)[names(t.avghD2019Y$coefficients) ==
#                                          "`Year >= 2019`TRUE:`Years since publication`"] <- "`Max H-index`:`Fully reproduced`"
# names(a2012.kitchen.sink$coefficients)[names(a2012.kitchen.sink$coefficients) ==
#                                          "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

# > names(t.avghD2019Y$coefficients)
# [1] "(Intercept)"
# [2] "`Avg. H-index`"
# [3] "`Fully reproduced`"
# [4] "`Years since publication`"
# [5] "`Year >= 2019`TRUE"
# [6] "`Years squared`"
# [7] "`Avg. H-index`:`Fully reproduced`"
# [8] "`Avg. H-index`:`Years since publication`"
# [9] "`Fully reproduced`:`Years since publication`"
# [10] "`Avg. H-index`:`Year >= 2019`TRUE"
# [11] "`Fully reproduced`:`Year >= 2019`TRUE"
# [12] "`Years since publication`:`Year >= 2019`TRUE"
# [13] "`Avg. H-index`:`Fully reproduced`:`Years since publication`"
# [14] "`Avg. H-index`:`Fully reproduced`:`Year >= 2019`TRUE"
# [15] "`Avg. H-index`:`Years since publication`:`Year >= 2019`TRUE"
# [16] "`Fully reproduced`:`Years since publication`:`Year >= 2019`TRUE"
# [17] "`Avg. H-index`:`Fully reproduced`:`Years since publication`:`Year >= 2019`TRUE"
order.D2019Y = c("`Avg. H-index`", "`Fully reproduced`" ,
                 "`Years since publication`","`Years squared`",
                 "`Year >= 2019`")
order.D2019Y.num =c(1,2,3,5,4,6,8,10,7,9)

stargazer(t.avgh, t.avghY,t.avghD2019Y,
          add.lines = list(c("Articles",t.articles,t.articles,t.articles)),
          title="OLS: ArcSinH Citations - Dynamic Effect ",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l",
          notes.append = FALSE,
          label="arcregdum:OA",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          #dep.var.labels = "New Citations",
          dep.var.labels.include = FALSE,
          order = order.D2019Y.num,
          omit = c("`Years since publication`:`Year >= 2019`",
                   "`Avg. H-index`:`Fully reproduced`:`Year >= 2019`TRUE:`Years since publication`",
                   "`Fully reproduced`:`Year >= 2019`TRUE:`Years since publication`",
                   "`Avg. H-index`:`Year >= 2019`TRUE:`Years since publication`"),
          notes = c(footnote.base,footnote.new.all,footnote.lag.hindex,
                    footnote.full.partial,footnote.sample.attempted),
          out=file.path(Outputs,"table_arcreg3OA4.tex"))


###  Robustness regression: only post2012


a2012.avgh <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Fully reproduced` +",
                               kitchensink.spec)),
             mainOA.4yrpost2012)
a2012.toph <- lm(as.formula(paste0(lhs.arcsin,"~`Max H-index`* `Fully reproduced`+",
                               kitchensink.spec)),
             mainOA.4yrpost2012)
a2012.lowh <- lm(as.formula(paste0(lhs.arcsin,"~`Min H-index`* `Fully reproduced`+",
                               kitchensink.spec)),
             mainOA.4yrpost2012)
a2012.kitchen.sink <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Fully reproduced`+
                     `Max H-index`* `Fully reproduced`+
                     `Min H-index`* `Fully reproduced`+",
                                       kitchensink.spec)),
                     mainOA.4yrpost2012)

a2012.avghp <- lm(as.formula(paste0(lhs.arcsin,"~`Avg. H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yrpost2012)
a2012.tophp <- lm(as.formula(paste0(lhs.arcsin,"~`Max H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yrpost2012)
a2012.lowhp <- lm(as.formula(paste0(lhs.arcsin,"~`Min H-index`* `Full or Partial`+",
                                kitchensink.spec)),
              mainOA.4yrpost2012)
a2012.kitchen.sink.p <- lm(as.formula(paste0(lhs.arcsin,"~ `Avg. H-index`* `Full or Partial`+
                     `Max H-index`* `Full or Partial`+
                     `Min H-index`* `Full or Partial`+",
                                         kitchensink.spec)),
                       mainOA.4yrpost2012)


# weird stuff


names(a2012.kitchen.sink$coefficients)[names(a2012.kitchen.sink$coefficients) ==
                                   "`Fully reproduced`:`Max H-index`"] <- "`Max H-index`:`Fully reproduced`"
names(a2012.kitchen.sink$coefficients)[names(a2012.kitchen.sink$coefficients) ==
                                   "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

names(a2012.kitchen.sink.p$coefficients)[names(a2012.kitchen.sink.p$coefficients) ==
                                     "`Full or partial`:`Max H-index`"] <- "`Max H-index`:`Full or partial`"
names(a2012.kitchen.sink.p$coefficients)[names(a2012.kitchen.sink.p$coefficients) ==
                                     "`Full or partial`:`Min H-index`"] <- "`Min H-index`:`Full or partial`"

# Print table
stargazer(a2012.avgh,a2012.toph,a2012.lowh,a2012.kitchen.sink,
          title="OLS: Arcsin Citations on Reproduction Outcomes, post-2012",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="arcregp2012:OA",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag0.hindex,
                    footnote.full.partial,footnote.post2012),
          out=file.path(Outputs,"table_arcregpost12OA.tex")
          )

stargazer(a2012.avghp,a2012.tophp,a2012.lowhp,a2012.kitchen.sink.p,
          title="OLS: Arcsin Citations on Reproduction Outcomes, post-2012",
          align=TRUE,
          no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="arcregp2012:OA:partial",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("adj.rsq","n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial,footnote.sample.attempted,footnote.post2012),
          out=file.path(Outputs,"table_arcregpost12OA_partial.tex")
)


###

mainOAnoconf  <- readRDS(file=file.path(interwrk,"47_mainOA_noconf.Rds"))

# table of summary stats
st1 <- mainOA.4yr %>% select(`Fully reproduced`, `Full or Partial`, `AsinH(YTD citations)`,  `Avg. H-index`,`Max H-index`, `Min H-index`)
st2 <- mainOA.4yr %>% filter(!is.na(Reproduced)) %>% select(`Fully reproduced`, `Full or Partial`, `AsinH(YTD citations)`,  `Avg. H-index`,`Max H-index`, `Min H-index`)

stargazer(st1,
          title="Summary statistics", align=TRUE,no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="tab:statdesc:assessed",
          style="aer",
          flip=FALSE,
          #font.size = fs,
          column.sep.width = cw,
          notes = c("Notes: ", footnote.full.partial, footnote.sample.assessed),
          out=file.path(Outputs,"table_stassessed.tex"))

stargazer(st2,
          title="Summary statistics", align=TRUE,no.space=TRUE,rownames=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="tab:statdesc:attemped",
          style="aer",
          flip=FALSE,
          #font.size = fs,
          column.sep.width = cw,
          notes = c("Notes: ", footnote.full.partial, footnote.sample.attempted),
          out=file.path(Outputs,"table_stattempted.tex"))

