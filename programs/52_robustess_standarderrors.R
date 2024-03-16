## OLS REGRESSIONS:
#### Create variables for similar OLS regressions as the ones before
### this redoes some of the regresssions from before, but with robust errors
### Add author information such as institution and region


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


### Get data from 50


mainOA.4yr <- readRDS(file.path(interwrk,"50_mainOA_4yr.Rds"))
mainOA.4yrpost2012 <- readRDS(file.path(interwrk,"50_mainOA_4yr_p2012.Rds"))
mainOA.allyears <- readRDS(file.path(interwrk,"50_mainOA_allyears.Rds"))

### repeated footnotes ============================================================


source(file.path(programs,"_footnotes.R"))


# Run Regression: total citations on h-indices and confidential data
ols.cite.avgh <- lm(`YTD citations`~`Avg. H-index`* `Confidential data`,mainOA.4yr)
ols.cite.toph <- lm(`YTD citations`~`Max H-index`* `Confidential data`,mainOA.4yr)
ols.cite.lowh <- lm(`YTD citations`~`Min H-index`* `Confidential data`,mainOA.4yr)

covavgh <- vcovHC(ols.cite.avgh, type = "HC")
robust.seavgh <- sqrt(diag(covavgh))
covtoph <- vcovHC(ols.cite.toph, type = "HC")
robust.setoph <- sqrt(diag(covtoph))
covlowh <- vcovHC(ols.cite.lowh, type = "HC")
robust.selowh <- sqrt(diag(covlowh))

stargazer(ols.cite.avgh,ols.cite.toph,ols.cite.lowh,
          title="OLS: Citations and Confidential Data",
          align=TRUE,no.space=FALSE,
          notes.append = FALSE,
          label="reg2:OA:robust", se=list(NULL, robust.seavgh, robust.setoph,  robust.selowh),
          dep.var.labels = "Total Citations",
          style="qje", type="latex",
          column.sep.width = "-15pt",
          keep.stat = c("n"), out=file.path(Outputs,"table_reg2OA_rob.tex"),
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


# Regressions

avgh <- lm(`YTD citations`~`Avg. H-index`* `Fully reproduced`,mainOA.4yr)
toph <- lm(`YTD citations`~`Max H-index`* `Fully reproduced`,mainOA.4yr)
lowh <- lm(`YTD citations`~`Min H-index`* `Fully reproduced`,mainOA.4yr)

avghp <- lm(`YTD citations`~`Avg. H-index`* `Full or Partial`,mainOA.4yr)
tophp <- lm(`YTD citations`~`Max H-index`* `Full or Partial`,mainOA.4yr)
lowhp <- lm(`YTD citations`~`Min H-index`* `Full or Partial`,mainOA.4yr)
covavgh <- vcovHC(avgh, type = "HC")
robust.seavgh <- sqrt(diag(covavgh))
covtoph <- vcovHC(toph, type = "HC")
robust.setoph <- sqrt(diag(covtoph))
covlowh <- vcovHC(lowh, type = "HC")
robust.selowh <- sqrt(diag(covlowh))
covavghp <- vcovHC(avghp, type = "HC")
robust.seavghp <- sqrt(diag(covavghp))
covtophp <- vcovHC(tophp, type = "HC")
robust.setophp <- sqrt(diag(covtophp))
covlowhp <- vcovHC(lowhp, type = "HC")
robust.selowhp <- sqrt(diag(covlowhp))

# Make table
stargazer(avgh,toph,lowh, avghp, tophp,lowhp,
          title="OLS: Citations on Reproducibility (OA)",
          align=TRUE,
          no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="reg3:OA", se=list(NULL, robust.seavgh, robust.setoph,  robust.selowh, robust.seavghp, robust.setophp,  robust.selowhp),
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial, footnote.sample.attempted),
          out=file.path(Outputs,"table_reg3OA_rob.tex")
)



##### Same but with log: table 14

# Run Regression: citations on h-indices and replication success
avgh <- lm(`Log(YTD citations)`~`Avg. H-index`* `Fully reproduced`,mainOA.4yr)
toph <- lm(`Log(YTD citations)`~`Max H-index`* `Fully reproduced`,mainOA.4yr)
lowh <- lm(`Log(YTD citations)`~`Min H-index`* `Fully reproduced`,mainOA.4yr)

avghp <- lm(`Log(YTD citations)`~`Avg. H-index`* `Full or Partial`,mainOA.4yr)
tophp <- lm(`Log(YTD citations)`~`Max H-index`* `Full or Partial`,mainOA.4yr)
lowhp <- lm(`Log(YTD citations)`~`Min H-index`* `Full or Partial`,mainOA.4yr)

covavgh <- vcovHC(avgh, type = "HC")
robust.seavgh <- sqrt(diag(covavgh))
covtoph <- vcovHC(toph, type = "HC")
robust.setoph <- sqrt(diag(covtoph))
covlowh <- vcovHC(lowh, type = "HC")
robust.selowh <- sqrt(diag(covlowh))
covavghp <- vcovHC(avghp, type = "HC")
robust.seavghp <- sqrt(diag(covavghp))
covtophp <- vcovHC(tophp, type = "HC")
robust.setophp <- sqrt(diag(covtophp))
covlowhp <- vcovHC(lowhp, type = "HC")
robust.selowhp <- sqrt(diag(covlowhp))

# Print table
stargazer(avgh,toph, lowh, avghp, tophp, lowhp,
          title="OLS: Log Citations on Reproducibility (OA)", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="logreg3alt:OA",
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,
                    "Log(YTD citations) is computed as log(YTD Citations + 1).",
                    footnote.lag.hindex, footnote.full.partial,
                    footnote.sample.attempted),
          out=file.path(Outputs,"table_logreg3OA_rob.tex"))

##### Same but with arcsin table  13 and 14 alt



# Run Regression: citations on h-indices and replication success
avgh <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Fully reproduced`,mainOA.4yr)
toph <- lm(`AsinH(YTD citations)`~`Max H-index`* `Fully reproduced`,mainOA.4yr)
lowh <- lm(`AsinH(YTD citations)`~`Min H-index`* `Fully reproduced`,mainOA.4yr)

avghp <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Full or Partial`,mainOA.4yr)
tophp <- lm(`AsinH(YTD citations)`~`Max H-index`* `Full or Partial`,mainOA.4yr)
lowhp <- lm(`AsinH(YTD citations)`~`Min H-index`* `Full or Partial`,mainOA.4yr)

covavgh <- vcovHC(avgh, type = "HC")
robust.seavgh <- sqrt(diag(covavgh))
covtoph <- vcovHC(toph, type = "HC")
robust.setoph <- sqrt(diag(covtoph))
covlowh <- vcovHC(lowh, type = "HC")
robust.selowh <- sqrt(diag(covlowh))
covavghp <- vcovHC(avghp, type = "HC")
robust.seavghp <- sqrt(diag(covavghp))
covtophp <- vcovHC(tophp, type = "HC")
robust.setophp <- sqrt(diag(covtophp))
covlowhp <- vcovHC(lowhp, type = "HC")
robust.selowhp <- sqrt(diag(covlowhp))
# Print table
stargazer(avgh,toph, lowh, avghp, tophp, lowhp,
          title="OLS: Arcsin Citations on Reproducibility (OA)", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="arcreg3alt:OA:rob",se=list(NULL, robust.seavgh, robust.setoph,  robust.selowh, robust.seavghp, robust.setophp,  robust.selowhp),
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial,footnote.sample.attempted),
          out=file.path(Outputs,"table_arcreg3OA_rob.tex"))


##### Now, bring in new information we have
# Run Regression: citations on h-indices and replication success, control for US region
avgh2 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Fully reproduced` + `Author at US university`,mainOA.4yr)
toph2 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Fully reproduced` + `Author at US university`,mainOA.4yr)
lowh2 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Fully reproduced` + `Author at US university`,mainOA.4yr)

avghp2 <- lm(`AsinH(YTD citations)`~`Avg. H-index`* `Full or Partial` + `Author at US university`,mainOA.4yr)
tophp2 <- lm(`AsinH(YTD citations)`~`Max H-index`* `Full or Partial` + `Author at US university`,mainOA.4yr)
lowhp2 <- lm(`AsinH(YTD citations)`~`Min H-index`* `Full or Partial` + `Author at US university`,mainOA.4yr)

covavgh2 <- vcovHC(avgh2, type = "HC")
robust.seavgh2 <- sqrt(diag(covavgh2))
covtoph2 <- vcovHC(toph2, type = "HC")
robust.setoph2 <- sqrt(diag(covtoph2))
covlowh2 <- vcovHC(lowh2, type = "HC")
robust.selowh <- sqrt(diag(covlowh))
covavghp <- vcovHC(avghp, type = "HC")
robust.seavghp <- sqrt(diag(covavghp))
covtophp <- vcovHC(tophp, type = "HC")
robust.setophp <- sqrt(diag(covtophp))
covlowhp <- vcovHC(lowhp, type = "HC")
robust.selowhp <- sqrt(diag(covlowhp))

stargazer(avgh2,toph2, lowh2,avghp2, tophp2, lowhp2,
          title="OLS: Arcsin Citations on Reproducibility, control for region (OA)", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="arcreg3cont:a:OA:rob",
          dep.var.labels = "Total Citations",
          style="aer", type="latex",
          column.sep.width = "-20pt",
          keep.stat = c("n"),
          notes.label = "",
          notes = c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                    footnote.full.partial,
                    footnote.region.us,
                    footnote.sample.attempted),
          out=file.path(Outputs,"table_arcreg3OA2_rob.tex"))



