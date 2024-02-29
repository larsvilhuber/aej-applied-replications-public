## OLS REGRESSIONS:
#### Create variables for similar OLS regressions as the ones before
### Reproduce the regressions with Poisson instead


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


### repeated footnotes ============================================================

source(file.path(programs,"_footnotes.R"))



mainOA.4yr <- readRDS(file.path(interwrk,"50_mainOA_4yr.Rds"))
mainOA.4yrpost2012 <- readRDS(file.path(interwrk,"50_mainOA_4yr_p2012.Rds"))

# Run regression


lhs.level = "`YTD citations`"
kitchensink.spec = "`Highest Institution Publications` +  `Highest Co-author Experience` +
             `Number of authors` + `Author at US university` +`Solo-authored`"

pavgh <- glm(as.formula(paste0(lhs.level," ~`Avg. H-index`*`Fully reproduced` +",
                               kitchensink.spec)),
             family="poisson", data=mainOA.4yr)
ptoph <- glm(as.formula(paste0(lhs.level," ~ `Max H-index`*`Fully reproduced` +",
                               kitchensink.spec)),
             family="poisson", data=mainOA.4yr)
plowh <- glm(as.formula(paste0(lhs.level," ~ `Min H-index`*`Fully reproduced` +",
                               kitchensink.spec)),
             family="poisson", data=mainOA.4yr)
p.kitchen.sink <- glm(as.formula(paste0(lhs.level," ~ `Avg. H-index`*`Fully reproduced` +",
                                        "`Max H-index`*`Fully reproduced` +",
                                        "`Min H-index`*`Fully reproduced` +",
                                        kitchensink.spec)),
             family="poisson", data=mainOA.4yr)



# weird stuff

names(p.kitchen.sink$coefficients)[names(p.kitchen.sink$coefficients) ==
                                     "`Fully reproduced`:`Max H-index`"] <- "`Max H-index`:`Fully reproduced`"
names(p.kitchen.sink$coefficients)[names(p.kitchen.sink$coefficients) ==
                                     "`Fully reproduced`:`Min H-index`"] <- "`Min H-index`:`Fully reproduced`"

# SE
cov.pavgh <- vcovHC(pavgh , type="HC1")
std.errpavgh <- sqrt(diag(cov.pavgh ))
r.estpavgh <- cbind(Estimate= coef(pavgh ), "Robust SE" = std.errpavgh,
                    "Pr(>|z|)" = 2 * pnorm(abs(coef(pavgh )/std.errpavgh), lower.tail=FALSE))

cov.ptoph <- vcovHC(ptoph , type="HC1")
std.errtoph <- sqrt(diag(cov.ptoph ))
r.esttoph <- cbind(Estimate= coef(ptoph ), "Robust SE" = std.errtoph,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(ptoph )/std.errtoph), lower.tail=FALSE))

cov.plowh <- vcovHC(plowh , type="HC1")
std.errlowh <- sqrt(diag(cov.plowh ))
r.estlowh <- cbind(Estimate= coef(plowh ), "Robust SE" = std.errlowh,
                   "Pr(>|z|)" = 2 * pnorm(abs(coef(plowh )/std.errlowh), lower.tail=FALSE))

cov.p.kitchen.sink <- vcovHC(p.kitchen.sink , type="HC1")
std.errp.kitchen.sink <- sqrt(diag(cov.p.kitchen.sink ))
r.estp.kitchen.sink <- cbind(Estimate= coef(p.kitchen.sink ), "Robust SE" = std.errp.kitchen.sink,
                     "Pr(>|z|)" = 2 * pnorm(abs(coef(p.kitchen.sink )/std.errp.kitchen.sink), lower.tail=FALSE))




stargazer(pavgh,ptoph,plowh,p.kitchen.sink,
          type = "text",
          se = list(NULL, r.estpavgh,r.esttoph,r.estlowh,r.estp.kitchen.sink),
          keep.stat = c("adj.rsq","n"),
          title="Poisson: YTD Citations on Reproduction Outcomes", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,label="poisson:OA",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer",
          column.sep.width = "-20pt",
          out=file.path(Outputs,"table_poissonreg.tex"),
          notes=c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                  footnote.full.partial,footnote.sample.attempted,footnote.HC),
          notes.label = "")


## Partial



pavghp <- glm(as.formula(paste0(lhs.level," ~  `Avg. H-index`*`Full or Partial` +",
                                kitchensink.spec)),
              family="poisson", data=mainOA.4yr)
ptophp <- glm(as.formula(paste0(lhs.level," ~ `Max H-index`*`Full or Partial` +",
                                kitchensink.spec)),
              family="poisson", data=mainOA.4yr)
plowhp <- glm(as.formula(paste0(lhs.level," ~ `Min H-index`*`Full or Partial` +",
                                kitchensink.spec)),
              family="poisson", data=mainOA.4yr)
p.kitchen.sink.p <- glm(as.formula(paste0(lhs.level," ~ `Avg. H-index`*`Full or Partial` +",
                                          "`Max H-index`*`Full or Partial` +",
                                          "`Min H-index`*`Full or Partial` +",
                                          kitchensink.spec)),
                        family="poisson", data=mainOA.4yr)


# weird stuff

names(p.kitchen.sink.p$coefficients)[names(p.kitchen.sink.p$coefficients) ==
                                       "`Full or Partial`:`Max H-index`"] <- "`Max H-index`:`Full or Partial`"
names(p.kitchen.sink.p$coefficients)[names(p.kitchen.sink.p$coefficients) ==
                                       "`Full or Partial`:`Min H-index`"] <- "`Min H-index`:`Full or Partial`"

# SE
cov.pavghp <- vcovHC(pavghp , type="HC1")
std.errpavghp <- sqrt(diag(cov.pavghp ))
r.estpavghp <- cbind(Estimate= coef(pavghp ), "Robust SE" = std.errpavghp,
                     "Pr(>|z|)" = 2 * pnorm(abs(coef(pavghp )/std.errpavghp), lower.tail=FALSE))

cov.ptophp <- vcovHC(ptophp , type="HC1")
std.errtophp <- sqrt(diag(cov.ptophp ))
r.esttophp <- cbind(Estimate= coef(ptophp ), "Robust SE" = std.errtophp,
                    "Pr(>|z|)" = 2 * pnorm(abs(coef(ptophp )/std.errtophp), lower.tail=FALSE))

cov.plowhp <- vcovHC(plowhp , type="HC1")
std.errlowhp <- sqrt(diag(cov.plowhp ))
r.estlowhp <- cbind(Estimate= coef(plowhp ), "Robust SE" = std.errlowhp,
                    "Pr(>|z|)" = 2 * pnorm(abs(coef(plowhp )/std.errlowhp), lower.tail=FALSE))
cov.p.kitchen.sink.p <- vcovHC(p.kitchen.sink.p , type="HC1")
std.errp.kitchen.sink.p <- sqrt(diag(cov.p.kitchen.sink.p ))
r.estp.kitchen.sink.p <- cbind(Estimate= coef(p.kitchen.sink.p ), "Robust SE" = std.errp.kitchen.sink.p,
                               "Pr(>|z|)" = 2 * pnorm(abs(coef(p.kitchen.sink.p )/std.errp.kitchen.sink.p), lower.tail=FALSE))

stargazer(pavghp,ptophp,plowhp,p.kitchen.sink.p,
          type = "text",
          se = list(NULL, r.estpavghp, r.esttophp,r.estlowhp,r.estp.kitchen.sink.p),
          keep.stat = c("n"),
          title="Poisson: YTD Citations on Reproduction Outcomes", align=TRUE,no.space=FALSE,
          notes.align = "l", notes.append = FALSE,
          label="poisson:OA:partial",
          dep.var.labels.include = FALSE,
          #dep.var.labels = "Total Citations",
          style="aer",
          column.sep.width = "-20pt",
          out=file.path(Outputs,"table_poissonreg_partial.tex"),
          notes=c(footnote.base,"Notes: ",footnote.ytd4,footnote.lag.hindex,
                  footnote.full.partial,footnote.sample.attempted,footnote.HC),
          notes.label = "")
