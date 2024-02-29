## OLS REGRESSIONS:
#### Create variables for similar OLS regressions as the ones before
### Reproduce the regressions with web of Sciences
### Add author information such as institution and region


source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# footnote text
source(file.path(programs,"_footnotes.R"))

##========================== Pub and author stats ============================


### Clean readback

mainOA           <- readRDS(file=file.path(interwrk,"47_mainOA_complete.Rds"))
biblio.derived   <- readRDS(file=file.path(interwrk,"47_hindex_allyears.Rds"))
hindex.allyears  <- readRDS(file=file.path(interwrk,"47_hindex_allyears.Rds"))
institutions.quantiles <- readRDS(file.path(interwrk,"43_institution_quantiles.Rds"))

works.q33 <- institutions.quantiles %>% filter(quantile =="33%") %>% pull(value)
works.q67 <- institutions.quantiles %>% filter(quantile =="67%") %>% pull(value)



# pretty names
name.mapping     <- read_excel(file.path(auxilloc,"mainOA-mapping.xlsx")) %>%
  # one fix
  mutate(model.varnames = case_when(
    model.varnames == "Data absent: confidential"~ "Confidential data",
    .default = model.varnames
  ))

## Compute paper and author stats


pub_author_stats.doi <- mainOA %>%
  left_join(biblio.derived %>% filter(!is.na(avghindex)) %>%
              filter(years_since_publication <=5)) %>% #filter(!is.na(replicated)) %>%
  # build composite variable
  mutate(Outcome = case_when(
    flag_main_issue_confdata ~  "Confidential data",
    replicated == "No" ~ "Unsuccessful",
    replicated == "Yes" ~ "Successful",
    replicated == "Partial" ~ "Partial",
    flag_entry_confdata ==1 ~ "Confidential data",
    .default = NA))

pub_author_stats <- pub_author_stats.doi %>%
  group_by(Outcome,years_since_publication) %>%
  summarise(`Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(nauthors,na.rm=TRUE),
            `Citations`=mean(new_cites,na.rm=TRUE),
            `Highest experience`  =mean(max_experience,na.rm = TRUE),
            `Institutional productivity`=mean(max_institution_works/10000,na.rm = TRUE),
            `Percent of authors in US` =mean(100*metaregion_US,na.rm=TRUE),
            `N` = n())%>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits=2)

pub_author_stats %>% group_by(years_since_publication) %>%
  summarise(n=sum(N))

pub_author_stats %>% filter(years_since_publication ==4) %>%
  select(-years_since_publication) -> pub_author_stats4

pub_author_stats4 %>%
  transpose(keep.names = "Name") -> tmp
names(tmp) <- as.character(unlist(tmp[1,]))
tmp[2:nrow(tmp),] %>%
  select(Outcome,"Confidential data","Unsuccessful","Partial","Successful") %>%
  rename(` ` = Outcome) -> pub_author_stats4.t
rm(tmp)

pub_author_stats %>% filter(years_since_publication ==0) %>%
  select(-years_since_publication)  -> pub_author_stats0

pub_author_stats0 %>%
  transpose(keep.names = "Name") -> tmp
names(tmp) <- as.character(unlist(tmp[1,]))
tmp[2:nrow(tmp),] %>%
  select(Outcome,"Confidential data","Unsuccessful","Partial","Successful") %>%
  rename(` ` = Outcome) ->pub_author_stats0.t
rm(tmp)

footnote.m1  <- ", except for 1 author dropped for inconsistent OA data."

# Print table
stargazer(pub_author_stats4.t,
          out=file.path(Outputs,"table_metrics_OA.tex"),
          type="latex",title = "Publication and Author Metrics",
          label="tab:metrics:OA",
          style="aer",flip=F, summary=F, rownames = F,
          #font.size = fs,
          column.sep.width = cw,
          digits = 2,
          notes=c(paste0("Notes: All assessed articles",footnote.m1),
                  "Author and institutional characteristics are measured 4 years after publication, ",
                  "due to limitations in the OA data availability.",
                  footnote.prod1k))

stargazer(pub_author_stats0.t,
          out=file.path(Outputs,"table_metrics_OA_app0.tex"),
          type="latex",title = "Publication and Author Metrics (2012+)",
          label="tab:metrics:OA:zero",
          style="aer",flip=F, summary=F, rownames = F,
          #font.size = fs,
          column.sep.width = cw,
          notes=c("Notes: All assessed articles published after 2011, except for 1 author dropped ",
                  "for inconsistent OA data.with . Author and institutional characteristics are measured ",
                  "in the year of publication. ",
                  footnote.prod1k))

# stargazer(pub_author_stats4.t %>% select(-),
#           out=file.path(Outputs,"table_metrics_OA_app.tex"),
#           type="latex",title = "Publication and Author Metrics (no confidential data)",
#           label="tab:metrics:OA:noconf",
#           style="aer",flip=T, summary=F, rownames = F,
#           #font.size = fs,
#           column.sep.width = cw,
#           notes=c("Notes: All articles for which reproduction was attempted.",
#                   footnote.prod1k))

#### Some extra numbers
nauthors   <-round(weighted.mean(pub_author_stats4$`Number of Authors`,pub_author_stats4$N),2)
ncite      <-round(weighted.mean(pub_author_stats4$`Citations`,pub_author_stats4$N),2)
navghindex <-round(weighted.mean(pub_author_stats4$`Avg h-index`,pub_author_stats4$N),2)
nexperience<-round(weighted.mean(pub_author_stats4$`Highest experience`,pub_author_stats4$N),2)
nproductvt <-round(weighted.mean(pub_author_stats4$`Institutional productivity`,pub_author_stats4$N),0)*10000


cat(nauthors, 			file=file.path(TexIncludes,"nauthorsOA.tex"))
cat(ncite, 					file=file.path(TexIncludes,"nciteOA.tex"))
cat(navghindex, 		file=file.path(TexIncludes,"navghindexOA.tex"))
cat(nexperience, 		file=file.path(TexIncludes,"nexperienceOA.tex"))
cat(nproductvt, 		file=file.path(TexIncludes,"nproductvtOA.tex"))



#############################################################################
# Compute stats for articles that were in the WoS data.

pub_author_stats.doi.wos <- readRDS(file=file.path(interwrk,"31_authorstats_doi.Rds"))

# merge the two

combined_wos_oa <- left_join(pub_author_stats.doi.wos %>%distinct(DOI) %>%
                               mutate(src="wos"),
                             pub_author_stats.doi)

pub_author_stats.wos <- combined_wos_oa %>%
  group_by(Outcome,years_since_publication) %>%
  summarise(`Avg h-index`=mean(avghindex,na.rm=TRUE),
            `Lowest h-index`=mean(lowhindex,na.rm=TRUE),
            `Number of Authors`=mean(nauthors,na.rm=TRUE),
            `Citations`=mean(new_cites,na.rm=TRUE),
            `Highest experience`  =mean(max_experience,na.rm = TRUE),
            `Institutional productivity`=mean(max_institution_works/10000,na.rm = TRUE),
            `Percent of authors in US` =mean(100*metaregion_US,na.rm=TRUE),
            `N` = n())%>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits=2)

pub_author_stats.wos %>% filter(years_since_publication ==4) %>%
  select(-years_since_publication) -> pub_author_stats4.wos

pub_author_stats4.wos %>%
  transpose(keep.names = "Name") -> tmp
names(tmp) <- as.character(unlist(tmp[1,]))
tmp[2:nrow(tmp),] %>%
  select(Outcome,"Unsuccessful","Partial","Successful") %>%
  rename(` ` = Outcome) -> pub_author_stats4.wos.t
rm(tmp)

stargazer(pub_author_stats4.wos.t,
          out=file.path(Outputs,"table_metrics_OA_wos.tex"),
          type="latex",title = "Publication and Author Metrics for WoS sample",
          label="tab:metrics:OA:WoS",
          style="aer",flip=F, summary=F, rownames = F,
          #font.size = fs,
          column.sep.width = cw,
          digits = 2,
          notes=c("Notes: Assessed articles matched to Web of Science database extract. Author ",
                  "and institutional characteristics are measured 4 years after publication.",
                  footnote.prod1k))


#############################################################################
# table of ratios
t1 <- mainOA %>% select('Fully reproduced','Full or Partial') %>%
  summarise(`Number of Articles` = n(),
            `n_full`=
              percent(sum(`Fully reproduced`,
                                                     na.rm=TRUE)/ n(),
                                                 digits=2),
            `n_partial`=
              percent(sum(`Full or Partial`,na.rm=TRUE)/ n(),
                          digits=2)) %>%
  mutate(Denominator = "$d_assessed$") %>%
  select(Denominator,everything())
t1

t2 <- mainOA %>% filter(!is.na(replicated)) %>%
  summarise(`Number of Articles` = n(),
            `n_full`=
              percent(sum(`Fully reproduced`,
                          na.rm=TRUE)/ n(),
                      digits=2),
            `n_partial`=
              percent(sum(`Full or Partial`,na.rm=TRUE)/ n(),
                      digits=2)) %>%
  mutate(Denominator = "$d_attempted$") %>%
  select(Denominator,everything())
t2

t3 <- mainOA %>% filter(!is.na(replicated)) %>%
  filter(!flag_main_issue_confdata) %>%
  summarise(`Number of Articles` = n(),
            `n_full`=
              percent(sum(`Fully reproduced`,
                          na.rm=TRUE)/ n(),
                      digits=2),
            `n_partial`=
              percent(sum(`Full or Partial`,na.rm=TRUE)/ n(),
                      digits=2)) %>%
  mutate(Denominator = "$d_nonconfidential$") %>%
  select(Denominator,everything())
t3

t_ratiosum <- rbind(t1,t2,t3) %>%
  select(Denominator,`Number of Articles`,`n_full (%)`=n_full,`n_partial (%)`=n_partial)



stargazer(t_ratiosum,
          title="Reproduction Rates", align=TRUE,no.space=TRUE,
          notes.align = "l", notes.append = FALSE,
          label="tab:ratios",
          style="aer",
          flip=FALSE,summary=FALSE, rownames = FALSE,
          #font.size = fs,
          column.sep.width = cw,
          notes = c("\\footnotesize Notes: ", footnote.full.partial),
          out=file.path(Outputs,"table_ratios.tex"))


##### Run probits on Full or (Full + partial) reproducibility



# Run Regression: total citations on h-indices and confidential data
# We're going to start by running the analysis on the sub-sample
# with data in year0.


mainOA.0yr <- mainOA %>%
  # first, get the citations in Year 4
  left_join(hindex.allyears %>%
              filter(years_since_publication == 0 ) ) %>%
  filter(year>2011) %>%
  mutate(log_ytd_cites = log(ytd_cites+1),
         asin_ytd_cites = asinh(ytd_cites))
nrow(mainOA.0yr)

## Same for year 4

mainOA.4yr <- mainOA %>%
  # first, get the citations in Year 4
  left_join(hindex.allyears %>%
              filter(years_since_publication == 4 ) ) %>%
  mutate(log_ytd_cites = log(ytd_cites+1),
         asin_ytd_cites = asinh(ytd_cites))
nrow(mainOA.4yr)

run_probits <- function(data,number,.footnotes=c(footnote.base)) {

  # pretty names


  # pretty names
  names.main <- names(data) %>%
    as_tibble() %>%
    rename(names=value)
  left_join(names.main,name.mapping) %>%
    mutate(model.varnames = if_else(is.na(model.varnames),names,model.varnames)) ->
    mainOA.mapping
  mainOA.mapping %>% filter(is.na(model.varnames)) -> test.na
  if ( nrow(test.na) > 0 ) {
    print(test.na)
    stop("Error in variable name mapping")
  }

  # assign varnames by renaming
  names(data) <- mainOA.mapping$model.varnames
  names(data)


  # probit data

  probit.data <- data %>%
    filter(`Reproduction failed: confidential data`==0 &
             `Confidential data`==0) %>%
    mutate(`Solo-authored` = (`Number of authors`==1),
           `Institution Publications (bottom)`= (`Highest Institution Publications` < works.q33),
           `Institution Publications (top)`= (`Highest Institution Publications` > works.q67),
           `Highest Institution Publications` = `Highest Institution Publications`/10000)
  message(paste0("Number of obs in regression data: ",nrow(probit.data)))

  probit.full.avgh <- glm(`Fully reproduced`~`Avg. H-index` +
                            `Highest Institution Publications` +
                            `Highest Co-author Experience` +
                            `Number of authors` +
                            `Author at US university`,
                          data = probit.data,
                          family = binomial(link = "probit"))
  probit.full.maxh <- glm(`Fully reproduced`~`Max H-index` +
                            `Highest Institution Publications` +
                            `Highest Co-author Experience` +
                            `Number of authors` +
                            `Author at US university`,
                          data = probit.data,
                          family = binomial(link = "probit"))
  probit.full.minh <- glm(`Fully reproduced`~`Min H-index` +
                            `Highest Institution Publications` +
                            `Highest Co-author Experience` +
                            `Number of authors` +
                            `Author at US university`,
                          data = probit.data,
                          family = binomial(link = "probit"))
  probit.full.avgh.solo <- glm(`Fully reproduced`~`Avg. H-index` +
                                 `Institution Publications (top)` +
                                 `Institution Publications (bottom)` +
                                 `Highest Co-author Experience` +
                                 `Number of authors` +
                                 `Author at US university` +
                                 `Solo-authored`,
                               data = probit.data,
                               family = binomial(link = "probit"))
  probit.full.kitchen.sink <- glm(`Fully reproduced`~`Avg. H-index` +
                                    `Max H-index` +
                                    `Min H-index` +
                                    `Institution Publications (top)` +
                                    `Institution Publications (bottom)` +
                                    `Highest Co-author Experience` +
                                    `Number of authors` +
                                    `Author at US university` +
                                    `Solo-authored`,
                                  data = probit.data,
                                  family = binomial(link = "probit"))


  probit.fullpartial.avgh <- glm(`Full or Partial`~`Avg. H-index` +
                                   `Highest Institution Publications` +
                                   `Highest Co-author Experience` +
                                   `Number of authors` +
                                   `Author at US university`,
                                 data = probit.data,
                                 family = binomial(link = "probit"))
  probit.fullpartial.maxh <- glm(`Full or Partial`~`Max H-index` +
                                   `Highest Institution Publications` +
                                   `Highest Co-author Experience` +
                                   `Number of authors` +
                                   `Author at US university`,
                                 data = probit.data,
                                 family = binomial(link = "probit"))
  probit.fullpartial.minh <- glm(`Full or Partial`~`Min H-index` +
                                   `Highest Institution Publications` +
                                   `Highest Co-author Experience` +
                                   `Number of authors` +
                                   `Author at US university`,
                                 data = probit.data,
                                 family = binomial(link = "probit"))
  probit.fullpartial.avgh.solo <- glm(`Full or Partial`~`Avg. H-index` +
                                        `Institution Publications (top)` +
                                        `Institution Publications (bottom)` +
                                        `Highest Co-author Experience` +
                                        `Number of authors` +
                                        `Author at US university` +
                                        `Solo-authored`,
                                      data = probit.data,
                                      family = binomial(link = "probit"))
  probit.fullpartial.kitchen.sink <- glm(`Full or Partial`~`Avg. H-index` +
                                           `Max H-index` +
                                           `Min H-index` +
                                           `Institution Publications (top)` +
                                           `Institution Publications (bottom)` +
                                           `Highest Co-author Experience` +
                                           `Number of authors` +
                                           `Author at US university` +
                                           `Solo-authored`,
                                         data = probit.data,
                                         family = binomial(link = "probit"))


  #Full reproducibility
  stargazer(probit.full.avgh,probit.full.maxh,probit.full.minh,
            probit.full.kitchen.sink,
            title=paste0("Probit: Determinants of Reproducibility, Year ",number),
            align=TRUE,no.space=TRUE,
            notes.append = FALSE,
            label=paste0("reg:probit:reproducibility:full:",number),
            dep.var.labels = "Outcome: Full Reproduction",
            style="qje", type="latex",
            column.sep.width = "-15pt",
            keep.stat = c("n"),
            out=file.path(Outputs,
                          paste0("table_reg_probit_",number,"_full.tex")),
            notes.label = "",
            notes.align="l",
            notes = .footnotes
  )

  stargazer(probit.fullpartial.avgh,probit.fullpartial.maxh,#probit.fullpartial.minh,
            probit.fullpartial.kitchen.sink,
            title=paste0("Probit: Determinants of Reproducibility, Year ",number),
            align=TRUE,no.space=TRUE,
            notes.append = FALSE,
            label=paste0("reg:probit:reproducibility:fullpartial:",number),
            dep.var.labels = "Outcome: Full or Partial Reproduction",
            style="qje", type="latex",
            column.sep.width = "-15pt",
            keep.stat = c("n"),
            out=file.path(Outputs,
                         paste0("table_reg_probit_",number,"_fullpartial.tex")),
            notes.label = "",
            notes.align="l",
            notes = .footnotes
)

}

# run probits for year 0

run_probits(mainOA.0yr,0,.footnotes = c(footnote.base,footnote.post2012))
run_probits(mainOA.4yr,4)
