# Getting in-scope articles
# Code derived from another project
# NOTE: THIS REQUIRES R 4.2.0 and newer packages!
#

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


# we need the raw entry dataset


# Read analysis data
source(file.path(programs,"_read_analysis_data.R"),echo=TRUE)


# get the publication year from the article database

aej_dois <- readRDS(file=openalex.Rds)
nrow(aej_dois)

nrow(aej_dois %>% distinct(DOI))


# Remove NA entries for DOIs
d_entry <- entry %>% filter(!is.na(DOI))
nrow(d_entry)

d_entry %>%
  mutate(original_date = lubridate::floor_date(lubridate::mdy_hms(Timestamp)),
         assignment_year=year(original_date)) %>%
  select(DOI, assignment_year) %>%
  left_join(aej_dois %>% select(DOI,publication_year)) -> assignment.year

assignments <- table(assignment.year$publication_year,
                     assignment.year$assignment_year)
xtable(assignments,
      caption="Assignments by publication year",
      label="tab:assignments") -> assignments.table

# titles

title <- list(pos=list(0), command="")
title$pos[[1]] <- -1
title$command <- c('&\\multicolumn{5}{c}{Assignment Years}\\\\')

print(assignments.table,file=file.path(TexIncludes,"table_assignments.tex"),
      caption.placement = "top",
      add.to.row = title,hline.after = c(0,nrow(assignments.table)))



