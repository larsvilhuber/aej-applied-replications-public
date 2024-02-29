#' ---
#' title: "Preparing the H-index data"
#' author: "Lars Vilhuber"
#' date: "March 30, 2017"
#' output:
#'    html_document:
#'       highlight: tango
#'       keep_md: yes
#'       theme: readable
#'       toc: yes
#' ---

source(file.path(rprojroot::find_rstudio_root_file(),"pathconfig.R"),echo=FALSE)
source(file.path(basepath,"global-libraries.R"),echo=FALSE)
source(file.path(programs,"libraries.R"), echo=FALSE)
source(file.path(programs,"config.R"), echo=FALSE)


#' ## Get the h-index data
#' The data is originally in a Google Spreadsheet. We downloaded it by hand from there.
#'
#+ data prep
h.index <- read.csv(paste(hindexloc,HindexRaw,sep="/"),header = TRUE)

# Reformat the somewhat quirky data
h.index.melt <- reshape2::melt(data = h.index, id.vars=c('DOI','Total.Citations','Average.per.Year'),
                     measure.vars=c('Author.1','Author.2','Author.3','Author.4','Author.5','Author.6')
                     )
# Rename some of the columns from (variable, value)
names(h.index.melt)[4:5] <- c("Author.Order","Author.Name")
# Remove the "Author." prefix
h.index.melt$Author.Order <- sub("Author.([1-6])","\\1",h.index.melt$Author.Order,perl = TRUE)

# Now do the same thing with the second set of variables to obtain the per-author h-index
h.index.melt2 <- reshape2::melt(data = h.index, id.vars=c('Title','DOI','Total.Citations','Average.per.Year'),
                      measure.vars=c('h_index..Author.1.','h_index..Author.2.','h_index..Author.3.','h_index..Author.4.','h_index..Author.5.','h_index..Author.6.')
                      )
names(h.index.melt2)[5:6] <- c("Author.Order","h-index")
h.index.melt2$Author.Order <- sub("h_index..Author.([1-6]).","\\1",h.index.melt2$Author.Order,perl = TRUE)

# merge them back together to obtain the final file
h.index.clean <- merge(h.index.melt[which(h.index.melt$DOI != ""
                                     & h.index.melt$Author.Name != ""),],
                  h.index.melt2[which(h.index.melt2$DOI != ""),])

#' ## Compute some statistics
#' How many authors
nrow(h.index.clean)
#' How many UNIQUE authors
#+ unique_authors
authors.publications <- as.data.frame(table(h.index.clean$Author.Name))
names(authors.publications)[1] <- "Author.Name"
nrow(authors.publications)
#' Of which x have published more than once
#+ dups
multiply.published <- authors.publications[which(authors.publications[,2]>1),]
#' Thus, `r nrow(authors.publications)` have published `r nrow(h.index)` articles, of which `r nrow(multiply.published)`
#' authors have published multiple times.
#'
#' ## More  statistics
#'
#' - The original data is unduplicated by article
#' - The modified data has duplicated authors
#' - data.authors has unduplicated author information
#'
#' ### mean, median, max for h-index
#+ stats_authors
data.authors <- h.index.clean[!duplicated(h.index.clean$Author.Name),c("Author.Name","h-index")]
summary(data.authors[,2])[c(3,4,6)]
hist(data.authors[,2],
     main = "h-index",
     xlab = "",
     breaks = max(data.authors[,2],na.rm = TRUE)
     )
#' mean number of citations of articles, not adjusted for publication year
#+ stats_papers
summary(h.index$Total.Citations,na.rm = TRUE)[c(3,4,6)]
hist(h.index$Total.Citations,
     main = "Total number of citations",
     xlab = "",
     max(h.index$Total.Citations,na.rm = TRUE)
     )

#' ## Save stuff
write.csv(h.index.clean, paste(Outputs,HindexClean,sep="/"), row.names=FALSE)
