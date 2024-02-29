### R code from vignette source 'appendix.Rnw'

###################################################
### code chunk number 1: set-parent-app
###################################################
#set_parent('Replication_aejae.Rnw')
#set_parent('results1.Rnw')
#set_parent('results2.Rnw')
#load("../data/interwrk/my_work_space.RData")


###################################################
### code chunk number 2: appendix.Rnw:9-13
###################################################

# Q : where do these numbers come from? Need to be computed further up. 
# A: I replicated thistable but we do not need this anymore
#if ( file.exists(file.path(interwrk,"table-sample.Rds"))) {

#  sample <- readRDS(file.path(interwrk,"table-sample.Rds"))
  #sample <- matrix(c(303,274,209,180,162),ncol=1,byrow=TRUE)

#  colnames(sample) <-c("Count")
#  rownames(sample) <- c("Assessed articles","Assessed with complete records", "Articles with non-confidential / non missing data", "Eligible articles with non confidential data, complete and unique records)","Amenable for replication, after removing confidential data articles identified during replication")
#sample <- as.table(sample)


###################################################
### code chunk number 3: tabappeendix
###################################################
# Print table
# stargazer(sample,
#           type="latex",title = "Summary of data",
#           label="tab:appendix",
#           out=file.path(Outputs,"table_appendix.tex"),
#           style="aer",align = TRUE,
#           flip=FALSE,summary=FALSE,
#           rownames = TRUE,
#           font.size = fs,column.sep.width = cw)
# 
# } else {
#   message("Did not find sample statistics")
# }
