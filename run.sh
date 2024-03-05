#!/bin/bash
#
# Bash script to run all the paper analyses
# Alternatively:
# - open the "replication-paper.Rproj" in Rstudio, and "knit" the README.Rmd

cd programs

R CMD BATCH 01_download_replication_data.R
#R CMD BATCH 02_get_crossref.R
R CMD BATCH 04_clean_replicationlist.R
#R CMD BATCH 06_gen_hindex_list.R
R CMD BATCH 07_readclean_hindex_list.R
#R CMD BATCH 08_get_crossref_bibs.R
R CMD BATCH 25_prepare_sample.R
R CMD BATCH 30_results1.R
R CMD BATCH 31_results2.R
R CMD BATCH 32_conclusion.R
R CMD BATCH 35_appendix.R
R CMD BATCH 36_list-articles.R
R CMD BATCH 40_supplementary1.R
R CMD BATCH 41_supplementary2.R
R CMD BATCH 42_prepare_authors.R
R CMD BATCH 43_supplementary4.R
R CMD BATCH 44_supplementary_authexp.R
R CMD BATCH 45_supplementary_hindex.R
R CMD BATCH 46_prepare_analysis.R
R CMD BATCH 47_prepare_mainOA.R
R CMD BATCH 48_mainOA_authorpaper_stats.R
R CMD BATCH 49_assignments.R
R CMD BATCH 50_analysis_openAlex.R
R CMD BATCH 51_analysis_Poisson.R
R CMD BATCH 52_robustess_standarderrors.R
R CMD BATCH 99_write_nums.R
R CMD BATCH 99_zz_info.R
