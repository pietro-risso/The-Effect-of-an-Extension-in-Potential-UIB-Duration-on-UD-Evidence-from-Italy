This repository contains the two R files for the paper "The Effect of an Extension in Potential Unemployment Insurance Benefits Duration on Unemployment Duration: Evidence from Italy".

The folder RCFL_RAW contains the R file used for the data tidying and merging. The data used for the analysis is ISTAT's Rilevazione Continua sulle Forze di Lavoro (RCFL) from 2014 to 2022. The raw data files can be publicly downloaded here (https://www.istat.it/it/archivio/127792). In my analysis, I renamed the raw files as follows: RCFL_YEAR_QUARTER.txt. The R file data-clening.R uses such naming.

Finally, the repository RCFL_ANALYSIS contains the R file used to create the tables and graphs in the paper and the cleaned data.
