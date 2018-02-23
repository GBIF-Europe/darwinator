#!/usr/bin/Rscript

library(darwinator)
library(readr)

dwc_terms

message("storing DWC terms locally in terminator.csv")
write_excel_csv(dwc_terms, "terminator.csv")
