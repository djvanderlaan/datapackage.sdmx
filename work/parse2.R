requireNamespace("xml2")

source("R/sdmx_codelist_reader.R")

dta <- read_sdmx_codelist("work/ESTAT+CLS_NACE_REV2+1.0.xml")


path <- "work/ESTAT+CLS_NACE_REV2+1.0.xml"

library(codelist)

cl <- as.codelist(dta)

