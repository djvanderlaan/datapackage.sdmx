pkgload::load_all()
library(codelist)


dta <- read_sdmx_codelist("work/ESTAT+CLS_NACE_REV2+1.0.xml")


cl <- as.codelist(dta)


