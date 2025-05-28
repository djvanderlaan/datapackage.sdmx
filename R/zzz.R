
.onLoad <- function(libname, pkgname) {
  datapackage::dp_add_reader("sdmx-codelist", sdmx_codelist_reader, 
    mediatypes = "application/x-sdmx",
    extensions = "xml")
}

