#' Read a SDMX-codelist from a Data Resource
#'
#' @param path path to the data set. 
#' 
#' @param resource a Data Resource.
#'
#' @seealso
#' Generally used by calling \code{\link[datapackage]{dp_get_data}} from the
#' 'datapackage' package.
#'
#' When the 'datapackage.sdmx' package is loaded the reader for parquet
#' files is registered with the 'datapackage' package. Therefore, when
#' using \code{\link[datapackage]{dp_get_data}} to get the data for a data
#' resource for which the data is stored in an "sdmx-codelist' file the
#' correct reader is automatically used.
#'
#' @return
#' Returns a \code{data.frame} with the data.
#' 
#' @export
sdmx_codelist_reader <- function(path, resource) {
  if (length(path) > 1) 
    warning("Path has length > 1; only first element used.")
  res <- read_sdmx_codelist(path[1])
  structure(res, resource = resource)
}


