#' Read a SDMX-codelist from a Data Resource
#'
#' @param path path to the data set. 
#' 
#' @param resource a Data Resource.
#'
#' @param standardise convert the result to a format in which data package
#' expect a categories list to be. See the 'value' section for more details.
#'
#' @param lang in case of \code{standardise = TRUE} the language of the labels
#' and descriptions.
#' 
#' @param ... Passes on to \code{standardise_categorieslist_sdmx}.
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
#' Returns a \code{data.frame} with the data. See
#' \code{\link{read_sdmx_codelist}} for the output when 
#' \code{standardise = FALSE}.
#'
#' When \code{standardise = TRUE} the data is transformed into wide format with
#' respect to the languages in the code list. Also the column names are changed
#' to those expected by the \code{datapackage} package.
#'
#' @export
sdmx_codelist_reader <- function(path, resource, standardise = TRUE, lang = getOption("LANG", "EN"), ...) {
  if (length(path) > 1) 
    warning("Path has length > 1; only first element used.")
  res <- read_sdmx_codelist(path[1])
  if (standardise) res <- standardise_categorieslist_sdmx(res, lang = lang, ...)
  structure(res, resource = resource)
}


