#' Read a code list from a SDMX file
#'
#' @param path to file
#'
#' @return
#' Returns a \code{data.frame} with the code list with the following columns:
#'
#' \item{id}{A character column with the codes/values/id's.}
#'
#' \item{name}{A character column with the labels/names of the codes. }
#'
#' \item{description}{The description of the code. } 
#'
#' \item{lang}{The language of the label and description.}
#' 
#' \item{parent}{The parent code of the code. This defines a hierarchical code
#' list. Values can be missing. This either means that the code is a top-level
#' code or, when all parents are missing, that the code list does not define a
#' hierarchy.}
#'
#' @export
read_sdmx_codelist <- function(path) {
  res <- extract_codelist_from_sdmx(path)
  res <- fill_languages(res)
  res <- res[c("id", "name", "description", "lang", "parent")]
  #names(res) <- c("value", "label", "description", "locale", "parent")
  res
}


find_child <- function(parent, child_name) {
  if (length(child_name) > 1) {
    node <- find_child(parent, child_name[1])
    if (is.null(node)) {
      node
    } else {
      find_child(node, utils::tail(child_name, -1))
    }
  } else {
    child_name <- tolower(child_name)
    children <- xml2::xml_children(parent)
    for (i in seq_along(children)) {
      if (tolower(xml2::xml_name(children[[i]])) == child_name) return(children[[i]])
    }
    NULL
  }
}

get_locale_strings <- function(parent, child_name) {
  strings <- lapply(xml2::xml_children(parent), function(node) {
    if (tolower(xml2::xml_name(node)) == tolower(child_name)) {
      lang <- xml2::xml_attr(node, "lang")
      value <- xml2::xml_contents(node) |> as.character()
      data.frame(value = value, lang = lang)
    } else {
      NULL
    }
  })
  res <- do.call(rbind, strings)
  if (!is.null(res)) names(res) <- c(child_name, "lang")
  res
}

parse_code <- function(node) {
  id <- xml2::xml_attr(node, "id")
  if (is.na(id)) id <- xml2::xml_attr(node, "value")
  if (is.na(id)) stop("Could not find id of code.")
  names <- get_locale_strings(node, "Name")
  descriptions <- get_locale_strings(node, "Description")
  names <- if (is.null(names)) descriptions else names
  names(names) <- c("Name", "lang")
  descriptions <- if (is.null(descriptions)) names else descriptions
  names(descriptions) <- c("Description", "lang")
  if (is.null(names) && is.null(descriptions)) {
    res <- data.frame(name = id, description = id, lang = "")
  } else {
    res <- merge(names, descriptions, by = "lang")
  }
  parent <- find_child(node, c("Parent", "Ref"))
  parent <- ifelse(is.null(parent), NA_character_, xml2::xml_attr(parent, "id"))
  res$parent <- parent
  res$id <- id
  names(res) <- tolower(names(res))
  res[, c("id", "name", "description", "lang", "parent")]
}

extract_codelist_from_sdmx <- function(path) {
  doc <- xml2::read_xml(path)
  codelist <- find_child(doc, c("Structures", "Codelists", "Codelist"))
  if (is.null(codelist)) {
    codelist <- find_child(doc, c("Codelists", "Codelist"))
  }
  if (is.null(codelist)) {
    stop("Could not find a codelist in the document.")
  }
  codes <- lapply(xml2::xml_children(codelist), function(node) {
    if (xml2::xml_name(node) == "Code") {
      parse_code(node)
    } else {
      NULL
    }
  })
  codes <- do.call(rbind, codes)
  codes
}

fill_languages <- function(codelist) {
  lang <- name <- NULL
  if (anyNA(codelist$name) || anyNA(codelist$description)) 
    stop("There are missing values in 'name' and/or 'description'.")
  tmp <- expand.grid(id = unique(codelist$id), lang = unique(codelist$lang),
    stringsAsFactors = FALSE)
  tmp <- merge(tmp, codelist, by = c("id", "lang"), all = TRUE)
  # Try if we can substitute missing names and descriptions with english ones
  langs <- unique(codelist$lang)
  if ((anyNA(tmp$name) || anyNA(tmp$description)) && ("en" %in% tolower(langs))) {
    warning("Some languages are missing for some of the codes. ", 
      "Trying to find English labels and descriptions.")
    m <- subset(tmp, tolower(lang) == "en", select = c("id", "name", "description"))
    tmp <- merge(tmp, m, by = "id", suffixes = c("", "_en"), all = TRUE)
    tmp$name[is.na(tmp$name)] <- tmp$name_en[is.na(tmp$name)]
    tmp$description[is.na(tmp$description)] <- tmp$description_en[is.na(tmp$description)]
    tmp$name_en <- NULL
    tmp$description_en <- NULL
  }
  # Try to substitue missing names and descriptions with first non-missing one
  if (anyNA(tmp$name) || anyNA(tmp$description)) {
    warning("English labels and descriptions could not be found. Trying other languages.")
    m <- subset(tmp, !is.na(name), select = c("id", "name", "description"))
    m <- m[!duplicated(subset(m, select = "id")), ]
    tmp <- merge(tmp, m, by = "id", all = TRUE, suffixes = c("", "_en"))
    tmp$name[is.na(tmp$name)] <- tmp$name_en[is.na(tmp$name)]
    tmp$description[is.na(tmp$description)] <- tmp$description_en[is.na(tmp$description)]
    tmp$name_en <- NULL
    tmp$description_en <- NULL
  }
  if (anyNA(tmp$name) || anyNA(tmp$description)) 
    stop("There are still missing values in 'name' and/or 'description'.")
  tmp
}
