requireNamespace("xml2")


find_child <- function(parent, child_name) {
  if (length(child_name) > 1) {
    node <- find_child(parent, child_name[1])
    if (is.null(node)) {
      node
    } else {
      find_child(node, utils::tail(child_name, -1))
    }
  } else {
    children <- xml2::xml_children(parent)
    for (i in seq_along(children)) {
      if (xml2::xml_name(children[[i]]) == child_name) return(children[[i]])
    }
    NULL
  }
}

get_locale_strings <- function(parent, child_name) {
  strings <- lapply(xml2::xml_children(parent), function(node) {
    if (xml2::xml_name(node) == child_name) {
      lang <- xml2::xml_attr(node, "lang")
      value <- xml2::xml_contents(node) |> as.character()
      data.frame(value = value, lang = lang)
    } else {
      NULL
    }
  })
  res <- do.call(rbind, strings)
  names(res) <- c(child_name, "lang")
  res
}

parse_code <- function(node) {
  names <- get_locale_strings(node, "Name")
  descriptions <- get_locale_strings(node, "Description")
  res <- merge(names, descriptions, by = "lang")
  parent <- find_child(node, c("Parent", "Ref"))
  parent <- ifelse(is.null(parent), NA_character_, xml2::xml_attr(parent, "id"))
  res$parent <- parent
  res$id <- xml2::xml_attr(node, "id")
  names(res) <- tolower(names(res))
  res[, c("id", "name", "description", "lang", "parent")]
}

extract_codelist_from_sdmx <- function(path) {
  doc <- xml2::read_xml(path)
  codelist <- find_child(doc, c("Structures", "Codelists", "Codelist"))
  if (is.null(codelist)) 
    stop("Could not find a codelist in the document.")
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

library(data.table)

url <- "https://classificaties-rest.cbsp.nl/api/v1/Downloads/codelist/126105d5-5b62-753a-e7b5-7bae99ea6cff?fileType=sdmx"
codelist <- extract_codelist_from_sdmx(url)
setDT(codelist)


# https://webgate.ec.europa.eu/sdmxrr/rest/codelist/ESTAT/CLS_NACE_REV2/1.0
codelist <- extract_codelist_from_sdmx("ESTAT+CLS_NACE_REV2+1.0.xml")
setDT(codelist)

setkey(codelist, id, lang)

tmp <- codelist[CJ(id, lang, unique=TRUE)]

tmp

nrow(tmp)
nrow(codelist)

tmp <- expand.grid(id = unique(codelist$id), name = unique(codelist$name))
tmp <- merge(tmp, codelist, by = c("id", "name"), all = TRUE)

# Try if we can substitute missing names and descriptions with english ones
langs <- unique(codelist$lang)
if ((anyNA(tmp$name) || anyNA(tmp$description)) && ("en" %in% tolower(langs))) {
  m <- subset(tmp, tolower(lang) == "en", select = c("id", "name", "description"))
  tmp <- merge(tmp, m, by = "id", suffixes = c("", "_en"), all = TRUE)
  tmp$name[is.na(tmp$name)] <- tmp$name_en[is.na(tmp$name)]
  tmp$description[is.na(tmp$description)] <- tmp$description_en[is.na(tmp$description)]
  tmp$name_en <- NULL
  tmp$description_en <- NULL
}
# Try to substitue missing names and descriptions with first non-missing one
if (anyNA(tmp$name) || anyNA(tmp$description)) {
  m <- subset(tmp, !is.na(name), select = c("id", "name", "description"))
  m <- m[!duplicated(subset(d, select = "id")), ]
  tmp <- merge(tmp, m, by = "id", all = TRUE, suffixes = c("", "_en"))
  tmp$name[is.na(tmp$name)] <- tmp$name_en[is.na(tmp$name)]
  tmp$description[is.na(tmp$description)] <- tmp$description_en[is.na(tmp$description)]
  tmp$name_en <- NULL
  tmp$description_en <- NULL
}

codelist <- tmp


nrow(tmp)
sum(is.na(tmp$name))
sum(is.na(tmp$description))



codelist

library(codelist)

codelist <- codelist[!(locale %in% c("cs")]

codelist(codes = codelist$id, labels = codelist$name, descriptions=codelist$description, parent = codelist$parent, locale = codelist$lang)




