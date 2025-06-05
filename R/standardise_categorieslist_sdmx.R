
standardise_categorieslist_sdmx <- function(x, dataresource = attr(x, "resource"), 
    lang = getOption("LANG", "EN"), warn = TRUE, keep_default_lang = TRUE, ...) {
  # Make sure x has the right columns
  names(x)[names(x) == "id"] <- "value"
  names(x)[names(x) == "name"] <- "label"
  cols <- c("value", "label", "description", "lang", "parent", "missing")
  x <- subset(x, select = intersect(names(x), cols))
  hasparent <- utils::hasName(x, "parent")
  if (!hasparent) {
    x$parent <- x$value
    x$value[] <- NA
  }
  hasmissing <- utils::hasName(x, "missing")
  if (!hasmissing) x$missing <- FALSE
  haslang <- utils::hasName(x, "lang")
  if (!haslang) x$lang <- NA_character_
  x <- subset(x, select = cols)
  # Convert languages from long to wide format
  languages <- unique(x$lang)
  lang <- default_lang(languages, lang = lang, warn = warn)
  if (length(languages) > 1) {
    tmp <- split(x, x$lang)
    tmp <- lapply(tmp, function(x) {
      x$lang <- NULL
      x
    })
    res <- tmp[[lang]]
    for (n in names(tmp)) {
      res <- merge(res, tmp[[n]], by = c("value", "parent", "missing"), all = TRUE, 
        suffixes = c("", paste0("@", n)))
    }
    # Put columns in order
    cols <- c(setdiff(cols, "lang"), 
      paste0(c("label@", "description@"), rep(languages, each=2)))
    res <- subset(res, select = cols)
  } else {
    res <- x
  }
  # Remove unneeded columns
  if (!hasparent) res$parent <- NULL
  if (!hasmissing) res$missing <- NULL
  if (all(is.na(res$parent))) res$parent <- NULL
  if (!keep_default_lang) {
    res[[paste0("label@", lang)]] <- NULL
    res[[paste0("description@", lang)]] <- NULL
  }
  attr(res, "languages") <- languages
  res
}


default_lang <- function(langs, lang = getOption("LANG", "EN"), warn = TRUE) {
  langs <- unique(langs)
  stopifnot(length(langs) > 0)
  # WE can shortcut the case where the is only one lang; this also handles the 
  # case where there is no languate "langs = NA"
  if (length(langs) == 1) return(langs)
  stopifnot(all(!is.na(langs)))
  if (tolower(lang) %in% tolower(langs)) {
    langs[match(tolower(lang), tolower(langs))]
  } else {
    lang <- utils::head(langs, 1)
    if (warn) warning("Default language not present in available languages. ", 
      "Choosing first available language '", lang, "'. ", 
      "Set option `LANG` to an available language to choose another language.")
    lang
  }
}


