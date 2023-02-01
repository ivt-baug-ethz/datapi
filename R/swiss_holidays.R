## https://www.ferienwiki.ch/exports/ch

canton <-
  c(
    "aargau",
    "appenzell-ausserrhoden",
    "appenzell-innerrhoden",
    "basel-landschaft",
    "basel-stadt",
    "bern",
    "freiburg",
    "genf",
    "glarus",
    "graubuenden",
    "jura",
    "luzern",
    "neuenburg",
    "nidwalden",
    "obwalden",
    "schaffhausen",
    "schwyz",
    "solothurn",
    "st-gallen",
    "tessin",
    "thurgau",
    "uri",
    "waadt",
    "wallis",
    "zug",
    "zuerich"
  )


#' Swiss holidays (Ferien und Feiertage)
#'
#' @param canton character: one of ivttools::canton
#' @param year character
#' @param holidays logical
#' @param feiertage logical
#'
#' @return list of dfs
#' @export
swiss_holidays <- function(canton = ivttools:::canton, year, holidays = TRUE, feiertage = TRUE) {

  canton <- match.arg(canton, several.ok = FALSE)
  
  swiss_holidays <- list()
  
  url_holidays <- glue::glue("https://www.ferienwiki.ch/exports/ferien/{year}/ch/{canton}")
  url_feiertage <- glue::glue("https://www.ferienwiki.ch/exports/feiertage/{year}/ch/{canton}")
  
  download_handler <- function(url) {
    temp <- tempfile()
    download.file(url, temp)
    df <- ical::ical_parse_df(temp)
    unlink(temp)
    df
  }
  
  restructor <- function(df) {
    df$start <- as.Date(df$start)
    df$end <- as.Date(df$end)
    df$description <- stringr::str_remove_all(df$description, " - Importiert von Ferienwiki.de")
    df <- df[!(names(df) %in% c("last.modified", "status"))]
    df
  }
  
  holi <- NULL
  if(holidays) {
    holi <- download_handler(url_holidays)
    holi <- restructor(holi)
    holi$label <- "holidays"
  }
  
  feier <- NULL
  if(feiertage) {
    feier <- download_handler(url_feiertage)
    feier <- restructor(feier)
    feier$label <- "feiertag"
  }
  
  swiss_holidays$holidays <- holi
  swiss_holidays$feiertage <- feier
  
  swiss_holidays
}