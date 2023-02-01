noga_sectors <- function(export_type = c("levelexport", "multilevels"),
                         from = c("1", "2", "3", "4", "5"),
                         to = c("1", "2", "3", "4", "5"),
                         lang = c("en", "de", "fr", "it"),
                         annotations = FALSE) {

  FORMAT <- "CSV"


  export_type <- match.arg(export_type)
  from <- match.arg(from)
  to <- match.arg(to)
  lang <- match.arg(lang)

  annotations <- ifelse(annotations, "true", "false")

  base_url_sl <- "https://www.i14y.admin.ch/api/Nomenclatures/HCL_NOGA/levelexport/{FORMAT}?language={lang}&level={from}&annotations={annotations}"
  base_url_ml <- "https://www.i14y.admin.ch/api/Nomenclatures/HCL_NOGA/multiplelevels/{FORMAT}?language={lang}&levelFrom={from}&levelTo={to}&annotations={annotations}"

  if(export_type == "levelexport") {
    url <- glue::glue(base_url_sl)
  } else {
    url <- glue::glue(base_url_ml)
  }

  tmp <- tempfile()
  download.file(url, tmp)
  df <- dplyr::tibble(read.delim(tmp, sep = ","))

  return(df)
}
