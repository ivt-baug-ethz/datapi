

#' Codebook for swiss weather data
#' 
#' @seealso `swiss_weather_data()`
#'
#' @return
#' @export
swiss_weather_codebook <- function()
{
  ## https://opendata.swiss/en/dataset/klimamessnetz-tageswerte/resource/9652a2a6-2467-43f6-bb82-13d08b021e91
  
  cb <- 
    paste(
      "Parameters,           Unit,             Description",
      "gre000d0,             W/m²,             Global radiation; daily mean",
      "hto000d0,             cm,               Total snow depth; morning recording at 6 UTC",
      "nto000d0,             %,                Cloud cover; daily mean",
      "prestad0,             hPa,              Pressure at station level (QFE); daily mean",
      "rre150d0,             mm,               Precipitation; daily total 6 UTC - 6 UTC following day",
      "sre000d0,             min,              Sunshine duration; daily total",
      "tre200d0,             °C,               Air temperature 2 m above ground; daily mean",
      "tre200dn,             °C,               Air temperature 2 m above ground; daily minimum",
      "tre200dx,             °C,               Air temperature 2 m above ground; daily maximum",
      "ure200d0,             %,                Relative air humidity; 2 m above ground; daily mean",
      sep = "\n"
    )
  
  codebook <-
    read.table(text = cb, sep = ",", header = TRUE) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), trimws)) %>%
    dplyr::tibble()
  
  names(codebook) <- tolower(names(codebook))
  codebook
}







#' Download Swiss weather data
#' 
#' @seealso `swiss_weather_codebook()`
#'
#' @return
#' @export
swiss_weather_data <- function()
{
  url <- "https://data.geo.admin.ch/ch.meteoschweiz.klima/nbcn-tageswerte/liste-download-nbcn-d.csv"
  
  how_to_download <-
    url %>%
    readr::read_delim(delim = ";",
                      show_col_types = FALSE,
                      progress = FALSE) %>%
    dplyr::select(lon = Longitude,
                  lat = Latitude,
                  canton = Canton,
                  url_previous = `URL Previous years (verified data)`,
                  url_current = `URL Current year`) %>%
    tidyr::drop_na()
  
  
  
  
  downloader <- function(url, ...)
  {
    readr::read_delim(url,
                      delim = ";",
                      show_col_types = FALSE,
                      progress = FALSE,
                      na = "-", ...)
  }
  
  
  
  weather <-
    how_to_download %>%
    dplyr::mutate(data_previous = purrr::map(url_previous, ~ downloader(.x)),
                  data_current = purrr::map(url_current, ~ downloader(.x)))
  
  
  
  weather_data <-
    weather %>%
    dplyr::mutate(weather = purrr::map2(data_previous, data_current, ~ rbind(.x, .y))) %>%
    dplyr::select(!tidyr::contains("data")) %>%
    tidyr::unnest(weather) %>%
    dplyr::mutate(date = as.Date(as.character(date), "%Y%m%d")) %>%
    dplyr::filter(date > as.Date("2019-09-01") & date < Sys.Date()) %>%
    dplyr::select(-url_previous, -url_current)
  
  
  new_names <-
    ivttools::swiss_weather_codebook() %>%
    dplyr::select(parameters, description) %>%
    dplyr::mutate(description = description %>%
                    stringr::str_remove_all(";|\\(|\\)") %>%
                    stringr::str_replace_all("-", "") %>%
                    stringr::str_replace_all(" ", "_") %>%
                    tolower())
  
  pl <- new_names$parameters
  
  weather_data <-
    weather_data %>%
    tidyr::pivot_longer(all_of(pl), names_to = "parameters") %>%
    dplyr::left_join(new_names, by = "parameters") %>%
    dplyr::select(-parameters) %>%
    tidyr::pivot_wider(names_from = description, values_from = value)
  
  weather_data
  
}