#' Clean and format earthqake data
#'
#'  \code{eq_clean_data} creates a date class field and converts location
#'  coordinates into numeric vectors.
#'
#'  The YEAR field ranges from -2000 or 2000 BC to more contemporary times
#'  which the usual date functions don't manage well with. This function
#'  splits the years into AD and BC and creates a DATE field from the
#'  YEAR, MONTH and DAY fields (most of the MONTH and DAY fields for
#'  the BC years are NA)
#'
#' @param data a data frame holding the earthquake data
#'
#' @importFrom dplyr "%>%"
#'
#' @examples
#' eq_clean_data(eq_test_data)
#'
#' @return A data frame with the date and location fields formated correctly
#' @export
eq_clean_data <- function(data){

  year <- as.character(abs(data[["YEAR"]])) # convert to character to pad years
  year <- stringr::str_pad(year, 4, "left", pad = 0)
  month <- as.character(data[["MONTH"]])
  day <- as.character(data[["DAY"]])
  era <- ifelse(data$YEAR < 0, "BC", "AD") # classify AD and BC

  # NAs are converted to the first of whatever the date in terms of year or
  # year and month.
  month <- ifelse(is.na(month), 1, month)
  day <- ifelse(is.na(day), 1, day)


  datum <- lubridate::ymd(paste0(year,"-", month, "-", day))

  lat <- as.numeric(data[["LATITUDE"]])
  lon <- as.numeric(data[["LONGITUDE"]])

  # Add in the correctly formated date and era and extract the unnecessary
  # fields.
  cldata <- dplyr::mutate(data, DATE = datum, ERA = era,
                        LATITUDE = lat,
                        LONGITUDE = lon) %>%
    dplyr::select_(quote(-YEAR),quote(-MONTH),quote(-DAY))%>%
    eq_location_clean()

  return(cldata)

}

#' Format the LOCATION_NAME field of the data
#'
#' \code{eq_location_clean} reformats the LOCATION_NAME field to exclude
#' the country name.
#'
#' Where possible the description up to the colon is stripped out and the
#' remaining title is set to title case and stored.
#'
#' @param data a data frame holding the earthquake data.
#'
#' @examples
#' eq_location_clean(eq_test_data)
#'
#' @return the earthquake data frame with the LOCATION_NAME field reformatted.
#' @export
eq_location_clean <- function(data){

  loc_name <- data$LOCATION_NAME
  loc_name <- strsplit(loc_name, ":")
  loc_name <- sapply(loc_name,
                     function(x) {
                       if (is.na(x[2])) x[1] else x[2]
                       }
                     )
  loc_name <- trimws(loc_name, "both")
  loc_name <- stringr::str_to_title(loc_name)

  cldata <- dplyr::mutate(data, LOCATION_NAME = loc_name)

  return(cldata)
}
