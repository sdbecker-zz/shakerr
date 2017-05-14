#'Function to plot the time line of earthquakes
#'
#'\code{quaketimeline_plot} plots the magnitude and number of deaths
#'of earthquakes on a timeline.
#'
#'@param eqdata a data frame of properly formatted earthquake data.
#'
#'@param x_min an atomic character designating the lower end of a date
#'range.
#'
#'@param x_max an atomic character designating the upper end of a date
#'range.
#'
#'@return a ggplot2 graphic
#'
#'@export
quaketimeline_plot <- function(eqdata, x_min, x_max){

    #Convert the format of some of the fields
    eqdata[["EQ_PRIMARY"]] <- as.numeric(eqdata[["EQ_PRIMARY"]])
    eqdata[["DEATHS"]] <- as.numeric(eqdata[["DEATHS"]])/1000

    gr <- ggplot2::ggplot(eqdata,
            ggplot2::aes_( x = quote(DATE), y = quote(COUNTRY),
                           size = quote(EQ_PRIMARY),
                    col = quote(DEATHS))) +
    geom_timeline(x_min = x_min, x_max = x_max) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs( size = "Richter Scale", col = "# Deaths '000s ")
  return(gr)
}

#'Function to plot the time line of earthquakes
#'
#'\code{quaketimeline_plot} plots the magnitude and number of deaths
#'of earthquakes on a timeline.
#'
#'@param eqdata a data frame of properly formatted earthquake data.
#'
#'@param x_min an atomic character designating the lower end of a date
#'range.
#'
#'@param x_max an atomic character designating the upper end of a date
#'range.
#'
#'@param n_max and atomic numeric designating the number of top earthquakes
#'by magnitude to label.
#'
#'@return a ggplot2 graphic
#'
#'@export
quaketimelinelabel_plot <- function(eqdata, x_min, x_max, n_max){

  #Convert the format of some of the fields
  eqdata[["EQ_PRIMARY"]] <- as.numeric(eqdata[["EQ_PRIMARY"]])
  eqdata[["DEATHS"]] <- as.numeric(eqdata[["DEATHS"]])/1000


  gr <- ggplot2::ggplot(eqdata,
                        ggplot2::aes_( x = quote(DATE), y = quote(COUNTRY),
                                       size = quote(EQ_PRIMARY),
                                       col = quote(DEATHS))) +
     geom_timeline(x_min = x_min, x_max = x_max) +
     ggplot2::theme_classic() +
     ggplot2::theme(legend.position = "bottom") +
     ggplot2::labs( size = "Richter Scale", col = "# Deaths '000s ") +
      geom_timelinelabel(ggplot2::aes_(magnitude = quote(EQ_PRIMARY),
                             label = quote(LOCATION_NAME),
                             col = NULL),
                             x_min = x_min,
                             x_max = x_max,
                             n_max = n_max)

   return(gr)
}

#' Plot Epicentre of Earthquakes on a Map
#'
#' \code{eq_map} Will use a map to plot the location of the epicentre of an
#' earthquake. The plot will be done using a circle, with the radius of the
#' circle indicative of the magnitude of the earthquake.
#'
#' @param df The data frame holding the longitude and latitudes for the
#' epicentres of the earthquakes.
#'
#' @param annot_col An atomic character holding the field name of the column
#' to display on the map.
#'
#' @return Return a leaflet map
#'
#' @export
eq_map <- function(df, annot_col){

  #ensures that only data points with long and lat will work.
  df <- df[!is.na(df[["LONGITUDE"]]),]

  anno_vec <- df[[annot_col]]
  mp <- leaflet::leaflet(df) %>% leaflet::addTiles() %>%
    leaflet::addCircleMarkers(lng = ~LONGITUDE, lat = ~ LATITUDE,
                              radius = ~EQ_PRIMARY, popup = anno_vec)

  return(mp)

}

#'Create an HTML Label
#'
#'\code{eq_create_label} Creates a label using HTML in order to label points
#'on a map.
#'
#'@param df A data frame holding all the information of the earthquake data
#'
#'@return an HTML label for the point on a map.
#'
#'@export
eq_create_label <- function(df){

  mgn <- as.numeric(df[["EQ_PRIMARY"]])
  dths <- as.numeric(df[["DEATHS"]])

  locname <- ifelse(is.na(df[["LOCATION_NAME"]]), "" ,
         paste0("<b>Location: </b>",df[["LOCATION_NAME"]],"<br>"))

  magnitude <- ifelse(is.na(mgn), "",
                            paste0("<b>Magnitude: </b>",
                                   df[["EQ_PRIMARY"]],"<br>"))

  deaths <- ifelse(is.na(dths), "",
                   paste0("<b>Total deaths: </b>",df[["DEATHS"]]))

  lbl <- paste0(locname, magnitude, deaths)

  return(lbl)

}

