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


