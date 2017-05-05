quaketimeline_plot <- function(eqdata, x_min, x_max){

  suppressWarnings(
    ggplot2::ggplot(eqdata, aes( x = DATE, y = COUNTRY, size = as.numeric(EQ_PRIMARY),
                    col = as.numeric(DEATHS)/1000)) +
    geom_timeline(aes(x_min = x_min, x_max = x_max)) +
      ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs( size = "Richter Scale", col = "# Deaths '000s "))
}

quaketimelinelabel_plot <- function(eqdata, x_min, x_max, n_max){

   gr <-ggplot2::ggplot(eqdata,
                        ggplot2::aes( x = DATE, y = COUNTRY,
                                      size = as.numeric(EQ_PRIMARY),
                                      col = as.numeric(DEATHS)/1000)) +
     geom_timeline(ggplot2::aes(x_min = x_min, x_max = x_max)) +
     ggplot2::theme_classic() +
     ggplot2::theme(legend.position = "bottom") +
     ggplot2::labs( size = "Richter Scale", col = "# Deaths '000s ") +
      geom_timelinelabel(ggplot2::aes(magnitude = as.numeric(EQ_PRIMARY),
                             label = LOCATION_NAME,
                             x_min = x_min,
                             x_max = x_max),
                             n_max = n_max)

   return(gr)
}


