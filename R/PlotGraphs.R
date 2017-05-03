quaketimeline_plot <- function(eqdata, x_min, x_max){

  suppressWarnings(
    ggplot(eqdata, aes( x = DATE, y = COUNTRY, size = as.numeric(EQ_PRIMARY),
                    col = as.numeric(DEATHS)/1000)) +
    geom_timeline(aes(x_min = x_min, x_max = x_max)) +
    theme_classic() +
    theme(legend.position = "bottom") +
    labs( size = "Richter Scale", col = "# Deaths '000s "))
}


