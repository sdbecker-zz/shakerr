#' Geom class to support plotting geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                 required_aes = c("x","y","label"),

                                 draw_panel = function(data, panel_params, coord) {
                                   coords <- coord$transform(data, panel_params)

                                   grid::textGrob(
                                     label = coords$label,
                                     x = coords$x,
                                     y = coords$y+0.35,
                                     rot = 45,
                                     just = "centre"
                                     )}
                                   )

geom_timelinelabel <- function(mapping = NULL, data = NULL,
                          stat = "timeline", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomTimelinelabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

StatTimelinelabel <- ggplot2::ggproto("StatTimelinelabel", ggplot2::Stat,
                                 compute_group = function(data, scales,x_min, x_max) {
                                   ## Compute the line segment endpoints
                                   xmin_num <- as.numeric(as.Date(data$x_min))
                                   xmax_num <- as.numeric(as.Date(data$x_max))

                                   blflt <- data$x >= xmin_num & data$x <= xmax_num
                                   data <- data[blflt,]
                                   return(data)
                                 },
                                 default_aes = ggplot2::aes(y = 1),
                                 required_aes = "x"
)


stat_timelinelabel <- function(mapping = NULL, data = NULL, geom = "timeline",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimelinelabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
