#' Geom class to support plotting geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                  required_aes = c("x", "x_min", "x_max"),
                  default_aes = ggplot2::aes( shape = 19, colour = "black",
                                              size = 1.5, fill = NA,
                                              alpha = 0.5, stroke = 0.5, y = 1),
                  draw_key = ggplot2::draw_key_point,
                  draw_group = function(data, panel_params, coord) {

                  data <- dplyr::select_(data, quote(-x_min), quote(-x_max))
                  coords <- coord$transform(data, panel_params)

                      ln <- grid::linesGrob(
                        x = c(0,1),
                        y = coords$y,
                        gp = grid::gpar(col = "lightgray")
                        )

                      pnt <- grid::pointsGrob(
                        coords$x, coords$y,
                        pch = coords$shape,
                        gp = grid::gpar(
                          col = ggplot2::alpha(coords$colour, coords$alpha),
                          fill = ggplot2::alpha(coords$fill, coords$alpha),
                          # Stroke is added around the outside of the point
                          fontsize = coords$size * ggplot2::.pt + coords$stroke * ggplot2::.stroke / 2,
                          lwd = coords$stroke * ggplot2::.stroke / 2
                          )
                        )
                  grlist <- grid::gList(ln, pnt)

                  lbl <- grid::gTree(children = grlist)

                  return(lbl)
             }
)

#' Geom to plot earthquake data
#'
#' @inheritParams ggplot2::layer
#'
#' @param na.rm If \code{FALSE}, the default, missing values are removed with
#'   a warning. If \code{TRUE}, missing values are silently removed.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL,
                       stat = "timeline", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' Stat class to support the GeomTimeline class
#' @format NULL
#' @usage NULL
#' @export
StatTimeline <- ggplot2::ggproto("StatTimeline", ggplot2::Stat,

                       setup_data = function(data, params){

                         xmin_num <- as.numeric(as.Date(params$x_min))
                         xmax_num <- as.numeric(as.Date(params$x_max))

                         blflt <- data$x >= xmin_num & data$x <= xmax_num
                         data <- data[blflt,]
                         return(data)
                       },

                       default_aes = ggplot2::aes(y = 1),
                       required_aes = "x",

                       compute_group = function(data, scales,x_min, x_max) {
                         return(data)
                       }

)

#' Stat function to support Stat class
#' @inheritParams ggplot2::layer
#'
#' @param na.rm If \code{FALSE}, the default, missing values are removed with
#'   a warning. If \code{TRUE}, missing values are silently removed.
#' @param ... other arguments passed on to \code{\link{layer}}. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   \code{color = "red"} or \code{size = 3}. They may also be parameters
#'   to the paired geom/stat.
#' @param x_min An atomic character representing the low of a date range.
#'
#' @param x_max An atomic character representing the high of a date range.
#'
#' @export
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE, x_min,
                         x_max, ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, x_min, x_max, ...)
  )
}
