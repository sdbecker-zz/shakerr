#' Geom class to support plotting geom_timeline
#' @format NULL
#' @usage NULL
#' @export
GeomTimelinelabel <- ggplot2::ggproto("GeomTimelinelabel", ggplot2::Geom,
                                 required_aes = c("x","label"),
                                 default_aes = ggplot2::aes( y = 0),

                                 draw_panel = function(data, panel_params, coord) {

                                   coords <- coord$transform(data, panel_params)

                                    seg <- grid::segmentsGrob(
                                     x0 = coords$x,
                                     x1 = coords$x,
                                     y0 = coords$y,
                                     y1 = coords$y + 0.15
                                   )

                                   txt <- grid::textGrob(
                                     label = coords$label,
                                     x = coords$x,
                                     y = coords$y + 0.15,
                                     just = "left",
                                     gp = grid::gpar(fontsize = 10),
                                     rot = 45
                                     )

                                   grlist <- grid::gList(seg, txt)
                                   lbl <- grid::gTree(children = grlist)

                                   return(lbl)

                                    }
                                   )

#' Geom to annotate earthquake graph
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
geom_timelinelabel <- function(mapping = NULL, data = NULL,
                          stat = "timeline", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = "timelinelabel",
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

#' Stat class to support the GeomTimelinelabel class
#' @format NULL
#' @usage NULL
#' @export
StatTimelinelabel <- ggplot2::ggproto("StatTimelinelabel", ggplot2::Stat,
                          default_aes = ggplot2::aes(y = 1),
                         required_aes = c("x", "magnitude", "x_min",
                                          "x_max"),

                         setup_data = function(data, params){

                           xmin_num <- as.numeric(as.Date(data$x_min))
                           xmax_num <- as.numeric(as.Date(data$x_max))

                           blflt <- data$x >= xmin_num & data$x <= xmax_num
                           data <- data[blflt,]
                           return(data)
                         },

                         compute_group = function(data, scales,n_max = 4) {

                           srtind <- sort(data$magnitude,
                                      index.return = TRUE)
                           tpind <- srtind$ix[1:n_max[[1]]]

                           rtdat <- data[tpind,]

                           return(rtdat)
                           }
)

#' Stat function to support StatTimelinelabel class
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
stat_timelinelabel <- function(mapping = NULL, data = NULL, geom = "timelinelabel",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, n_max = NA,
                           ...) {
  ggplot2::layer(
    stat = StatTimelinelabel,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, n_max = n_max, ...)
  )
}
