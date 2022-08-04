#' Nudge points towards each other along a line
#'
#' This position function is primarily intended for use with [`geom_arrowsegment()`], and solves the problem
#' that the user may, for reasons of clarity or aesthetics, not want their arrows to actually start or end at
#' the position that they are "pointing from" or "pointing to". It works by shifting the points towards each
#' other along the line joining them, by either a proportional amount or a fixed distance.
#'
#'
#' @family position adjustments
#' @param start_shave,end_shave The amount of distance to "shave" off the line between (`x`, `y`) and (`xend`, `yend`),
#' at, respectively, the start and the end. Can be zero; cannot be negative. Units are determined by `type_shave`.
#' @param type_shave If `"proportion"` (the default) then this is a proportion of the total line length. If "distance"
#' then it is instead the raw distance along the line. The is only really recommended in combination with
#' [`ggplot2::coord_fixed()`]; results can be quite odd otherwise.
#' @import ggplot2 dplyr purrr
#' @export
#' @return A ggproto object
#' @examples
#'
#'  library(ggplot2)
#'  library(magrittr)
#'  library(tidyr)
#'
#'  # Generate some dummy data
#'
#'  ten.points <- data.frame(line.no = rep(1:5, each = 2), x = runif(10), y = runif(10),
#'                           position = rep(c("start", "end"), 5))
#'  five.segments <- ten.points %>% pivot_wider(names_from = position, values_from = c(x,y))
#'
#'  # Ten percent off the start and end
#'
#'  ggplot(five.segments) +
#'    geom_point(data = ten.points, aes(x = x, y = y)) +
#'    geom_arrowsegment(aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
#'                      position = position_attractsegment(start_shave = 0.1, end_shave = 0.1))
#'
#'  # Absolute distance of 0.02 at the end only
#'
#'  ggplot(five.segments) +
#'    geom_point(data = ten.points, aes(x = x, y = y)) +
#'    geom_arrowsegment(aes(x = x_start, xend = x_end, y = y_start, yend = y_end),
#'                      position = position_attractsegment(end_shave = 0.02,
#'                                                         type_shave = "distance")) +
#'    coord_fixed()
position_attractsegment <- function(start_shave = 0, end_shave = 0, type_shave = c("proportion", "distance")) {
  ggproto(NULL, PositionAttractsegment,
          start_shave = start_shave,
          end_shave = end_shave,
          type_shave = match.arg(type_shave)
  )
}

#' Move points towards each other a proportion of the distance between them
#' @param x,xend,y,yend The coordinates of the start and end of the segment
#' @param prop_start,prop_end How much (proportionally) to take off the start and end of the line
#' @return A named list of new coordinates for the start and end of the line
#' @keywords internal
#' @export
attract_proportionally <- function(x, xend, y, yend, prop_start, prop_end){
  list(x = x + prop_start*(xend-x), xend = xend- prop_end*(xend-x), y = y + prop_start*(yend-y), yend = yend - prop_end*(yend-y))
}

#' Move points towards each other an absolute distance
#' @param x,xend,y,yend The coordinates of the start and end of the segment
#' @param prop_start,prop_end How much to take off the start and end of the line (in the units of the data; beware this can make little sense unless both axes have the same units)
#' @return A named list of new coordinates for the start and end of the line
#' @keywords internal
#' @import glue
#' @importFrom rlang warn
#' @export
attract_by_distance <- function(x, xend, y, yend, length_start, length_end){
  euclidean_distance <- sqrt((x-xend)^2 + (y-yend)^2)
  if(euclidean_distance <= length_start + length_end){
    warn(glue("Segment (x={x}, y={y}) to (xend={xend}, yend={yend}) of length {euclidean_distance} shortened beyond zero; removing this entirely."))
    return(list(x = NA_real_, xend = NA_real_, y = NA_real_, yend = NA_real_))
  }

  prop_start = length_start/euclidean_distance
  prop_end = length_end/euclidean_distance

  attract_proportionally(x, xend, y, yend, prop_start, prop_end)

}

#' @rdname ggarchery-ggproto
#' @import ggplot2 grid
#' @importFrom rlang abort
#' @format NULL
#' @usage NULL
#' @export
PositionAttractsegment <- ggproto("PositionAttractsegment", ggplot2::Position,
                                  required_aes = c("x", "y", "xend", "yend"),
                                  setup_params = function(self, data) {
                                    list(
                                      start_shave = self$start_shave,
                                      end_shave = self$end_shave,
                                      type_shave = self$type_shave
                                    )
                                  },

                                  compute_layer = function(self, data, params, layout) {

                                    if(params$type_shave == "proportion"){

                                      # check the numbers make sense

                                      if(params$start_shave < 0 | params$start_shave > 1 | params$end_shave < 0 | params$end_shave > 1){
                                        abort("Proportional shaves must lie between 0 and 1")
                                      }
                                      if(params$start_shave + params$end_shave >= 1){
                                        abort("Proportional shaves must sum to less than 1 (or there will be no line left!)")
                                      }

                                      new_values <- pmap(list(data$x, data$xend, data$y, data$yend),
                                                         attract_proportionally, prop_start = params$start_shave, prop_end = params$end_shave) %>%
                                        bind_rows()


                                    } else {
                                      new_values <- pmap(list(data$x, data$xend, data$y, data$yend), attract_by_distance,
                                                         length_start = params$start_shave, length_end = params$end_shave)

                                      new_values <- new_values%>%
                                        bind_rows()
                                    }

                                    visible.lines <- which(!is.na(new_values$x))

                                    newdata <- data[visible.lines,]
                                    newdata[,c("x", "xend", "y", "yend")] <- new_values[visible.lines,]
                                    newdata

                                  }
)



