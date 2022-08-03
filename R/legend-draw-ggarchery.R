#' This function [geom_segment()] for `geom_arrowsegment()` and displays all the requested arrowheads.
#'
#' @return A grid grob.
#' @param data A single row data frame containing the scaled aesthetics to
#'   display in this key
#' @param params A list of additional parameters supplied to the geom.
#' @param size Width and height of key in mm.
#' @name draw_key_arrowpath
NULL

#' @export
#' @rdname draw_key_arrowpath
draw_key_arrowpath <- function(data, params, size) {

  if (is.null(data$linetype)) {
    data$linetype <- 0
  } else {
    data$linetype[is.na(data$linetype)] <- 0
  }

  # this is somewhat maniacal

  arrow_positions <- sort(params$arrow_positions)

  arrows <- params$arrows

  if(!is.null(attr(arrows,'class'))){
    if(attr(arrows,'class') == "arrow"){
      arrows <- list(arrows)
    }
  }

  arrow_fills <- params$arrow_fills

  if(length(arrows) == 1 & length(arrow_positions) > 1){
    arrows <- rep(arrows, length(arrow_positions))
  }

  if(!is.null(arrow_fills) &  length(arrow_fills) == 1 & length(arrow_positions) > 1){
    arrow_fills <- rep(arrow_fills, length(arrow_positions))
  }

  if(!is.null(arrow_fills) & arrow_positions[length(arrow_positions)] != 1){
    # arrow.positions = list(0.5) has one arrow at 0.5 but the line continues with no arrow
    # arrow.positions = as.list(c(0.5, 1)) puts two arrowheads at 0.5 and 1
    # hence the last segment needs a NULL arrow in the former case
    arrow_fills <- c(arrow_fills, NA_character_)
  }

  nsegments <- length(arrow_positions)

  if(arrow_positions[length(arrow_positions)] != 1){
    nsegments <- nsegments + 1
  }

  segment_length <- 0.4/nsegments

  out <- map(1:nsegments, function(sg){

    if(sg <= length(arrows)){
      current.arrow <- arrows[[sg]]
      current.arrow$length <-  unit(0.1, "inches")
    } else {
      current.arrow <- NULL
    }

    if(sg == 1){
      start.x <- 0.1
      end.x <- 0.5 + sg*segment_length
    } else {
      start.x <- 0.5 + (sg-1)*segment_length
      end.x <- 0.5 + sg*segment_length
    }

    segmentsGrob(start.x, 0.5, end.x, 0.5,
                 gp = gpar(
                   col = alpha(data$colour %||% "black", data$alpha),
                   fill = alpha(arrow_fills[sg] %||% data$fill %||% "black", data$alpha),
                   lwd = (data$linewidth %||% 0.5) * .pt,
                   lty = data$linetype %||% 1,
                   lineend = params$lineend %||% "butt"
                 ),
                 arrow = current.arrow
    )

  })

  tmp <- do.call("grobTree", out)

  return(tmp)

}
