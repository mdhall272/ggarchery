#' Test
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

  print(length(arrows))

  segmentsGrob(0.1, 0.5, 0.9, 0.5,
               gp = gpar(
                 col = alpha(data$colour %||% data$fill %||% "black", data$alpha),
                 fill = alpha(params$arrow.fill %||% data$colour
                              %||% data$fill %||% "black", data$alpha),
                 lwd = (data$linewidth %||% 0.5) * .pt,
                 lty = data$linetype %||% 1,
                 lineend = params$lineend %||% "butt"
               ),
               arrow = params$arrow
  )
}
