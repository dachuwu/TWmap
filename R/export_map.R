#' Export a map
#'
#' @param fname file name.
#' @param map a map generated by \code{twmap_static()} or a ggplot object.

#'
#' @return NULL.
#'
#' @importFrom ggplot2 ggsave
#'
#' @export
export_map <- function(fname = NULL, map = NULL){

  if(is.null(fname)) stop("filename must not be NULL")
  if(is.null(map)) stop("map must not be NULL")

  if("ggplot" %in% class(map)){
    fp <- paste0(fname, ".jpeg")
    ggplot2::ggsave(filename = fp, plot = map, scale = 1, dpi = 350, units = "cm",
           width = 29, height = 21)
  } else {
    stop("Unable to export non-ggplot object")
  }

}
