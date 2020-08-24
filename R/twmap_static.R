#' Creat a static map of geo-specific measurements
#'
#' @param geo.code area code in MOI format.
#' @param x plotted variable for each area. Must be an integer or numeric vector with the same length of `acode`.
#' @param geo.level the geographic level of `geo.code`. One of "town", "county", "region", "nation", and "urcanicity".
#' @param x.name the name of the plotted variable `x` which is the legend title.
#' @param col.control a list of  parameters controling the coloring scale.
#' (1)`option`: A character string indicating the colormap option to use. Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"), "viridis" (or "D", the default option) and "cividis" (or "E").
#' (2)`dir`: Sets the order of colors in the scale. If 1, the default, colors are ordered from darkest to lightest. If -1, the order of colors is reversed.
#' @param thm.control a list of  parameters controling the theme of the map.
#' (1) border: the color of the border lines. (2) back: the filled color of the map background.
#'
#' @return a ggplot object.
#'
#'
#' @importFrom ggplot2
#' @importFrom sf
#'
#' @export
#'
twmap_static <- function(geo.code, x,
                              geo.level = "county", x.name = "xname",
                              col.control = list(option = "viridis", dir = 1, na.color = "grey50"),
                              thm.control = list(border = "white", back = "grey90")){

  if(length(geo.code) != length(x)) stop("inconsistent lengths of geo.code and x")
  if(any(duplicated(geo.code))) stop("duplicated item(s) in geo.code")
  if(!class(x) %in% c("numeric", "integer", "factor")) stop("x must be one of numeric, integer, or factor")


  if(geo.level == "county"){
    bmap <- bord_county %>% rename(geo.code = city)
  }else if(geo.level == "town"){
    bmap <- bord_town %>% rename(geo.code = town)
  }else if(geo.level == "region"){
    bmap <- bord_region %>% rename(geo.code = region)
  }else if(geo.level == "nation"){
    bmap <- bord_nation %>% rename(geo.code = nation)
  }else if(geo.level == "urbanicity"){
    bmap <- bord_urban %>% rename(geo.code = urb)
  }else{
    stop("fail to specify the geographic level argument : geo.level")
  }

  gdf <- bmap %>%
    dplyr::left_join(data.frame(
      geo.code = geo.code,
      xv = x, stringsAsFactors = F
    ))


  G <- ggplot2::ggplot(gdf)+
    ggplot2::geom_sf(ggplot2::aes(fill = xv, geometry = geometry),
                     color = thm.control$border, size=.1)+
    ggplot2::coord_sf(xlim = c(118.4,122.8), ylim = c(21.85,25.35), ndiscr = 0)+
    ggplot2::labs(x="",y="")+
    ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = thm.control$back, color = thm.control$back),
          plot.margin = ggplot2::unit(c(0,0,0,0), "cm"),
          panel.background = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          legend.justification = c(1.1,0),
          legend.position = c(1, 0))


  if(class(gdf$xv) %in% c("numeric", "integer")){
    G <- G +
      ggplot2::scale_fill_viridis_c(option = col.control$option, direction = col.control$dir,
                                    na.value = col.control$na.color)+
      ggplot2::guides(fill = ggplot2::guide_colorbar(title = x.name))
  } else if(class(gdf$xv) == "factor"){
    G <- G +
      ggplot2::scale_fill_viridis_d(option = col.control$option, direction = col.control$dir,
                                    na.value =  col.control$na.color)+
      ggplot2::guides(fill = ggplot2::guide_legend(title = x.name))
  }


  return(G)

  }
