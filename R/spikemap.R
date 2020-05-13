#' @title Plot a Spike Map
#' @name spikemap
#' @description Plot a layer of spikes on a map.
#' @param x an sf object, a simple feature collection.
#' @param var name of the numeric field in x to plot.
#' @param inches size of the tallest spike, in inches.
#' @param width width of the spike's base.
#' @param col filling color of the spikes.
#' @param border border color of the spikes.
#' @param lwd width of spikes borders.
#' @param fixmax value of the tallest spike (see Details).
#' @param legend.pos position of the legend, "bottomright", or a
#' vector of two coordinates in map units (c(x, y)). If
#' legend.pos is "n" then the legend is not plotted.
#' @param legend.title.txt title of the legend.
#' @param legend.title.cex size of the legend title.
#' @param legend.values.cex size of the values in the legend.
#' @param legend.values.rnd number of decimal places of the values
#' displayed in the legend.
#' @param add whether to add the layer to an existing plot (TRUE) or
#' not (FALSE).
#' @details
#' Two maps with the same inches and fixmax parameters will be comparable.
#' @importFrom graphics lines polygon xinch
#' @importFrom sf st_bbox
#' @export
#' @examples
#' library(sf)
#' com <- st_read(system.file("gpkg/com.gpkg", package="spikemap"))
#' plot(st_geometry(com))
#' spikemap(x = com, var = "pop")
spikemap <- function(x, var,
                     inches = 3, width = .02,
                     fixmax,
                     col = "white",
                     border = "red", lwd = .5,
                     legend.pos = "bottomleft",
                     legend.title.txt = var,
                     legend.title.cex = 0.8,
                     legend.values.cex = 0.6,
                     legend.values.rnd = 0,
                     add = TRUE){


  pt <- spike(x = x, var = var, inches = inches, fixmax = fixmax)

  # plot from north to south
  pt <- pt[order(pt[,2], decreasing = TRUE),]

  # plot
  if (add == FALSE){
    bbx <- st_bbox(x)
    plot(0, type='n', axes = FALSE, ann = FALSE, asp = 1,
         xlim = bbx[c(1,3)], ylim = bbx[c(2,4)])
  }

  # plot spike fun
  spiky_fill <- function(x, col, width, border, lwd){
    polygon(x = c(x[1] - width, x[1]       , x[1] + width),
            y = c(x[2]        , x[2] + x[4], x[2]        ),
            col = col, border = NA, lwd = 1
    )
    lines(x = c(x[1] - width, x[1]       , x[1] + width),
          y = c(x[2]        , x[2] + x[4], x[2]        ),
          col = border, lwd = lwd
    )
  }

  n <- apply(X = pt, MARGIN = 1, FUN = spiky_fill,
             col = col, border = border, lwd = lwd, width = xinch(width))

  legv <- seq(max(pt[[3]]), min(pt[[3]]), length.out = 4)
  spikelegend(x = legv, inches = inches, width = width, fixmax = fixmax,
              pos = legend.pos, col =col, border = border, lwd = lwd,
              title.txt = legend.title.txt, title.cex = legend.title.cex,
              values.cex = legend.values.cex, values.rnd = legend.values.rnd)


  return(invisible())
}


#' @title Get Spike Tips
#' @description Get spikes' tips, may be usefull for labels
#' @param x an sf object, a simple feature collection.
#' @param var name of the numeric field in x to plot.
#' @param inches size of the tallest spike, in inches.
#' @param fixmax value of the tallest spike (see Details).
#'
#' @return An sf POINT layer of spikes tips is returned
#' @importFrom sf st_as_sf st_crs st_set_geometry
#' @export
#'
#' @examples
#' library(sf)
#' com <- st_read(system.file("gpkg/com.gpkg", package="spikemap"))
#' plot(st_geometry(com))
#' spikemap(x = com, var = "pop")
#' tips <- spikelabel(x = com, var = "pop")
#' plot(st_geometry(tips), add = TRUE)
spikelabel <- function(x, var, inches = 3, fixmax){
  pt <- spike(x = x, var = var, inches = inches, fixmax = fixmax)
  pt[[2]] <- pt[[2]] + pt[[4]]
  pts <- st_as_sf(cbind(st_set_geometry(x, NULL), pt[,1:2]),
                  coords = c("X","Y"), crs = st_crs(x))
  return(pts)
}


#' @title spike
#' @description Get df of coordinates for peaks
#' @param x an sf object, a simple feature collection.
#' @param var name of the numeric field in x to plot.
#' @param inches size of the tallest spike, in inches.
#' @param fixmax value of the tallest spike (see Details).
#' @importFrom sf st_as_sf st_crs st_set_geometry st_coordinates st_centroid
#' st_geometry
#' @noRd
spike <- function(x, var, inches, fixmax){
  # get centroid coords
  pt <- data.frame(st_coordinates(st_centroid(x = st_geometry(x),
                                              of_largest_polygon = TRUE)),
                   var = x[[var]])

  # remove NAs and 0 values
  pt <- pt[!is.na(pt[[3]]),]
  pt <- pt[pt[[3]]!=0, ]

  # turn to positive values
  pt[[3]] <- abs(pt[[3]])

  # max value of var
  if (missing(fixmax)){
    fixmax <- max(pt[[3]])
  }

  # compute sizes
  sizes <- pt[[3]] * inches  / fixmax
  pt$height <- xinch(sizes)

  return(pt)
}


spikelegend <- function(x, inches = 3, width = .02, fixmax, col, border, lwd,
                        pos, title.txt, title.cex, values.cex, values.rnd){

  # figdim in geo coordinates
  x1 <- graphics::par("usr")[1]
  y1 <- graphics::par("usr")[3]

  if(length(pos) == 1){
    if(pos != "bottomleft"){
      return(invisible())
    }
  }else{
    x1 <- pos[1]
    y1 <- pos[2]
  }


  # offsets
  delta <- xinch(0.05)

  # spike pos
  xc <- x1 + (delta * c(3, 6, 9, 12))
  yc <- rep(y1, 4) + delta * 2
  leg <- sf::st_as_sf(data.frame(lon = xc, lat = yc, lab = x),
                      coords = c("lon", "lat"), crs = NA)
  spikemap(x = leg, var = "lab", inches = inches, width = width, fixmax = fixmax,
           lwd = lwd, col = col, border = border, add = TRUE, legend.pos = "n")

  if (missing(fixmax)){
    fixmax <- x[1]
  }

  sizes <- xinch(x * inches  / fixmax)
  graphics::text(x = xc, yc + sizes, labels = round(x, values.rnd), pos = 4,
                 offset = .2, cex = values.cex)
  graphics::text(x = xc[1] - (xinch(width)/2), yc[1] + sizes[1] + delta * 2.5,
                 labels = title.txt, cex = title.cex, adj = c(0,0))
}


