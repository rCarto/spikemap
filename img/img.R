library(sf)
library(spikemap)
library(cartography)

# import the dataset from the package
com <- st_read(system.file("gpkg/com.gpkg", package="spikemap"))

# save figure
png(filename = "img/spiky2.png", width = 1280, height = 640, res = 100)
# set the margin of the figure
par(mar = c(0.2,0.2,0.2,0.2))

# plot the base map
plot(st_geometry(com), col="#99aed1", border = "#e1e5eb",
     lwd = 0.2, bg = "#e1e5eb")

# display spikes for municipalities under 1000 inhabitants.
# use fixmax arg to allow multiple spike plots with the same scale.
spikemap(x = com[com$pop<=1000, ], var = "pop",
         inches = 2.5, fixmax = 500000,
         col = "#ffffff90", border = "#94000090",  lwd = .5,
         legend.pos = "x")
# display spikes for other municipalities
# use locator() to pick a place for the legend or use "bottomleft".
spikemap(x = com[com$pop>1000, ], var = "pop",
         inches = 2.5, fixmax = 500000,
         col = "#ffffff", border = "#940000", lwd = 1.1,
         legend.pos = c(779307.2, 6128000),
         legend.title.txt = "Population",
         legend.values.rnd = -3)

# get the tips of the spikes
lbl <- spikelabel(x = com, var = "pop",
                  inches = 2.5, fixmax = 500000)
lbl <- lbl[order(lbl$pop, decreasing = T),]
# display only the 12 first, use various cex and halo
labelLayer(lbl[1:12,], txt = "name",
           pos = 3, offset = .5,
           halo = T, bg = "#99aed150",
           cex = c(1.3, 1.1, 1, rep(.8,12)),
           col = "grey30")

# add scale bar, north arrow, title, sources...
barscale(size = 20, pos= c(629638.7 ,6136862.3 ), lwd = 1)
north(pos = "topright", col = "grey60", x = com)
layoutLayer(title = "",
            sources = "ADMIN EXPRESS COG édition 2019, IGN",
            author = "T. Giraud, 2020 | spikemap 0.1.0",
            frame = FALSE, scale = FALSE)
mtext("Population \nin Occitanie",
      side = 3, adj = 0.01, padj = 0, line = -5,
      cex = 2.5, font = 3, col = "grey30")

dev.off()





library(sf)
library(spikemap)
library(cartography)

mtq <- st_read(system.file("gpkg/mtq.gpkg", package="cartography"))
png(filename = "img/spikymtq.png", width = 800, height = 935, res = 100)
par(mar = c(0.2,0.2,0.2,0.2))
getFigDim(mtq, width = 800, res = 100, mar = c(rep(.2,4)))
plot(st_geometry(mtq), col = "lightblue4",border = "lightblue3",
     bg = "lightblue1")
spikemap(x = mtq, var = "POP")
lbl <- spikelabel(x = mtq, var = "POP")
labelLayer(lbl[order(lbl$POP, decreasing = T),][1:3,],
           txt = "LIBGEO",
           pos = c(3,3,4), offset = .5,
           halo = T, bg = "white",
           cex = c(1.3, 1.1, 1),
           col = "grey30")
barscale(size = 5, pos = c(723658.9, 1592544), lwd = 1)
north(pos = "topleft", col = "grey60", x = mtq)
layoutLayer(title = "",
            sources = "Sources: Insee and IGN, 2018",
            author = "T. Giraud, 2020 | spikemap 0.1.0",
            frame = FALSE, scale = FALSE, horiz = F)
mtext("Population\nDistribution\nin Martinique",
      side = 3, adj = 0.99, padj = 0, line = -7.5,
      cex = 2.5, font = 3, col = "grey30")
dev.off()




download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/countries.geojson",
              destfile = "img/country.geojson")
# Graticules layer
download.file(url = "https://raw.githubusercontent.com/riatelab/basemaps/master/World/graticule30.geojson",
              destfile = "img/graticule.geojson")

download.file(url = "https://population.un.org/wup/Download/Files/WUP2018-F12-Cities_Over_300K.xls",
              destfile = "img/citypop.xls")

library(sf)
library(readxl)
country <- st_read(dsn = "img/country.geojson", quiet = TRUE)
graticule <- st_read(dsn = "img/graticule.geojson", quiet = TRUE)
city <- data.frame(read_excel("img/citypop.xls", skip = 16))

city <- st_as_sf(city,
                 coords = c("Longitude","Latitude"),
                 crs = 4326)

# transformation de la projection WGS84 => Robinson
country <- st_transform(x = country, crs = 54030 )
graticule <- st_transform(x = graticule, crs = 54030)
city <- st_transform(x = city, crs = 54030)

# Cartes
png(filename = "img/spikywrld.png", width = 1000, height = 700, res = 100)
par(mar = c(0,0,0,0))
plot(st_geometry(graticule), col = "lightblue", border = "white", lwd = 0.2,
     xlim = st_bbox(graticule)[c(1,3)], ylim = c(st_bbox(graticule)[2], 15500000) )
plot(st_geometry(country), col = "ivory4", border ="ivory3", lwd = 0.5, add=TRUE)
spikemap(x = city, var = "X2020", inches = 3.2,border = "gold3", col = "white",
         legend.title.txt = "Population (2020)\nin thousands")

mtext("Population of Urban Agglomerations\nwith 300,000 Inhabitants or More",
      side = 3, adj = 0.1, padj = 0, line = -6,
      cex = 2.5, font = 3, col = "grey30")

layoutLayer(title = "",
            author = "World Urbanization Prospects, 2018",
            sources = "T. Giraud, 2020 | spikemap 0.1.0",
            scale = NULL, tabtitle = TRUE, frame = FALSE, horiz = F)
dev.off()

