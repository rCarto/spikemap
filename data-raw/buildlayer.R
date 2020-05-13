## Import and select
library(sf)
com <- st_read("data-raw/Occitanie.gpkg")
com <- st_transform(com, 2154)
com <- com[,c("INSEE_COM", "NOM_COM_M", "POPULATION")]
com$NOM_COM_M <- tools::toTitleCase(tolower(com$NOM_COM_M))
names(com)[1:3] <- c("id", "name", "pop")

## Simplify
library(rmapshaper)
com <- ms_simplify(com ,keep_shapes = T)

## Clean
invalid <- com[!(st_is_valid(com)),]
repared <- st_make_valid(invalid)
clean <- st_collection_extract(repared, "POLYGON")
st_geometry(com[!(st_is_valid(com)),]) <- st_geometry(clean)

## Export
st_write(com, "inst/gpkg/com.gpkg", delete_dsn = T)
