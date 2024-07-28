#' helper function to read in empirical seed zones from raw sources
#'
#' This function helps read in and corral the original empirical seed zone data.
#' @param x path to file
#' 
empSZ_reader <- function(x, colname){
  
  name <- sub('.*\\/', "", x) # grab file name
  name <- gsub('[.]shp', "", name) # remove extension
  spp_name <- gsub('-.*$', '', name) # get species name
  spp_name <- gsub('[.]', '', spp_name) # get species name
  domain_name <- sub('.*?-', '', name) # domains
  
  geodata <- st_read(x, quiet = T)
  if(st_crs(geodata) == 4326){geodata} else {geodata <- st_transform(geodata, 4326)}
  if(all(st_is_valid(geodata)) == TRUE){geodata} else {geodata <- st_make_valid(geodata)}

  geodata <- geodata |>
    dplyr::select(Zone = all_of(colname)) |>
    dplyr::mutate(Species = spp_name,
                  Domain = domain_name, .before = Zone,
                  Zone = as.character(Zone)) |>
    dplyr::arrange(Zone)
  
  return(geodata)
}

#' MapMaker
#' Create standardized maps for empirical seed transfer zones
#' @param x the vector (e.g. shapefile) or raster dataset to plot, note vector data will be coerced
#' to raster before plotting. 
#' @param species character string, the name of the species which is being mapped. 
#' @param outdir a directory to save the map to.  
#' @param ecoregions Boolean, whether to draw ecoregions or not. Defaults to TRUE
#' @param cities boolean, whether to draw major U.S. cities or not. Defaults to FALSE
#' @param landscape boolean, whether to draw the map in a landscape orientation or not. Defaults to TRUE
mapmaker <- functionx(x, species, outdir, ecoregions, cities){
  
  # make a copy of the data set so we can measure it's extent 
  # Buffer the map so that the species only occupies roughly 90% of the total area. 
  sf::st_buffer()
  
}

borders <- spData::world |>
  dplyr::filter(iso_a2 %in% c('CA', 'US', 'MX')) |>
  dplyr::select(name_long)

states <- tigris::states(cb = TRUE)
