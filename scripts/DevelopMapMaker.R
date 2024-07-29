# determine which packages we are going to use
library(sf)
library(tidyverse)
library(tigris)
library(rnaturalearth)
library(ggspatial)

#' MapMaker
#' Create standardized maps for empirical seed transfer zones
#' @param x the vector (e.g. shapefile) or raster dataset to plot, note vector data will be coerced
#' to raster before plotting. 
#' @param species character string, the name of the species which is being mapped. 
#' @param outdir a directory to save the map to.  
#' @param ecoregions Boolean, whether to draw ecoregions or not. Defaults to TRUE
#' @param landscape boolean, whether to draw the map in a landscape orientation or not. Defaults to TRUE
#' @param caption text for a caption, it's best to mention any published product related to the data set. 
#' Defaults to omitting any caption
mapmaker <- function(x, species, outdir, ecoregions, landscape, caption){ 
  
  # Buffer the map so that the species only doesn't occupy the entire region. 
  extent <- buffeR(x) 
  
  # ggplot does the cropping to an extent, but we'll manually specify the borders
  # and what data we want here. 
  countries <- spData::world |> 
    dplyr::filter(iso_a2 %in% c('CA', 'US', 'MX')) |> 
    dplyr::select(name_long) |> 
    sf::st_transform(sf::st_crs(extent)) 
  countries <- sf::st_intersection(sf::st_as_sfc(extent), countries) 
  
  states <- tigris::states(cb = TRUE) |> 
    sf::st_transform(5070) 
  states <- sf::st_intersection(sf::st_as_sfc(extent), states) 
  
  omernik <- sf::st_intersection(sf::st_as_sfc(extent), omernik) 
   
  p <- ggplot() + 
    geom_sf(data = x, aes(fill = factor(zone)), color = NA, inherit.aes = TRUE) + 
    geom_sf(data = states, fill = NA, lwd = 0.25, color = 'black', alpha = 0.5) + # typical line for state borders. 
    geom_sf(data = omernik, fill = NA, lwd = 0.5, lty = 3, color = 'grey50') + 
    geom_sf(data = countries, fill = NA, lwd = 1, color = 'black') + # thick line for national borders
    coord_sf(
      xlim = extent[c(1,3)], 
      ylim = extent[c(2,4)], 
      expand = F) + 
    
    labs(x = NULL, y = NULL, fill = 'Zone', 
         caption = caption,
         title = paste0('*', species, '*'), 
         subtitle = 'Seed Transfer Zones') +
    theme(
      plot.title = ggtext::element_markdown(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.title = element_text(hjust = 0.5), 
      legend.position = "bottom"
      ) + 
    
    ggspatial::annotation_scale(location = "br", width_hint = 0.25) +
    ggspatial::annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.25, "in"), pad_y = unit(0.25, "in"), 
                           height = unit(0.35, "in"), width = unit(0.35, "in")) 
  
  return(p) 
  
} 

# For example data we are going to use the Astragalus lonchocarpus data
setwd('/media/steppe/hdd/EmpiricalSeedZones/scripts') 
acth <- st_read('../data/geodata/step1/Achnatherum_thurberianum-WUS.shp') |> 
  select(zone) 

ab <- mapmaker(acth, species = 'Achnatherum thurberianum', 
               caption = 'See Massatii for details') 
ab 


##############              CREATE DATA FOR PROJECT            #################
###        We will include an omernik level 3 data set with the package.     ###
###              We'll also include a doi regions shape files                ###

regions_abb <- data.frame( 
  REG_NUM = 1:12, 
  REG_ABB = c('NAA', 'SAG', 'GL', 'MSB', 'MOB', 'ART', 'UCB', 'LCB', 'CPN', 'CGB', 'AK', 'PI')
) 

regions <- st_read( 
  '../data/geodata/doi_unified_regions/DOI_12_Unified_Regions_20180801.shp') |> 
  select(REG_NUM, REG_NAME) |> 
  left_join(regions_abb) 

rm(regions_abb) 

omernik <- st_read( 
 '../data/geodata/us_eco_l3/us_eco_l3.shp' 
) |> 
  select(US_L3CODE, US_L3NAME) |> 
  group_by(US_L3CODE, US_L3NAME) |> 
  summarise(geometry = st_union(geometry)) |> 
  ungroup() |> 
  mutate(US_L3CODE = as.numeric(US_L3CODE)) |> 
  arrange(US_L3CODE) 

omernik <- rmapshaper::ms_simplify(omernik, keep = 0.01, keep_shapes = TRUE) |> 
  st_transform(5070) 

## FINALIZED FUNCTIONS BENEATH ## 

#' buffer an input STZ to determine map extents
#' @param x an input STZ, as vector or raster data -vector is much preferred for speed.  
#' @param buf_prcnt the amount of buffering to add to the map, defaults to 0.025 or 2.5%
buffeR <- function(x, buf_prcnt){
  
  if(missing(buf_prcnt)){buf_prcnt <- 0.025}
  
  if(class(acth)[1] == 'sf'){ 
    if(sf::st_crs(x) != 5070){x <- sf::st_transform(x, 5070)} 
  } else if(class(acth)[1] == 'SpatRaster') { 
    if(terra::crs(x, describe = TRUE)$code != '5070'){ 
      x <- terra::project(x, 'epsg:5070') 
    } 
  } else {stop('This function only supports data of classes `sf` (preferred) or `spatraster`.')} 
  
  range <- sf::st_bbox(x) 
  x_buf <- (range[['xmax']] - range[['xmin']]) * buf_prcnt 
  y_buf <- (range[['ymax']] - range[['ymin']]) * buf_prcnt 

  range[['xmax']] <- range[['xmax']] + x_buf 
  range[['xmin']] <- range[['xmin']] - x_buf
  range[['ymax']] <- range[['ymax']] + y_buf 
  range[['ymin']] <- range[['ymin']] - y_buf
 
  return(range) 
 # return(list(x = x_buf, y =  y_buf))
}


extent <- buffeR(acth) 

extent$x / extent$y
