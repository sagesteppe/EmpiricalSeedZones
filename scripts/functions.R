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