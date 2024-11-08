library(tidyverse)
library(sf)


setwd('/media/steppe/hdd/EmpiricalSeedZones/scripts')
p <- '../data/geodata/step1'
f <- file.path(p, list.files(p, pattern = '[.]shp$'))


colReporter <- function(x){

  dat <- sf::st_read(x, quiet = T)

  n <- ncol(dat); if(n>5){n=5}
  dat <- dat[,1:n]

  col_head <- head(dat, n = 10) |>
    sf::st_drop_geometry() |>
    cbind(
      Geometry = as.character(unique(sf::st_geometry_type(dat))),
      File = basename(x))

}


out <- lapply(f, colReporter)

# we want to score three things

# 1) Does the Seed Zone containing column contain the word 'Zone'
# 2) Is the Geometry type Polygon or Multipolygon?
# 3) Is area included ? (Y/N) Are the units labelled?

ob <- bind_rows(out)
