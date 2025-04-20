library(tidyverse)
set.seed(12)


#' Rotate simple features for 3D layers
#' @description Rotates a simple features layer using a shear matrix transformation on the 
#' \code{geometry} column. This can get nice for visualization and works with
#' points, lines and polygons. Code by Stefan Jünger Denis Cohen, perhaps based upon work by
#' spacedman. 
#'
#' @param data an object of class \code{sf}
#' @param x_add integer; x value to move geometry in space
#' @param y_add integer; x value to move geometry in space
#'
#' @importFrom magrittr %>%
rotate_sf <- function(data, x_add = 0, y_add = 0) {
  
  shear_matrix <- function (x) { 
    matrix(c(2, 1.2, 0, 1), 2, 2) 
  }
  
  rotate_matrix <- function(x) { 
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2) 
  }
  
  data %>% 
    dplyr::mutate(
      geometry = 
        .$geometry * shear_matrix() * rotate_matrix(pi / 20) + c(x_add, y_add)
    )
}





# dummy data for individual sieves. 

dat <- data.frame(
  matrix(data = c(
    sample(0:1, size = 36, replace = T, prob = c(0.25, 0.75)), 
    sample(0:1, size = 36, replace = T, prob = c(0.2, 0.80)), 
    sample(0:1, size = 36, replace = T, prob = c(0.15, 0.85))
    ),
    ncol = 3)
  )

focus <- apply(dat, MARGIN = 1, min)
dat <- cbind(dat, focus) |>
  setNames(c('landcover', 'nlcd', 'seeded', 'focus'))

rm(focus)

# combine our dummy data with some dummy coordinates, which will become points
# in the x and y direction. We will buffer out the points to turn them into
# raster tiles. 
sf_lyrs <- cbind(
  dat,
  x = rep(1:6, each = 6), 
  y = rep(1:6) # automatically recycles  
) |>
  data.frame() |>
  sf::st_as_sf(coords = c('x', 'y'), remove = FALSE) |>
  sf::st_buffer(0.5, endCapStyle = 'SQUARE') |>
  dplyr::mutate(across(landcover:focus, ~ factor(.x)))

rm(dat)

# create a specified palette for our classified layers s
cols <- c("0" = 'black', "1" = 'white')

# create the plot using the rotate_sf function. 
r_plot <- ggplot() + 
  
  geom_sf(data = rotate_sf(sf_lyrs), # bottom layer
          aes(fill = focus)) +
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 7), # lower layer
    aes(fill = seeded), alpha = .7) + 
  
  geom_sf(data = rotate_sf(sf_lyrs, y_add = 14), # middle layer
    aes(fill = nlcd),  alpha = .7) + 

  geom_sf(data = rotate_sf(sf_lyrs, y_add = 21), # uppermost layer, y_add relative to base
    aes(fill = landcover),  alpha = .7) +
  
  scale_fill_manual(values = cols, name = 'Suitable:') + 
  theme_void() + 
  theme(
    legend.position="bottom", 
    plot.title = element_text(hjust = 0.5)) + 
  labs(title = 'Land use `Sieve`') + 
  annotate("text", x = 18, y = -1.5, size=5, color="gray15", label = "Suitable Areas") +

  annotate("text", x = 18, y = 5, size = 3.75, color="gray15", # lowest sieve
           label = "Historic Seeding") + 
  annotate("text", x = 18, y = 12, size = 3.75, color="gray15", # middle sieve
           label = "NLCD") + 
  annotate("text", x = 18, y = 19, size = 3.75, color="gray15", # upper sieve
           label = "Land Cover") 
  
r_plot
ggsave('../figures/EnvironmentalSieve.png', r_plot, bg = 'transparent', 
       dpi = 300, units = 'in', height = 5, width = 3.5)


rm(rotate_sf, r_plot, sf_lyrs)