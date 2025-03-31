
df <- data.frame(
  ID = 1:4,
  SeedZone = 1:4,
  SZName = c('Salt Desert', 'Desert Scrub', 'Pinyon-Juniper/Oak Brush', 'Montane'), 
  AreaAcres = c(12340, 14230, 30142, 9872),
  'BIO1_R' = c(20.2, 19.1, 15.1, 12.3),
  'BIO2_mean' = c(5.1, 7.1, 10.1, 12.3)
)

library(gt)

gt_tbl <- 
  gt(df) |>
  tab_header(
    title = "Example field names in a shapefile",
  ) |>
  tab_options(
    table.background.color = '#BCD8C1', 
    column_labels.background.color = '#E9D985') |>
  tab_style(
    style = cell_fill(color = "#007991" |> adjust_luminance(steps = 2)),
    locations = cells_body(columns = 1:4)
  ) |>
  tab_style(
    style = cell_fill(color = "#439A86" |> adjust_luminance(steps = 2)),
    locations = cells_body(columns = 5:6)
  ) |>
  tab_footnote(
    footnote = "The first four (blue) fields should be in every file. More fields are optional."
  )
