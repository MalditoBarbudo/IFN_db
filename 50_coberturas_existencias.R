library(tidyverse)
library(sf)

mc <- sf::read_sf('data_raw/MCSC/mcsc09.shp')
mc_thes <- readr::read_csv2('data_raw/MCSC/tesauro_mcsc09.csv')
clip_and_calculate <- function(comarca, mc, mc_thes) {
  message(glue::glue("{comarca[['NOMCOMAR']]}"))
  mc %>%
    dplyr::left_join(mc_thes, by = c('c_simple' = 'codi_cober')) %>%
    sf::st_crop(sf::st_bbox(comarca[['geometry']])) %>%
    sf::st_intersection(comarca[['geometry']]) %>%
    dplyr::group_by(cat_niv_2) %>%
    dplyr::summarise() %>%
    dplyr::mutate(
      comar = comarca[['NOMCOMAR']],
      geometry_comar = comarca[['geometry']],
      area_comar = sf::st_area(geometry_comar) / 1000000,
      area_mc = sf::st_area(geometry) / 1000000,
      perc_mc = (area_mc / area_comar) * 100
    ) # %>%
    # dplyr::select(
    #   county = comar, forest_type = cat_niv_2,
    #   geometry_comar, geometry_mc = geometry,
    #   dplyr::starts_with('area_'), dplyr::starts_with('perc_')
    # )
}

## Comarcas ####
comarcas <- sf::read_sf('data_raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp')
1:nrow(comarcas) %>%
  purrr::map(~ dplyr::slice(comarcas, .x)) %>%
  purrr::map_dfr(~ clip_and_calculate(.x, mc, mc_thes)) -> counties_cob_and_existencies

save(counties_cob_and_existencies, 'data_raw/counties_cob_and_existencies.RData')

