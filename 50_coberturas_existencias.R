library(tidyverse)
library(sf)

### creating the statistics, really time consuming ####
mc <- sf::read_sf('data_raw/MCSC/mcsc09.shp')
mc_thes <- readr::read_csv2('data_raw/MCSC/tesauro_mcsc09.csv')
clip_and_calculate <- function(polygon_to_clip, mc, mc_thes, name = 'NOMCOMAR') {
  message(glue::glue("{polygon_to_clip[[name]]}"))
  mc %>%
    dplyr::left_join(mc_thes, by = c('c_simple' = 'codi_cober')) %>%
    purrr::walk(~ message("cropping...")) %>%
    sf::st_crop(sf::st_bbox(polygon_to_clip[['geometry']])) %>%
    purrr::walk(~ message("intersecting...")) %>%
    sf::st_intersection(polygon_to_clip[['geometry']]) %>%
    dplyr::group_by(cat_niv_2) %>%
    purrr::walk(~ message("summarising...")) %>%
    dplyr::summarise() %>%
    purrr::walk(~ message("calculating areas...")) %>%
    dplyr::mutate(
      polygon_name = polygon_to_clip[[name]],
      geometry_polygon_to_clip = polygon_to_clip[['geometry']],
      area_polygon_to_clip = sf::st_area(geometry_polygon_to_clip) / 10000, # ha
      area_mc = sf::st_area(geometry) / 10000, # ha
      perc_mc = (area_mc / area_polygon_to_clip) * 100
    ) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      polygon_name, forest_type = cat_niv_2,
      dplyr::starts_with('area_'), dplyr::starts_with('perc_')
    )
}

ifn_db <- tidyNFI::nfi_connect(host = '158.109.46.23', port = 5433)
## Comarcas ####
plots_comar <- dplyr::tbl(ifn_db, 'PLOTS') %>%
  dplyr::select(plot_id, admin_region) %>%
  dplyr::group_by(admin_region) %>%
  dplyr::count() %>%
  dplyr::collect()
comarcas <- sf::read_sf('data_raw/shapefiles/bm5mv20sh0tpc1_20180101_0.shp')
1:nrow(comarcas) %>%
  purrr::map(~ dplyr::slice(comarcas, .x)) %>%
  purrr::map_dfr(~ clip_and_calculate(.x, mc, mc_thes, 'NOMCOMAR')) %>%
  dplyr::left_join(plots_comar, by = c('polygon_name' = 'admin_region')) -> counties_stocks
save(counties_stocks, file = 'data_raw/counties_stocks.RData')

# load('data_raw/counties_stocks.RData') %>%
#   tibble::as_tibble() -> counties_stocks

## Municipalities ####
plots_muni <- dplyr::tbl(ifn_db, 'PLOTS') %>%
  dplyr::select(plot_id, admin_municipality) %>%
  dplyr::group_by(admin_municipality) %>%
  dplyr::count() %>%
  dplyr::collect()
municipios <- sf::read_sf('data_raw/shapefiles/bm5mv20sh0tpm1_20180101_0.shp')
1:nrow(municipios) %>%
  purrr::map(~ dplyr::slice(municipios, .x)) %>%
  purrr::map_dfr(~ clip_and_calculate(.x, mc, mc_thes, 'NOMMUNI')) %>%
  dplyr::left_join(plots_muni, by = c('polygon_name' = 'admin_municipality')) -> muni_stocks
save(muni_stocks, file = 'data_raw/muni_stocks.RData')

# load('data_raw/muni_stocks.RData') %>%
#   tibble::as_tibble() -> muni_stocks

## close connections ####
tidyNFI::nfi_close(ifn_db)
