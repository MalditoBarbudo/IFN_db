#### STEP 1 PLOT general table ####
# Let's build the overall table, joining what we have to join based on NFI4 first, NFI3
# later and for those left, NFI2.
# But before, as the admin and ownership info are for all plots already, lets join that
# first
ifn2_ifn3_ifn4_plots %>%
  left_join(admin_info, by = 'plot_id') %>%
  left_join(ownership_info, by = 'plot_id') %>%
  # here we let join by all coincidental variables, not only by plot_id
  left_join(topo_clim_info %>%
              # we need to remove the old_idclasse_nfi3 and nfi4 as they are in the
              # ifn2_ifn3_ifn4_plots and they create problems in the join
              select(-old_idclasse_nfi3, -old_idclasse_nfi4)) %>%
  # lets rename some variables to maintain the logic (coords_ prefix for coordinates...)
  # and order the variables
  select(
    plot_id,
    presence_NFI_2 = NFI_2,
    presence_NFI_3 = NFI_3,
    presence_NFI_4 = NFI_4,
    coords_utm_x_ETRS89 = utm_x_ETRS89,
    coords_utm_y_ETRS89 = utm_y_ETRS89,
    coords_longitude = longitude,
    coords_latitude = latitude,
    starts_with('admin_'),
    starts_with('topo_'),
    starts_with('feat_'),
    starts_with('clim_'),
    starts_with('old_')
  ) -> PLOTS

# Miramon maps are in ED50 projection, so we need to convert the coordinates that we
# have from ETRS89 to ED50, load the raster images in the geotiff format (with the
# raster::raster function) and extract the values at the coordinates (with the
# raster::extract function)
PLOTS %>%
  select(coords_utm_x_ETRS89, coords_utm_y_ETRS89) %>% 
  sp::SpatialPoints(sp::CRS('+init=epsg:23031')) -> plots_sppoints_ed50

etp_year_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S.tif')
etp_jan_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_1.tif')
etp_feb_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_2.tif')
etp_mar_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_3.tif')
etp_apr_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_4.tif')
etp_may_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_5.tif')
etp_jun_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_6.tif')
etp_jul_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_7.tif')
etp_aug_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_8.tif')
etp_sep_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_8.tif')
etp_oct_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_10.tif')
etp_nov_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_11.tif')
etp_dec_raster <- raster::raster('data_raw/mmm_geotiffs/ETP_S versión GeoTIF/ETP_S_12.tif')
etr_year_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10.tif')
etr_jan_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_1.tif')
etr_feb_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_2.tif')
etr_mar_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_3.tif')
etr_apr_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_4.tif')
etr_may_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_5.tif')
etr_jun_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_6.tif')
etr_jul_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_7.tif')
etr_aug_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_8.tif')
etr_sep_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_8.tif')
etr_oct_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_10.tif')
etr_nov_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_11.tif')
etr_dec_raster <- raster::raster('data_raw/mmm_geotiffs/ETR_K10 versión GeoTIF/ETR_K10_12.tif')

PLOTS %<>%
  select(-starts_with('clim_etp_'), -starts_with('clim_etr_')) %>%
  mutate(
    clim_pet_year = raster::extract(etp_year_raster, plots_sppoints_ed50),
    clim_pet_jan = raster::extract(etp_jan_raster, plots_sppoints_ed50),
    clim_pet_feb = raster::extract(etp_feb_raster, plots_sppoints_ed50),
    clim_pet_mar = raster::extract(etp_mar_raster, plots_sppoints_ed50),
    clim_pet_apr = raster::extract(etp_apr_raster, plots_sppoints_ed50),
    clim_pet_may = raster::extract(etp_may_raster, plots_sppoints_ed50),
    clim_pet_jun = raster::extract(etp_jun_raster, plots_sppoints_ed50),
    clim_pet_jul = raster::extract(etp_jul_raster, plots_sppoints_ed50),
    clim_pet_aug = raster::extract(etp_aug_raster, plots_sppoints_ed50),
    clim_pet_sep = raster::extract(etp_sep_raster, plots_sppoints_ed50),
    clim_pet_oct = raster::extract(etp_oct_raster, plots_sppoints_ed50),
    clim_pet_nov = raster::extract(etp_nov_raster, plots_sppoints_ed50),
    clim_pet_dec = raster::extract(etp_dec_raster, plots_sppoints_ed50),
    clim_ret_year = raster::extract(etr_year_raster, plots_sppoints_ed50),
    clim_ret_jan = raster::extract(etr_jan_raster, plots_sppoints_ed50),
    clim_ret_feb = raster::extract(etr_feb_raster, plots_sppoints_ed50),
    clim_ret_mar = raster::extract(etr_mar_raster, plots_sppoints_ed50),
    clim_ret_apr = raster::extract(etr_apr_raster, plots_sppoints_ed50),
    clim_ret_may = raster::extract(etr_may_raster, plots_sppoints_ed50),
    clim_ret_jun = raster::extract(etr_jun_raster, plots_sppoints_ed50),
    clim_ret_jul = raster::extract(etr_jul_raster, plots_sppoints_ed50),
    clim_ret_aug = raster::extract(etr_aug_raster, plots_sppoints_ed50),
    clim_ret_sep = raster::extract(etr_sep_raster, plots_sppoints_ed50),
    clim_ret_oct = raster::extract(etr_oct_raster, plots_sppoints_ed50),
    clim_ret_nov = raster::extract(etr_nov_raster, plots_sppoints_ed50),
    clim_ret_dec = raster::extract(etr_dec_raster, plots_sppoints_ed50)
  )

#### STEP 2 PLOT dynamic tables ####
# ***_dynamic_plot_topo_clim_vars comes from 02 script
PLOTS_NFI_2_DYNAMIC_INFO <- ifn2_dynamic_plot_topo_clim_vars
PLOTS_NFI_3_DYNAMIC_INFO <- ifn3_dynamic_plot_topo_clim_vars
PLOTS_NFI_4_DYNAMIC_INFO <- ifn4_dynamic_plot_topo_clim_vars
