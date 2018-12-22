#### STEP 1 Topo and climatic info ####
# Let's start to add static info to the main table, starting from gis and climatic tables.
# For this we must do some things:
# 
#   1. Start with the NFI_4, it has the better info. Later on NFI_3 and NFI_2
#   
#   2. Standardize the variables and their names across the different NFI versions.
#      The topographic variables will have the topo_ prefix
#      The administrative variables will have the admin_ prefix
#      The plot features variables will have the feat_ prefix
#      The plot climatic variables will have the clim_ prefix

## NFI 4

# we will need later the table TesaureGruixMO to be able to convert the numeric id of
# organic matter thickness to the real character value
gruixmo_thes <- tbl(access4_db, 'TesaureGruixMO') %>%
  collect()

tbl(access4_db, 'Parcela_MDT') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  ## separate the idparcela and idclasse as they are joined, removing the _ character
  ## between the class and subclass
  tidyr::separate(
    idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
  ) %>%
  mutate(idclasse = stringr::str_remove(idclasse, '_')) %>%
  ## select, renaming in the way, the variables
  select(
    old_idparcela = idparcela,
    old_idclasse_nfi4 = idclasse,
    topo_altitude_asl = altitud,
    topo_fdm_slope_degrees = pendentgraus,
    topo_fdm_slope_percentage = pendentpercent,
    topo_fdm_aspect_degrees = orientacio,
    topo_fdm_aspect_cardinal_8 = orientacio_8,
    topo_fdm_aspect_cardinal_4 = orientacio_4,
    topo_fdm_curvature = curvatura
  ) %>%
  
  ## join with the admin, features and more topo vars
  full_join(
    {
      tbl(access4_db, 'ParcelaIFN4_OLAP') %>%
        collect() %>% 
        ## again to lower letters
        {magrittr::set_names(., tolower(names(.)))} %>%
        # there is a problem with the access database, as in the postgresql conversion
        # dates were lost. we need to load the table as csv and remove na dates and include
        # the correct ones
        select(-datainici, -datafi, -horainici, -horafi) %>%
        left_join(
          readr::read_csv2('data_raw/ParcelaIFN4_OLAP.txt', col_names = FALSE) %>%
            select(X1:X2, X38:X41) %>%
            magrittr::set_names(
              value = c(
                'idparcela', 'idclasse', 'datainici', 'datafi', 'horainici', 'horafi'
              )
            ),
          by = c('idparcela', 'idclasse')
        ) %>%
        mutate(
          datainici = as.POSIXct(datainici, format = '%d/%m/%Y %H:%M:%S'),
          datafi = as.POSIXct(datafi, format = '%d/%m/%Y %H:%M:%S'),
          horainici = as.POSIXct(horainici, format = '%d/%m/%Y %H:%M:%S'),
          horafi = as.POSIXct(horafi, format = '%d/%m/%Y %H:%M:%S')
        ) %>%
        ## select, renaming in the way, the variables (these names will be used later on
        ## the nfi3 and nfi2 datasets)
        select(
          old_idparcela = idparcela,
          old_idclasse_nfi4 = idclasse,
          feat_plot_type = tipusparcela,
          feat_soil_use = ussol,
          feat_forest_cover = coberturabosc,
          feat_forest_type_lvl3 = tipusbosc,
          feat_total_canopy_cover = fcctotal,
          feat_tree_canopy_cover = fccarbrada,
          feat_spatial_distribution = distribucioespacial,
          feat_specific_composition = composicioespecifica,
          feat_rocky = rocositat,
          feat_soil_texture = texturasol,
          feat_soil_type_1 = tipussol1,
          feat_soil_type_2 = tipussol2,
          feat_erosion = erosio,
          feat_combustion_model = modelcombustible,
          feat_org_matter_thickness = gruixmo,
          feat_cutoff_stock = existenciatallades,
          feat_stand_improvement_1 = tractamentmillora1,
          feat_stand_improvement_2 = tractamentmillora2,
          feat_soil_improvement_1 = tractamentmillorasol1,
          feat_soil_improvement_2 = tractamentmillorasol2,
          # topo_aspect_1 = orientacio1, # eliminar
          # topo_aspect_2 = orientacio2, # eliminar
          topo_max_slope_percentage_1 = maxpendent1,
          # topo_max_slope_percentage_2 = maxpendent2, # eliminar
          feat_pinpoint_easiness = localizacio,
          feat_access_easiness = acces,
          feat_sampling_easiness = mostreig,
          feat_sampling_start_date = datainici,
          feat_sampling_end_date = datafi,
          feat_sampling_start_time = horainici,
          feat_sampling_end_time = horafi,
          feat_observations = observacions
        )
    },
    by = c('old_idparcela', 'old_idclasse_nfi4')
  ) %>%
  
  ## join with the climatic data
  full_join(
    {
      tbl(access4_db, 'Parcela_Clima') %>%
        collect() %>%
        ## separate the idparcela and idclasse as they are joined, removing the _ character
        ## between the class and subclass
        {magrittr::set_names(., tolower(names(.)))} %>%
        tidyr::separate(
          idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
        ) %>%
        mutate(idclasse = stringr::str_remove(idclasse, '_')) %>%
        {
          ## let's change the names to english (in this case with a simple replace)
          set_names(., stringr::str_replace_all(
            names(.),
            c(
              'gener' = '_jan', 'febrer' = '_feb', 'març' = '_mar', 'abril' = '_apr',
              'maig' = '_may', 'juny' = '_jun', 'juliol' = '_jul', 'agost' = '_aug',
              'setembre' = '_sep', 'octubre' = '_oct', 'novembre' = '_nov',
              'desembre' = '_dec', 'anual' = '_year', 'tmit' = 'tmean'
            )
          ))
        } %>%
        {
          ## add the clim_ prefix
          set_names(., glue::glue("clim_{names(.)}"))
        } %>% 
        ## columns order by climatic variable
        select(
          old_idparcela = clim_idparcela, old_idclasse_nfi4 = clim_idclasse,
          -clim_id_grafic, -starts_with('clim_coor'), starts_with('clim_prec'),
          starts_with('clim_rad'), starts_with('clim_t')
        )
    },
    by = c('old_idparcela', 'old_idclasse_nfi4')
  ) %>%
  full_join(
    {
      tbl(access4_db, 'Parcela_ETP_ETR') %>%
        collect() %>%
        ## separate the idparcela and idclasse as they are joined, removing the _ character
        ## between the class and subclass
        {magrittr::set_names(., tolower(names(.)))} %>%
        tidyr::separate(
          idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
        ) %>%
        mutate(idclasse = stringr::str_remove(idclasse, '_')) %>%
        {
          ## let's change the names to english (in this case with a simple replace)
          set_names(., stringr::str_replace_all(
            names(.),
            c(
              'gener' = '_jan', 'febrer' = '_feb', 'març' = '_mar', 'abril' = '_apr',
              'maig' = '_may', 'juny' = '_jun', 'juliol' = '_jul', 'agost' = '_aug',
              'setembre' = '_sep', 'octubre' = '_oct', 'novembre' = '_nov',
              'desembre' = '_dec', 'anual' = '_year'
            )
          ))
        } %>%
        {
          ## add the clim_ prefix
          set_names(., glue::glue("clim_{names(.)}"))
        } %>% 
        ## columns order by climatic variable
        select(
          old_idparcela = clim_idparcela, old_idclasse_nfi4 = clim_idclasse,
          -clim_id_grafic, -starts_with('clim_coor'), starts_with('clim_etp'),
          starts_with('clim_etr')
        )
    },
    by = c('old_idparcela', 'old_idclasse_nfi4')
  ) %>%
  ## as there are topo_ and other prefixes variables mixed, let's order the columns again
  select(
    starts_with('old'), starts_with('admin'), starts_with('topo'), starts_with('feat'),
    starts_with('clim')
  ) %>%
  left_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(NFI_4) %>%
        select(plot_id, old_idparcela, old_idclasse_nfi4)
      
    }
  ) %>%
  # convert the feat_org_matter_thickness to the real character value
  left_join(
    gruixmo_thes %>% select(1:2), by = c('feat_org_matter_thickness' = 'IdGruixMO')
  ) %>% 
  mutate(
    feat_org_matter_thickness = GruixMO
  ) %>% 
  select(plot_id, everything(), -GruixMO) -> ifn4_plot_topo_clim_vars

ifn4_dynamic_plot_topo_clim_vars <- ifn4_plot_topo_clim_vars %>%
  select(
    plot_id,
    contains('cutoff'),
    contains('improvement'),
    contains('erosio'),
    feat_forest_cover,
    contains('forest_type_lvl'),
    contains('observation'),
    feat_org_matter_thickness,
    feat_plot_type,
    contains('sampling'),
    contains('pinpoint'),
    contains('canopy_cover')
  ) %>%
  mutate(
    feat_sampling_year = lubridate::year(feat_sampling_start_date)
  )

ifn4_plot_topo_clim_vars %<>%
  select(
    -contains('cutoff'),
    -contains('improvement'),
    -contains('erosio'),
    -feat_forest_cover,
    -contains('forest_type_lvl'),
    -contains('observation'),
    -feat_org_matter_thickness,
    -feat_plot_type,
    -contains('sampling'),
    -contains('pinpoint'),
    -contains('canopy_cover')
  )

## NFI3
tbl(oracle_db, 'parcelaifn3') %>%
  collect() %>%
  # we select (and rename in the fly) the variables of interest in this table
  select(
    old_idparcela = idparcela,
    old_idclasse_nfi3 = idclasse,
    feat_plot_type = tipusparcela,
    feat_soil_use = ussol,
    feat_forest_type_lvl_2 = nivell2,
    feat_forest_cover = fccnivell2,
    feat_forest_type_lvl3 = nivell2_3,
    feat_total_canopy_cover = fcctotal,
    feat_tree_canopy_cover = fccarboria,
    feat_spatial_distribution = distribucioespacial,
    feat_specific_composition = composicioespecifica,
    feat_rocky = rocositat,
    feat_soil_texture = texturasol,
    # feat_org_matter_content = contingutmo,
    # feat_soil_ph_class = phsol,
    # feat_soil_ph_value = valorphsol,
    feat_soil_type_1 = tipussol1,
    feat_soil_type_2 = tipussol2,
    feat_erosion = erosio,
    # feat_soil_surface_ph = phsolsuperficie,
    feat_combustion_model = idmodelcombustible,
    feat_org_matter_thickness = gruixmo,
    feat_cutoff_stock_type = talladaregeneracio,
    feat_stand_improvement_1 = milloravol1,
    feat_stand_improvement_2 = milloravol2,
    feat_soil_improvement_1 = millorasol1,
    feat_soil_improvement_2 = millorasol2,
    # topo_aspect_1 = orientacio1, # eliminar
    # topo_aspect_2 = orientacio2, # eliminar
    topo_max_slope_percentage_1 = pendent1,
    # topo_max_slope_percentage_2 = pendent2, # eliminar
    feat_pinpoint_easiness = localitzacio,
    feat_access_easiness = acces,
    feat_sampling_easiness = aixecament,
    feat_sampling_start_date = datamostreig,
    # feat_sampling_end_date = datafi, # does not exist
    # feat_sampling_start_time = tempsmostreig, # is duration, not start point
    # feat_sampling_end_time = horafi, # does not exist
    feat_observations = observacions
  ) %>%
  full_join(
    {
      tbl(oracle_db, 'parcelaifn3_sig') %>%
        collect() %>%
        # selecting and renaming the vars
        select(
          old_idparcela = idparcela,
          old_idclasse_nfi3 = idclasse,
          topo_altitude_asl = altitud,
          topo_fdm_slope_degrees = pendentgraus,
          topo_fdm_slope_percentage = pendentpercentatge,
          topo_fdm_aspect_degrees = orientacio,
          topo_fdm_aspect_cardinal_8 = orientacio_c8,
          topo_fdm_aspect_cardinal_4 = orientacio_c4,
        )
    },
    by = c('old_idparcela', 'old_idclasse_nfi3')
  ) %>%
  full_join(
    {
      tbl(oracle_db, 'parcelaifn3_clima') %>%
        collect() %>%
        {
          ## let's change the names to english (in this case with a simple replace)
          set_names(., stringr::str_replace_all(
            names(.),
            c(
              # months and year
              'gener' = '_jan', 'febrer' = '_feb', 'març' = '_mar', 'abril' = '_apr',
              'maig' = '_may', 'juny' = '_jun', 'juliol' = '_jul', 'agost' = '_aug',
              'setembre' = '_sep', 'octubre' = '_oct', 'novembre' = '_nov',
              'desembre' = '_dec', 'anual' = '_year',
              
              # var names
              'radiacio' = 'rad', 'temperaturaminima' = 'tmin',
              'temperaturamaxima' = 'tmax', 'temperaturamitjana' = 'tmean',
              'precipitacio' = 'prec',
              'etr_s_' = 'etr_s', 'etr_p_' = 'etr_p', 'etp_' = 'etp'
            )
          ))
        } %>%
        {
          ## add the clim_ prefix
          set_names(., glue::glue("clim_{names(.)}"))
        } %>% 
        ## columns order by climatic variable
        select(
          old_idparcela = clim_idparcela, old_idclasse_nfi3 = clim_idclasse,
          -starts_with('clim_coor'), starts_with('clim_prec'),
          starts_with('clim_rad'), starts_with('clim_t'),
          starts_with('clim_etp'), starts_with('clim_etr'), starts_with('clim_npp')
        )
    },
    by = c('old_idparcela', 'old_idclasse_nfi3')
  ) %>%
  mutate(
    # we need to convert to cardinal points the topo_fdm_cardinal_X vars
    topo_fdm_aspect_cardinal_8 = case_when(
      topo_fdm_aspect_cardinal_8 == 0 ~ 'N',
      topo_fdm_aspect_cardinal_8 == 45 ~ 'NE',
      topo_fdm_aspect_cardinal_8 == 90 ~ 'E',
      topo_fdm_aspect_cardinal_8 == 135 ~ 'SE',
      topo_fdm_aspect_cardinal_8 == 180 ~ 'S',
      topo_fdm_aspect_cardinal_8 == 225 ~ 'SW',
      topo_fdm_aspect_cardinal_8 == 270 ~ 'W',
      topo_fdm_aspect_cardinal_8 == 315 ~ 'NW'
    ),
    topo_fdm_aspect_cardinal_4 = case_when(
      topo_fdm_aspect_cardinal_4 == 0 ~ 'N',
      topo_fdm_aspect_cardinal_4 == 90 ~ 'E',
      topo_fdm_aspect_cardinal_4 == 180 ~ 'S',
      topo_fdm_aspect_cardinal_4 == 270 ~ 'W'
    ),
    # feat_combustion_model must be character
    feat_combustion_model = as.character(feat_combustion_model)
  ) %>% 
  left_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(NFI_3) %>%
        select(plot_id, old_idparcela, old_idclasse_nfi3)
      
    }
  ) -> ifn3_all_plot_topo_clim_vars

ifn3_all_plot_topo_clim_vars %>%
  right_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(!NFI_4, NFI_3) %>%
        select(plot_id, old_idparcela)
    }
  ) %>%
  select(plot_id, everything()) -> ifn3_plot_topo_clim_vars

ifn3_dynamic_plot_topo_clim_vars <- ifn3_all_plot_topo_clim_vars %>%
  filter(old_idparcela != '251955') %>%
  select(
    plot_id,
    contains('cutoff'),
    contains('improvement'),
    contains('erosio'),
    feat_forest_cover,
    contains('forest_type_lvl'),
    contains('observation'),
    feat_org_matter_thickness,
    feat_plot_type,
    contains('sampling'),
    contains('pinpoint'),
    contains('canopy_cover')
  ) %>%
  mutate(
    feat_sampling_year = lubridate::year(feat_sampling_start_date)
  )

ifn3_plot_topo_clim_vars %<>%
  select(
    -contains('cutoff'),
    -contains('improvement'),
    -contains('erosio'),
    -feat_forest_cover,
    -contains('forest_type_lvl'),
    -contains('observation'),
    -feat_org_matter_thickness,
    -feat_plot_type,
    -contains('sampling'),
    -contains('pinpoint'),
    -contains('canopy_cover')
  )

## NFI2
tbl(oracle_db, 'parcelaifn2') %>%
  collect() %>%
  # we select (and rename in the fly) the variables of interest in this table
  select(
    old_idparcela = idparcela,
    feat_soil_use = ussolcamp,
    #feat_canopy_cover_level_2 = classecobertura, ## No se añade
    feat_total_canopy_cover = fcccamp,
    feat_spatial_distribution = distribucioespacial,
    feat_specific_composition = composicioespecifica,
    feat_soil_texture = texturasol,
    feat_erosion = erosio,
    feat_cutoff_stock_type = talladaregeneracio,
    feat_stand_improvement_1 = milloravol1,
    feat_stand_improvement_2 = milloravol2,
    feat_soil_improvement_1 = millorasol1,
    feat_soil_improvement_2 = millorasol2,
    topo_max_slope_percentage_1 = pendent1,
    feat_sampling_start_date = anymostreig
  ) %>%
  full_join(
    {
      tbl(oracle_db, 'parcelaifn2_sig') %>%
        collect() %>%
        select(
          old_idparcela = idparcela,
          feat_canopy_type = tipuscoberta,
          topo_altitude_asl = altitud,
          topo_fdm_slope_degrees = pendentgraus,
          topo_fdm_slope_percentage = pendentpercentatge,
          topo_fdm_aspect_degrees = orientacio,
          topo_fdm_aspect_cardinal_8 = orientacio_c8,
          topo_fdm_aspect_cardinal_4 = orientacio_c4
        )
    },
    by = 'old_idparcela'
  ) %>%
  full_join(
    {
      tbl(oracle_db, 'parcelaifn2_clima') %>%
        collect() %>%
        {
          ## let's change the names to english (in this case with a simple replace)
          set_names(., stringr::str_replace_all(
            names(.),
            c(
              # months and year
              'gener' = '_jan', 'febrer' = '_feb', 'març' = '_mar', 'abril' = '_apr',
              'maig' = '_may', 'juny' = '_jun', 'juliol' = '_jul', 'agost' = '_aug',
              'setembre' = '_sep', 'octubre' = '_oct', 'novembre' = '_nov',
              'desembre' = '_dec', 'anual' = '_year',
              
              # var names
              'radiacio' = 'rad', 'temperaturaminima' = 'tmin',
              'temperaturamaxima' = 'tmax', 'temperaturamitjana' = 'tmean',
              'precipitacio' = 'prec',
              'etr_s_' = 'etr_s', 'etr_p_' = 'etr_p', 'etp_' = 'etp'
            )
          ))
        } %>%
        {
          ## add the clim_ prefix
          set_names(., glue::glue("clim_{names(.)}"))
        } %>% 
        ## columns order by climatic variable
        select(
          old_idparcela = clim_idparcela,
          -starts_with('clim_coor'), starts_with('clim_prec'),
          starts_with('clim_rad'), starts_with('clim_t'),
          starts_with('clim_etp'), starts_with('clim_etr'), starts_with('clim_npp')
        )
    },
    by = 'old_idparcela'
  ) %>%
  mutate(
    # we need to convert to dttm the feat_sampling_start_date
    feat_sampling_start_date = as.POSIXct(as.character(feat_sampling_start_date), format = '%Y'),
    # we need to convert to cardinal points the topo_fdm_cardinal_X vars
    topo_fdm_aspect_cardinal_8 = case_when(
      topo_fdm_aspect_cardinal_8 == 0 ~ 'N',
      topo_fdm_aspect_cardinal_8 == 45 ~ 'NE',
      topo_fdm_aspect_cardinal_8 == 90 ~ 'E',
      topo_fdm_aspect_cardinal_8 == 135 ~ 'SE',
      topo_fdm_aspect_cardinal_8 == 180 ~ 'S',
      topo_fdm_aspect_cardinal_8 == 225 ~ 'SW',
      topo_fdm_aspect_cardinal_8 == 270 ~ 'W',
      topo_fdm_aspect_cardinal_8 == 315 ~ 'NW'
    ),
    topo_fdm_aspect_cardinal_4 = case_when(
      topo_fdm_aspect_cardinal_4 == 0 ~ 'N',
      topo_fdm_aspect_cardinal_4 == 90 ~ 'E',
      topo_fdm_aspect_cardinal_4 == 180 ~ 'S',
      topo_fdm_aspect_cardinal_4 == 270 ~ 'W'
    )
  ) %>%
  left_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(NFI_2) %>%
        select(plot_id, old_idparcela)
    }
  ) -> ifn2_all_plot_topo_clim_vars

ifn2_all_plot_topo_clim_vars %>% right_join(
  {
    ifn2_ifn3_ifn4_plots %>%
      filter(!NFI_4, !NFI_3, NFI_2) %>%
      select(plot_id, old_idparcela)
  }
) %>%
  select(plot_id, everything()) -> ifn2_plot_topo_clim_vars

ifn2_dynamic_plot_topo_clim_vars <- ifn2_all_plot_topo_clim_vars %>%
  select(
    plot_id,
    contains('cutoff'),
    contains('improvement'),
    contains('erosio'),
    # feat_forest_cover,
    contains('forest_type_lvl'),
    contains('observation'),
    # feat_org_matter_thickness,
    # feat_plot_type,
    contains('sampling'),
    contains('pinpoint'),
    contains('canopy_cover')
  ) %>%
  mutate(
    feat_sampling_year = lubridate::year(feat_sampling_start_date)
  )

ifn2_plot_topo_clim_vars %<>%
  select(
    -contains('cutoff'),
    -contains('improvement'),
    -contains('erosio'),
    # -feat_forest_cover,
    -contains('forest_type_lvl'),
    -contains('observation'),
    # -feat_org_matter_thickness,
    # -feat_plot_type,
    -contains('sampling'),
    -contains('pinpoint'),
    -contains('canopy_cover')
  )


topo_clim_info <- bind_rows(
  ifn2_plot_topo_clim_vars, ifn3_plot_topo_clim_vars, ifn4_plot_topo_clim_vars
) %>%
  arrange(plot_id)

#### STEP 2 Admin and Ownership info ####
## We need the ownership info, located in a shapefile. We load the shapefile and we apply
## an over to the belonging of the plots to the variables present in the shapefile.
## This, we do it with all the plots, as the maps are the most recent ones

ifn2_ifn3_ifn4_plots %>%
  select(longitude, latitude) %>% 
  sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
  sp::over(
    {
      rgdal::readOGR('data_raw/ownership_layer', 'Forests',
                     GDAL1_integer64_policy = FALSE) %>%
        sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
    }
  ) %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  as_data_frame() %>%
  mutate_if(
    is.factor, ~tolower(as.character(.x))
  ) %>%
  mutate(
    elen = case_when(
      is.na(elen) ~ NA,
      elen == 'no' ~ FALSE,
      TRUE ~ TRUE
    ),
    conveni = case_when(
      is.na(conveni) ~ NA,
      conveni == 'no' ~ FALSE,
      TRUE ~ TRUE
    ),
    ordenacio = case_when(
      is.na(ordenacio) ~ NA,
      ordenacio == 'no' ~ FALSE,
      TRUE ~ TRUE
    ),
    certific = case_when(
      is.na(certific) ~ NA,
      certific == 'no' ~ FALSE,
      TRUE ~ TRUE
    ),
    # we create a new variable because the ownership type private is not really private,
    # and all the NA plots are really really private
    feat_ownership_regime = case_when(
      is.na(tip_prop) ~ 'privat',
      TRUE ~ 'public'
    )
  ) %>%
  bind_cols(
    ifn2_ifn3_ifn4_plots %>%
      select(plot_id)
  ) %>%
  select(
    plot_id,
    #feat_forest_id = fo_codi,
    feat_forest_name = forest,
    feat_ownership_type = tip_prop,
    feat_ownership_regime,
    feat_cup = cup, # TODO Revisar
    feat_elen = elen, # TODO Revisar
    feat_owner = titular,
    feat_agreement = conveni,
    feat_management = ordenacio,
    feat_certificate = certific
  ) -> ownership_info

## We also need the updated administrative divisions info. For that, again we load the
## shapefiles from the administrative divs and use sp::over

ifn2_ifn3_ifn4_plots %>%
  select(longitude, latitude) %>% 
  sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
  sp::over(
    {
      rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpm1_20180101_0',
                     GDAL1_integer64_policy = FALSE) %>%
        sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpc1_20180101_0',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        )
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpv1_20180101_0',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        )
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpp1_20180101_0',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        )
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'delegacions2018',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        )
    }
  ) %>% 
  ## Add the enpes, pein and xn2000 belongings
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'enpe_2017',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        ) %>% 
        select(nom_enpe = nom)
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'pein_2017',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        ) %>% 
        select(nom_pein = nom)
    }
  ) %>%
  bind_cols(
    {
      ifn2_ifn3_ifn4_plots %>%
        select(longitude, latitude) %>% 
        sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84")) %>%
        sp::over(
          {
            rgdal::readOGR('data_raw/shapefiles', 'xn2000_2017',
                           GDAL1_integer64_policy = FALSE) %>%
              sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))
          }
        ) %>% 
        select(nom_xn2000 = nom_n2)
    }
  ) %>%
  as_data_frame() %>%
  mutate_if(
    is.factor, as.character
  ) %>%
  bind_cols(
    ifn2_ifn3_ifn4_plots %>%
      select(plot_id)
  ) %>%
  select(
    plot_id,
    admin_province = NOMPROV,
    # admin_delegation = comarcas_d,
    admin_region = NOMCOMAR,
    admin_vegueria = NOMVEGUE,
    admin_municipality = NOMMUNI,
    admin_province_id = CODIPROV,
    admin_region_id = CODICOMAR,
    admin_municipality_id = CODIMUNI,
    admin_natural_interest_area = nom_enpe,
    admin_special_protection_natural_area = nom_pein,
    admin_natura_network_2000 = nom_xn2000
  ) -> admin_info

## TODO Check this cases
# admin_info %>% filter(admin_province == 'Barcelona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Girona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Lleida') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Tarragona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Girona', admin_delegation == 'Barcelona')

#### STEP 3 Species/Genus... thesauruses ####

# We need to create the species/genus/... thesauruses to be able to link the functional
# groups to the old databases.
# Probably we will need to create a new id for each functional group as they change
# between NFI versions
tbl(oracle_db, 'tesaureespecie') %>%
  collect() %>% 
  select(
    code_id = idcodi,
    species_id = idespecie,
    simpspecies_id = idespeciesimple,
    genus_id = idgenere,
    dec_id = idcaducesclerconif,
    bc_id = idplanifconif,
    species_id_nfi2 = idespecieifn2
  ) -> species_table_oracle

# tbl(access4_db, 'TesaureEspecieIFN3') %>%
#   collect() %>% 
#   select(
#     code_id = IdEspecieIFN3,
#     species_id = Especie
#   ) -> species_table_access_ifn3

tbl(access4_db, 'TesaureEspecieIFN4') %>%
  collect() %>% 
  select(
    code_id = IdEspecieIFN4,
    species_id = Especie,
    simpspecies_id = EspecieSimplificat,
    genus_id = Genere,
    dec_id = CaducEsclerConif,
    bc_id = PlanifConif,
    species_id_nfi2 = EspecieIFN2
  ) -> species_table_access

species_table_oracle %>%
  # full_join(
  #   species_table_access_ifn3, by = 'code_id', suffix = c('_oracle', '_accessIFN3')
  # ) %>%
  full_join(
    species_table_access, by = 'code_id', suffix = c('_NFI2_3', '_NFI4')
  ) -> species_table

## TODO Check with Vayreda if this is ok
