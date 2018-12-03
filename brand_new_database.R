# New database version script
# 
# This is needed because the db must be improved to better implementation of all
# the data.

#### Libraries and data access ####
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(magrittr)
library(sp)
library(rgdal)
library(stringr)
library(glue)
# library(tidyIFN)

# db connections
oracle_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn'
)

access4_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifn4_access'
)

brand_new_nfi_db <- pool::dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'tururu'
)

#### STEP 1 NFI2 and NFI3 plots ####
# The most complete table is the parcela_ifn3 table from the oracle database.
# It has all the IFN2 and IFN3 plots, but (and this is a big BUT) some of the
# plots present in the IFN3 are different from those in IFN2, even though they
# have same id and coordinates.
# So, the first thing is to retrieve the parcela_ifn3 table, check which of those
# plots are different in the ifn2

ifn2_plots <- tbl(oracle_db, 'parcelaifn2_sig') %>%
  select(1:4) %>%
  collect()
# coordinates change to ETRS89
ifn2_plots %>%
  select(utm_x, utm_y) %>%
  collect() -> coords_utm_ED50_ifn2

coordinates(coords_utm_ED50_ifn2) <- ~ utm_x+utm_y
proj4string(coords_utm_ED50_ifn2) <- CRS('+init=epsg:23031')

coords_utm_ETRS89_ifn2 <- spTransform(
  coords_utm_ED50_ifn2,
  CRS('+init=epsg:25831')
)

ifn2_plots %<>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn2@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn2@coords[,2])
  )

ifn3_plots <- tbl(oracle_db, 'parcelaifn3_sig') %>%
  select(1:5) %>%
  mutate(
    NFI_3 = TRUE,
    # there is a problem with plot 251955 as it is classified as A1 but there is no record
    # for it in the NFI2. So we will transform it class to NN
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  collect()

# coordinates change to ETRS89
ifn3_plots %>%
  select(utm_x, utm_y) %>%
  collect() -> coords_utm_ED50_ifn3

coordinates(coords_utm_ED50_ifn3) <- ~ utm_x+utm_y
proj4string(coords_utm_ED50_ifn3) <- CRS('+init=epsg:23031')

coords_utm_ETRS89_ifn3 <- spTransform(
  coords_utm_ED50_ifn3,
  CRS('+init=epsg:25831')
)

ifn3_plots %<>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn3@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn3@coords[,2])
  )

# in both, 2 and 3
ifn3_A1 <- ifn3_plots %>%
  filter(idclasse == 'A1')

# in both, 2 and 3
ifn3_A3C <- ifn3_plots %>%
  filter(idclasse == 'A3C')

# only in 3, but with idparcela present in 2 corresponding to the a3c plots
ifn3_A3E <- ifn3_plots %>%
  filter(idclasse == 'A3E')

# only in 3, but with idparcela present in 2
ifn3_A4 <- ifn3_plots %>%
  filter(idclasse == 'A4')

# only in 3, but with idparcela present in 2
ifn3_A4C <- ifn3_plots %>%
  filter(idclasse == 'A4C')

# only in 3, but with idparcela present in 2
ifn3_A6C <- ifn3_plots %>%
  filter(idclasse == 'A6C')

# only in 3, no idparcela in 2
ifn3_NN <- ifn3_plots %>%
  filter(idclasse == 'NN')

# satellites
ifn3_R <- ifn3_plots %>%
  filter(idclasse == 'R1' | idclasse == 'R2')

# nrow(ifn3_A1) + nrow(ifn3_A3C) + nrow(ifn3_A3E) + nrow(ifn3_A4) + nrow(ifn3_A4C) +
#   nrow(ifn3_A6C) + nrow(ifn3_NN) + nrow(ifn3_R) == nrow(ifn3_plots)

# ifn2 classe creation to mimic ifn3 ones and this way be able to join by idparcela
# and idclasse
ifn2_plots %<>%
  mutate(
    idclasse = case_when(
      # when plots in ifn3 are A1 or A3C and the plot exists in ifn2 we put on those classes
      # on the ifn2 brand new idclasse var
      .[['idparcela']] %in% ifn3_A1[['idparcela']] ~ 'A1',
      .[['idparcela']] %in% ifn3_A3C[['idparcela']] ~ 'A3C',
      # if the plot has any other class, then the ifn2 class will be NN
      TRUE ~ 'NR'
    ),
    NFI_2 = TRUE
  )

# join both ifns
ifn2_ifn3_plots <- full_join(
  ifn2_plots, ifn3_plots, by = c('idparcela', 'idclasse'), suffix = c('_nfi2', '_nfi3')
) %>%
  mutate(
    NFI_2 = if_else(
      is.na(NFI_2), FALSE, NFI_2
    ),
    NFI_3 = if_else(
      is.na(NFI_3), FALSE, NFI_3
    ),
    old_idclasse_nfi3 = idclasse
  )

#### STEP 2 NFI4 plots added ####
# Now we need to join the NFI4 plots. In this case only the A1 class from this inventory
# can be in the NFI3 (even if in the NFI3 they have other class), the rest are new, even
# if they have the same idparcela. But remember, the NFI2 and NFI3 A3C plots don't exist
# in the NFI4, only the NFI3 A3E can be in the NFI4 

ifn4_plots <- tbl(access4_db, 'ParcelaIFN4_OLAP') %>%
  select(IdParcela, IdClasse, CoordX, Coordy) %>%
  rename(
    idparcela = IdParcela, idclasse = IdClasse,
    utm_x_nfi4 = CoordX, utm_y_nfi4 = Coordy
  ) %>%
  mutate(NFI_4 = TRUE, old_idclasse_nfi4 = idclasse) %>%
  collect()

# in both, ifn3 and ifn4
ifn4_A1 <- ifn4_plots %>%
  filter(old_idclasse_nfi4 == 'A1')

# only ifn4
ifn4_A4 <- ifn4_plots %>%
  filter(old_idclasse_nfi4 == 'A4')

# only ifn4, without big trees
ifn4_A4C <- ifn4_plots %>%
  filter(old_idclasse_nfi4 == 'A4C')

# only ifn4, with big trees
ifn4_A6C <- ifn4_plots %>%
  filter(old_idclasse_nfi4 == 'A6C')

# only ifn4, new ones
ifn4_NN <- ifn4_plots %>%
  filter(old_idclasse_nfi4 == 'NN')

# satellites, forget about them :P
ifn4_NUMBER <- ifn4_plots %>%
  filter(old_idclasse_nfi4 %in% c('16C', '14', '11', '12'))

# In order to join the ifn4 plots to the ifn2_ifn3 plots we need to establish a new class
# that involves the A1 IFN4 plots (the only ones in this version that may be available
# in the ifn2_ifn3) and the A1, A4, A3E, A4C, A6C and NN classes in the ifn2_ifn3 table
# as this ones may have correspondence in the fourth version

ifn2_ifn3_plots %<>%
  mutate(
    idclasse = case_when(
      NFI_3 & idclasse %in% c('A1', 'A4', 'A3E', 'A4C', 'A6C', 'NN') ~ 'PC',
      TRUE ~ idclasse
    )
  )

ifn4_plots %<>%
  mutate(
    idclasse = case_when(
      idclasse == 'A1' ~ 'PC',
      TRUE ~ idclasse
    )
  )

# and finally join the tables
ifn2_ifn3_ifn4_plots <- full_join(
  ifn2_ifn3_plots, ifn4_plots, by = c('idparcela', 'idclasse')
) %>%
  mutate(
    NFI_2 = if_else(
      is.na(NFI_2), FALSE, NFI_2
    ),
    NFI_3 = if_else(
      is.na(NFI_3), FALSE, NFI_3
    ),
    NFI_4 = if_else(
      is.na(NFI_4), FALSE, NFI_4
    )
  )

#### STEP 3 New id for the plots, and lotlang coordinates ####

# We create an unique plot_id for each of the plots. This id will be P (from plot) and an
# incremental number
ifn2_ifn3_ifn4_plots %<>%
  arrange(idparcela) %>% 
  mutate(
    plot_id = paste0('P_', str_pad(1:nrow(.), 5, 'left', '0'))
  ) %>%
  
  # Now, and this is tricky, we need to update the info from ifn4 in those plots that have
  # info for ifn4 plots, as this is the most updated info. We already converted the ED50
  # from ifn2 and ifn3 to ETRS89, so we can also create the lat long values
  
  mutate(
    utm_x_ETRS89 = case_when(
      NFI_4 ~ utm_x_nfi4 * 1.0, # because nfi4 is integer and nfi2 and 3 are double
      NFI_3 & !NFI_4 ~ utm_x_nfi3,
      NFI_2 & !NFI_3 & !NFI_4 ~ utm_x_nfi2,
      TRUE ~ NA_real_
    ),
    utm_y_ETRS89 = case_when(
      NFI_4 ~ utm_y_nfi4 * 1.0, # because nfi4 is integer and nfi2 and 3 are double
      NFI_3 & !NFI_4 ~ utm_y_nfi3,
      NFI_2 & !NFI_3 & !NFI_4 ~ utm_y_nfi2,
      TRUE ~ NA_real_
    )
  )

ifn2_ifn3_ifn4_plots %>%
  select(utm_x_ETRS89, utm_y_ETRS89) -> coords_utm_ETRS89

coordinates(coords_utm_ETRS89) <- ~ utm_x_ETRS89+utm_y_ETRS89
proj4string(coords_utm_ETRS89) <- CRS('+init=epsg:25831')

coords_latlong_WGS84 <- spTransform(
  coords_utm_ETRS89,
  CRS("+proj=longlat +datum=WGS84")
)

ifn2_ifn3_ifn4_plots %<>%
  mutate(
    longitude = as.numeric(coords_latlong_WGS84@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84@coords[,2])
  ) %>%
  
  # now we reorder and select the variables that we want, and also left the old ones with
  # the old_ prefix
  select(
    plot_id, utm_x_ETRS89, utm_y_ETRS89, longitude, latitude, NFI_2, NFI_3, NFI_4,
    old_idparcela = idparcela, old_idclasse_nfi3, old_idclasse_nfi4,
    old_utm_x_nfi2 = utm_x_nfi2,
    old_utm_y_nfi2 = utm_y_nfi2,
    old_utm_x_nfi3 = utm_x_nfi3,
    old_utm_y_nfi3 = utm_y_nfi3
  )

#### STEP 4 Topo and climatic info ####
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

## TODO Dates are not imported from access database :( They must to be done separately

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
    feat_sampling_start_time = tempsmostreig,
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

#### STEP 5 Admin and Ownership info ####
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
    admin_natural_interest_area = nom_pein,
    admin_special_protection_natural_area = nom_enpe,
    admin_natura_network_2000 = nom_xn2000
  ) -> admin_info

## TODO Check this cases
# admin_info %>% filter(admin_province == 'Barcelona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Girona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Lleida') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Tarragona') %>% pull(admin_delegation) %>% unique()
# admin_info %>% filter(admin_province == 'Girona', admin_delegation == 'Barcelona')

#### STEP 13 Species/Genus... thesauruses ####

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
## TODO The rest of the regeneration tables (nfi3 and nfi4) must be done

#### STEP 6 PLOT general table ####
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

# dynamics PLOTS tables
PLOTS_NFI_2_DYNAMIC_INFO <- ifn2_dynamic_plot_topo_clim_vars
PLOTS_NFI_3_DYNAMIC_INFO <- ifn3_dynamic_plot_topo_clim_vars
PLOTS_NFI_4_DYNAMIC_INFO <- ifn4_dynamic_plot_topo_clim_vars


#### STEP 7 Results tables ####
# Now, we need to create the results tables, renaming the variables and standardizing
# everything.

## We are gonna need the new plot id for each nfi level
plot_id_nfi_2 <- PLOTS %>%
  filter(presence_NFI_2) %>%
  select(plot_id, old_idparcela)

plot_id_nfi_3 <- PLOTS %>%
  filter(presence_NFI_3) %>%
  select(plot_id, old_idparcela, old_idclasse_nfi3)

plot_id_nfi_4 <- PLOTS %>%
  filter(presence_NFI_4) %>%
  select(plot_id, old_idparcela, old_idclasse_nfi4)

## Results at plot level for each nfi level
tbl(oracle_db, 'r_parcela_ifn2') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    basal_area = ab,
    basal_area_dead = abmorts,
    basal_area_dec_dominant = caducesclerconifab,
    density_dec_dominant = caducesclerconifdens,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    basal_area_species_dominant = especieab,
    density_species_dominant = especiedens,
    basal_area_simp_species_dominant = especiesimpleab,
    density_simp_species_dominant = especiesimpledens,
    basal_area_genus_dominant = genereab,
    density_genus_dominant = generedens,
    #over_bark_volume_increment = iavc,
    basal_area_dec_percentage = percabcaducesclerconif,
    basal_area_species_percentage = percabespecie,
    basal_area_simp_species_percentage = percabespeciesimple,
    basal_area_genus_percentage = percabgenere,
    basal_area_bc_percentage = percabplanifconif,
    density_dec_percentage = percdenscaducesclerconif,
    density_species_percentage = percdensespecie,
    density_simp_species_percentage = percdensespeciesimple,
    density_genus_percentage = percdensgenere,
    density_bc_percentage = percdensplanifconif,
    basal_area_bc_dominant = planifconifab,
    density_bc_dominant = planifconifdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #fuelwood_volume = vle,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) -> PLOT_NFI_2_RESULTS

tbl(oracle_db, 'r_parcela_ifn3') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    basal_area_dec_dominant = caducesclerconifab,
    density_dec_dominant = caducesclerconifdens,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    basal_area_species_dominant = especieab,
    density_species_dominant = especiedens,
    basal_area_simp_species_dominant = especiesimpleab,
    density_simp_species_dominant = especiesimpledens,
    basal_area_genus_dominant = genereab,
    density_genus_dominant = generedens,
    lai = iaf,
    #over_bark_volume_increment = iavc,
    #over_bark_volume_increment_creaf = iavc_creaf,
    basal_area_dec_percentage = percabcaducesclerconif,
    basal_area_species_percentage = percabespecie,
    basal_area_simp_species_percentage = percabespeciesimple,
    basal_area_genus_percentage = percabgenere,
    basal_area_bc_percentage = percabplanifconif,
    density_dec_percentage = percdenscaducesclerconif,
    density_species_percentage = percdensespecie,
    density_simp_species_percentage = percdensespeciesimple,
    density_genus_percentage = percdensgenere,
    density_bc_percentage = percdensplanifconif,
    # gross_leaf_production = ph,
    basal_area_bc_dominant = planifconifab,
    density_bc_dominant = planifconifdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #fuelwood_volume = vle,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) -> PLOT_NFI_3_RESULTS

tbl(access4_db, 'Resultat_IFN4_CREAF_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id, #everything()
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    basal_area_dec_dominant = caducesclerconifab,
    density_dec_dominant = caducesclerconifdens,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    basal_area_species_dominant = especieab,
    density_species_dominant = especiedens,
    # basal_area_simp_species_dominant = especiesimpleab,
    # density_simp_species_dominant = especiesimpledens,
    basal_area_genus_dominant = genereab,
    density_genus_dominant = generedens,
    lai = iaf,
    # #over_bark_volume_increment = iavc,
    # #over_bark_volume_increment_creaf = iavc_creaf,
    basal_area_dec_percentage = percabcaducesclerconif,
    basal_area_species_percentage = percabespecie,
    # basal_area_simp_species_percentage = percabespeciesimple,
    basal_area_genus_percentage = percabgenere,
    basal_area_bc_percentage = percabplanifconif,
    density_dec_percentage = percdensitatcaducesclerconif,
    density_species_percentage = percdensitatespecie,
    # density_simp_species_percentage = percdensitatespeciesimple,
    density_genus_percentage = percdensitatgenere,
    density_bc_percentage = percdensitatplanifconif,
    # gross_leaf_production = ph,
    basal_area_bc_dominant = planifconifab,
    density_bc_dominant = planifconifdens,
    canopy_cover = rc,
    basal_area_forest_type = tipusboscab,
    density_forest_type = tipusboscdens,
    over_bark_volume = vcc,
    over_bark_volume_dead = vcc_morts,
    # #fuelwood_volume = vle,
    under_bark_volume = vsc,
    under_bark_volume_dead = vsc_morts
  ) -> PLOT_NFI_4_RESULTS ## TODO add simplified species when the that table is available

## Results at species level for each nfi level
tbl(oracle_db, 'r_especie_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    species_id = idespecieifn2,
    basal_area = ab,
    basal_area_dead = abmorts,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_2_RESULTS

tbl(oracle_db, 'r_especie_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    species_id = idespecie,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_3_RESULTS

tbl(access4_db, 'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id, #everything()
    species_id = especie,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdensitat,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_4_RESULTS

## Results at simplified species level for each nfi level
tbl(oracle_db, 'r_espsimple_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    simpspecies_id = idespeciesimple,
    basal_area = ab,
    basal_area_dead = abmorts,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_2_RESULTS

tbl(oracle_db, 'r_espsimple_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    simpspecies_id = idespeciesimple,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_3_RESULTS

# for simplified species at nfi_4 level, we need to build the table as it does not exist
tbl(access4_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, EspecieSimplificat) %>%
      # Rhamnus alaternus has not simplified species value (is NA), which will throw an
      # error when using simpspecies_id as part of the primary key in the build database
      # step. So we will put the species as simpspecies for this shrub/tree
      mutate(
        EspecieSimplificat = case_when(
          Especie == 'Rhamnus alaternus' ~ 'Rhamnus alaternus',
          TRUE ~ EspecieSimplificat
        )
      ) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, EspecieSimplificat, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, EspecieSimplificat) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, EspecieSimplificat, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespeciesimp = especiesimplificat,
    percdens = percdensitat
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything()
    simpspecies_id = idespeciesimp,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_4_RESULTS

## Results at genus level for each nfi level
tbl(oracle_db, 'r_genere_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_2_RESULTS

tbl(oracle_db, 'r_genere_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_3_RESULTS

# for genus at nfi_4 level, we need to build the table as it does not exist
tbl(access4_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, Genere) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, Genere, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, Genere) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, Genere, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idgenere = genere,
    percdens = percdensitat
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything()
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_4_RESULTS

## Results at broadleaf-conifer level for each nfi level
tbl(oracle_db, 'r_plancon_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_2_RESULTS

tbl(oracle_db, 'r_plancon_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_3_RESULTS

tbl(access4_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, PlanifConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, PlanifConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, PlanifConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, PlanifConif, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idplanifconif = planifconif,
    percdens = percdensitat
  ) %>% 
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything()
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_4_RESULTS

## Results at deciduous-esclerophyl-conifer level for each nfi level
tbl(oracle_db, 'r_cadesclcon_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    dec_id = idcaducesclerconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_2_RESULTS

tbl(oracle_db, 'r_cadesclcon_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    dec_id = idcaducesclerconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_3_RESULTS

tbl(access4_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, CaducEsclerConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, CaducEsclerConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, CaducEsclerConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, CaducEsclerConif, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idcadesccon = caducesclerconif,
    percdens = percdensitat
  ) %>% 
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything()
    dec_id = idcadesccon,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    dbh = dbh,
    dbh_dead = dbhmorts,
    density = densitat,
    density_dead = densitatmorts,
    order_basal_area = ordreab,
    order_density = ordredens,
    basal_area_percentage = percab,
    density_percentage = percdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    order_basal_area = order_basal_area * 1.0,
    order_density = order_density * 1.0
    # diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_4_RESULTS

## Results at plot level and broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_cd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> PLOT_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_cd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> PLOT_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db, 'ResultatCD_IFN4_CREAF_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> PLOT_NFI_4_DIAMCLASS_RESULTS ## TODO add simplified species when the that table is available

## Results at species level broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_especiecd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    species_id = idespecieifn2,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_especiecd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    species_id = idespecie,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db, 'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    species_id = especie,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_NFI_4_DIAMCLASS_RESULTS

## Results at simplified species level broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_espsimplecd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    simpspecies_id = idespeciesimple,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_espsimplecd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    simpspecies_id = idespeciesimple,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, EspecieSimplificat) %>%
      # Rhamnus alaternus has not simplified species value (is NA), which will throw an
      # error when using simpspecies_id as part of the primary key in the build database
      # step. So we will put the species as simpspecies for this shrub/tree
      mutate(
        EspecieSimplificat = case_when(
          Especie == 'Rhamnus alaternus' ~ 'Rhamnus alaternus',
          TRUE ~ EspecieSimplificat
        )
      ) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespeciesimp = especiesimplificat
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    simpspecies_id = idespeciesimp,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS

## Results at genus level broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_generecd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_generecd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, Genere) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, Genere, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, Genere) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, Genere,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idgenere = genere
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    genus_id = idgenere,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> GENUS_NFI_4_DIAMCLASS_RESULTS

## Results at dec level broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_cadesclconcd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    dec_id = idcaducesclerconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_cadesclconcd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    dec_id = idcaducesclerconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, CaducEsclerConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idcadesccon = caducesclerconif
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    dec_id = idcadesccon,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> DEC_NFI_4_DIAMCLASS_RESULTS

## Results at bc level broken down by diameter classes for each nfi level
tbl(oracle_db, 'r_planconcd_ifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    #what_the_hell_is_this = vc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    #what_the_hell_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_2_DIAMCLASS_RESULTS

tbl(oracle_db, 'r_planconcd_ifn3_creaf') %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_3_DIAMCLASS_RESULTS

tbl(access4_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(access4_db, 'TesaureEspecieIFN4') %>%
      select(Especie, PlanifConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, PlanifConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, PlanifConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, PlanifConif,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idplanifconif = planifconif
  ) %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  ungroup() %>% 
  select(
    plot_id, #everything(),
    diamclass_id = idcd,
    bc_id = idplanifconif,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = bh,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = br,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = ch,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = cr,
    density = densitat,
    density_dead = densitatmorts,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) %>%
  # uniformize some variables in terms of class
  mutate(
    # order_basal_area = order_basal_area * 1.0,
    # order_density = order_density * 1.0
    diamclass_id = as.character(diamclass_id)
  ) -> BC_NFI_4_DIAMCLASS_RESULTS

#### STEP 8 Plot comparision tables ####
tbl(oracle_db, 'r_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> PLOT_COMP_NFI2_NFI3_RESULTS

tbl(access4_db, 'Resultat_IFN4_IFN3_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  filter(idclasseifn4 == 'A1') %>%
  left_join(
    plot_id_nfi_4 %>% select(old_idparcela, old_idclasse_nfi4, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    density_diss = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_har = densitat_a,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_diss = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_har = ab_a,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_diss = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_har = vcc_a,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_diss = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_har = vsc_a,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_diss = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_har = dbh_a,
    dbh_balance = dbh_balanç,
    dbh_growth = dbh_creixement
  ) %>%
  mutate(
    density_rem = density_har + density_diss,
    basal_area_rem = basal_area_har + basal_area_diss,
    volume_over_bark_rem = volume_over_bark_har + volume_over_bark_diss,
    volume_under_bark_rem = volume_under_bark_har + volume_under_bark_diss,
    dbh_rem = dbh_har + dbh_diss
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_4_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3,
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> PLOT_COMP_NFI3_NFI4_RESULTS

# diameter classes
tbl(oracle_db, 'r_cd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
    # dbh_rem = dbh_d,
    # dbh_inc = dbh_i,
    # dbh_dead = dbh_m,
    # dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
    # dbh_rem = dbh_rem / years_diff,
    # dbh_inc = dbh_inc / years_diff,
    # dbh_dead = dbh_dead / years_diff
  ) -> PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

tbl(access4_db, 'ResultatCD_IFN4_IFN3_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  filter(idclasseifn4 == 'A1') %>%
  left_join(
    plot_id_nfi_4 %>% select(old_idparcela, old_idclasse_nfi4, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    density_diss = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_har = densitat_a,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_diss = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_har = ab_a,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_diss = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_har = vcc_a,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_diss = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_har = vsc_a,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  mutate(
    density_rem = density_har + density_diss,
    basal_area_rem = basal_area_har + basal_area_diss,
    volume_over_bark_rem = volume_over_bark_har + volume_over_bark_diss,
    volume_under_bark_rem = volume_under_bark_har + volume_under_bark_diss
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_4_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3,
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### STEP 9 Functional groups comparision tables ####

## species
tbl(oracle_db, 'r_especie_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    species_id = idespecie,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> SPECIES_COMP_NFI2_NFI3_RESULTS

tbl(oracle_db, 'r_especiecd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    species_id = idespecie,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

tbl(access4_db, 'ResultatEspecie_IFN4_IFN3_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  filter(idclasseifn4 == 'A1') %>%
  left_join(
    plot_id_nfi_4 %>% select(old_idparcela, old_idclasse_nfi4, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    species_id = especie,
    density_diss = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_har = densitat_a,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_diss = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_har = ab_a,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_diss = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_har = vcc_a,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_diss = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_har = vsc_a,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_diss = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_har = dbh_a,
    dbh_balance = dbh_balanç,
    dbh_growth = dbh_creixement
  ) %>%
  mutate(
    density_rem = density_har + density_diss,
    basal_area_rem = basal_area_har + basal_area_diss,
    volume_over_bark_rem = volume_over_bark_har + volume_over_bark_diss,
    volume_under_bark_rem = volume_under_bark_har + volume_under_bark_diss,
    dbh_rem = dbh_har + dbh_diss
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_4_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3
  ) -> norate_species_comp_nfi3_nfi4_results
# we need the norate_*_results beacuse the calculation of other functional groups must be
# done in the raw data not in the rate data

norate_species_comp_nfi3_nfi4_results %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> SPECIES_COMP_NFI3_NFI4_RESULTS

tbl(access4_db, 'ResultatEspecieCD_IFN4_IFN3_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  filter(idclasseifn4 == 'A1') %>%
  left_join(
    plot_id_nfi_4 %>% select(old_idparcela, old_idclasse_nfi4, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    species_id = especie,
    density_diss = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_har = densitat_a,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_diss = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_har = ab_a,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_diss = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_har = vcc_a,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_diss = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_har = vsc_a,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  mutate(
    density_rem = density_har + density_diss,
    basal_area_rem = basal_area_har + basal_area_diss,
    volume_over_bark_rem = volume_over_bark_har + volume_over_bark_diss,
    volume_under_bark_rem = volume_under_bark_har + volume_under_bark_diss
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_4_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3
  ) -> norate_species_comp_nfi3_nfi4_diamclass_results
# we need the norate_*_results beacuse the calculation of other functional groups must be
# done in the raw data not in the rate data

norate_species_comp_nfi3_nfi4_diamclass_results %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

## simplified species
tbl(oracle_db, 'r_espsimple_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    simpspecies_id = idespeciesimple,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> SIMPSPECIES_COMP_NFI2_NFI3_RESULTS

tbl(oracle_db, 'r_espsimplecd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    simpspecies_id = idespeciesimple,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

# simplified species and so on in the nfi3-nfi4 comparisions must be calculated from
# the species table, as they are not present in the original database
norate_species_comp_nfi3_nfi4_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, simpspecies_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, simpspecies_id = simpspecies_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, simpspecies_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> SIMPSPECIES_COMP_NFI3_NFI4_RESULTS

norate_species_comp_nfi3_nfi4_diamclass_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, simpspecies_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, simpspecies_id = simpspecies_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, diamclass_id, simpspecies_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

## genus
tbl(oracle_db, 'r_genere_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    genus_id = idgenere,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> GENUS_COMP_NFI2_NFI3_RESULTS

tbl(oracle_db, 'r_generecd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    genus_id = idgenere,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

# simplified species and so on in the nfi3-nfi4 comparisions must be calculated from
# the species table, as they are not present in the original database
norate_species_comp_nfi3_nfi4_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, genus_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, genus_id = genus_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, genus_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> GENUS_COMP_NFI3_NFI4_RESULTS

norate_species_comp_nfi3_nfi4_diamclass_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, genus_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, genus_id = genus_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, diamclass_id, genus_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

## dec
tbl(oracle_db, 'r_cadesclcon_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    dec_id = idcaducesclerconif,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> DEC_COMP_NFI2_NFI3_RESULTS

tbl(oracle_db, 'r_cadesclconcd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    dec_id = idcaducesclerconif,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

# simplified species and so on in the nfi3-nfi4 comparisions must be calculated from
# the species table, as they are not present in the original database
norate_species_comp_nfi3_nfi4_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, dec_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, dec_id = dec_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, dec_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> DEC_COMP_NFI3_NFI4_RESULTS

norate_species_comp_nfi3_nfi4_diamclass_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, dec_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, dec_id = dec_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, diamclass_id, dec_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

## bc
tbl(oracle_db, 'r_plancon_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    bc_id = idplanifconif,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement,
    dbh_rem = dbh_d,
    dbh_inc = dbh_i,
    dbh_dead = dbh_m,
    dbh_balance = dbh_balanç
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> BC_COMP_NFI2_NFI3_RESULTS

tbl(oracle_db, 'r_planconcd_ifn3_ifn2_creaf') %>%
  collect() %>%
  # we remove the a4c and a6c plots as they have no data for ifn2 comparable at tree level
  # (the origin of the comparisions)
  filter(!idclasse %in% c('A4C', 'A6C')) %>%
  left_join(
    plot_id_nfi_3 %>% select(old_idparcela, old_idclasse_nfi3, plot_id),
    by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  filter(!is.na(plot_id)) %>%
  select(
    plot_id,
    diamclass_id = idcd,
    bc_id = idplanifconif,
    density_rem = densitat_d,
    density_inc = densitat_i,
    density_dead = densitat_m,
    density_balance = densitat_balanç,
    density_growth = densitat_creixement,
    basal_area_rem = ab_d,
    basal_area_inc = ab_i,
    basal_area_dead = ab_m,
    basal_area_balance = ab_balanç,
    basal_area_growth = ab_creixement,
    volume_over_bark_rem = vcc_d,
    volume_over_bark_inc = vcc_i,
    volume_over_bark_dead = vcc_m,
    volume_over_bark_balance = vcc_balanç,
    volume_over_bark_growth = vcc_creixement,
    volume_under_bark_rem = vsc_d,
    volume_under_bark_inc = vsc_i,
    volume_under_bark_dead = vsc_m,
    volume_under_bark_balance = vsc_balanç,
    volume_under_bark_growth = vsc_creixement
  ) %>%
  # for the _d, _a, _i and _m vars we need to divide by the years between samplings to
  # express the vars as a rate. We get the years from the dynamic info tables,
  # substracting the nfi2 start year from nfi3 start year
  left_join(
    PLOTS_NFI_2_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id'
  ) %>%
  left_join(
    PLOTS_NFI_3_DYNAMIC_INFO %>% select(plot_id, feat_sampling_year), by = 'plot_id',
    suffix = c('_nfi2', '_nfi3')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi3 - feat_sampling_year_nfi2,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS

# simplified species and so on in the nfi3-nfi4 comparisions must be calculated from
# the species table, as they are not present in the original database
norate_species_comp_nfi3_nfi4_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, bc_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, bc_id = bc_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, bc_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff
  ) -> BC_COMP_NFI3_NFI4_RESULTS

norate_species_comp_nfi3_nfi4_diamclass_results %>%
  left_join(
    species_table %>% select(species_id_NFI4, bc_id_NFI4),
    by = c('species_id' = 'species_id_NFI4')
  ) %>%
  select(plot_id, bc_id = bc_id_NFI4, everything(), -species_id) %>%
  group_by(plot_id, diamclass_id, bc_id) %>%
  summarise_if(is.numeric, .funs = funs(sum(., na.rm = TRUE))) %>%
  # rebuild years_diff and related vars, which had been summed up the same as the other
  # variables
  select(-years_diff, -feat_sampling_year_nfi3, -feat_sampling_year_nfi4) %>%
  left_join(
    PLOT_COMP_NFI3_NFI4_RESULTS %>%
      select(plot_id, feat_sampling_year_nfi3, feat_sampling_year_nfi4, years_diff),
    by = 'plot_id'
  ) %>%
  mutate(
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff
  ) -> BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### STEP 10 Tree tables ####
# we need the forest_volume_measurement (cubicacio) thesaurus, as we need to convert the
# fvm numeric value to the long character value

tbl(oracle_db, 'arbreifn2_creaf') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id,
    tree_id = idarbre,
    diamclass_id = cd,
    species_id = idespecieifn2,
    tree_heading = rumb,
    tree_distance = distancia,
    tree_dbh = dn,
    tree_height = ht,
    tree_quality = qualitat,
    forest_volume_measurement = formacubicacio,
    tree_basal_area = g,
    tree_canopy_diameter = dc,
    tree_over_bark_volume = vcc,
    tree_under_bark_volume = vsc,
    #tree_what_the_hell_is_this = vc, ## TODO Repasar!!
    tree_conversion_factor = factor
  ) %>%
  left_join(
    tbl(oracle_db, 'tesaureformacubicacio') %>% collect() %>%
      select(idforma, formacubicacio),
    by = c('forest_volume_measurement' = 'idforma')
  ) %>% 
  mutate(
    diamclass_id = as.character(diamclass_id),
    forest_volume_measurement = formacubicacio
  ) %>%
  select(-formacubicacio) -> TREES_NFI_2_INFO

tbl(oracle_db, 'arbreifn3_creaf') %>%
  collect() %>%
  collect() %>%
  # there is a problem with plot 251955 as it is classified as A1 but there is no record
  # for it in the NFI2. So we will transform it class to NN
  mutate(
    idclasse = case_when(
      idparcela == '251955' ~ 'NN',
      TRUE ~ idclasse
    )
  ) %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id,
    tree_id = idarbre,
    diamclass_id = cd,
    species_id = idespecie,
    # tree_heading = rumb,
    # tree_distance = distancia,
    tree_status = estatus,
    tree_dbh = dn,
    tree_height = ht,
    tree_quality = qualitat,
    forest_volume_measurement = formacubicacio,
    tree_basal_area = g,
    tree_canopy_diameter = dc,
    tree_over_bark_volume = vcc,
    tree_under_bark_volume = vsc,
    # tree_what_the_hell_is_this = vc, ## TODO Repasar!!
    tree_conversion_factor = factor,
    tree_aerial_biomass_total = bat,
    tree_trunk_bark_biomass = bc,
    tree_leaf_biomass = bh,
    tree_trunk_wood_biomass = bm,
    tree_branch_wo_leaves_biomass = br,
    tree_aerial_carbon_total = cat,
    tree_trunk_bark_carbon = cc,
    tree_accumulated_aerial_co2 = cca,
    tree_leaf_carbon = ch,
    tree_trunk_wood_carbon = cm,
    tree_branch_wo_leaves_carbon = cr
  ) %>%
  left_join(
    tbl(oracle_db, 'tesaureformacubicacio') %>% collect() %>%
      select(idforma, formacubicacio),
    by = c('forest_volume_measurement' = 'idforma')
  ) %>% 
  mutate(
    diamclass_id = as.character(diamclass_id),
    forest_volume_measurement = formacubicacio
  ) %>%
  select(-formacubicacio) -> TREES_NFI_3_INFO

tbl(access4_db, 'ArbreIFN4_CREAF_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasseifn4' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    tree_id = idarbreifn4,
    diamclass_id = cd,
    species_id = especie,
    # tree_heading = rumb,
    # tree_distance = distancia,
    tree_status = estatus,
    tree_dbh = dn,
    tree_height = ht,
    tree_quality = qualitat,
    forest_volume_measurement = idforma,
    # tree_basal_area = g,
    # tree_canopy_diameter = dc,
    tree_over_bark_volume = vcc,
    tree_under_bark_volume = vsc,
    # tree_what_the_hell_is_this = vc, ## TODO Repasar!!
    tree_conversion_factor = factor,
    tree_aerial_biomass_total = bat,
    tree_trunk_bark_biomass = bc,
    tree_leaf_biomass = bh,
    tree_trunk_wood_biomass = bm,
    tree_branch_wo_leaves_biomass = br,
    tree_aerial_carbon_total = cat,
    tree_trunk_bark_carbon = cc,
    tree_accumulated_aerial_co2 = cca,
    tree_leaf_carbon = ch,
    tree_trunk_wood_carbon = cm,
    tree_branch_wo_leaves_carbon = cr,
    #tree_over_bark_volume_increment_creaf = iavc_creaf
  ) %>%
  mutate(
    forest_volume_measurement = as.numeric(forest_volume_measurement)
  ) %>% 
  ## TODO check if thesaurus for cubicacio in nfi4 does not really  exist
  left_join(
    tbl(oracle_db, 'tesaureformacubicacio') %>% collect() %>%
      select(idforma, formacubicacio),
    by = c('forest_volume_measurement' = 'idforma')
  ) %>% 
  mutate(
    diamclass_id = as.character(diamclass_id),
    forest_volume_measurement = formacubicacio
  ) %>%
  select(-formacubicacio) -> TREES_NFI_4_INFO

#### STEP 11 Shrub tables ####
tbl(oracle_db, 'especiematollarifn2') %>%
  collect() %>%
  left_join(plot_id_nfi_2, by = c('idparcela' = 'old_idparcela')) %>%
  select(
    plot_id,
    species_id = idespecieifn2,
    shrub_canopy_cover = fcc,
    shrub_mean_height = hm
  ) -> SHRUB_NFI_2_INFO

tbl(oracle_db, 'especiematollarifn3') %>%
  collect() %>%
  left_join(
    plot_id_nfi_3, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi3')
  ) %>%
  select(
    plot_id,
    species_id = idespecie,
    shrub_canopy_cover = fcc,
    shrub_mean_height = hm
  ) -> SHRUB_NFI_3_INFO

tbl(access4_db, 'EspecieMatollarIFN4_OLAP') %>%
  collect() %>%
  ## change the var names to lower letters (not capital)
  {magrittr::set_names(., tolower(names(.)))} %>%
  left_join(
    plot_id_nfi_4, by = c('idparcela' = 'old_idparcela', 'idclasse' = 'old_idclasse_nfi4')
  ) %>%
  select(
    plot_id,
    species_id = especie,
    shrub_canopy_cover = fcc,
    shrub_mean_height = hm
  ) -> SHRUB_NFI_4_INFO

#### STEP 12 Variables thesauruses ####

## The main theasurus is the VARIABLES_THESAURUS, which will contain all the variables,
## their old names, the translations, the scenarios in which they are involved, their
## type (chr, num...), their descriptions in each lenguage, their presence in the
## different versions...

# lets obtain the variable names (as var_id) and the class (as var_type) for each table
# and join them all together, to remove the repeated ones later on

PLOTS %>%
  summarise_all(~class(.x)[1]) %>%
  gather(var_id, var_type) %>%
  bind_rows(
    {
      PLOTS_NFI_2_DYNAMIC_INFO %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOTS_NFI_3_DYNAMIC_INFO %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOTS_NFI_4_DYNAMIC_INFO %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      PLOT_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SPECIES_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      GENUS_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  
  bind_rows(
    {
      BC_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      BC_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      BC_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      BC_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      BC_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      BC_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_2_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_2_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_3_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_3_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_4_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  bind_rows(
    {
      DEC_NFI_4_DIAMCLASS_RESULTS %>%
        summarise_all(~class(.x)[1]) %>%
        gather(var_id, var_type)
    }
  ) %>%
  # remove the vars repeated
  dplyr::distinct() %>%
  mutate(
    # mutate with case_when to programatically retrieve the old names (we need to
    # manually write them though)
    old_name = case_when(
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'old_idclasse_nfi4' ~ 'idclasse',
      var_id == 'topo_altitude_asl' ~ 'altitud',
      var_id == 'topo_fdm_slope_degrees' ~ 'pendentgraus',
      var_id == 'topo_fdm_slope_percentage' ~ 'pendentpercent',
      var_id == 'topo_fdm_aspect_degrees' ~ 'orientacio',
      var_id == 'topo_fdm_aspect_cardinal_8' ~ 'orientacio_8',
      var_id == 'topo_fdm_aspect_cardinal_4' ~ 'orientacio_4',
      var_id == 'topo_fdm_curvature' ~ 'curvatura',
      
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'old_idclasse_nfi4' ~ 'idclasse',
      var_id == 'feat_plot_type' ~ 'tipusparcela',
      var_id == 'feat_soil_use' ~ 'ussol',
      var_id == 'feat_forest_cover' ~ 'coberturabosc',
      var_id == 'feat_forest_type_lvl3' ~ 'tipusbosc',
      var_id == 'feat_total_canopy_cover' ~ 'fcctotal',
      var_id == 'feat_tree_canopy_cover' ~ 'fccarbrada',
      var_id == 'feat_spatial_distribution' ~ 'distribucioespacial',
      var_id == 'feat_specific_composition' ~ 'composicioespecifica',
      var_id == 'feat_rocky' ~ 'rocositat',
      var_id == 'feat_soil_texture' ~ 'texturasol',
      var_id == 'feat_soil_type_1' ~ 'tipussol1',
      var_id == 'feat_soil_type_2' ~ 'tipussol2',
      var_id == 'feat_erosion' ~ 'erosio',
      var_id == 'feat_combustion_model' ~ 'modelcombustible',
      var_id == 'feat_org_matter_thickness' ~ 'gruixmo',
      var_id == 'feat_cutoff_stock' ~ 'existenciatallades',
      var_id == 'feat_stand_improvement_1' ~ 'tractamentmillora1',
      var_id == 'feat_stand_improvement_2' ~ 'tractamentmillora2',
      var_id == 'feat_soil_improvement_1' ~ 'tractamentmillorasol1',
      var_id == 'feat_soil_improvement_2' ~ 'tractamentmillorasol2',
      var_id == 'topo_max_slope_percentage_1' ~ 'maxpendent1',
      var_id == 'feat_pinpoint_easiness' ~ 'localizacio',
      var_id == 'feat_access_easiness' ~ 'acces',
      var_id == 'feat_sampling_easiness' ~ 'mostreig',
      var_id == 'feat_sampling_start_date' ~ 'datainici',
      var_id == 'feat_sampling_end_date' ~ 'datafi',
      var_id == 'feat_sampling_start_time' ~ 'horainici',
      var_id == 'feat_sampling_end_time' ~ 'horafi',
      var_id == 'feat_observations' ~ 'observacions',
      
      var_id == 'feat_org_matter_thickness' ~ 'GruixMO',
      
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'old_idclasse_nfi3' ~ 'idclasse',
      var_id == 'feat_plot_type' ~ 'tipusparcela',
      var_id == 'feat_soil_use' ~ 'ussol',
      var_id == 'feat_forest_type_lvl_2' ~ 'nivell2', ## TODO Repasar
      var_id == 'feat_forest_cover' ~ 'fccnivell2',
      var_id == 'feat_total_canopy_cover' ~ 'fcctotal',
      var_id == 'feat_tree_canopy_cover' ~ 'fccarboria',
      var_id == 'feat_spatial_distribution' ~ 'distribucioespacial',
      var_id == 'feat_specific_composition' ~ 'composicioespecifica',
      var_id == 'feat_rocky' ~ 'rocositat',
      var_id == 'feat_soil_texture' ~ 'texturasol',
      # var_id == 'feat_org_matter_content' ~ 'contingutmo',
      # var_id ==  '#feat_soil_ph_class' ~ 'phsol',
      # var_id == 'feat_soil_ph_value' ~ 'valorphsol',
      var_id == 'feat_soil_type_1' ~ 'tipussol1',
      var_id == 'feat_soil_type_2' ~ 'tipussol2',
      var_id == 'feat_erosion' ~ 'erosio',
      # var_id == 'feat_soil_surface_ph' ~ 'phsolsuperficie',
      var_id == 'feat_combustion_model' ~ 'idmodelcombustible',
      var_id == 'feat_org_matter_thickness' ~ 'gruixmo',
      var_id == 'feat_cutoff_stock_type' ~ 'talladaregeneracio',
      var_id == 'feat_stand_improvement_1' ~ 'milloravol1',
      var_id == 'feat_stand_improvement_2' ~ 'milloravol2',
      var_id == 'feat_soil_improvement_1' ~ 'millorasol1',
      var_id == 'feat_soil_improvement_2' ~ 'millorasol2',
      var_id == 'topo_max_slope_percentage_1' ~ 'pendent1',
      var_id == 'feat_pinpoint_easiness' ~ 'localitzacio',
      var_id == 'feat_access_easiness' ~ 'acces',
      var_id == 'feat_sampling_easiness' ~ 'aixecament',
      var_id == 'feat_sampling_start_date' ~ 'datamostreig',
      var_id == 'feat_sampling_start_time' ~ 'tempsmostreig',
      var_id == 'feat_observations' ~ 'observacions',
      
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'old_idclasse_nfi3' ~ 'idclasse',
      var_id == 'topo_altitude_asl' ~ 'altitud',
      var_id == 'topo_fdm_slope_degrees' ~ 'pendentgraus',
      var_id == 'topo_fdm_slope_percentage' ~ 'pendentpercentatge',
      var_id == 'topo_fdm_aspect_degrees' ~ 'orientacio',
      var_id == 'topo_fdm_aspect_cardinal_8' ~ 'orientacio_c8',
      var_id == 'topo_fdm_aspect_cardinal_4' ~ 'orientacio_c4',
      
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'feat_soil_use' ~ 'ussolcamp',
      # var_id == 'feat_canopy_cover_level_2' ~ 'classecobertura', ## TODO Repasar
      var_id == 'feat_total_canopy_cover' ~ 'fcccamp',
      var_id == 'feat_spatial_distribution' ~ 'distribucioespacial',
      var_id == 'feat_specific_composition' ~ 'composicioespecifica',
      var_id == 'feat_soil_texture' ~ 'texturasol',
      var_id == 'feat_erosion' ~ 'erosio',
      var_id == 'feat_cutoff_stock_type' ~ 'talladaregeneracio',
      var_id == 'feat_stand_improvement_1' ~ 'milloravol1',
      var_id == 'feat_stand_improvement_2' ~ 'milloravol2',
      var_id == 'feat_soil_improvement_1' ~ 'millorasol1',
      var_id == 'feat_soil_improvement_2' ~ 'millorasol2',
      var_id == 'topo_max_slope_percentage_1' ~ 'pendent1',
      var_id == 'feat_sampling_start_date' ~ 'anymostreig',
      
      var_id == 'old_idparcela' ~ 'idparcela',
      var_id == 'feat_canopy_type' ~ 'tipuscoberta',
      var_id == 'topo_altitude_asl' ~ 'altitud',
      var_id == 'topo_fdm_slope_degrees' ~ 'pendentgraus',
      var_id == 'topo_fdm_slope_percentage' ~ 'pendentpercentatge',
      var_id == 'topo_fdm_aspect_degrees' ~ 'orientacio',
      var_id == 'topo_fdm_aspect_cardinal_8' ~ 'orientacio_c8',
      var_id == 'topo_fdm_aspect_cardinal_4' ~ 'orientacio_c4',
      
      # var_id == 'feat_forest_id' ~ 'fo_codi',
      var_id == 'feat_forest_name' ~ 'forest',
      var_id == 'feat_ownership_type' ~ 'tip_prop',
      var_id == 'feat_ownership_regime' ~ NA_character_,
      var_id == 'feat_cup' ~ 'cup', # TODO Revisar
      var_id == 'feat_elen' ~ 'elen', # TODO Revisar
      var_id == 'feat_owner' ~ 'titular',
      var_id == 'feat_agreement' ~ 'conveni',
      var_id == 'feat_management' ~ 'ordenacio',
      var_id == 'feat_certificate' ~ 'certific',
      
      var_id == 'basal_area' ~ 'ab',
      var_id == 'basal_area_dead' ~ 'abmorts',
      var_id == 'basal_area_dec_dominant' ~ 'caducesclerconifab',
      var_id == 'density_dec_dominant' ~ 'caducesclerconifdens',
      var_id == 'dbh' ~ 'dbh',
      var_id == 'dbh_dead' ~ 'dbhmorts',
      var_id == 'density' ~ 'densitat',
      var_id == 'density_dead' ~ 'densitatmorts',
      var_id == 'basal_area_species_dominant' ~ 'especieab',
      var_id == 'density_species_dominant' ~ 'especiedens',
      var_id == 'basal_area_simp_species_dominant' ~ 'especiesimpleab',
      var_id == 'density_simp_species_dominant' ~ 'especiesimpledens',
      var_id == 'basal_area_genus_dominant' ~ 'genereab',
      var_id == 'density_genus_dominant' ~ 'generedens',
      # var_id == '#over_bark_volume_increment' ~ 'iavc',
      var_id == 'basal_area_dec_percentage' ~ 'percabcaducesclerconif',
      var_id == 'basal_area_species_percentage' ~ 'percabespecie',
      var_id == 'basal_area_simp_species_percentage' ~ 'percabespeciesimple',
      var_id == 'basal_area_genus_percentage' ~ 'percabgenere',
      var_id == 'basal_area_bc_percentage' ~ 'percabplanifconif',
      var_id == 'density_dec_percentage' ~ 'percdenscaducesclerconif',
      var_id == 'density_species_percentage' ~ 'percdensespecie',
      var_id == 'density_simp_species_percentage' ~ 'percdensespeciesimple',
      var_id == 'density_genus_percentage' ~ 'percdensgenere',
      var_id == 'density_bc_percentage' ~ 'percdensplanifconif',
      var_id == 'basal_area_bc_dominant' ~ 'planifconifab',
      var_id == 'density_bc_dominant' ~ 'planifconifdens',
      var_id == 'canopy_cover' ~ 'rc',
      var_id == 'over_bark_volume' ~ 'vcc',
      var_id == 'over_bark_volume_dead' ~ 'vccmorts',
      # var_id == 'fuelwood_volume' ~ 'vle',
      var_id == 'under_bark_volume' ~ 'vsc',
      var_id == 'under_bark_volume_dead' ~ 'vscmorts',
      
      var_id == 'basal_area' ~ 'ab',
      var_id == 'basal_area_dead' ~ 'abmorts',
      var_id == 'aerial_biomass_total' ~ 'bat',
      var_id == 'trunk_bark_biomass' ~ 'bc',
      var_id == 'leaf_biomass' ~ 'bh',
      var_id == 'trunk_wood_biomass' ~ 'bm',
      var_id == 'branch_wo_leaves_biomass' ~ 'br',
      var_id == 'basal_area_dec_dominant' ~ 'caducesclerconifab',
      var_id == 'density_dec_dominant' ~ 'caducesclerconifdens',
      var_id == 'aerial_carbon_total' ~ 'cat',
      var_id == 'trunk_bark_carbon' ~ 'cc',
      var_id == 'accumulated_aerial_co2' ~ 'cca',
      var_id == 'leaf_carbon' ~ 'ch',
      var_id == 'trunk_wood_carbon' ~ 'cm',
      var_id == 'branch_wo_leaves_carbon' ~ 'cr',
      var_id == 'dbh' ~ 'dbh',
      var_id == 'dbh_dead' ~ 'dbhmorts',
      var_id == 'density' ~ 'densitat',
      var_id == 'density_dead' ~ 'densitatmorts',
      var_id == 'basal_area_species_dominant' ~ 'especieab',
      var_id == 'density_species_dominant' ~ 'especiedens',
      var_id == 'basal_area_simp_species_dominant' ~ 'especiesimpleab',
      var_id == 'density_simp_species_dominant' ~ 'especiesimpledens',
      var_id == 'basal_area_genus_dominant' ~ 'genereab',
      var_id == 'density_genus_dominant' ~ 'generedens',
      var_id == 'lai' ~ 'iaf',
      # var_id == '#over_bark_volume_increment' ~ 'iavc',
      # var_id == '#over_bark_volume_increment_creaf' ~ 'iavc_creaf',
      var_id == 'basal_area_dec_percentage' ~ 'percabcaducesclerconif',
      var_id == 'basal_area_species_percentage' ~ 'percabespecie',
      var_id == 'basal_area_simp_species_percentage' ~ 'percabespeciesimple',
      var_id == 'basal_area_genus_percentage' ~ 'percabgenere',
      var_id == 'basal_area_bc_percentage' ~ 'percabplanifconif',
      var_id == 'density_dec_percentage' ~ 'percdenscaducesclerconif',
      var_id == 'density_species_percentage' ~ 'percdensespecie',
      var_id == 'density_simp_species_percentage' ~ 'percdensespeciesimple',
      var_id == 'density_genus_percentage' ~ 'percdensgenere',
      var_id == 'density_bc_percentage' ~ 'percdensplanifconif',
      # var_id == 'gross_leaf_production' ~ 'ph',
      var_id == 'basal_area_bc_dominant' ~ 'planifconifab',
      var_id == 'density_bc_dominant' ~ 'planifconifdens',
      var_id == 'canopy_cover' ~ 'rc',
      var_id == 'over_bark_volume' ~ 'vcc',
      var_id == 'over_bark_volume_dead' ~ 'vccmorts',
      # var_id == '#fuelwood_volume' ~ 'vle',
      var_id == 'under_bark_volume' ~ 'vsc',
      var_id == 'under_bark_volume_dead' ~ 'vscmorts',
      
      var_id == 'basal_area' ~ 'ab',
      var_id == 'basal_area_dead' ~ 'abmorts',
      var_id == 'aerial_biomass_total' ~ 'bat',
      var_id == 'trunk_bark_biomass' ~ 'bc',
      var_id == 'leaf_biomass' ~ 'bh',
      var_id == 'trunk_wood_biomass' ~ 'bm',
      var_id == 'branch_wo_leaves_biomass' ~ 'br',
      var_id == 'basal_area_dec_dominant' ~ 'caducesclerconifab',
      var_id == 'density_dec_dominant' ~ 'caducesclerconifdens',
      var_id == 'aerial_carbon_total' ~ 'cat',
      var_id == 'trunk_bark_carbon' ~ 'cc',
      var_id == 'accumulated_aerial_co2' ~ 'cca',
      var_id == 'leaf_carbon' ~ 'ch',
      var_id == 'trunk_wood_carbon' ~ 'cm',
      var_id == 'branch_wo_leaves_carbon' ~ 'cr',
      var_id == 'dbh' ~ 'dbh',
      var_id == 'dbh_dead' ~ 'dbhmorts',
      var_id == 'density' ~ 'densitat',
      var_id == 'density_dead' ~ 'densitatmorts',
      var_id == 'basal_area_species_dominant' ~ 'especieab',
      var_id == 'density_species_dominant' ~ 'especiedens',
      var_id == 'basal_area_genus_dominant' ~ 'genereab',
      var_id == 'density_genus_dominant' ~ 'generedens',
      var_id == 'lai' ~ 'iaf',
      var_id == 'basal_area_dec_percentage' ~ 'percabcaducesclerconif',
      var_id == 'basal_area_species_percentage' ~ 'percabespecie',
      var_id == 'basal_area_genus_percentage' ~ 'percabgenere',
      var_id == 'basal_area_bc_percentage' ~ 'percabplanifconif',
      var_id == 'density_dec_percentage' ~ 'percdensitatcaducesclerconif',
      var_id == 'density_species_percentage' ~ 'percdensitatespecie',
      var_id == 'density_genus_percentage' ~ 'percdensitatgenere',
      var_id == 'density_bc_percentage' ~ 'percdensitatplanifconif',
      var_id == 'basal_area_bc_dominant' ~ 'planifconifab',
      var_id == 'density_bc_dominant' ~ 'planifconifdens',
      var_id == 'canopy_cover' ~ 'rc',
      var_id == 'basal_area_forest_type' ~ 'tipusboscab',
      var_id == 'density_forest_type' ~ 'tipusboscdens',
      var_id == 'over_bark_volume' ~ 'vcc',
      var_id == 'over_bark_volume_dead' ~ 'vcc_morts',
      var_id == 'under_bark_volume' ~ 'vsc',
      var_id == 'under_bark_volume_dead' ~ 'vsc_morts',
      
      var_id == 'species_id' ~ 'idespecie',
      var_id == 'basal_area' ~ 'ab',
      var_id == 'basal_area_dead' ~ 'abmorts',
      var_id == 'aerial_biomass_total' ~ 'bat',
      var_id == 'trunk_bark_biomass' ~ 'bc',
      var_id == 'leaf_biomass' ~ 'bh',
      var_id == 'trunk_wood_biomass' ~ 'bm',
      var_id == 'branch_wo_leaves_biomass' ~ 'br',
      var_id == 'aerial_carbon_total' ~ 'cat',
      var_id == 'trunk_bark_carbon' ~ 'cc',
      var_id == 'accumulated_aerial_co2' ~ 'cca',
      var_id == 'leaf_carbon' ~ 'ch',
      var_id == 'trunk_wood_carbon' ~ 'cm',
      var_id == 'branch_wo_leaves_carbon' ~ 'cr',
      var_id == 'dbh' ~ 'dbh',
      var_id == 'dbh_dead' ~ 'dbhmorts',
      var_id == 'density' ~ 'densitat',
      var_id == 'density_dead' ~ 'densitatmorts',
      var_id == 'order_basal_area' ~ 'ordreab',
      var_id == 'order_density' ~ 'ordredens',
      var_id == 'basal_area_percentage' ~ 'percab',
      var_id == 'density_percentage' ~ 'percdens',
      var_id == 'canopy_cover' ~ 'rc',
      var_id == 'over_bark_volume' ~ 'vcc',
      var_id == 'over_bark_volume_dead' ~ 'vccmorts',
      var_id == 'under_bark_volume' ~ 'vsc',
      var_id == 'under_bark_volume_dead' ~ 'vscmorts',
      
      var_id == 'simpspecies_id' ~ 'idespeciesimple',
      var_id == 'genus_id' ~ 'idgenere',
      var_id == 'dec_id' ~ 'idcadesccon',
      var_id == 'bc_id' ~ 'idplanifconif',
      
      var_id == 'diamclass_id' ~ 'idcd'
      
      ## TODO clim_vars
    ),
    
    ## also a mutate with case_when to programatically detect the variable presence in
    ## the different NFI versions
    presence_nfi2 = case_when(
      var_id %in% {c(
        names(PLOTS), names(PLOT_NFI_2_RESULTS), names(PLOT_NFI_2_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_3_RESULTS), names(PLOT_NFI_3_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_4_RESULTS), names(PLOT_NFI_4_DIAMCLASS_RESULTS),
        names(SPECIES_NFI_2_RESULTS), names(SPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_3_RESULTS), names(SPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_4_RESULTS), names(SPECIES_NFI_4_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_2_RESULTS), names(SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_3_RESULTS), names(SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_4_RESULTS), names(SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS),
        names(GENUS_NFI_2_RESULTS), names(GENUS_NFI_2_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_3_RESULTS), names(GENUS_NFI_3_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_4_RESULTS), names(GENUS_NFI_4_DIAMCLASS_RESULTS),
        names(BC_NFI_2_RESULTS), names(BC_NFI_2_DIAMCLASS_RESULTS),
        # names(BC_NFI_3_RESULTS), names(BC_NFI_3_DIAMCLASS_RESULTS),
        # names(BC_NFI_4_RESULTS), names(BC_NFI_4_DIAMCLASS_RESULTS),
        names(DEC_NFI_2_RESULTS), names(DEC_NFI_2_DIAMCLASS_RESULTS)
        # names(DEC_NFI_3_RESULTS), names(DEC_NFI_3_DIAMCLASS_RESULTS),
        # names(DEC_NFI_4_RESULTS), names(DEC_NFI_4_DIAMCLASS_RESULTS)
      ) %>% unique()} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    presence_nfi3 = case_when(
      var_id %in% {c(
        names(PLOTS), #names(PLOT_NFI_2_RESULTS), names(PLOT_NFI_2_DIAMCLASS_RESULTS),
        names(PLOT_NFI_3_RESULTS), names(PLOT_NFI_3_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_4_RESULTS), names(PLOT_NFI_4_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_2_RESULTS), names(SPECIES_NFI_2_DIAMCLASS_RESULTS),
        names(SPECIES_NFI_3_RESULTS), names(SPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_4_RESULTS), names(SPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_2_RESULTS), names(SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_3_RESULTS), names(SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_4_RESULTS), names(SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_2_RESULTS), names(GENUS_NFI_2_DIAMCLASS_RESULTS),
        names(GENUS_NFI_3_RESULTS), names(GENUS_NFI_3_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_4_RESULTS), names(GENUS_NFI_4_DIAMCLASS_RESULTS),
        # names(BC_NFI_2_RESULTS), names(BC_NFI_2_DIAMCLASS_RESULTS),
        names(BC_NFI_3_RESULTS), names(BC_NFI_3_DIAMCLASS_RESULTS),
        # names(BC_NFI_4_RESULTS), names(BC_NFI_4_DIAMCLASS_RESULTS),
        # names(DEC_NFI_2_RESULTS), names(DEC_NFI_2_DIAMCLASS_RESULTS)
        names(DEC_NFI_3_RESULTS), names(DEC_NFI_3_DIAMCLASS_RESULTS)
        # names(DEC_NFI_4_RESULTS), names(DEC_NFI_4_DIAMCLASS_RESULTS)
      ) %>% unique()} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    presence_nfi4 = case_when(
      var_id %in% {c(
        names(PLOTS), #names(PLOT_NFI_2_RESULTS), names(PLOT_NFI_2_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_3_RESULTS), names(PLOT_NFI_3_DIAMCLASS_RESULTS),
        names(PLOT_NFI_4_RESULTS), names(PLOT_NFI_4_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_2_RESULTS), names(SPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_3_RESULTS), names(SPECIES_NFI_3_DIAMCLASS_RESULTS),
        names(SPECIES_NFI_4_RESULTS), names(SPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_2_RESULTS), names(SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_3_RESULTS), names(SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_4_RESULTS), names(SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_2_RESULTS), names(GENUS_NFI_2_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_3_RESULTS), names(GENUS_NFI_3_DIAMCLASS_RESULTS),
        names(GENUS_NFI_4_RESULTS), names(GENUS_NFI_4_DIAMCLASS_RESULTS),
        # names(BC_NFI_2_RESULTS), names(BC_NFI_2_DIAMCLASS_RESULTS),
        # names(BC_NFI_3_RESULTS), names(BC_NFI_3_DIAMCLASS_RESULTS),
        names(BC_NFI_4_RESULTS), names(BC_NFI_4_DIAMCLASS_RESULTS),
        # names(DEC_NFI_2_RESULTS), names(DEC_NFI_2_DIAMCLASS_RESULTS)
        # names(DEC_NFI_3_RESULTS), names(DEC_NFI_3_DIAMCLASS_RESULTS),
        names(DEC_NFI_4_RESULTS), names(DEC_NFI_4_DIAMCLASS_RESULTS)
      ) %>% unique()} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # again mutate with case_when for programatically detect which scenario the variables
    # are in
    presence_scenario1 = case_when(
      var_id %in% {c(
        names(PLOTS), names(PLOT_NFI_2_RESULTS), names(PLOT_NFI_2_DIAMCLASS_RESULTS),
        names(PLOT_NFI_3_RESULTS), names(PLOT_NFI_3_DIAMCLASS_RESULTS),
        names(PLOT_NFI_4_RESULTS), names(PLOT_NFI_4_DIAMCLASS_RESULTS)
        # names(SPECIES_NFI_2_RESULTS), names(SPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_3_RESULTS), names(SPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SPECIES_NFI_4_RESULTS), names(SPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_2_RESULTS), names(SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_3_RESULTS), names(SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS),
        # names(SIMPSPECIES_NFI_4_RESULTS), names(SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_2_RESULTS), names(GENUS_NFI_2_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_3_RESULTS), names(GENUS_NFI_3_DIAMCLASS_RESULTS),
        # names(GENUS_NFI_4_RESULTS), names(GENUS_NFI_4_DIAMCLASS_RESULTS),
        # names(BC_NFI_2_RESULTS), names(BC_NFI_2_DIAMCLASS_RESULTS),
        # names(BC_NFI_3_RESULTS), names(BC_NFI_3_DIAMCLASS_RESULTS),
        # names(BC_NFI_4_RESULTS), names(BC_NFI_4_DIAMCLASS_RESULTS),
        # names(DEC_NFI_2_RESULTS), names(DEC_NFI_2_DIAMCLASS_RESULTS),
        # names(DEC_NFI_3_RESULTS), names(DEC_NFI_3_DIAMCLASS_RESULTS),
        # names(DEC_NFI_4_RESULTS), names(DEC_NFI_4_DIAMCLASS_RESULTS)
      ) %>% unique()} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    presence_scenario2 = case_when(
      var_id %in% {c(
        names(PLOTS), #names(PLOT_NFI_2_RESULTS), names(PLOT_NFI_2_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_3_RESULTS), names(PLOT_NFI_3_DIAMCLASS_RESULTS),
        # names(PLOT_NFI_4_RESULTS), names(PLOT_NFI_4_DIAMCLASS_RESULTS)
        names(SPECIES_NFI_2_RESULTS), names(SPECIES_NFI_2_DIAMCLASS_RESULTS),
        names(SPECIES_NFI_3_RESULTS), names(SPECIES_NFI_3_DIAMCLASS_RESULTS),
        names(SPECIES_NFI_4_RESULTS), names(SPECIES_NFI_4_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_2_RESULTS), names(SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_3_RESULTS), names(SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS),
        names(SIMPSPECIES_NFI_4_RESULTS), names(SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS),
        names(GENUS_NFI_2_RESULTS), names(GENUS_NFI_2_DIAMCLASS_RESULTS),
        names(GENUS_NFI_3_RESULTS), names(GENUS_NFI_3_DIAMCLASS_RESULTS),
        names(GENUS_NFI_4_RESULTS), names(GENUS_NFI_4_DIAMCLASS_RESULTS),
        names(BC_NFI_2_RESULTS), names(BC_NFI_2_DIAMCLASS_RESULTS),
        names(BC_NFI_3_RESULTS), names(BC_NFI_3_DIAMCLASS_RESULTS),
        names(BC_NFI_4_RESULTS), names(BC_NFI_4_DIAMCLASS_RESULTS),
        names(DEC_NFI_2_RESULTS), names(DEC_NFI_2_DIAMCLASS_RESULTS),
        names(DEC_NFI_3_RESULTS), names(DEC_NFI_3_DIAMCLASS_RESULTS),
        names(DEC_NFI_4_RESULTS), names(DEC_NFI_4_DIAMCLASS_RESULTS)
      ) %>% unique()} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    presence_scenario3 = presence_scenario1,
    presence_scenario4 = presence_scenario2,
    
    ## empty variables to contain the translations, to be filled by hand
    translation_spa = '',
    translation_cat = '',
    translation_eng = '',
    var_description_spa = '',
    var_description_cat = '',
    var_description_eng = ''
  ) -> vars_table

## now, the translations must be done in a collaborative document. so we save this as an
## excel file
writexl::write_xlsx(vars_table, 'variables_thesaurus.xlsx')

## now we create the derived thesaurus from the variables_thesaurus:

# categorial
categorical_variables <- vars_table %>%
  filter(var_type == 'character') %>%
  select(var_id) %>%
  mutate(var_values_spa = '', var_values_cat = '', var_values_eng = '')

# numerical
numerical_variables <- vars_table %>%
  filter(var_type %in% c('numeric', 'integer')) %>%
  select(var_id) %>%
  mutate(var_limits = 0, var_units = '')

# logical
logical_variables <- vars_table %>%
  filter(var_type == 'logical') %>%
  select(var_id)

# dates
dttm_variables <- vars_table %>%
  filter(var_type == 'POSIXct') %>%
  select(var_id)

#### STEP 14 Regeneration tables ####
# In this case, we do it after the thesauruses because we need the species tables as
# the original tables have only the code (at least for 2 and 3 versions). Also, we have
# the data for all plots in spain (at least for 2 and 3 versions) so we need to start
# with the nfi version plots
plot_id_nfi_2 %>%
  left_join(
    tbl(oracle_db, 'regeneracioifn2_espanya') %>% collect(),
    by = c('old_idparcela' = 'idparcela')
  ) %>%
  # here we join the species table to get the species names
  left_join(
    species_table %>% select(code_id, species_id_NFI2_3),
    by = c('idespecie' = 'code_id')
  ) %>%
  mutate(iddensitat = as.numeric(iddensitat)) %>%
  # we need to conver the iddensitat var to its value
  left_join(
    tbl(access4_db, 'TesaureRegeneracio') %>% collect() %>% select(IdDensitat, Densitat),
    by = c('iddensitat' = 'IdDensitat')
  ) %>%
  select(
    plot_id,
    species_id = species_id_NFI2_3,
    density_class = Densitat,
    tree_number = nombrepeus,
    tree_mean_height = hm
  ) -> REGENERATION_NFI_2
## TODO Q. pyrenaica, seriously?????? Check with Vayreda about the species, and talk with
## Raúl also.


#### STEP X Build the database ####

## Plots static info
PLOTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOTS', overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOTS"
   ADD PRIMARY KEY (plot_id);'
)

## Plots dynamic info
PLOTS_NFI_2_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOTS_NFI_2_DYNAMIC_INFO', overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOTS_NFI_2_DYNAMIC_INFO"
   ADD PRIMARY KEY (plot_id);'
)

PLOTS_NFI_3_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOTS_NFI_3_DYNAMIC_INFO', overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOTS_NFI_3_DYNAMIC_INFO"
   ADD PRIMARY KEY (plot_id);'
)

PLOTS_NFI_3_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOTS_NFI_3_DYNAMIC_INFO', overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOTS_NFI_3_DYNAMIC_INFO"
   ADD PRIMARY KEY (plot_id);'
)

## Plot level results
PLOT_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_2_RESULTS"
   ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_2_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_3_RESULTS"
   ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_3_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_4_RESULTS"
   ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_NFI_4_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id);'
)

## Functional groups tables
# Species
SPECIES_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_2_RESULTS"
   ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_2_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_3_RESULTS"
   ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_3_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_4_RESULTS"
   ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_NFI_4_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

# simplified species
SIMPSPECIES_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_2_RESULTS"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_3_RESULTS"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_4_RESULTS"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

# genus
GENUS_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_2_RESULTS"
   ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_2_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_3_RESULTS"
   ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_3_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_4_RESULTS"
   ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_NFI_4_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

# dec
DEC_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_2_RESULTS"
   ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_2_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_3_RESULTS"
   ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_3_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_4_RESULTS"
   ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_NFI_4_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

# bc
BC_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_2_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_2_RESULTS"
   ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_2_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_2_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_3_RESULTS"
   ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_3_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_4_RESULTS"
   ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_NFI_4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_NFI_4_DIAMCLASS_RESULTS"
   ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

## Variables Thesaurus
vars_table %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_THESAURUS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_THESAURUS"
   ADD PRIMARY KEY (var_id);'
)

categorical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_CATEGORICAL',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_CATEGORICAL"
   ADD PRIMARY KEY (var_id);'
)

numerical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_NUMERICAL',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_NUMERICAL"
   ADD PRIMARY KEY (var_id);'
)

logical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_LOGICAL',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_LOGICAL"
   ADD PRIMARY KEY (var_id);'
)

dttm_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_DTTM',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_DTTM"
   ADD PRIMARY KEY (var_id);'
)

## Species thesaurus
species_table %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_THESAURUS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'code_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_THESAURUS"
   ADD PRIMARY KEY (code_id);'
)

#### CLOSE POOLS ####
poolClose(oracle_db)
poolClose(access4_db)
poolClose(brand_new_nfi_db)
