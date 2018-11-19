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

#### STEP 1 ####
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

#### STEP 2 ####
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

#### STEP 3 ####

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

#### STEP 4 ####
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
          feat_forest_type = tipusbosc,
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

## TODO Separate the dynamic data from the static data

## NFI3
tbl(oracle_db, 'parcelaifn3') %>%
  collect() %>%
  # we select (and rename in the fly) the variables of interest in this table
  select(
    old_idparcela = idparcela,
    old_idclasse_nfi3 = idclasse,
    feat_plot_type = tipusparcela,
    feat_soil_use = ussol,
    feat_level_2 = nivell2, ## TODO Repasar
    feat_forest_cover = fccnivell2,
    feat_total_canopy_cover = fcctotal,
    feat_tree_canopy_cover = fccarboria,
    feat_spatial_distribution = distribucioespacial,
    feat_specific_composition = composicioespecifica,
    feat_rocky = rocositat,
    feat_soil_texture = texturasol,
    feat_org_matter_content = contingutmo,
    feat_soil_ph_class = phsol,
    feat_soil_ph_value = valorphsol,
    feat_soil_type_1 = tipussol1,
    feat_soil_type_2 = tipussol2,
    feat_erosion = erosio,
    feat_soil_surface_ph = phsolsuperficie,
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
              'etr_s_' = 'etr_s', 'etr_p_' = 'etr_p'
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
  right_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(!NFI_4, NFI_3) %>%
        select(plot_id, old_idparcela, old_idclasse_nfi3)
      
    }
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
  select(plot_id, everything()) -> ifn3_plot_topo_clim_vars

## TODO Separate the dynamic data from the static data

## NFI2
tbl(oracle_db, 'parcelaifn2') %>%
  collect() %>%
  # we select (and rename in the fly) the variables of interest in this table
  select(
    old_idparcela = idparcela,
    feat_soil_use = ussolcamp,
    feat_canopy_cover_level_2 = classecobertura, ## TODO Repasar
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
          topo_fdm_aspect_cardinal_4 = orientacio_c4,
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
              'etr_s_' = 'etr_s', 'etr_p_' = 'etr_p'
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
  right_join(
    {
      ifn2_ifn3_ifn4_plots %>%
        filter(!NFI_4, !NFI_3, NFI_2) %>%
        select(plot_id, old_idparcela)
    }
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
  select(plot_id, everything()) -> ifn2_plot_topo_clim_vars

topo_clim_info <- bind_rows(
  ifn2_plot_topo_clim_vars, ifn3_plot_topo_clim_vars, ifn4_plot_topo_clim_vars
) %>%
  arrange(plot_id)

## TODO Separate the dynamic data from the static data

#### STEP 5 ####
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
    feat_forest_id = fo_codi,
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
    admin_delegation = comarcas_d,
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
admin_info %>% filter(admin_province == 'Barcelona') %>% pull(admin_delegation) %>% unique()
admin_info %>% filter(admin_province == 'Girona') %>% pull(admin_delegation) %>% unique()
admin_info %>% filter(admin_province == 'Lleida') %>% pull(admin_delegation) %>% unique()
admin_info %>% filter(admin_province == 'Tarragona') %>% pull(admin_delegation) %>% unique()
admin_info %>% filter(admin_province == 'Girona', admin_delegation == 'Barcelona')


#### STEP 6 ####
# Let's build the overall table, joining what we have to join based on NFI4 first, NFI3
# later and for those left, NFI2.
# But before, as the admin and ownership info are for all plots already, lets join that
# first
ifn2_ifn3_ifn4_plots %>%
  left_join(admin_info, by = 'plot_id') %>%
  left_join(ownership_info, by = 'plot_id') %>%
  # here we let join by all coincidental variables, not only by plot_id
  left_join(topo_clim_info) %>%
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

## TODO Recalculate ETR and ETP from Miramon maps


#### STEP 7 ####
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
    over_bark_volume_increment = iavc,
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
    fulewood_volume = vle,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) -> PLOT_NFI_2_RESULTS

tbl(oracle_db, 'r_parcela_ifn3') %>%
  collect() %>%
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
    over_bark_volume_increment = iavc,
    over_bark_volume_increment_creaf = iavc_creaf,
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
    gross_leaf_production = ph,
    basal_area_bc_dominant = planifconifab,
    density_bc_dominant = planifconifdens,
    canopy_cover = rc,
    over_bark_volume = vcc,
    over_bark_volume_dead = vccmorts,
    fulewood_volume = vle,
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
    # over_bark_volume_increment = iavc,
    # over_bark_volume_increment_creaf = iavc_creaf,
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
    # fulewood_volume = vle,
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
    what_the_heel_is_this_dead = vcmorts,
    under_bark_volume = vsc,
    under_bark_volume_dead = vscmorts
  ) -> SPECIES_NFI_2_RESULTS

tbl(oracle_db, 'r_especie_ifn3_creaf') %>%
  collect() %>%
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
  ) -> SPECIES_NFI_4_RESULTS







#### CLOSE POOLS ####
poolClose(oracle_db)
poolClose(access4_db)






