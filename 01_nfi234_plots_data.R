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
