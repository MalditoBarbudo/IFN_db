#### STEP 1 Plot comparision tables ####

#### PLOT_COMP_NFI*_NFI*_RESULTS ####
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
    density_dec_dominant_nfi2 = caducesclerconifdens_ifn2,
    density_dec_dominant_nfi3 = caducesclerconifdens_ifn3,
    density_bc_dominant_nfi2 = planifconifdens_ifn2,
    density_bc_dominant_nfi3 = planifconifdens_ifn3,
    density_genus_dominant_nfi2 = generedens_ifn2,
    density_genus_dominant_nfi3 = generedens_ifn3,
    density_species_dominant_nfi2 = especiedens_ifn2,
    density_species_dominant_nfi3 = especiedens_ifn3,
    density_simpspecies_dominant_nfi2 = especiesimpledens_ifn2,
    density_simpspecies_dominant_nfi3 = especiesimpledens_ifn3,
    basal_area_dec_dominant_nfi2 = caducesclerconifab_ifn2,
    basal_area_dec_dominant_nfi3 = caducesclerconifab_ifn3,
    basal_area_bc_dominant_nfi2 = planifconifab_ifn2,
    basal_area_bc_dominant_nfi3 = planifconifab_ifn3,
    basal_area_genus_dominant_nfi2 = genereab_ifn2,
    basal_area_genus_dominant_nfi3 = genereab_ifn3,
    basal_area_species_dominant_nfi2 = especieab_ifn2,
    basal_area_species_dominant_nfi3 = especieab_ifn3,
    basal_area_simpspecies_dominant_nfi2 = especiesimpleab_ifn2,
    basal_area_simpspecies_dominant_nfi3 = especiesimpleab_ifn3,
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
    
    density_dec_dominant_nfi4 = caducesclerconifdens_ifn4,
    density_dec_dominant_nfi3 = caducesclerconifdens_ifn3,
    density_bc_dominant_nfi4 = planifconifdens_ifn4,
    density_bc_dominant_nfi3 = planifconifdens_ifn3,
    density_genus_dominant_nfi4 = generedens_ifn4,
    density_genus_dominant_nfi3 = generedens_ifn3,
    density_species_dominant_nfi4 = especiedens_ifn4,
    density_species_dominant_nfi3 = especiedens_ifn3,
    # density_simpspecies_dominant_nfi4 = especiesimpledens_ifn4,
    # density_simpspecies_dominant_nfi3 = especiesimpledens_ifn3,
    basal_area_dec_dominant_nfi4 = caducesclerconifab_ifn4,
    basal_area_dec_dominant_nfi3 = caducesclerconifab_ifn3,
    basal_area_bc_dominant_nfi4 = planifconifab_ifn4,
    basal_area_bc_dominant_nfi3 = planifconifab_ifn3,
    basal_area_genus_dominant_nfi4 = genereab_ifn4,
    basal_area_genus_dominant_nfi3 = genereab_ifn3,
    basal_area_species_dominant_nfi4 = especieab_ifn4,
    basal_area_species_dominant_nfi3 = especieab_ifn3,
    # basal_area_simpspecies_dominant_nfi4 = especiesimpleab_ifn4,
    # basal_area_simpspecies_dominant_nfi3 = especiesimpleab_ifn3,
    
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
  # lets join the simpspecies dominant info from the plots tables
  left_join(
    PLOT_NFI_3_RESULTS %>% select(plot_id, density_simpspecies_dominant, basal_area_simpspecies_dominant),
    by = 'plot_id'
  ) %>%
  left_join(
    PLOT_NFI_4_RESULTS %>% select(plot_id, density_simpspecies_dominant, basal_area_simpspecies_dominant),
    by = 'plot_id', suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3,
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff,
    dbh_har = dbh_har / years_diff
  ) -> PLOT_COMP_NFI3_NFI4_RESULTS

#### PLOT_COMP_NFI*_NFI*_DIAMCLASS_RESULTS ####
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
  # lets join the simpspecies dominant info from the plots tables
  left_join(
    PLOT_NFI_2_RESULTS %>% select(plot_id, contains('dominant')),
    by = 'plot_id'
  ) %>%
  left_join(
    PLOT_NFI_3_RESULTS %>% select(plot_id, contains('dominant')),
    by = 'plot_id', suffix = c('_nfi2', '_nfi3')
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
    diamclass_id = as.character(diamclass_id)
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
  # lets join the simpspecies dominant info from the plots tables
  left_join(
    PLOT_NFI_3_RESULTS %>% select(plot_id, contains('dominant')),
    by = 'plot_id'
  ) %>%
  left_join(
    PLOT_NFI_4_RESULTS %>% select(plot_id, contains('dominant')),
    by = 'plot_id', suffix = c('_nfi3', '_nfi4')
  ) %>%
  mutate(
    years_diff = feat_sampling_year_nfi4 - feat_sampling_year_nfi3,
    density_diss = density_diss / years_diff,
    density_rem = density_rem / years_diff,
    density_inc = density_inc / years_diff,
    density_dead = density_dead / years_diff,
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    diamclass_id = as.character(diamclass_id)
  ) -> PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### STEP 2 Functional groups comparision tables ####

#### SPECIES_COMP_**** ####
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
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    diamclass_id = as.character(diamclass_id)
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    dbh_diss = dbh_diss / years_diff,
    dbh_rem = dbh_rem / years_diff,
    dbh_inc = dbh_inc / years_diff,
    dbh_dead = dbh_dead / years_diff,
    dbh_har = dbh_har / years_diff
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    diamclass_id = as.character(diamclass_id)
  ) -> SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### SIMPSPECIES_COMP_**** ####
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
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    diamclass_id = as.character(diamclass_id)
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    # dbh must be recalculated as it is not the sum
    dbh_diss = sqrt((basal_area_diss*40000)/(pi*density_diss)),
    dbh_inc = sqrt((basal_area_inc*40000)/(pi*density_inc)),
    dbh_dead = sqrt((basal_area_dead*40000)/(pi*density_dead)),
    dbh_har = sqrt((basal_area_har*40000)/(pi*density_har)),
    dbh_rem = sqrt((basal_area_rem*40000)/(pi*density_rem)),
    dbh_balance = sqrt((basal_area_balance*40000)/(pi*density_balance)),
    dbh_growth = sqrt((basal_area_growth*40000)/(pi*density_growth))
  ) %>%
  ungroup() -> SIMPSPECIES_COMP_NFI3_NFI4_RESULTS

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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff
  ) %>%
  ungroup() %>%
  mutate(
    diamclass_id = as.character(diamclass_id)
  ) -> SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### GENUS_COMP_**** ####
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
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    diamclass_id = as.character(diamclass_id)
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    # dbh must be recalculated as it is not the sum
    dbh_diss = sqrt((basal_area_diss*40000)/(pi*density_diss)),
    dbh_inc = sqrt((basal_area_inc*40000)/(pi*density_inc)),
    dbh_dead = sqrt((basal_area_dead*40000)/(pi*density_dead)),
    dbh_har = sqrt((basal_area_har*40000)/(pi*density_har)),
    dbh_rem = sqrt((basal_area_rem*40000)/(pi*density_rem)),
    dbh_balance = sqrt((basal_area_balance*40000)/(pi*density_balance)),
    dbh_growth = sqrt((basal_area_growth*40000)/(pi*density_growth))
  ) %>%
  ungroup() -> GENUS_COMP_NFI3_NFI4_RESULTS

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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff
  ) %>%
  ungroup() %>%
  mutate(
    diamclass_id = as.character(diamclass_id)
  ) -> GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### DEC_COMP_**** ####
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
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    diamclass_id = as.character(diamclass_id)
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    # dbh must be recalculated as it is not the sum
    dbh_diss = sqrt((basal_area_diss*40000)/(pi*density_diss)),
    dbh_inc = sqrt((basal_area_inc*40000)/(pi*density_inc)),
    dbh_dead = sqrt((basal_area_dead*40000)/(pi*density_dead)),
    dbh_har = sqrt((basal_area_har*40000)/(pi*density_har)),
    dbh_rem = sqrt((basal_area_rem*40000)/(pi*density_rem)),
    dbh_balance = sqrt((basal_area_balance*40000)/(pi*density_balance)),
    dbh_growth = sqrt((basal_area_growth*40000)/(pi*density_growth))
  ) %>%
  ungroup() -> DEC_COMP_NFI3_NFI4_RESULTS

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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff
  ) %>%
  ungroup() %>%
  mutate(
    diamclass_id = as.character(diamclass_id)
  ) -> DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS

#### BC_COMP_**** ####
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
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    diamclass_id = as.character(diamclass_id)
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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff,
    # dbh must be recalculated as it is not the sum
    dbh_diss = sqrt((basal_area_diss*40000)/(pi*density_diss)),
    dbh_inc = sqrt((basal_area_inc*40000)/(pi*density_inc)),
    dbh_dead = sqrt((basal_area_dead*40000)/(pi*density_dead)),
    dbh_har = sqrt((basal_area_har*40000)/(pi*density_har)),
    dbh_rem = sqrt((basal_area_rem*40000)/(pi*density_rem)),
    dbh_balance = sqrt((basal_area_balance*40000)/(pi*density_balance)),
    dbh_growth = sqrt((basal_area_growth*40000)/(pi*density_growth))
  ) %>%
  ungroup() -> BC_COMP_NFI3_NFI4_RESULTS

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
    density_har = density_har / years_diff,
    basal_area_diss = basal_area_diss / years_diff,
    basal_area_rem = basal_area_rem / years_diff,
    basal_area_inc = basal_area_inc / years_diff,
    basal_area_dead = basal_area_dead / years_diff,
    basal_area_har = basal_area_har / years_diff,
    volume_over_bark_diss = volume_over_bark_diss / years_diff,
    volume_over_bark_rem = volume_over_bark_rem / years_diff,
    volume_over_bark_inc = volume_over_bark_inc / years_diff,
    volume_over_bark_dead = volume_over_bark_dead / years_diff,
    volume_over_bark_har = volume_over_bark_har / years_diff,
    volume_under_bark_diss = volume_under_bark_diss / years_diff,
    volume_under_bark_rem = volume_under_bark_rem / years_diff,
    volume_under_bark_inc = volume_under_bark_inc / years_diff,
    volume_under_bark_dead = volume_under_bark_dead / years_diff,
    volume_under_bark_har = volume_under_bark_har / years_diff
  ) %>%
  ungroup() %>%
  mutate(
    diamclass_id = as.character(diamclass_id)
  ) -> BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS
