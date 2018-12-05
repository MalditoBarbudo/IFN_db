#### STEP 1 Tree tables ####
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

#### STEP 2 Shrub tables ####
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

#### STEP 3 Regeneration tables ####
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
## TODO The rest of the regeneration tables (nfi3 and nfi4) must be done


