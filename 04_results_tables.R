#### STEP 1 Results tables ####
# Now, we need to create the results tables, renaming the variables and standardizing
# everything.

#### PLOT_NFI_*_RESULTS ####
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
  ########################################################################################
  # Here, bh and br (as well as ch and cr) are exchanged, so we have to fix it. the easy #
  # way of doing this is changing them here in the select/rename step                    #
  ########################################################################################
  select(
    plot_id, #everything()
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = br,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = bh,
    basal_area_dec_dominant = caducesclerconifab,
    density_dec_dominant = caducesclerconifdens,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = cr,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = ch,
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
    ## This is not the final table, as it lacks of the simpspecies dominants and
    ## percentages. We will do this later when the simpspecies_nfi_4_results table is done
  ) -> plot_nfi_4_results_temp 

#### SPECIES_NFI_*_RESULTS ####
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

#### SIMPSPECIES_NFI_*_RESULTS ####
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

# we have now the simpspecies_nfi_4_results, so we can finish the plot_nfi_4_results
# table
plot_nfi_4_results_temp %>%
  left_join(
    SIMPSPECIES_NFI_4_RESULTS %>%
      select(plot_id, simpspecies_id, basal_area_percentage) %>%
      group_by(plot_id) %>%
      slice(which.max(basal_area_percentage)) %>%
      select(
        plot_id,
        basal_area_simp_species_dominant = simpspecies_id,
        basal_area_simp_species_percentage = basal_area_percentage
      ),
    by = 'plot_id'
  ) %>%
  left_join(
    SIMPSPECIES_NFI_4_RESULTS %>%
      select(plot_id, simpspecies_id, density_percentage) %>%
      group_by(plot_id) %>%
      slice(which.max(density_percentage)) %>%
      select(
        plot_id,
        density_simp_species_dominant = simpspecies_id,
        density_simp_species_percentage = density_percentage
      ),
    by = 'plot_id'
  ) -> PLOT_NFI_4_RESULTS

#### GENUS_NFI_*_RESULTS ####
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

#### BC_NFI_*_RESULTS ####
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

#### DEC_NFI_*_RESULTS ####
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

#### PLOT_NFI_*_DIAMCLASS_RESULTS ####
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
  ########################################################################################
  # Here, bh and br (as well as ch and cr) are exchanged, so we have to fix it. the easy #
  # way of doing this is changing them here in the select/rename step                    #
  ########################################################################################
  select(
    plot_id, #everything()
    diamclass_id = idcd,
    basal_area = ab,
    basal_area_dead = abmorts,
    aerial_biomass_total = bat,
    trunk_bark_biomass = bc,
    leaf_biomass = br,
    trunk_wood_biomass = bm,
    branch_wo_leaves_biomass = bh,
    aerial_carbon_total = cat,
    trunk_bark_carbon = cc,
    accumulated_aerial_co2 = cca,
    leaf_carbon = cr,
    trunk_wood_carbon = cm,
    branch_wo_leaves_carbon = ch,
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
  ) -> PLOT_NFI_4_DIAMCLASS_RESULTS

#### SPECIES_NFI_*_DIAMCLASS_RESULTS ####
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

#### SIMPSPECIES_NFI_*_DIAMCLASS_RESULTS ####
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

#### GENUS_NFI_*_DIAMCLASS_RESULTS ####
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

#### DEC_NFI_*_DIAMCLASS_RESULTS ####
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

#### BC_NFI_*_DIAMCLASS_RESULTS ####
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
