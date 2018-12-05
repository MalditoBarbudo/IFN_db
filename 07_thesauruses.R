#### STEP 1 Variables thesauruses ####

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
      # var_id == 'feat_sampling_start_time' ~ 'tempsmostreig',
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
writexl::write_xlsx(vars_table, 'data_raw/variables_thesaurus.xlsx')

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

