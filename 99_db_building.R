#### STEP 1 Build the database ####

#### Plots static info ####
PLOTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOTS'), overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plots"
  ADD PRIMARY KEY (plot_id);'
)

#### Plots dynamic info ####
PLOTS_NFI_2_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOTS_NFI_2_DYNAMIC_INFO'), overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plots_nfi_2_dynamic_info"
  ADD PRIMARY KEY (plot_id);'
)

PLOTS_NFI_3_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOTS_NFI_3_DYNAMIC_INFO'), overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plots_nfi_3_dynamic_info"
  ADD PRIMARY KEY (plot_id);'
)

PLOTS_NFI_4_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOTS_NFI_4_DYNAMIC_INFO'), overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plots_nfi_4_dynamic_info"
  ADD PRIMARY KEY (plot_id);'
)

#### Plot level results ####
PLOT_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_2_results"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_3_results"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_4_results"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

#### Functional groups tables ####
# Species
SPECIES_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_2_results"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_3_results"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_4_results"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

# simplified species
SIMPSPECIES_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_2_results"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_3_results"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_4_results"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

# genus
GENUS_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_2_results"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_3_results"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_4_results"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

# dec
DEC_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_2_results"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_3_results"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_4_results"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

# bc
BC_NFI_2_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_2_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_2_results"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_2_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_2_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_2_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_NFI_3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_3_results"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_NFI_4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_4_results"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_NFI_4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_NFI_4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_nfi_4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

#### comparision tables ####

PLOT_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_comp_nfi3_nfi4_results"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "plot_comp_nfi3_nfi4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

SPECIES_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_comp_nfi3_nfi4_results"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_comp_nfi3_nfi4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

GENUS_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_comp_nfi3_nfi4_results"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "genus_comp_nfi3_nfi4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

DEC_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_comp_nfi3_nfi4_results"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "dec_comp_nfi3_nfi4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

BC_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_comp_nfi3_nfi4_results"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "bc_comp_nfi3_nfi4_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

SIMPSPECIES_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_COMP_NFI2_NFI3_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_comp_nfi2_nfi3_results"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "simpspecies_comp_nfi2_nfi3_diamclass_results"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_COMP_NFI3_NFI4_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
# pool::dbExecute(
#   brand_new_nfi_db,
#   'ALTER TABLE "simpspecies_comp_nfi3_nfi4_results"
#   ADD PRIMARY KEY (plot_id, simpspecies_id);'
# )

SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
# pool::dbExecute(
#   brand_new_nfi_db,
#   'ALTER TABLE "simpspecies_comp_nfi3_nfi4_diamclass_results"
#   ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
# )

#### Shrub tables ####
SHRUB_NFI_2_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SHRUB_NFI_2_INFO'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "shrub_nfi_2_info"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SHRUB_NFI_3_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SHRUB_NFI_3_INFO'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "shrub_nfi_3_info"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SHRUB_NFI_4_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SHRUB_NFI_4_INFO'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "shrub_nfi_4_info"
  ADD PRIMARY KEY (plot_id, species_id);'
)

#### Regeneration tables ####
REGENERATION_NFI_2 %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('REGENERATION_NFI_2'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "regeneration_nfi_2"
  ADD PRIMARY KEY (plot_id, species_id);'
)


REGENERATION_NFI_3 %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('REGENERATION_NFI_3'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "regeneration_nfi_3"
  ADD PRIMARY KEY (plot_id, species_id);'
)

REGENERATION_NFI_4 %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('REGENERATION_NFI_4'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "regeneration_nfi_4"
  ADD PRIMARY KEY (plot_id, species_id);'
)
#### Variables Thesaurus ####
# vars_table %>%
readr::read_csv('data_raw/variables_thesaurus_modified.csv') %>%
  dplyr::mutate(var_table = tolower(var_table)) %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('VARIABLES_THESAURUS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('var_id', 'var_table')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "variables_thesaurus"
  ADD PRIMARY KEY (var_id,var_table);'
)

# let's try to be arrays
# pool::dbExecute(
#   brand_new_nfi_db,
#   'ALTER TABLE "VARIABLES_CATEGORICAL" ALTER COLUMN var_values TYPE text[]'
# )
# 
# categorical_variables %>%
#   copy_to(
#     brand_new_nfi_db, df = ., name = tolower('test'),
#     overwrite = TRUE, temporary = FALSE
#   )
# 
# glue::glue("{[categorical_variables$var_values]}", .open = "[", .close = "]")

categorical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('VARIABLES_CATEGORICAL'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'dummy_id', 'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "variables_categorical"
  ADD PRIMARY KEY (dummy_id, var_id);'
)

# numerical_variables %>%
readr::read_csv('data_raw/numerical_variables.csv') %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('VARIABLES_NUMERICAL'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id', 'var_table'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "variables_numerical"
  ADD PRIMARY KEY (var_id, var_table);'
)

logical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('VARIABLES_LOGICAL'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "variables_logical"
   ADD PRIMARY KEY (var_id);'
)

dttm_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('VARIABLES_DTTM'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('var_id', 'var_table')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "variables_dttm"
   ADD PRIMARY KEY (var_id, var_table);'
)

#### Species thesaurus ####
species_table %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('SPECIES_THESAURUS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'code_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "species_thesaurus"
   ADD PRIMARY KEY (code_id);'
)



# pool::dbExecute(
#   brand_new_nfi_db,
#   build_sql(
#     "CREATE TABLE test (
#      var_id TEXT PRIMARY KEY,
#      var_values TEXT []
#   );
#   ",
#     con = brand_new_nfi_db
#   )
# )

#### App texts thesaurus ####
readr::read_csv('data_raw/texts_thesaurus.csv') %>%
  copy_to(
    brand_new_nfi_db, df = ., name = tolower('TEXTS_THESAURUS'),
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'text_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "texts_thesaurus"
  ADD PRIMARY KEY (text_id);'
)

