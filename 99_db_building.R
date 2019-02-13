#### STEP 1 Build the database ####

#### Plots static info ####
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

#### Plots dynamic info ####
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

PLOTS_NFI_4_DYNAMIC_INFO %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOTS_NFI_4_DYNAMIC_INFO', overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'plot_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOTS_NFI_4_DYNAMIC_INFO"
  ADD PRIMARY KEY (plot_id);'
)

#### Plot level results ####
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

#### Functional groups tables ####
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

#### comparision tables ####

PLOT_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

PLOT_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_COMP_NFI3_NFI4_RESULTS"
  ADD PRIMARY KEY (plot_id);'
)

PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id);'
)

SPECIES_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

SPECIES_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_COMP_NFI3_NFI4_RESULTS"
  ADD PRIMARY KEY (plot_id, species_id);'
)

SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'species_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, species_id);'
)

GENUS_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

GENUS_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_COMP_NFI3_NFI4_RESULTS"
  ADD PRIMARY KEY (plot_id, genus_id);'
)

GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'genus_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, genus_id);'
)

DEC_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

DEC_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_COMP_NFI3_NFI4_RESULTS"
  ADD PRIMARY KEY (plot_id, dec_id);'
)

DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'dec_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, dec_id);'
)

BC_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

BC_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_COMP_NFI3_NFI4_RESULTS"
  ADD PRIMARY KEY (plot_id, bc_id);'
)

BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'bc_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, bc_id);'
)

SIMPSPECIES_COMP_NFI2_NFI3_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_COMP_NFI2_NFI3_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_COMP_NFI2_NFI3_RESULTS"
  ADD PRIMARY KEY (plot_id, simpspecies_id);'
)

SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS"
  ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
)

SIMPSPECIES_COMP_NFI3_NFI4_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_COMP_NFI3_NFI4_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'simpspecies_id')
    )
  )
# pool::dbExecute(
#   brand_new_nfi_db,
#   'ALTER TABLE "SIMPSPECIES_COMP_NFI3_NFI4_RESULTS"
#   ADD PRIMARY KEY (plot_id, simpspecies_id);'
# )

SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('plot_id', 'diamclass_id', 'simpspecies_id')
    )
  )
# pool::dbExecute(
#   brand_new_nfi_db,
#   'ALTER TABLE "SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS"
#   ADD PRIMARY KEY (plot_id, diamclass_id, simpspecies_id);'
# )

#### Variables Thesaurus ####
# vars_table %>%
readr::read_csv('data_raw/variables_thesaurus_modified.csv') %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_THESAURUS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      c('var_id', 'var_table')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_THESAURUS"
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
#     brand_new_nfi_db, df = ., name = 'test',
#     overwrite = TRUE, temporary = FALSE
#   )
# 
# glue::glue("{[categorical_variables$var_values]}", .open = "[", .close = "]")

categorical_variables %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_CATEGORICAL',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'dummy_id', 'var_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_CATEGORICAL"
  ADD PRIMARY KEY (dummy_id, var_id);'
)

# numerical_variables %>%
readr::read_csv('data_raw/numerical_variables.csv') %>%
  copy_to(
    brand_new_nfi_db, df = ., name = 'VARIABLES_NUMERICAL',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'var_id', 'var_table'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_NUMERICAL"
  ADD PRIMARY KEY (var_id, var_table);'
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
      c('var_id', 'var_table')
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "VARIABLES_DTTM"
   ADD PRIMARY KEY (var_id, var_table);'
)

#### Species thesaurus ####
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
    brand_new_nfi_db, df = ., name = 'TEXTS_THESAURUS',
    overwrite = TRUE, temporary = FALSE,
    indexes = list(
      'text_id'
    )
  )
pool::dbExecute(
  brand_new_nfi_db,
  'ALTER TABLE "TEXTS_THESAURUS"
  ADD PRIMARY KEY (text_id);'
)

