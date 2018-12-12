#### STEP 1 Main Variable table ####

## The main theasurus is the VARIABLES_THESAURUS, which will contain all the variables,
## their old names, the translations, the scenarios in which they are involved, their
## type (chr, num...), their descriptions in each lenguage, their presence in the
## different versions...

# lets obtain the variable names (as var_id) and the class (as var_type) for each table
# and join them all together, to remove the repeated ones later on

## helper function to avoid c&p a lot
table_and_type <- function(x) {
  rlang::sym(x) %>%
    rlang::eval_tidy() %>%
    summarise_all(~class(.x)[1]) %>%
    gather(var_id, var_type) %>%
    mutate(var_table = x) %>%
    select(var_id, var_table, var_type)
}

tables_names <- c(
  'PLOTS', 'PLOTS_NFI_2_DYNAMIC_INFO', 'PLOTS_NFI_3_DYNAMIC_INFO', 'PLOTS_NFI_4_DYNAMIC_INFO',
  'PLOT_NFI_2_RESULTS', 'PLOT_NFI_2_DIAMCLASS_RESULTS',
  'PLOT_NFI_3_RESULTS','PLOT_NFI_3_DIAMCLASS_RESULTS',
  'PLOT_NFI_4_RESULTS', 'PLOT_NFI_4_DIAMCLASS_RESULTS',
  'SPECIES_NFI_2_RESULTS', 'SPECIES_NFI_2_DIAMCLASS_RESULTS',
  'SPECIES_NFI_3_RESULTS', 'SPECIES_NFI_3_DIAMCLASS_RESULTS',
  'SPECIES_NFI_4_RESULTS', 'SPECIES_NFI_4_DIAMCLASS_RESULTS',
  'SIMPSPECIES_NFI_2_RESULTS', 'SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS',
  'SIMPSPECIES_NFI_3_RESULTS', 'SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS',
  'SIMPSPECIES_NFI_4_RESULTS', 'SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS',
  'GENUS_NFI_2_RESULTS', 'GENUS_NFI_2_DIAMCLASS_RESULTS',
  'GENUS_NFI_3_RESULTS', 'GENUS_NFI_3_DIAMCLASS_RESULTS',
  'GENUS_NFI_4_RESULTS', 'GENUS_NFI_4_DIAMCLASS_RESULTS',
  'BC_NFI_2_RESULTS', 'BC_NFI_2_DIAMCLASS_RESULTS',
  'BC_NFI_3_RESULTS', 'BC_NFI_3_DIAMCLASS_RESULTS',
  'BC_NFI_4_RESULTS', 'BC_NFI_4_DIAMCLASS_RESULTS',
  'DEC_NFI_2_RESULTS', 'DEC_NFI_2_DIAMCLASS_RESULTS',
  'DEC_NFI_3_RESULTS', 'DEC_NFI_3_DIAMCLASS_RESULTS',
  'DEC_NFI_4_RESULTS', 'DEC_NFI_4_DIAMCLASS_RESULTS',
  'PLOT_COMP_NFI2_NFI3_RESULTS', 'PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'PLOT_COMP_NFI3_NFI4_RESULTS', 'PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
  'SPECIES_COMP_NFI2_NFI3_RESULTS', 'SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'SPECIES_COMP_NFI3_NFI4_RESULTS', 'SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
  'SIMPSPECIES_COMP_NFI2_NFI3_RESULTS', 'SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'SIMPSPECIES_COMP_NFI3_NFI4_RESULTS', 'SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
  'GENUS_COMP_NFI2_NFI3_RESULTS', 'GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'GENUS_COMP_NFI3_NFI4_RESULTS', 'GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
  'DEC_COMP_NFI2_NFI3_RESULTS', 'DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'DEC_COMP_NFI3_NFI4_RESULTS', 'DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
  'BC_COMP_NFI2_NFI3_RESULTS', 'BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
  'BC_COMP_NFI3_NFI4_RESULTS', 'BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS'
)

tables_names %>%
  purrr::map(table_and_type) %>%
  bind_rows() %>%
  mutate(
    presence_scenario1 = case_when(
      var_table %in% {c(
        'PLOTS', 'PLOT_NFI_2_RESULTS', 'PLOT_NFI_2_DIAMCLASS_RESULTS',
        'PLOT_NFI_3_RESULTS', 'PLOT_NFI_3_DIAMCLASS_RESULTS',
        'PLOT_NFI_4_RESULTS', 'PLOT_NFI_4_DIAMCLASS_RESULTS',
        # 'SPECIES_NFI_2_RESULTS', 'SPECIES_NFI_2_DIAMCLASS_RESULTS',
        # 'SPECIES_NFI_3_RESULTS', 'SPECIES_NFI_3_DIAMCLASS_RESULTS',
        # 'SPECIES_NFI_4_RESULTS', 'SPECIES_NFI_4_DIAMCLASS_RESULTS',
        # 'SIMPSPECIES_NFI_2_RESULTS', 'SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS',
        # 'SIMPSPECIES_NFI_3_RESULTS', 'SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS',
        # 'SIMPSPECIES_NFI_4_RESULTS', 'SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS',
        # 'GENUS_NFI_2_RESULTS', 'GENUS_NFI_2_DIAMCLASS_RESULTS',
        # 'GENUS_NFI_3_RESULTS', 'GENUS_NFI_3_DIAMCLASS_RESULTS',
        # 'GENUS_NFI_4_RESULTS', 'GENUS_NFI_4_DIAMCLASS_RESULTS',
        # 'BC_NFI_2_RESULTS', 'BC_NFI_2_DIAMCLASS_RESULTS',
        # 'BC_NFI_3_RESULTS', 'BC_NFI_3_DIAMCLASS_RESULTS',
        # 'BC_NFI_4_RESULTS', 'BC_NFI_4_DIAMCLASS_RESULTS',
        # 'DEC_NFI_2_RESULTS', 'DEC_NFI_2_DIAMCLASS_RESULTS',
        # 'DEC_NFI_3_RESULTS', 'DEC_NFI_3_DIAMCLASS_RESULTS',
        # 'DEC_NFI_4_RESULTS', 'DEC_NFI_4_DIAMCLASS_RESULTS',
        'PLOT_COMP_NFI2_NFI3_RESULTS', 'PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'PLOT_COMP_NFI3_NFI4_RESULTS', 'PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        # 'SPECIES_COMP_NFI2_NFI3_RESULTS', 'SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'SPECIES_COMP_NFI3_NFI4_RESULTS', 'SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        # 'SIMPSPECIES_COMP_NFI2_NFI3_RESULTS', 'SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'SIMPSPECIES_COMP_NFI3_NFI4_RESULTS', 'SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        # 'GENUS_COMP_NFI2_NFI3_RESULTS', 'GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'GENUS_COMP_NFI3_NFI4_RESULTS', 'GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        # 'DEC_COMP_NFI2_NFI3_RESULTS', 'DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'DEC_COMP_NFI3_NFI4_RESULTS', 'DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        # 'BC_COMP_NFI2_NFI3_RESULTS', 'BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'BC_COMP_NFI3_NFI4_RESULTS', 'BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'PLOTS_NFI_2_DYNAMIC_INFO', 'PLOTS_NFI_3_DYNAMIC_INFO', 'PLOTS_NFI_4_DYNAMIC_INFO'
      )} ~ TRUE,
      TRUE ~ FALSE
    ),
    
    presence_scenario2 = case_when(
      var_table %in% {c(
        'PLOTS', #'PLOT_NFI_2_RESULTS', 'PLOT_NFI_2_DIAMCLASS_RESULTS',
        # 'PLOT_NFI_3_RESULTS', 'PLOT_NFI_3_DIAMCLASS_RESULTS',
        # 'PLOT_NFI_4_RESULTS', 'PLOT_NFI_4_DIAMCLASS_RESULTS',
        'SPECIES_NFI_2_RESULTS', 'SPECIES_NFI_2_DIAMCLASS_RESULTS',
        'SPECIES_NFI_3_RESULTS', 'SPECIES_NFI_3_DIAMCLASS_RESULTS',
        'SPECIES_NFI_4_RESULTS', 'SPECIES_NFI_4_DIAMCLASS_RESULTS',
        'SIMPSPECIES_NFI_2_RESULTS', 'SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS',
        'SIMPSPECIES_NFI_3_RESULTS', 'SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS',
        'SIMPSPECIES_NFI_4_RESULTS', 'SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS',
        'GENUS_NFI_2_RESULTS', 'GENUS_NFI_2_DIAMCLASS_RESULTS',
        'GENUS_NFI_3_RESULTS', 'GENUS_NFI_3_DIAMCLASS_RESULTS',
        'GENUS_NFI_4_RESULTS', 'GENUS_NFI_4_DIAMCLASS_RESULTS',
        'BC_NFI_2_RESULTS', 'BC_NFI_2_DIAMCLASS_RESULTS',
        'BC_NFI_3_RESULTS', 'BC_NFI_3_DIAMCLASS_RESULTS',
        'BC_NFI_4_RESULTS', 'BC_NFI_4_DIAMCLASS_RESULTS',
        'DEC_NFI_2_RESULTS', 'DEC_NFI_2_DIAMCLASS_RESULTS',
        'DEC_NFI_3_RESULTS', 'DEC_NFI_3_DIAMCLASS_RESULTS',
        'DEC_NFI_4_RESULTS', 'DEC_NFI_4_DIAMCLASS_RESULTS',
        # 'PLOT_COMP_NFI2_NFI3_RESULTS', 'PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        # 'PLOT_COMP_NFI3_NFI4_RESULTS', 'PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'SPECIES_COMP_NFI2_NFI3_RESULTS', 'SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'SPECIES_COMP_NFI3_NFI4_RESULTS', 'SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'SIMPSPECIES_COMP_NFI2_NFI3_RESULTS', 'SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'SIMPSPECIES_COMP_NFI3_NFI4_RESULTS', 'SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'GENUS_COMP_NFI2_NFI3_RESULTS', 'GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'GENUS_COMP_NFI3_NFI4_RESULTS', 'GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'DEC_COMP_NFI2_NFI3_RESULTS', 'DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'DEC_COMP_NFI3_NFI4_RESULTS', 'DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'BC_COMP_NFI2_NFI3_RESULTS', 'BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS',
        'BC_COMP_NFI3_NFI4_RESULTS', 'BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS',
        'PLOTS_NFI_2_DYNAMIC_INFO', 'PLOTS_NFI_3_DYNAMIC_INFO', 'PLOTS_NFI_4_DYNAMIC_INFO'
      )} ~ TRUE,
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

#### STEP 2 By type thesauruses ####
# categorial
categorical_variables <- vars_table %>%
  filter(var_type == 'character') %>%
  select(var_id, var_table)

## little helper
categorical_values <- function(x) {
  rlang::sym(x) %>%
    rlang::eval_tidy() %>%
    select(one_of(categorical_variables %>% pull(var_id) %>% unique())) %>%
    gather('var_id', 'var_values') %>%
    distinct() %>%
    nest(-var_id, .key = 'var_values') %>%
    mutate(var_values = map(var_values, pull), var_table = x)
}

categorical_variables <- tables_names %>%
  purrr::map(categorical_values) %>%
  bind_rows %>%
  right_join(categorical_variables, by = c('var_id', 'var_table')) %>%
  # no easy manage of arrays exists yet between postgres and r so, let's unnest and create
  # a key, that in combination with var_id is unique
  unnest() %>%
  mutate(dummy_id = 1:nrow(.)) %>%
  select(dummy_id, var_id, var_table, var_values)

## numerical
numerical_variables <- vars_table %>%
  filter(var_type %in% c('numeric', 'integer')) %>%
  select(var_id) %>%
  mutate(var_units = '')

# min_max values for numerical vars

# plots table 
PLOTS %>%
  select(one_of(numerical_variables %>% pull(var_id))) %>%
  summarise_all(
    .funs = dplyr::funs(
      min = floor(min(., na.rm = TRUE)), max = ceiling(max(., na.rm = TRUE))
    )
  ) -> PLOTS_min_max

PLOTS_min_max %>%
  select(ends_with('_min')) %>%
  magrittr::set_names(
    ., stringr::str_replace(names(.), '_min$', '')
  ) %>%
  tidyr::gather('var_id', 'var_min') %>%
  dplyr::full_join(
    PLOTS_min_max %>%
      select(ends_with('_max')) %>%
      magrittr::set_names(
        ., stringr::str_replace(names(.), '_max$', '')
      ) %>%
      tidyr::gather('var_id', 'var_max'),
    by = 'var_id'
  ) -> PLOTS_numerical_limits

# plots dynamic info tables
PLOTS_NFI_2_DYNAMIC_INFO %>%
  bind_rows(PLOTS_NFI_3_DYNAMIC_INFO) %>%
  bind_rows(PLOTS_NFI_4_DYNAMIC_INFO) %>%
  select(one_of(numerical_variables %>% pull(var_id))) %>%
  summarise_all(
    .funs = dplyr::funs(
      min = floor(min(., na.rm = TRUE)), max = ceiling(max(., na.rm = TRUE))
    )
  ) -> PLOTS_DYNAMIC_INFO_min_max

PLOTS_DYNAMIC_INFO_min_max %>%
  select(ends_with('_min')) %>%
  magrittr::set_names(
    ., stringr::str_replace(names(.), '_min$', '')
  ) %>%
  tidyr::gather('var_id', 'var_min') %>%
  dplyr::full_join(
    PLOTS_DYNAMIC_INFO_min_max %>%
      select(ends_with('_max')) %>%
      magrittr::set_names(
        ., stringr::str_replace(names(.), '_max$', '')
      ) %>%
      tidyr::gather('var_id', 'var_max'),
    by = 'var_id'
  ) -> PLOTS_DYNAMIC_numerical_limits

# results tables
list(
  PLOT_NFI_2_RESULTS, PLOT_NFI_3_RESULTS, PLOT_NFI_4_RESULTS,
  PLOT_NFI_2_DIAMCLASS_RESULTS, PLOT_NFI_3_DIAMCLASS_RESULTS, PLOT_NFI_4_DIAMCLASS_RESULTS,
  SPECIES_NFI_2_RESULTS, SPECIES_NFI_3_RESULTS, SPECIES_NFI_4_RESULTS,
  SPECIES_NFI_2_DIAMCLASS_RESULTS, SPECIES_NFI_3_DIAMCLASS_RESULTS, SPECIES_NFI_4_DIAMCLASS_RESULTS,
  SIMPSPECIES_NFI_2_RESULTS, SIMPSPECIES_NFI_3_RESULTS, SIMPSPECIES_NFI_4_RESULTS,
  SIMPSPECIES_NFI_2_DIAMCLASS_RESULTS, SIMPSPECIES_NFI_3_DIAMCLASS_RESULTS, SIMPSPECIES_NFI_4_DIAMCLASS_RESULTS,
  GENUS_NFI_2_RESULTS, GENUS_NFI_3_RESULTS, GENUS_NFI_4_RESULTS,
  GENUS_NFI_2_DIAMCLASS_RESULTS, GENUS_NFI_3_DIAMCLASS_RESULTS, GENUS_NFI_4_DIAMCLASS_RESULTS,
  DEC_NFI_2_RESULTS, DEC_NFI_3_RESULTS, DEC_NFI_4_RESULTS,
  DEC_NFI_2_DIAMCLASS_RESULTS, DEC_NFI_3_DIAMCLASS_RESULTS, DEC_NFI_4_DIAMCLASS_RESULTS,
  BC_NFI_2_RESULTS, BC_NFI_3_RESULTS, BC_NFI_4_RESULTS,
  BC_NFI_2_DIAMCLASS_RESULTS, BC_NFI_3_DIAMCLASS_RESULTS, BC_NFI_4_DIAMCLASS_RESULTS
) %>%
  reduce(bind_rows) %>%
  select(one_of(numerical_variables %>% pull(var_id))) %>%
  summarise_all(
    .funs = dplyr::funs(
      min = floor(min(., na.rm = TRUE)), max = ceiling(max(., na.rm = TRUE))
    )
  ) -> RESULTS_min_max

RESULTS_min_max %>%
  select(ends_with('_min')) %>%
  magrittr::set_names(
    ., stringr::str_replace(names(.), '_min$', '')
  ) %>%
  tidyr::gather('var_id', 'var_min') %>%
  dplyr::full_join(
    RESULTS_min_max %>%
      select(ends_with('_max')) %>%
      magrittr::set_names(
        ., stringr::str_replace(names(.), '_max$', '')
      ) %>%
      tidyr::gather('var_id', 'var_max'),
    by = 'var_id'
  ) -> RESULTS_numerical_limits

# comparation tables
# list(
#   PLOT_COMP_NFI2_NFI3_RESULTS, PLOT_COMP_NFI3_NFI4_RESULTS,
#   PLOT_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, PLOT_COMP_NFI3_NFI4_DIAMCLASS_RESULTS,
#   SPECIES_COMP_NFI2_NFI3_RESULTS, SPECIES_COMP_NFI3_NFI4_RESULTS,
#   SPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, SPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS,
#   SIMPSPECIES_COMP_NFI2_NFI3_RESULTS, SIMPSPECIES_COMP_NFI3_NFI4_RESULTS,
#   SIMPSPECIES_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, SIMPSPECIES_COMP_NFI3_NFI4_DIAMCLASS_RESULTS,
#   GENUS_COMP_NFI2_NFI3_RESULTS, GENUS_COMP_NFI3_NFI4_RESULTS,
#   GENUS_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, GENUS_COMP_NFI3_NFI4_DIAMCLASS_RESULTS,
#   DEC_COMP_NFI2_NFI3_RESULTS, DEC_COMP_NFI3_NFI4_RESULTS,
#   DEC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, DEC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS,
#   BC_COMP_NFI2_NFI3_RESULTS, BC_COMP_NFI3_NFI4_RESULTS,
#   BC_COMP_NFI2_NFI3_DIAMCLASS_RESULTS, BC_COMP_NFI3_NFI4_DIAMCLASS_RESULTS
# ) %>%
#   reduce(bind_rows) %>%
#   select(one_of(numerical_variables %>% pull(var_id))) %>%
#   summarise_all(
#     .funs = dplyr::funs(
#       min = floor(min(., na.rm = TRUE)), max = ceiling(max(., na.rm = TRUE))
#     )
#   ) -> COMP_RESULTS_min_max
# 
# COMP_RESULTS_min_max %>%
#   select(ends_with('_min')) %>%
#   magrittr::set_names(
#     ., stringr::str_replace(names(.), '_min$', '')
#   ) %>%
#   tidyr::gather('var_id', 'var_min') %>%
#   dplyr::full_join(
#     COMP_RESULTS_min_max %>%
#       select(ends_with('_max')) %>%
#       magrittr::set_names(
#         ., stringr::str_replace(names(.), '_max$', '')
#       ) %>%
#       tidyr::gather('var_id', 'var_max'),
#     by = 'var_id'
#   ) -> COMP_RESULTS_numerical_limits

numerical_variables %<>%
  left_join(
    PLOTS_numerical_limits %>%
      union(PLOTS_DYNAMIC_numerical_limits) %>%
      union(RESULTS_numerical_limits),
    by = 'var_id'
  )

### TODO rest of min max vars, by row binding the results tables, calculate the
### min and max and join with the numerical var

# logical
logical_variables <- vars_table %>%
  filter(var_type == 'logical') %>%
  select(var_id)

# dates
dttm_variables <- vars_table %>%
  filter(var_type == 'POSIXct') %>%
  select(var_id)

