# Script to generate the raw tables before removing the noise in the consult
# tables

# libraries
library(dplyr)
library(pool)

# dbs
ifndb <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifndb'
)

# generating the raw tables
for (table in dbListTables(ifndb)) {
  
  tbl(ifndb, table) %>%
    collect() %>%
    copy_to(
      dest = ifndb, df = ., name = glue::glue("{table}_raw"),
      overwrite = TRUE, temporary = FALSE
    )
  
}

# removing R* plots from ifn3 and ifn4 (n*)
ifn_vars_to_maintain <- tbl(ifndb, 'ifn3_parcela_res') %>%
  collect() %>%
  select(-starts_with('v'), -starts_with('ia'), -starts_with('ph'), -idclasse) %>%
  names() %>%
  sort() %>%
  c(., 'ordreab', 'ordredens')

# ifn3
tbl(ifndb, 'ifn3_sig') %>%
  collect() %>%
  # cleaning the R plots
  filter(!stringr::str_detect(.$idclasse, 'R')) %>%
  copy_to(
    dest = ifndb, df = ., name = 'ifn3_sig',
    overwrite = TRUE, temporary = FALSE
  )

ifn3_plots <- tbl(ifndb, 'ifn3_sig') %>%
  collect() %>%
  pull(idparcela)

# tables from ifn3 (not raw ones)
ifn3_tables <- dbListTables(ifndb)[stringr::str_detect(dbListTables(ifndb), 'ifn3') &
                                     !stringr::str_detect(dbListTables(ifndb), '_raw') &
                                     !stringr::str_detect(dbListTables(ifndb), '_clima') &
                                     !stringr::str_detect(dbListTables(ifndb), '_sig')]

for (table in ifn3_tables) {
  tbl(ifndb, table) %>%
    collect() %>%
    # filtering the R plots
    filter(idparcela %in% ifn3_plots) %>%
    # selecting the variables
    dplyr::select(one_of(ifn_vars_to_maintain)) %>%
    # copying to db
    copy_to(
      dest = ifndb, df = ., name = table,
      overwrite = TRUE, temporary = FALSE
    )
}

# ifn4
tbl(ifndb, 'ifn4_sig') %>%
  collect() %>%
  filter(!stringr::str_detect(.$idclasse, '^[0-9]')) %>%
  copy_to(
    dest = ifndb, df = ., name = 'ifn4_sig',
    overwrite = TRUE, temporary = FALSE
  )

ifn4_plots <- tbl(ifndb, 'ifn4_sig') %>%
  collect() %>%
  pull(idparcela)

# tables from ifn4 (not raw ones)
ifn4_tables <- dbListTables(ifndb)[stringr::str_detect(dbListTables(ifndb), 'ifn4') &
                                     !stringr::str_detect(dbListTables(ifndb), '_raw') &
                                     !stringr::str_detect(dbListTables(ifndb), '_clima') &
                                     !stringr::str_detect(dbListTables(ifndb), '_sig')]

for (table in ifn4_tables) {
  tbl(ifndb, table) %>%
    collect() %>%
    filter(idparcela %in% ifn4_plots) %>%
    # selecting the variables
    select(one_of(ifn_vars_to_maintain)) %>%
    copy_to(
      dest = ifndb, df = ., name = table,
      overwrite = TRUE, temporary = FALSE
    )
}


# tables from ifn2 (not raw ones)
ifn2_tables <- dbListTables(ifndb)[stringr::str_detect(dbListTables(ifndb), 'ifn2') &
                                     !stringr::str_detect(dbListTables(ifndb), '_raw') &
                                     !stringr::str_detect(dbListTables(ifndb), '_clima') &
                                     !stringr::str_detect(dbListTables(ifndb), '_sig')]

for (table in ifn2_tables) {
  tbl(ifndb, table) %>%
    collect() %>%
    # selecting the variables
    dplyr::select(one_of(ifn_vars_to_maintain)) %>%
    # copying to db
    copy_to(
      dest = ifndb, df = ., name = table,
      overwrite = TRUE, temporary = FALSE
    )
}
################################################################################
# close pool
poolClose(ifndb)
