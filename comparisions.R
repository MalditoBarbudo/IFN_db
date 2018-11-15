# Script for create the comparision tables

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

# sigs
ifn2_sig <- tbl(ifndb, 'ifn2_sig') %>% collect()
ifn3_sig <- tbl(ifndb, 'ifn3_sig') %>% collect()
ifn4_sig <- tbl(ifndb, 'ifn4_sig') %>% collect()

plots_2_3 <- inner_join(
  ifn2_sig %>% select(idparcela),
  ifn3_sig %>% select(idparcela, idclasse)
)

plots_3_4 <- inner_join(
  ifn3_sig %>% select(idparcela, idclasse),
  ifn4_sig %>% select(idparcela, idclasse)
)


plots_2_3_4 <- purrr::reduce(
  list(
    ifn2_sig %>% select(idparcela),
    ifn3_sig %>% select(idparcela, idclasse),
    ifn4_sig %>% select(idparcela, idclasse)
  ),
  inner_join
)

# test plots
ifn2_parcela_res_inc <- tbl(ifndb, 'ifn2_parcela_res') %>%
  collect() %>%
  left_join(plots_2_3) %>%
  arrange(idparcela)

ifn3_parcela_res_inc <- tbl(ifndb, 'ifn3_parcela_res') %>%
  collect() %>%
  left_join(plots_2_3) %>%
  arrange(idparcela)
