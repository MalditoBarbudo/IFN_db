## IFN_db modifications ####

# libraries
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)

# db connections
oracle_ifn <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn'
)

## Modifications

# Coordinates ####

# Fix coordinates mismatch between IFN2,3 and IFN4
# IFN2 and 3 comes in ED50 datum (+init=23031), whereas IFN4 comes in more
# modern ETRS89 datum (+init=25831). Here we will uniformize the utm coords
# datum in all IFNs, to the more modern ETRS89 datum, so we need to transform
# coordinates in sig tables for IFN2 and IFN3

# parcelaifn2_sig
tbl(oracle_ifn, 'parcelaifn2_sig')
