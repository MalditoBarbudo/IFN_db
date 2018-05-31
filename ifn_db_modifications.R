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
