# New database version script
# 
# This is needed because the db must be improved to better implementation of all
# the data.

#### Libraries and data access ####
library(tidyverse)
library(dbplyr)
library(RPostgres)
library(pool)
library(magrittr)
library(sp)
# library(rgdal)
library(stringr)
library(glue)
# library(tidyIFN)

# db connections
oracle_db <- dbPool(
  RPostgres::Postgres(),
  user = Sys.getenv("DB_ADMIN"),
  password = Sys.getenv("DB_PASS"),
  dbname = 'oracle_ifn',
  host = Sys.getenv("DB_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT"))
)

access4_db <- dbPool(
  RPostgres::Postgres(),
  user = Sys.getenv("DB_ADMIN"),
  password = Sys.getenv("DB_PASS"),
  dbname = 'ifn4_access',
  host = Sys.getenv("DB_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT"))
)

brand_new_nfi_db <- pool::dbPool(
  RPostgres::Postgres(),
  user = Sys.getenv("DB_ADMIN"), host = Sys.getenv("DB_HOST"),
  password = Sys.getenv("DB_PASS"),
  dbname = 'tururu', port = as.numeric(Sys.getenv("DB_PORT"))
)

#### plots data ####
source('01_nfi234_plots_data.R')

#### topo clim own species info ####
source('02_topo_climatic_ownership_species_info.R')

#### PLOT statatic and dynamic tables ####
source('03_PLOT_static_and_dynamic_tables.R')

#### results tables ####
source('04_results_tables.R')

#### comparision tables ####
source('05_comparision_tables.R')

#### tree, shrub and regeneration tables ####
source('06_tree_shrub_regeneration_tables.R')

#### thesauruses ####
source('07_thesauruses.R')

#### building the database ####
source('99_db_building.R')

#### CLOSE POOLS ####
poolClose(oracle_db)
poolClose(access4_db)
poolClose(brand_new_nfi_db)
