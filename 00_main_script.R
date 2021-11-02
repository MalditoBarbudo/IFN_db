# New database version script
# 
# This is needed because the db must be improved to better implementation of all
# the data.

#### Libraries and data access ####
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(magrittr)
library(sp)
library(rgdal)
library(stringr)
library(glue)
# library(tidyIFN)

# db connections
oracle_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'guest',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn',
  host = 'laboratoriforestal.creaf.cat',
  port = 5432
)

access4_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifn4_access',
  host = 'laboratoriforestal.creaf.cat',
  port = 5432
)

brand_new_nfi_db <- pool::dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn', host = 'laboratoriforestal.creaf.cat',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'tururu', port = 5432
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
