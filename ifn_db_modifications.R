## IFN_db modifications ####

# libraries
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(sp)
library(rgdal)

# db connections
oracle_ifn <- dbPool(
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

## parcelaifn2_sig
# access ifn2_sig table
# select utm coordinates variables
# transform to coords object
# sPtransform them to ETRS89
# create the latlong variables in WGS84
# update the database

tbl(oracle_ifn, 'parcelaifn2_sig') %>%
  select(utm_x, utm_y) %>%
  collect() -> coords_utm_ED50_ifn2

coordinates(coords_utm_ED50_ifn2) <- ~ utm_x+utm_y
proj4string(coords_utm_ED50_ifn2) <- CRS('+init=epsg:23031')

coords_utm_ETRS89_ifn2 <- spTransform(
  coords_utm_ED50_ifn2,
  CRS('+init=epsg:25831')
)

coords_latlong_WGS84_ifn2 <- spTransform(
  coords_utm_ED50_ifn2,
  CRS("+proj=longlat +datum=WGS84")
)

tbl(oracle_ifn, 'parcelaifn2_sig') %>%
  collect() %>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn2@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn2@coords[,2]),
    longitude = as.numeric(coords_latlong_WGS84_ifn2@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84_ifn2@coords[,2])
  ) %>%
  copy_to(
    dest = oracle_ifn, df = ., name = 'parcelaifn2_sig_etrs89',
    overwrite = TRUE, temporary = FALSE
  )

## parcelaifn3_sig
# access ifn3_sig table
# select utm coordinates variables
# transform to coords object
# sPtransform them to ETRS89
# create the latlong variables in WGS84
# update the database

tbl(oracle_ifn, 'parcelaifn3_sig') %>%
  select(utm_x, utm_y) %>%
  collect() -> coords_utm_ED50_ifn3

coordinates(coords_utm_ED50_ifn3) <- ~ utm_x+utm_y
proj4string(coords_utm_ED50_ifn3) <- CRS('+init=epsg:23031')

coords_utm_ETRS89_ifn3 <- spTransform(
  coords_utm_ED50_ifn3,
  CRS('+init=epsg:25831')
)

coords_latlong_WGS84_ifn3 <- spTransform(
  coords_utm_ED50_ifn3,
  CRS("+proj=longlat +datum=WGS84")
)

tbl(oracle_ifn, 'parcelaifn3_sig') %>%
  collect() %>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn3@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn3@coords[,2]),
    longitude = as.numeric(coords_latlong_WGS84_ifn3@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84_ifn3@coords[,2])
  ) %>%
  copy_to(
    dest = oracle_ifn, df = ., name = 'parcelaifn3_sig_etrs89',
    overwrite = TRUE, temporary = FALSE
  )

# name of general result table must be changed from r_ifnX to r_parcela_ifnX
tbl(oracle_ifn, 'r_ifn2') %>% 
  copy_to(
    dest = oracle_ifn, df = ., name = 'r_parcela_ifn2',
    overwrite = TRUE, temporary = FALSE
  )

tbl(oracle_ifn, 'r_ifn3') %>% 
  copy_to(
    dest = oracle_ifn, df = ., name = 'r_parcela_ifn3',
    overwrite = TRUE, temporary = FALSE
  )

# Nombres vegueries ####

# Las veguerias en IFN2 e IFN3 tienen los nombres antiguos y solo 7 veguerias,
# mientras que los mapas administrativos del cartografic tienen los nuevos con
# 8 veguerias. Por tanto hay que modificar las tablas que contengan veguerias
# con los nombres nuevos.

