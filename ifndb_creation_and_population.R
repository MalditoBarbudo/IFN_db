## Script for creating and populating the ifndb

# libraries ####
library(tidyverse)
library(dbplyr)
library(RPostgreSQL)
library(pool)
library(sp)
library(rgdal)
library(tidyIFN)

# db connections ####
origin_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'oracle_ifn'
)

final_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifndb'
)

# Creating the tables ####

## SIG tables
## ifn[x]_sig

# Fix coordinates mismatch between IFN2,3 and IFN4
# IFN2 and 3 comes in ED50 datum (+init=23031), whereas IFN4 comes in more
# modern ETRS89 datum (+init=25831). Here we will uniformize the utm coords
# datum in all IFNs, to the more modern ETRS89 datum, so we need to transform
# coordinates in sig tables for IFN2 and IFN3

## ifn2_sig
# access ifn2_sig table
# select utm coordinates variables
# transform to coords object
# sPtransform them to ETRS89
# create the latlong variables in WGS84
# update the database
tbl(origin_db, 'parcelaifn2_sig') %>%
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

tbl(origin_db, 'parcelaifn2_sig') %>%
  collect() %>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn2@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn2@coords[,2]),
    longitude = as.numeric(coords_latlong_WGS84_ifn2@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84_ifn2@coords[,2])
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_sig',
    overwrite = TRUE, temporary = FALSE
  )

## ifn3_sig
# access ifn3_sig table
# select utm coordinates variables
# transform to coords object
# sPtransform them to ETRS89
# create the latlong variables in WGS84
# update the database

tbl(origin_db, 'parcelaifn3_sig') %>%
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

tbl(origin_db, 'parcelaifn3_sig') %>%
  collect() %>%
  mutate(
    utm_x = as.numeric(coords_utm_ETRS89_ifn3@coords[,1]),
    utm_y = as.numeric(coords_utm_ETRS89_ifn3@coords[,2]),
    longitude = as.numeric(coords_latlong_WGS84_ifn3@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84_ifn3@coords[,2])
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_sig',
    overwrite = TRUE, temporary = FALSE
  )

## CLIMA tables
## ifn[x]_clima

## ifn2_sig
tbl(origin_db, 'parcelaifn2_clima') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_clima',
    overwrite = TRUE, temporary = FALSE
  )
## ifn3_sig
tbl(origin_db, 'parcelaifn3_clima') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_clima',
    overwrite = TRUE, temporary = FALSE
  )

## RESULTS tables
## ifn[x]_[parcela|especie|especiesimp|genere|conifplanif|cadesccon]_res

# ifn2_parcela_res
tbl(origin_db, 'r_ifn2') %>%
  rename(
    cadesccon_dom_percdens = caducesclerconifdens,
    cadesccon_dom_percdens_val = percdenscaducesclerconif,
    cadesccon_dom_percab = caducesclerconifab,
    cadesccon_dom_percab_val = percabcaducesclerconif,
    planifconif_dom_percdens = planifconifdens,
    planifconif_dom_percdens_val = percdensplanifconif,
    planifconif_dom_percab = planifconifab,
    planifconif_dom_percab_val = percabplanifconif,
    genere_dom_percdens = generedens,
    genere_dom_percdens_val = percdensgenere,
    genere_dom_percab = genereab,
    genere_dom_percab_val = percabgenere,
    especie_dom_percdens = especiedens,
    especie_dom_percdens_val = percdensespecie,
    especie_dom_percab = especieab,
    especie_dom_percab_val = percabespecie,
    especiesimp_dom_percdens = especiesimpledens,
    especiesimp_dom_percdens_val = percdensespeciesimple,
    especiesimp_dom_percab = especiesimpleab,
    especiesimp_dom_percab_val = percabespeciesimple,
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_parcela_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_parcela_cd_res
tbl(origin_db, 'r_cd_ifn2') %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_parcela_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_parcela_res
tbl(origin_db, 'r_ifn3') %>%
  rename(
    cadesccon_dom_percdens = caducesclerconifdens,
    cadesccon_dom_percdens_val = percdenscaducesclerconif,
    cadesccon_dom_percab = caducesclerconifab,
    cadesccon_dom_percab_val = percabcaducesclerconif,
    planifconif_dom_percdens = planifconifdens,
    planifconif_dom_percdens_val = percdensplanifconif,
    planifconif_dom_percab = planifconifab,
    planifconif_dom_percab_val = percabplanifconif,
    genere_dom_percdens = generedens,
    genere_dom_percdens_val = percdensgenere,
    genere_dom_percab = genereab,
    genere_dom_percab_val = percabgenere,
    especie_dom_percdens = especiedens,
    especie_dom_percdens_val = percdensespecie,
    especie_dom_percab = especieab,
    especie_dom_percab_val = percabespecie,
    especiesimp_dom_percdens = especiesimpledens,
    especiesimp_dom_percdens_val = percdensespeciesimple,
    especiesimp_dom_percab = especiesimpleab,
    especiesimp_dom_percab_val = percabespeciesimple,
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_parcela_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_parcela_cd_res
tbl(origin_db, 'r_cd_ifn3') %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_parcela_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_especie_res
tbl(origin_db, 'r_especie_ifn2') %>%
  rename(
    idespecie = idespecieifn2
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_especie_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_especie_cd_res
tbl(origin_db, 'r_especiecd_ifn2') %>%
  rename(
    idespecie = idespecieifn2
  ) %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_especie_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_especie_res
tbl(origin_db, 'r_especie_ifn3') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_especie_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_especie_cd_res
tbl(origin_db, 'r_especiecd_ifn3') %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_especie_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_especiesimp_res
tbl(origin_db, 'r_espsimple_ifn2') %>%
  rename(
    idespeciesimp = idespeciesimple
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_especiesimp_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_especiesimp_cd_res
tbl(origin_db, 'r_espsimplecd_ifn2') %>%
  rename(
    idespeciesimp = idespeciesimple
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_especiesimp_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_especiesimp_res
tbl(origin_db, 'r_espsimple_ifn3') %>%
  rename(
    idespeciesimp = idespeciesimple
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_especiesimp_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_especiesimp_cd_res
tbl(origin_db, 'r_espsimplecd_ifn3') %>%
  rename(
    idespeciesimp = idespeciesimple
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_especiesimp_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_genere_res
tbl(origin_db, 'r_genere_ifn2') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_genere_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_genere_cd_res
tbl(origin_db, 'r_generecd_ifn2') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_genere_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_genere_res
tbl(origin_db, 'r_genere_ifn3') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_genere_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_genere_cd_res
tbl(origin_db, 'r_generecd_ifn3') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_genere_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_planifconif_res
tbl(origin_db, 'r_plancon_ifn2') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_planifconif_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_planifconif_cd_res
tbl(origin_db, 'r_planconcd_ifn2') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn2_planifconif_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_planifconif_res
tbl(origin_db, 'r_plancon_ifn3') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_planifconif_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_planifconif_cd_res
tbl(origin_db, 'r_planconcd_ifn3') %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn3_planifconif_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_cadesccon_res
tbl(origin_db, 'r_cadesclcon_ifn2') %>%
  rename(
    idcadesccon = idcaducesclerconif
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_cadesccon_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn2_cadesccon_cd_res
tbl(origin_db, 'r_cadesclconcd_ifn2') %>%
  rename(
    idcadesccon = idcaducesclerconif
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn2_cadesccon_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_cadesccon_res
tbl(origin_db, 'r_cadesclcon_ifn3') %>%
  rename(
    idcadesccon = idcaducesclerconif
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_cadesccon_res',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3_cadesccon_cd_res
tbl(origin_db, 'r_cadesclconcd_ifn3') %>%
  rename(
    idcadesccon = idcaducesclerconif
  ) %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn3_cadesccon_cd_res',
    overwrite = TRUE, temporary = FALSE
  )

## Closing pools ####
poolClose(origin_db)
poolClose(final_db)
