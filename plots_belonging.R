# Script for fixing the plots administrative divisions belonging

# libraries
library(dplyr)
library(sp)
library(rgdal)
library(pool)

# dbs
ifndb <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifndb'
)

# shapefiles load
polygons_municipis <- rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpm1_20180101_0',
                                     GDAL1_integer64_policy = FALSE) %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_comarques <- rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpc1_20180101_0',
                                     GDAL1_integer64_policy = FALSE) %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_vegueries <- rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpv1_20180101_0',
                                     GDAL1_integer64_policy = FALSE) %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

polygons_provincies <- rgdal::readOGR('data_raw/shapefiles', 'bm5mv20sh0tpp1_20180101_0',
                                      GDAL1_integer64_policy = FALSE) %>%
  # rmapshaper::ms_simplify(0.01) %>%
  sp::spTransform(sp::CRS("+proj=longlat +datum=WGS84"))

# ifn2 sig table
ifn2_sig <- tbl(ifndb, 'ifn2_sig') %>%
  collect()

ifn2_coordinates <- ifn2_sig %>%
  select(longitude, latitude) %>% 
  sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84"))

municipis_var <- sp::over(ifn2_coordinates, polygons_municipis) %>%
  select(NOMMUNI) %>%
  rename(municipi = NOMMUNI)

comarcas_var <- sp::over(ifn2_coordinates, polygons_comarques) %>%
  select(NOMCOMAR) %>%
  rename(comarca = NOMCOMAR)

vegueries_var <- sp::over(ifn2_coordinates, polygons_vegueries) %>%
  select(NOMVEGUE) %>%
  rename(vegueria = NOMVEGUE)

provincies_var <- sp::over(ifn2_coordinates, polygons_provincies) %>%
  select(NOMPROV) %>%
  rename(provincia = NOMPROV)

ifn2_sig[['municipi']] <- municipis_var[['municipi']]
ifn2_sig[['comarca']] <- comarcas_var[['comarca']]
ifn2_sig[['vegueria']] <- vegueries_var[['vegueria']]
ifn2_sig[['provincia']] <- provincies_var[['provincia']]

ifn2_sig %>%
  copy_to(
    dest = ifndb, df = ., name = 'ifn2_sig',
    overwrite = TRUE, temporary = FALSE
  )

# ifn3 sig table
ifn3_sig <- tbl(ifndb, 'ifn3_sig') %>%
  collect()

ifn3_coordinates <- ifn3_sig %>%
  select(longitude, latitude) %>% 
  sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84"))

municipis_var <- sp::over(ifn3_coordinates, polygons_municipis) %>%
  select(NOMMUNI) %>%
  rename(municipi = NOMMUNI)

comarcas_var <- sp::over(ifn3_coordinates, polygons_comarques) %>%
  select(NOMCOMAR) %>%
  rename(comarca = NOMCOMAR)

vegueries_var <- sp::over(ifn3_coordinates, polygons_vegueries) %>%
  select(NOMVEGUE) %>%
  rename(vegueria = NOMVEGUE)

provincies_var <- sp::over(ifn3_coordinates, polygons_provincies) %>%
  select(NOMPROV) %>%
  rename(provincia = NOMPROV)

ifn3_sig[['municipi']] <- municipis_var[['municipi']]
ifn3_sig[['comarca']] <- comarcas_var[['comarca']]
ifn3_sig[['vegueria']] <- vegueries_var[['vegueria']]
ifn3_sig[['provincia']] <- provincies_var[['provincia']]

ifn3_sig %>%
  copy_to(
    dest = ifndb, df = ., name = 'ifn3_sig',
    overwrite = TRUE, temporary = FALSE
  )

# ifn4 sig table
ifn4_sig <- tbl(ifndb, 'ifn4_sig') %>%
  collect()

ifn4_coordinates <- ifn4_sig %>%
  select(longitude, latitude) %>% 
  sp::SpatialPoints(sp::CRS("+proj=longlat +datum=WGS84"))

municipis_var <- sp::over(ifn4_coordinates, polygons_municipis) %>%
  select(NOMMUNI) %>%
  rename(municipi = NOMMUNI)

comarcas_var <- sp::over(ifn4_coordinates, polygons_comarques) %>%
  select(NOMCOMAR) %>%
  rename(comarca = NOMCOMAR)

vegueries_var <- sp::over(ifn4_coordinates, polygons_vegueries) %>%
  select(NOMVEGUE) %>%
  rename(vegueria = NOMVEGUE)

provincies_var <- sp::over(ifn4_coordinates, polygons_provincies) %>%
  select(NOMPROV) %>%
  rename(provincia = NOMPROV)

ifn4_sig[['municipi']] <- municipis_var[['municipi']]
ifn4_sig[['comarca']] <- comarcas_var[['comarca']]
ifn4_sig[['vegueria']] <- vegueries_var[['vegueria']]
ifn4_sig[['provincia']] <- provincies_var[['provincia']]

ifn4_sig %>%
  copy_to(
    dest = ifndb, df = ., name = 'ifn4_sig',
    overwrite = TRUE, temporary = FALSE
  )

################################################################################
# closing pools
poolClose(ifndb)
