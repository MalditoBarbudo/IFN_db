## Script for IFN4 data preparation

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
  dbname = 'ifn4_access'
)

final_db <- dbPool(
  RPostgreSQL::PostgreSQL(),
  user = 'ifn',
  password = rstudioapi::askForPassword('Password for ifn'),
  dbname = 'ifndb'
)

# Creating the tables ####
## SIG tables ####
## ifn[x]_sig

# Fix coordinates mismatch between IFN2,3 and IFN4
# IFN2 and 3 comes in ED50 datum (+init=23031), whereas IFN4 comes in more
# modern ETRS89 datum (+init=25831). Here we will uniformize the utm coords
# datum in all IFNs, to the more modern ETRS89 datum, so we need to transform
# coordinates in sig tables for IFN2 and IFN3

## ifn4_sig
# access ifn4 tables with sig info
# select utm coordinates variables
# transform to coords object
# sPtransform them to ETRS89
# create the latlong variables in WGS84
# update the database

ifn3_names <- tbl(final_db, 'ifn3_sig') %>% collect() %>% names()

joined_ifn4_sig_tables <- tbl(origin_db, 'ParcelaIFN4_OLAP') %>%
  collect() %>% 
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(utm_x = coordx, utm_y = coordy) %>%
  full_join(
    y = {
      tbl(origin_db, 'Parcela_MDT') %>%
        collect() %>%
        {magrittr::set_names(., tolower(names(.)))} %>%
        tidyr::separate(
          idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
        ) %>%
        mutate(idclasse = stringr::str_remove(idclasse, '_'))
    },
    by = c('idparcela', 'idclasse')
  )

ifn4_names <- names(joined_ifn4_sig_tables)
  

coords_to_transform <- joined_ifn4_sig_tables %>% 
  select(utm_x, utm_y)


coordinates(coords_to_transform) <- ~ utm_x+utm_y
proj4string(coords_to_transform) <- CRS('+init=epsg:25831')

coords_latlong_WGS84_ifn4 <- spTransform(
  coords_to_transform,
  CRS("+proj=longlat +datum=WGS84")
)

joined_ifn4_sig_tables %>%
  mutate(
    longitude = as.numeric(coords_latlong_WGS84_ifn4@coords[,1]),
    latitude = as.numeric(coords_latlong_WGS84_ifn4@coords[,2])
  ) %>%
  left_join({
    tbl(final_db, 'ifn3_sig') %>%
      select(
        idparcela, delegacio, vegueria, nomein, enpes, proteccio, nomppp,
        nomxarxa2000
      ) %>%
      
      collect()
  }) %>%
  select(
    idparcela, idclasse, utm_x, utm_y, municipi, comarca, delegacio, provincia,
    vegueria, nomein, enpes, proteccio, nomppp, nomxarxa2000, pendentpercent,
    pendentgraus, altitud, orientacio, orientacio_8, orientacio_4, longitude,
    latitude
  ) %>%
  rename(
    pendentpercentatge = pendentpercent
  ) %>% 
  copy_to(
    dest = final_db, df = ., name = 'ifn4_sig',
    overwrite = TRUE, temporary = FALSE
  )

## CLIMA tables ####
tbl(final_db, 'ifn3_clima') %>% collect %>% names -> names_clima_ifn3
clima_ifn4_names <- c(
  names_clima_ifn3[1:2], names_clima_ifn3[4:15], names_clima_ifn3[3],
  names_clima_ifn3[17:28], names_clima_ifn3[16], names_clima_ifn3[30:41],
  names_clima_ifn3[29], names_clima_ifn3[43:54], names_clima_ifn3[42],
  names_clima_ifn3[56:67], names_clima_ifn3[55], names_clima_ifn3[68:79],
  'etp_anual', names_clima_ifn3[80:91], 'etr_anual'
) %>%
  stringr::str_replace('_p_', '_')
tbl(origin_db, 'Parcela_Clima') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  tidyr::separate(
    idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
  ) %>%
  mutate(idclasse = stringr::str_remove(idclasse, '_')) %>%
  select(-starts_with('coor'), -id_grafic) %>%
  left_join({
    tbl(origin_db, 'Parcela_ETP_ETR') %>%
      collect() %>%
      {magrittr::set_names(., tolower(names(.)))} %>%
      tidyr::separate(
        idparcela, c('idparcela', 'idclasse'), sep = '[_]', extra = 'merge'
      ) %>%
      mutate(idclasse = stringr::str_remove(idclasse, '_')) %>%
      select(-starts_with('coor'), -id_grafic)
  }) %>%
  {magrittr::set_names(., clima_ifn4_names)} %>%
  copy_to(
    dest = final_db, df = ., name = 'ifn4_clima',
    overwrite = TRUE, temporary = FALSE
  )

## genere tables ####
# we build the genera tables from the species tables, but for that we need to
# know the genus value from the species table
tbl(origin_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, Genere) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, Genere, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, Genere) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, Genere, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idgenere = genere,
    percdens = percdensitat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_genere_res', overwrite = TRUE, temporary = FALSE 
  )

tbl(origin_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, Genere) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, Genere, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, Genere) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, Genere,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idgenere = genere
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_genere_cd_res', overwrite = TRUE, temporary = FALSE 
  )

## deciduous/esclerophyll/conifer tables ####
# we build the dec tables from the species tables, but for that we need to
# know the genus value from the species table
tbl(origin_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, CaducEsclerConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, CaducEsclerConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, CaducEsclerConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, CaducEsclerConif, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idcadesccon = caducesclerconif,
    percdens = percdensitat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_cadesccon_res', overwrite = TRUE, temporary = FALSE 
  )

tbl(origin_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, CaducEsclerConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, CaducEsclerConif,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idcadesccon = caducesclerconif
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_cadesccon_cd_res', overwrite = TRUE, temporary = FALSE 
  )

## broad_leaf/conifer tables ####
# we build the dec tables from the species tables, but for that we need to
# know the genus value from the species table
tbl(origin_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, PlanifConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, PlanifConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, PlanifConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, PlanifConif, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idplanifconif = planifconif,
    percdens = percdensitat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_planifconif_res', overwrite = TRUE, temporary = FALSE 
  )

tbl(origin_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, PlanifConif) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, PlanifConif, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, PlanifConif) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, PlanifConif,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idplanifconif = planifconif
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_planifconif_cd_res', overwrite = TRUE, temporary = FALSE 
  )

## simplified species tables ####
# we build the dec tables from the species tables, but for that we need to
# know the genus value from the species table
tbl(origin_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, EspecieSimplificat) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, EspecieSimplificat, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie, -OrdreDens, -OrdreAB, -DBH, -DBHMorts
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, EspecieSimplificat) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  # create the variables needed, DBH and orders.
  # DBH = sqrt((AB/Densitat)/pi)*2*100
  # Ordre* = order in the specified var, but reversed (desc)
  mutate(
    DBH = if_else(
      Densitat < 1, 0, sqrt((AB/Densitat)/pi) * 200
    ),
    DBHMorts = if_else(
      DensitatMorts < 1, 0, sqrt((ABMorts/DensitatMorts)/pi) * 200
    ),
    OrdreDens = min_rank(desc(Densitat)),
    OrdreAB = min_rank(desc(AB))
  ) %>%
  select(
    IdParcela, IdClasseIFN4, EspecieSimplificat, OrdreDens, OrdreAB, PercDensitat,
    PercAB, Densitat, DensitatMorts, AB, ABMorts, DBH, DBHMorts,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespeciesimp = especiesimplificat,
    percdens = percdensitat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_especiesimp_res', overwrite = TRUE, temporary = FALSE 
  )

tbl(origin_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  # join the tesaurus with the specie-genus relation
  left_join({
    tbl(origin_db, 'TesaureEspecieIFN4') %>%
      select(Especie, EspecieSimplificat) %>%
      filter(row_number() %in% c(
        1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
      ))
  }) %>%
  select(
    # ordenar
    IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat, everything(),
    # quitar las que no necesitamos o tenemos que recalcular
    -Especie
  ) %>%
  # group by plot, class and genus
  group_by(IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat) %>%
  # summarise all vars, by sum them
  summarise_all(funs(sum(., na.rm = TRUE))) %>%
  mutate(IdCD = as.character(IdCD)) %>%
  select(
    IdParcela, IdClasseIFN4, IdCD, EspecieSimplificat,
    everything()
  ) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespeciesimp = especiesimplificat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_especiesimp_cd_res', overwrite = TRUE, temporary = FALSE 
  )

## species tables ####
# species tables are done but, we need to change names and things like that
tbl(origin_db,'ResultatEspecie_IFN4_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespecie = especie,
    percdens = percdensitat
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_especie_res', overwrite = TRUE, temporary = FALSE
  )

tbl(origin_db,'ResultatEspecieCD_IFN4_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  rename(
    idclasse = idclasseifn4,
    idespecie = especie
  ) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_especie_cd_res', overwrite = TRUE, temporary = FALSE
  )

## plot tables ####
# plot tables are done, but we need to change names and things like that
tbl(origin_db, 'Resultat_IFN4_CREAF_OLAP') %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  select(
    idparcela, idclasse, caducesclerconifdens, percdensitatcaducesclerconif,
    caducesclerconifab, percabcaducesclerconif, planifconifdens,
    percabplanifconif, planifconifab, percdensitatplanifconif,
    generedens, percdensitatgenere, genereab, percabgenere,
    # especiesimpdens, percdensitatespeciesimp, especiesimpab, percabespeciesimp,
    especiedens, percdensitatespecie, especieab, percabespecie,
    everything(),
    -tipusboscdens, -tipusboscab
  ) %>%
  rename(
    cadesccon_dom_percdens = caducesclerconifdens,
    cadesccon_dom_percdens_val = percdensitatcaducesclerconif,
    cadesccon_dom_percab = caducesclerconifab,
    cadesccon_dom_percab_val = percabcaducesclerconif,
    ####
    # planifconif en la tabla original Resultat_IFN4_CREAF_OLAP tiene los valores
    # de percdens y percab cambiados, lo arreglo aqui
    planifconif_dom_percdens = planifconifdens,
    planifconif_dom_percdens_val = percabplanifconif,
    planifconif_dom_percab = planifconifab,
    planifconif_dom_percab_val = percdensitatplanifconif,
    ####
    genere_dom_percdens = generedens,
    genere_dom_percdens_val = percdensitatgenere,
    genere_dom_percab = genereab,
    genere_dom_percab_val = percabgenere,
    especie_dom_percdens = especiedens,
    especie_dom_percdens_val = percdensitatespecie,
    especie_dom_percab = especieab,
    especie_dom_percab_val = percabespecie
  ) %>%
  mutate(
    especiesimp_dom_percdens = especie_dom_percdens,
    # especiesimp_dom_percdens_val = percdensespecie,
    especiesimp_dom_percab = especie_dom_percab
    # especiesimp_dom_percab_val = percabespecie
  ) %>%
  bind_cols({
    tbl(origin_db, 'Resultat_IFN4_CREAF_OLAP') %>%
      collect() %>%
      select(EspecieDens) %>%
      # join the tesaurus with the specie-genus relation
      left_join({
        tbl(origin_db, 'TesaureEspecieIFN4') %>%
          select(Especie, EspecieSimplificat) %>%
          filter(row_number() %in% c(
            1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
          )) %>% 
          collect()
      }, by = c('EspecieDens' = 'Especie')) %>%
      bind_cols({
        tbl(origin_db, 'Resultat_IFN4_CREAF_OLAP') %>%
          collect() %>%
          select(EspecieAB) %>%
          # join the tesaurus with the specie-genus relation
          left_join({
            tbl(origin_db, 'TesaureEspecieIFN4') %>%
              select(Especie, EspecieSimplificat) %>%
              filter(row_number() %in% c(
                1:156, 158:327, 330:337, 339:348, 350:352, 354:356, 358:359
              )) %>%
              collect()
          }, by = c('EspecieAB' = 'Especie'))
      }) %>% 
      rename(especiesimp_dom_percdens_val = EspecieDens,
             especiesimp_dom_percab_val = EspecieAB) %>%
      select(especiesimp_dom_percdens_val, especiesimp_dom_percab_val)
  }) %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_parcela_res', overwrite = TRUE, temporary = FALSE
  )

tbl(origin_db, 'ResultatCD_IFN4_CREAF_OLAP') %>%
  mutate(IdCD = as.character(IdCD)) %>%
  collect() %>%
  {magrittr::set_names(., tolower(names(.)))} %>%
  copy_to(
    final_db, df = .,
    name = 'ifn4_parcela_cd_res', overwrite = TRUE, temporary = FALSE
  )


#### pool close ####
poolClose(origin_db)
poolClose(final_db)







