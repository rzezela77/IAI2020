###############################################################
#### global.R
##############################################################


# 1.0 Loading libraries ---------------------------------------------------

library(tidyverse) # for data manipulation

# loading libraries
library(haven) # for import stata data
library(labelled) # for label factor

library(DT) # for datatable
library(reactable) # for tabulate data

library(apexcharter) # for data visualization
library(highcharter)

# load libraries - creating map
library(leaflet) # create maps
library(rgdal) # for read shapefile

library(htmltools) # for html






# 2.0 Loading data --------------------------------------------------------

# loading data section A
ibsa_sec_a_dat <- read_dta("00_data/sec_a.dta")
# View(ibsa_sec_a_dat)


# Carregar os dados sobre Amostras
# sample data
library(readxl)
amostra_harmonizada_tbl <- read_excel("00_data/amostra_harmonizada_v3.xls")

# View(amostra_harmonizada_tbl)

# amostra_harmonizada_tbl %>% glimpse()


# 2.1 Examine the data ----------------------------------------------------

# convert data type
ibsa_sec_a_dat$a10 <- as.Date(ibsa_sec_a_dat$a10, "%d/%m/%Y")

# ibsa_sec_a_dat %>% glimpse()


# Remover registos cujo data de entrevista eh igual a NULL
ibsa_sec_a_dat <- ibsa_sec_a_dat[ !is.na(ibsa_sec_a_dat$a10), ]


# remover registos cujo data de entrevista seja inferior ao ano 2021
ibsa_sec_a_dat <- ibsa_sec_a_dat[lubridate::year(ibsa_sec_a_dat$a10) >= 2021 & ibsa_sec_a_dat$a10 <= Sys.Date(), ]



# 3.0 Data Manipulation ---------------------------------------------------

# Joining tables

amostra_modificada_tbl <- amostra_harmonizada_tbl[ , 2:12]

amostra_modificada_tbl <- amostra_modificada_tbl %>% unique()

amostra_modificada_tbl$provincia <- factor(amostra_modificada_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))


ibsa_sec_a_joined_tbl <-
    ibsa_sec_a_dat %>%
    left_join(amostra_modificada_tbl,
              by = c("upa" = "ibsa_id"))

# ibsa_sec_a_joined_tbl %>% glimpse()


# criar tabela
daily_interview_tbl <-
    ibsa_sec_a_joined_tbl %>%
    # filter(A10 >= as.Date('2021-01-01')) %>% 
    group_by(created_at = a10, provincia, prov) %>%
    summarise(no_entrevistas = n()) %>%
    ungroup()

# contar AE's por provincia
contar_upa_provincia_tbl <-
    ibsa_sec_a_joined_tbl %>%
    group_by(provincia, distrito) %>%
    summarise(num_UPA = n_distinct(upa)) %>%
    ungroup()

# contar_upa_provincia_tbl 

# # Tipo de exploracao
# tipo_exploracao_tbl <-
#     ibsa_sec_a_dat %>%
#     count(prov, a06) 

# or
# Tipo de exploracao
tipo_exploracao_tbl <-
    ibsa_sec_a_joined_tbl %>%
    count(prov, a06) 



# No. total de entrevistas por provincia
total_entrevistas_provincia_tbl <- 
    daily_interview_tbl %>% 
    group_by(provincia) %>% 
    summarise(no_total_entrevistas = sum(no_entrevistas)) %>% 
    ungroup()


# performance --------------------------------------------------

# AE entrevistadas
upa_provincia_entrevistadas_tbl <-
    contar_upa_provincia_tbl %>%
    group_by(provincia) %>%
    summarise(no_AE_entrevistadas = sum(num_UPA)) %>%
    ungroup() 

upa_provincia_entrevistadas_tbl

# AE planificadas
AE_planificadas_tbl <-
    amostra_modificada_tbl %>%
    group_by(provincia) %>%
    summarise(no_AE_planificada = n()) %>% 
    ungroup()


# AE_planificadas_tbl


upa_entrevistas_performance_tbl <- cbind(upa_provincia_entrevistadas_tbl, AE_planificadas_tbl)

upa_entrevistas_performance_tbl <- upa_entrevistas_performance_tbl[ , -3]

upa_entrevistas_performance_tbl$percent <- round((upa_entrevistas_performance_tbl$no_AE_entrevistadas/upa_entrevistas_performance_tbl$no_AE_planificada)*100, 2)

upa_entrevistas_performance_tbl


# 4.0 Creating Map --------------------------------------------------------

# Pesos dos dados de Producao
pesos_producao_tbl <- read_dta(file = "00_data/weightv0.dta")

# pesos_producao_tbl %>% glimpse()


# Dados da Seccao H 
ibsa_sec_h_dat <- read_dta("00_data/sec_hv1.dta")

# ibsa_sec_h_dat %>% glimpse()

# Juntar as tabelas com base nos campos, "caseid", "prov", "dist", "upa", "af"
ibsa_sec_h_merged_tbl <- 
    merge(ibsa_sec_h_dat, pesos_producao_tbl, by = c("caseid", "prov", "dist", "upa", "af"))

# ibsa_sec_h_merged_tbl %>% glimpse()

# Agrupar dados de producao por tipo de producao e usando os respectivos pesos
# Por Provincia

# renomear o campo 6a para tipo de cultura
colnames(ibsa_sec_h_merged_tbl)[6] <- "tipo_cultura"

producao_sec_h_provincia_tbl <- 
    ibsa_sec_h_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), tipo_cultura) %>%
    
    # multiplicar a quantidade pelo peso
    summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
    
    ungroup()

# producao_sec_h_provincia_tbl %>% glimpse()

# # Por distrito
# producao_sec_h_distrito_tbl <- 
#     ibsa_sec_h_merged_tbl %>% 
#     group_by(PROVINCIA = to_factor(prov), prov_dist, tipo_cultura) %>% 
#     
#     # multiplicar o peso pela quantidade 
#     summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
#     
#     ungroup()
# 
# producao_sec_h_distrito_tbl %>% glimpse()


# 4.1 Adding Shapefiles ---------------------------------------------------

# Importing shapefiles
# adding shapefiles
moz_province <- readOGR("00_data/Provincias/Provincias.shp", layer = "Provincias")


# moz_district <- readOGR(dsn=path.expand("~/R/funwithR/data/ne_110m_land"), layer="ne_110m_land")

# moz_district <- readOGR("shiny_app/00_data/Distritos/Distritos.shp", layer = "Distritos")


# # examinar os dados 
# moz_district@data %>% glimpse()

# rename all levels
levels(producao_sec_h_provincia_tbl$PROVINCIA) <- c("NIASSA", "CABO_DELGADO", "NAMPULA", "ZAMBEZIA", "TETE", "MANICA", "SOFALA", "INHAMBANE", "GAZA", "MAPUTO")


# criar tabela por tipo de cultura ----------------------------------------
tipo_producao_sec_h_provincia_tbl <-
    producao_sec_h_provincia_tbl[producao_sec_h_provincia_tbl$tipo_cultura == "FEIJAO MANTEIGA",]


# Juncao entre shapefiles e dados de producao por tipo de cultura ----
tipo_producao_sec_h_provincia_merged_tbl <-
    merge(moz_province,
          tipo_producao_sec_h_provincia_tbl,
          by = "PROVINCIA",
          all.x = F)


# 4.2 Loanding district data production -----------------------------------

# district shapefile
moz_district <- suppressWarnings(readOGR("00_data/Distritos/Distritos.shp", layer = "Distritos"))

# criar uma nova variavel: prov_dist
moz_district$prov_dist <- NA
moz_district$prov_dist[which(moz_district$OBJECTID == 1)] <- 402
moz_district$prov_dist[which(moz_district$OBJECTID == 3)] <- 202
moz_district$prov_dist[which(moz_district$OBJECTID == 4)] <- 302
moz_district$prov_dist[which(moz_district$OBJECTID == 5)] <- 502
moz_district$prov_dist[which(moz_district$OBJECTID == 6)] <- 203
moz_district$prov_dist[which(moz_district$OBJECTID == 7)] <- 602
moz_district$prov_dist[which(moz_district$OBJECTID == 8)] <- 902
moz_district$prov_dist[which(moz_district$OBJECTID == 9)] <- 1002
moz_district$prov_dist[which(moz_district$OBJECTID == 10)] <- 702
moz_district$prov_dist[which(moz_district$OBJECTID == 11)] <- 503
moz_district$prov_dist[which(moz_district$OBJECTID == 12)] <- 703
moz_district$prov_dist[which(moz_district$OBJECTID == 13)] <- 504
moz_district$prov_dist[which(moz_district$OBJECTID == 14)] <- 704
moz_district$prov_dist[which(moz_district$OBJECTID == 15)] <- 705
moz_district$prov_dist[which(moz_district$OBJECTID == 16)] <- 706
moz_district$prov_dist[which(moz_district$OBJECTID == 17)] <- 903
moz_district$prov_dist[which(moz_district$OBJECTID == 18)] <- 904
moz_district$prov_dist[which(moz_district$OBJECTID == 19)] <- 505
moz_district$prov_dist[which(moz_district$OBJECTID == 20)] <- 905
moz_district$prov_dist[which(moz_district$OBJECTID == 21)] <- 403
moz_district$prov_dist[which(moz_district$OBJECTID == 22)] <- 204
moz_district$prov_dist[which(moz_district$OBJECTID == 23)] <- 506
moz_district$prov_dist[which(moz_district$OBJECTID == 24)] <- 906
moz_district$prov_dist[which(moz_district$OBJECTID == 25)] <- 913
moz_district$prov_dist[which(moz_district$OBJECTID == 26)] <- 701
moz_district$prov_dist[which(moz_district$OBJECTID == 27)] <- 1001
moz_district$prov_dist[which(moz_district$OBJECTID == 28)] <- 601
moz_district$prov_dist[which(moz_district$OBJECTID == 29)] <- 801
moz_district$prov_dist[which(moz_district$OBJECTID == 30)] <- 101
moz_district$prov_dist[which(moz_district$OBJECTID == 31)] <- 1101
moz_district$prov_dist[which(moz_district$OBJECTID == 32)] <- 301
moz_district$prov_dist[which(moz_district$OBJECTID == 33)] <- 201
moz_district$prov_dist[which(moz_district$OBJECTID == 34)] <- 401
moz_district$prov_dist[which(moz_district$OBJECTID == 35)] <- 501
moz_district$prov_dist[which(moz_district$OBJECTID == 36)] <- 901
moz_district$prov_dist[which(moz_district$OBJECTID == 37)] <- 102
moz_district$prov_dist[which(moz_district$OBJECTID == 38)] <- 418
moz_district$prov_dist[which(moz_district$OBJECTID == 39)] <- 514
moz_district$prov_dist[which(moz_district$OBJECTID == 40)] <- 707
moz_district$prov_dist[which(moz_district$OBJECTID == 41)] <- 303
moz_district$prov_dist[which(moz_district$OBJECTID == 42)] <- 802
moz_district$prov_dist[which(moz_district$OBJECTID == 43)] <- 404
moz_district$prov_dist[which(moz_district$OBJECTID == 44)] <- 603
moz_district$prov_dist[which(moz_district$OBJECTID == 45)] <- 708
moz_district$prov_dist[which(moz_district$OBJECTID == 46)] <- 803
moz_district$prov_dist[which(moz_district$OBJECTID == 47)] <- 907
moz_district$prov_dist[which(moz_district$OBJECTID == 48)] <- 604
moz_district$prov_dist[which(moz_district$OBJECTID == 49)] <- 405
moz_district$prov_dist[which(moz_district$OBJECTID == 50)] <- 804
moz_district$prov_dist[which(moz_district$OBJECTID == 51)] <- 205
moz_district$prov_dist[which(moz_district$OBJECTID == 52)] <- 406
moz_district$prov_dist[which(moz_district$OBJECTID == 56)] <- 805
moz_district$prov_dist[which(moz_district$OBJECTID == 57)] <- 806
moz_district$prov_dist[which(moz_district$OBJECTID == 58)] <- 407
moz_district$prov_dist[which(moz_district$OBJECTID == 59)] <- 807
moz_district$prov_dist[which(moz_district$OBJECTID == 60)] <- 103
moz_district$prov_dist[which(moz_district$OBJECTID == 62)] <- 305
moz_district$prov_dist[which(moz_district$OBJECTID == 63)] <- 322
moz_district$prov_dist[which(moz_district$OBJECTID == 64)] <- 101
moz_district$prov_dist[which(moz_district$OBJECTID == 65)] <- 912
moz_district$prov_dist[which(moz_district$OBJECTID == 66)] <- 323
moz_district$prov_dist[which(moz_district$OBJECTID == 67)] <- 419
moz_district$prov_dist[which(moz_district$OBJECTID == 68)] <- 408
moz_district$prov_dist[which(moz_district$OBJECTID == 69)] <- 908
moz_district$prov_dist[which(moz_district$OBJECTID == 70)] <- 808
moz_district$prov_dist[which(moz_district$OBJECTID == 71)] <- 507
moz_district$prov_dist[which(moz_district$OBJECTID == 72)] <- 611
moz_district$prov_dist[which(moz_district$OBJECTID == 73)] <- 709
moz_district$prov_dist[which(moz_district$OBJECTID == 74)] <- 605
moz_district$prov_dist[which(moz_district$OBJECTID == 75)] <- 206
moz_district$prov_dist[which(moz_district$OBJECTID == 76)] <- 606
moz_district$prov_dist[which(moz_district$OBJECTID == 77)] <- 409
moz_district$prov_dist[which(moz_district$OBJECTID == 78)] <- 508
moz_district$prov_dist[which(moz_district$OBJECTID == 79)] <- 1003
moz_district$prov_dist[which(moz_district$OBJECTID == 80)] <- 105
moz_district$prov_dist[which(moz_district$OBJECTID == 81)] <- 306
moz_district$prov_dist[which(moz_district$OBJECTID == 82)] <- 106
moz_district$prov_dist[which(moz_district$OBJECTID == 83)] <- 909
moz_district$prov_dist[which(moz_district$OBJECTID == 84)] <- 1004
moz_district$prov_dist[which(moz_district$OBJECTID == 85)] <- 607
moz_district$prov_dist[which(moz_district$OBJECTID == 86)] <- 914
moz_district$prov_dist[which(moz_district$OBJECTID == 87)] <- 515
moz_district$prov_dist[which(moz_district$OBJECTID == 88)] <- 509
moz_district$prov_dist[which(moz_district$OBJECTID == 89)] <- 710
moz_district$prov_dist[which(moz_district$OBJECTID == 90)] <- 1005
moz_district$prov_dist[which(moz_district$OBJECTID == 91)] <- 711
moz_district$prov_dist[which(moz_district$OBJECTID == 92)] <- 107
moz_district$prov_dist[which(moz_district$OBJECTID == 93)] <- 910
moz_district$prov_dist[which(moz_district$OBJECTID == 94)] <- 809
moz_district$prov_dist[which(moz_district$OBJECTID == 95)] <- 911
moz_district$prov_dist[which(moz_district$OBJECTID == 96)] <- 1006
moz_district$prov_dist[which(moz_district$OBJECTID == 97)] <- 108
moz_district$prov_dist[which(moz_district$OBJECTID == 98)] <- 109
moz_district$prov_dist[which(moz_district$OBJECTID == 100)] <- 110
moz_district$prov_dist[which(moz_district$OBJECTID == 101)] <- 307
moz_district$prov_dist[which(moz_district$OBJECTID == 102)] <- 308
moz_district$prov_dist[which(moz_district$OBJECTID == 103)] <- 207
moz_district$prov_dist[which(moz_district$OBJECTID == 104)] <- 111
moz_district$prov_dist[which(moz_district$OBJECTID == 105)] <- 208
moz_district$prov_dist[which(moz_district$OBJECTID == 106)] <- 309
moz_district$prov_dist[which(moz_district$OBJECTID == 107)] <- 112
moz_district$prov_dist[which(moz_district$OBJECTID == 108)] <- 410
moz_district$prov_dist[which(moz_district$OBJECTID == 109)] <- 1007
moz_district$prov_dist[which(moz_district$OBJECTID == 110)] <- 510
moz_district$prov_dist[which(moz_district$OBJECTID == 111)] <- 209
moz_district$prov_dist[which(moz_district$OBJECTID == 112)] <- 411
moz_district$prov_dist[which(moz_district$OBJECTID == 113)] <- 420
moz_district$prov_dist[which(moz_district$OBJECTID == 114)] <- 310
moz_district$prov_dist[which(moz_district$OBJECTID == 115)] <- 311
moz_district$prov_dist[which(moz_district$OBJECTID == 116)] <- 421
moz_district$prov_dist[which(moz_district$OBJECTID == 117)] <- 312
moz_district$prov_dist[which(moz_district$OBJECTID == 118)] <- 313
moz_district$prov_dist[which(moz_district$OBJECTID == 119)] <- 210
moz_district$prov_dist[which(moz_district$OBJECTID == 120)] <- 412
moz_district$prov_dist[which(moz_district$OBJECTID == 121)] <- 413
moz_district$prov_dist[which(moz_district$OBJECTID == 122)] <- 811
moz_district$prov_dist[which(moz_district$OBJECTID == 123)] <- 314
moz_district$prov_dist[which(moz_district$OBJECTID == 124)] <- 608
moz_district$prov_dist[which(moz_district$OBJECTID == 125)] <- 712
moz_district$prov_dist[which(moz_district$OBJECTID == 126)] <- 315
moz_district$prov_dist[which(moz_district$OBJECTID == 127)] <- 211
moz_district$prov_dist[which(moz_district$OBJECTID == 128)] <- 113
moz_district$prov_dist[which(moz_district$OBJECTID == 129)] <- 212
moz_district$prov_dist[which(moz_district$OBJECTID == 130)] <- 422
moz_district$prov_dist[which(moz_district$OBJECTID == 131)] <- 316
moz_district$prov_dist[which(moz_district$OBJECTID == 132)] <- 511
moz_district$prov_dist[which(moz_district$OBJECTID == 133)] <- 317
moz_district$prov_dist[which(moz_district$OBJECTID == 134)] <- 318
moz_district$prov_dist[which(moz_district$OBJECTID == 135)] <- 319
moz_district$prov_dist[which(moz_district$OBJECTID == 136)] <- 1008
moz_district$prov_dist[which(moz_district$OBJECTID == 137)] <- 414
moz_district$prov_dist[which(moz_district$OBJECTID == 138)] <- 415
moz_district$prov_dist[which(moz_district$OBJECTID == 139)] <- 213
moz_district$prov_dist[which(moz_district$OBJECTID == 140)] <- 214
moz_district$prov_dist[which(moz_district$OBJECTID == 141)] <- 114
moz_district$prov_dist[which(moz_district$OBJECTID == 142)] <- 713
moz_district$prov_dist[which(moz_district$OBJECTID == 143)] <- 416
moz_district$prov_dist[which(moz_district$OBJECTID == 144)] <- 115
moz_district$prov_dist[which(moz_district$OBJECTID == 145)] <- 215
moz_district$prov_dist[which(moz_district$OBJECTID == 146)] <- 812
moz_district$prov_dist[which(moz_district$OBJECTID == 147)] <- 417
moz_district$prov_dist[which(moz_district$OBJECTID == 148)] <- 201
moz_district$prov_dist[which(moz_district$OBJECTID == 149)] <- 401
moz_district$prov_dist[which(moz_district$OBJECTID == 150)] <- 217
moz_district$prov_dist[which(moz_district$OBJECTID == 151)] <- 320
moz_district$prov_dist[which(moz_district$OBJECTID == 152)] <- 321
moz_district$prov_dist[which(moz_district$OBJECTID == 153)] <- 116
moz_district$prov_dist[which(moz_district$OBJECTID == 154)] <- 609
moz_district$prov_dist[which(moz_district$OBJECTID == 155)] <- 610
moz_district$prov_dist[which(moz_district$OBJECTID == 156)] <- 512
moz_district$prov_dist[which(moz_district$OBJECTID == 157)] <- 612
moz_district$prov_dist[which(moz_district$OBJECTID == 158)] <- 813
moz_district$prov_dist[which(moz_district$OBJECTID == 159)] <- 814
moz_district$prov_dist[which(moz_district$OBJECTID == 160)] <- 513

# moz_district@data %>% glimpse()


# District Production -----------------------------------------------------


# adicionar nova variavel: prov_dist
ibsa_sec_h_merged_tbl$prov_dist <- ibsa_sec_h_merged_tbl$prov*100 + ibsa_sec_h_merged_tbl$dist

# Por distrito
producao_sec_h_distrito_tbl <- 
    ibsa_sec_h_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), prov_dist, tipo_cultura) %>% 
    
    # multiplicar o peso pela quantidade 
    summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
    
    ungroup()

# producao_sec_h_distrito_tbl %>% glimpse()
