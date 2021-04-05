###############################################################
#### global.R
##############################################################


# 1.0 Loading libraries ---------------------------------------------------

library(tidyverse) # for data manipulation

# loading libraries
library(haven) # for import stata data
library(labelled)

library(reactable) # for tabulate data

library(apexcharter) # for data visualization



# 2.0 Loading data --------------------------------------------------------

# loading data section A
ibsa_sec_a_dat <- read_dta("~/projects/IAI2020/00_data/sec_a.dta")
# View(ibsa_sec_a_dat)


# Carregar os dados sobre Amostras
# sample data
library(readxl)
amostra_harmonizada_tbl <- read_excel("~/projects/IAI2020/00_data/amostra_harmonizada_v2.xls")

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
