
# 1.0 Carregar as Bibliotecas necessárias ---------------------------------

# loading the required libraries

# data manipulation
library(tidyverse)

# read excel files
library(readxl)

library(data.table)
library(DT)
library(reactable)


# 1.1 Carregar os dados ---------------------------------------------------

# Listagem
listagem_tbl <-
    read.csv(file = "00_data/Exported_Listagem.csv", header = TRUE, sep = ",")

listagem_tbl <-
    listagem_tbl %>% drop_na(PROV)

listagem_tbl <-
    listagem_tbl %>%
    filter(!(is.na(CLASSIF) | CLASSIF == 'V'))

# listagem_tbl[listagem_tbl$PROV == 8 & listagem_tbl$DIST == 12 & !is.na(listagem_tbl$EASTING), ]

listagem_tbl %>% glimpse()


# 1.2 Carregar os dados sobre Amostras ------------------------------------

amostra_harmonizada_tbl <- read_excel("00_data/amostra_harmonizada_v2.xls")

View(amostra_harmonizada_tbl)

amostra_harmonizada_tbl %>% glimpse()


# 2.0 Análise Descritiva dos Dados --------------------------------------------

# Juntar as tabelas entre Listagem e Amostra.

# Joining tables

amostra_modificada_tbl <- amostra_harmonizada_tbl[ , 2:12]

amostra_modificada_tbl <- amostra_modificada_tbl %>% unique()

amostra_modificada_tbl$provincia <- factor(amostra_modificada_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))


listagem_joined_tbl <-
    listagem_tbl %>%
    left_join(amostra_modificada_tbl,
              by = c("LI_IAIID" = "ibsa_id"))

listagem_joined_tbl <- listagem_joined_tbl[!is.na(listagem_joined_tbl$EASTING), ]

listagem_joined_tbl %>% glimpse()



# 2.1 Obter o numero de AE's a AF's listados por Provincia ----------------

# contar numeros de AE's e AF's listados
contar_AF_listados_tbl <- 
    listagem_joined_tbl %>%
    group_by(provincia, distrito) %>%
    summarise(num_AE = n_distinct(LI_IAIID),
              num_AF = n()) %>%
    ungroup() %>% 
    arrange(num_AF)

contar_AF_listados_tbl



# contar os tipos de exploracao por provincia ----

# listagem_joined_tbl[ is.na(listagem_joined_tbl$provincia), 1:3] %>% unique()

contar_tipo_Exploracao_tbl <-     
    listagem_joined_tbl %>%
    # filter(!is.na(CLASSIF)) %>% 
    count(provincia, distrito, CLASSIF) %>%
    # filter(!is.na(provincia)) %>% 
    pivot_wider(names_from = CLASSIF, values_from = n, values_fill = list(n = 0))

contar_tipo_Exploracao_tbl %>% glimpse()



# 3.0 Criar Reports -------------------------------------------------------

# report
stat_tbl <-
    contar_AF_listados_tbl %>%
    inner_join(contar_tipo_Exploracao_tbl, by = c("provincia", "distrito"))


stat_tbl$provincia <- factor(stat_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))

stat_tbl 


# Agrupar os dados por Provincia ----
stat_prov_tbl <-
    stat_tbl %>%
    group_by(provincia) %>%
    summarise(
        total_AE = sum(num_AE),
        total_AF = sum(num_AF),
        total_PE = sum(PE),
        total_ME = sum(ME),
        total_GE = sum(GE)
    ) %>%
    ungroup()

stat_prov_tbl

# colSums(stat_prov_tbl[ , 2:6])

# usando o pacote DT
datatable(stat_prov_tbl)


# Agregado por distrito ----
stat_distrito_tbl <- 
    stat_tbl %>% 
    group_by(provincia, distrito) %>% 
    summarise(total_AE = sum(num_AE),
              total_AF = sum(num_AF),
              total_PE = sum(PE),
              total_ME = sum(ME),
              total_GE = sum(GE)) %>% 
    ungroup()

stat_distrito_tbl


datatable(stat_distrito_tbl)



# 3.1 Reports usando o pacote Reactable -----------------------------------

reactable(stat_prov_tbl)

reactable(
    stat_prov_tbl,
    defaultPageSize = 11,
    columns = list(
        provincia = colDef(footer = "Total"),
        # total_AF = colDef(footer = function(values) sprintf("%.f", sum(values)))
        # total_AF = colDef(footer = function(values) sprintf("%.0f", sum(values)))
        total_AE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_AF = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_PE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_ME = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_GE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        )
    ),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    
)


# reactable: agregar por distrito
reactable(stat_distrito_tbl)


reactable(
    stat_distrito_tbl,
    defaultPageSize = 10,
    columns = list(
        provincia = colDef(footer = "Total"),
        # total_AF = colDef(footer = function(values) sprintf("%.f", sum(values)))
        # total_AF = colDef(footer = function(values) sprintf("%.0f", sum(values)))
        total_AE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_AF = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_PE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_ME = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        total_GE = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        )
    ),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    
)


# 3.2 Mostrar as AE's por Distrito -------------------------------

# mostrar as AE's por distrito
listagem_joined_tbl %>% glimpse()

listagem_joined_tbl$provincia <- factor(listagem_joined_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))

report_AE <- 
    listagem_joined_tbl %>% 
    # group_by(provincia, distrito, AE=LI_IAIID)
    count(provincia, distrito, ibsa_id=LI_IAIID, posto, localidade, CLASSIF) %>% 
    pivot_wider(names_from = CLASSIF, values_from = n, values_fill = list(n = 0))

report_AE$total_AF <- report_AE$PE + report_AE$ME + report_AE$GE

reactable(report_AE, groupBy = c("provincia"))



# 3.3 Performance ---------------------------------------------------------

amostra_modificada_tbl %>% glimpse()

AE_planificadas_tbl <-
    amostra_modificada_tbl %>%
    group_by(provincia) %>%
    summarise(no_AE_planificada = n()) %>% 
    ungroup()

AE_listadas_tbl <-
    report_AE %>%
    filter(!is.na(provincia)) %>%
    group_by(provincia) %>%
    summarise(no_AE_listada = n()) %>% 
    ungroup()

performance_tbl <- cbind(AE_listadas_tbl, AE_planificadas_tbl)

performance_tbl <- performance_tbl[ , -3]

performance_tbl$percent <- round((performance_tbl$no_AE_listada/performance_tbl$no_AE_planificada)*100, 2)

performance_tbl

datatable(performance_tbl)



reactable(
    performance_tbl,
    defaultPageSize = 11,
    columns = list(
        provincia = colDef(footer = "Total"),
        # total_AF = colDef(footer = function(values) sprintf("%.f", sum(values)))
        # total_AF = colDef(footer = function(values) sprintf("%.0f", sum(values)))
        no_AE_listada = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        no_AE_planificada = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        ),
        percent = colDef(
            footer = function(values) {
                format(mean(values),  nsmall = 1, big.mark = ",")
            }
        )
    ),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    
)


# 4.0 Visualizacao de Dados -----------------------------------------------
library(apexcharter)

performance_tbl[ order(performance_tbl$percent, decreasing = TRUE), ]

apex(data = performance_tbl, type = "bar", aes(x = provincia, y = percent))

apex(
    data = performance_tbl[order(performance_tbl$percent, decreasing = TRUE),],
    type = "bar",
    aes(x = provincia, y = percent)
) %>%
    ax_title(text = "Performance: número de AE's listadas vs número de AE's planificadas") %>%
    ax_subtitle(text = "IAI 2020") %>%
  ax_dataLabels(enabled = TRUE) %>% 
  ax_xaxis(title = list(text = "Percentagem de Execução (%)"))
    # ax_dataLabels(enabled = TRUE, dropShadow(enabled=TRUE))

    


apex(data = performance_tbl, type = "column", aes(x = provincia, y = no_AE_listada, fill = no_AE_planificada ))





# 5.0 Validar Reclamações -------------------------------------------------

listagem_help_tbl <-
    read.csv(file = "00_data/Exported_Listagem.csv", header = TRUE, sep = ",")


listagem_help_tbl %>% 
    filter(PROV == 5) %>% 
    distinct(PROV, LI_IAIID) %>% 
    arrange(LI_IAIID) %>% 
    select (PROV, IBSA_ID = LI_IAIID)


# IBSA_ID sem provincia
listagem_joined_tbl[is.na(listagem_joined_tbl$provincia), 1:3] %>% unique()
