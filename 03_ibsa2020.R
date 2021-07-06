############################################################
### 03_ibsa2020
############################################################

# loading libraries
library(tidyverse)

# read excel files
library(readxl)

library(reactable)

# loading data
exported_ibsa_id_exploracao_tbl <- read.csv(file = "~/TCHINGUE_CONSULTING/MADER/CSPro_data/exported_data/exported_ibsa_id_exploracao.csv")

exported_ibsa_id_exploracao_tbl %>% glimpse()

# convert datatype
exported_ibsa_id_exploracao_tbl$A10 <- as.Date(exported_ibsa_id_exploracao_tbl$A10, "%d/%m/%Y")


# examine the data
exported_ibsa_id_exploracao_tbl %>% summary

# Remover registos cujo data de entrevista eh igual a NULL
exported_ibsa_id_exploracao_tbl <- exported_ibsa_id_exploracao_tbl[!is.na(exported_ibsa_id_exploracao_tbl$A10), ]

# remover registos cujo data entrevista seja inferior ao ano 2021
exported_ibsa_id_exploracao_tbl <- exported_ibsa_id_exploracao_tbl[ !lubridate::year(exported_ibsa_id_exploracao_tbl$A10) == 2019, ]

exported_ibsa_id_exploracao_tbl %>% glimpse()




# 3.2 Carregar os dados sobre Amostras ------------------------------------

amostra_harmonizada_tbl <- read_excel("00_data/amostra_harmonizada_v2.xls")

# View(amostra_harmonizada_tbl)

amostra_harmonizada_tbl %>% glimpse()



# 2.0 Análise Descritiva dos Dados --------------------------------------------

# Juntar as tabelas entre ibsa2020 e Amostra.

# Joining tables

amostra_modificada_tbl <- amostra_harmonizada_tbl[ , 2:12]

amostra_modificada_tbl <- amostra_modificada_tbl %>% unique()

amostra_modificada_tbl$provincia <- factor(amostra_modificada_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))


ibsa2020_joined_tbl <-
    exported_ibsa_id_exploracao_tbl %>%
    left_join(amostra_modificada_tbl,
              by = c("UPA" = "ibsa_id"))

ibsa2020_joined_tbl$provincia <- factor(ibsa2020_joined_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))

# ibsa2020_joined_tbl %>% head()

ibsa2020_joined_tbl %>% glimpse()


# contar numeros de AE's e AF's listados

exported_ibsa_id_exploracao_tbl %>% glimpse()

exported_ibsa_id_exploracao_tbl[exported_ibsa_id_exploracao_tbl$UPA == 1592, ] 

exported_ibsa_id_exploracao_tbl %>% 
    filter(UPA == 1592) %>% 
    arrange(AF)


exported_ibsa_id_exploracao_tbl %>% 
    group_by(PROV, DIST) %>% 
    summarise(num_UPA = n_distinct(UPA)) %>% 
    ungroup()

?order

contar_UPA_PROV_tbl <- 
ibsa2020_joined_tbl %>% 
    group_by(provincia, distrito) %>% 
    summarise(num_UPA = n_distinct(UPA)) %>% 
    ungroup()

contar_UPA_PROV_tbl %>% glimpse()




# UPA_PROV_entrevistadas_tbl$provincia <- factor(UPA_PROV_entrevistadas_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))

UPA_PROV_entrevistadas_tbl %>% glimpse()

ibsa2020_joined_tbl[ is.na(ibsa2020_joined_tbl$provincia), c(1, 2, 3)] %>% unique()





# performance

# AE entrevistadas
UPA_PROV_entrevistadas_tbl <-
    contar_UPA_PROV_tbl %>%
    group_by(provincia) %>%
    summarise(no_AE_entrevistadas = sum(num_UPA)) %>%
    ungroup() 

# AE planificadas
AE_planificadas_tbl <-
    amostra_modificada_tbl %>%
    group_by(provincia) %>%
    summarise(no_AE_planificada = n()) %>% 
    ungroup()


AE_planificadas_tbl


entrevistas_performance_tbl <- cbind(UPA_PROV_entrevistadas_tbl, AE_planificadas_tbl)

entrevistas_performance_tbl <- entrevistas_performance_tbl[ , -3]

entrevistas_performance_tbl$percent <- round((entrevistas_performance_tbl$no_AE_entrevistadas/entrevistas_performance_tbl$no_AE_planificada)*100, 2)

entrevistas_performance_tbl


# reactable
reactable(
    entrevistas_performance_tbl,
    defaultPageSize = 11,
    columns = list(
        provincia = colDef(footer = "Total"),
        # total_AF = colDef(footer = function(values) sprintf("%.f", sum(values)))
        # total_AF = colDef(footer = function(values) sprintf("%.0f", sum(values)))
        no_AE_entrevistadas = colDef(
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


# 3.4 Visualizacao de Dados -----------------------------------------------
library(apexcharter)

apex(
    data = entrevistas_performance_tbl[order(entrevistas_performance_tbl$percent, decreasing = TRUE),],
    type = "bar",
    aes(x = provincia, y = percent)
) %>%
    ax_title(text = "Performance: número de AE's entrevistadas vs número de AE's planificadas") %>%
    ax_subtitle(text = "IAI 2020") %>%
    ax_dataLabels(enabled = TRUE) %>% 
    ax_xaxis(title = list(text = "Percentagem de Execução (%)"))
# ax_dataLabels(enabled = TRUE, dropShadow(enabled=TRUE))


# Comparação com a Média Geral
apex(
    data = entrevistas_performance_tbl,
    type = "column",
    aes(x = provincia, y = percent)
) %>%
    add_hline(value = mean(entrevistas_performance_tbl$percent),
              label = "Média Geral",
              dash = 2) %>% 
    ax_title(text = "Performance: número de AE's entrevistadas vs número de AE's planificadas") %>%
    ax_subtitle(text = "IAI 2020") %>%
    ax_dataLabels(enabled = TRUE) %>% 
    ax_xaxis(title = list(text = "Percentagem de Execução (%)"))
# ax_dataLabels(enabled = TRUE, dropShadow(enabled=TRUE))



# 3.5 Tendencia -----------------------------------------------------------

library(sqldf)


exported_ibsa_id_exploracao_tbl %>% glimpse()

sqldf("select PROV, DIST, UPA, AF, min(A10) from exported_ibsa_id_exploracao_tbl where upa = 1592 group by PROV, DIST, UPA, AF ")

# remover duplicados, seleccionando o registo com a data minima, ou seja a 1a entrevista
exported_ibsa_id_exploracao_cleaned_tbl <-
    exported_ibsa_id_exploracao_tbl %>%
    # filter(UPA == 1592) %>%
    group_by(PROV, DIST, UPA, AF) %>%
    slice_min(A10)
    
exported_ibsa_id_exploracao_cleaned_tbl %>% glimpse()


ibsa2020_joined_cleaned_tbl <-
    exported_ibsa_id_exploracao_cleaned_tbl %>%
    left_join(amostra_modificada_tbl,
              by = c("UPA" = "ibsa_id"))

ibsa2020_joined_cleaned_tbl %>% glimpse()

daily_interview_tbl <-
    ibsa2020_joined_cleaned_tbl %>%
    filter(A10 >= as.Date('2021-01-01')) %>% 
    group_by(created_at = A10, provincia, PROV) %>%
    summarise(no_entrevistas = n()) %>%
    ungroup()




plot_data <- daily_interview_tbl[ daily_interview_tbl$PROV == 10, ]

apex(plot_data, aes(x =created_at, y= no_entrevistas), "column") %>% 
    # ax_dataLabels(enabled = TRUE, dropShadow = TRUE) %>%
    ax_dataLabels(enabled = TRUE) %>% 
    ax_title(text = paste("Número diário de Entrevistas:", unique(plot_data$provincia))) %>%
    ax_subtitle(text = "IAI 2020") %>%
    ax_xaxis(title = list(text = "Data")) %>% 
    ax_yaxis(title = list(text = "Numero de Entrevistas"))
    

    


# ggplot by category

ggplot(data = daily_interview_tbl, aes(x = created_at, y = no_entrevistas, fill = provincia)) +
    
    # geometries
    geom_col() +
    # geom_label(aes(label = scales::comma(no_entrevistas))) +
    geom_smooth(method = 'lm', se = FALSE) +
    
    # facet
    facet_wrap( ~ provincia, scales = "free_y") +
    
    # formatting
    scale_y_continuous(labels = scales::comma)


# Total de Entrevistas por Provincia

total_entrevistas_prov_tbl <- 
    ibsa2020_joined_cleaned_tbl %>%
    filter(A10 >= as.Date('2021-01-01')) %>% 
    group_by(provincia) %>%
    summarise(no_entrevistas = n()) %>%
    ungroup()

reactable(
    total_entrevistas_prov_tbl,
    defaultPageSize = 11,
    columns = list(
        provincia = colDef(footer = "Total"),
        # total_AF = colDef(footer = function(values) sprintf("%.f", sum(values)))
        # total_AF = colDef(footer = function(values) sprintf("%.0f", sum(values)))
        no_entrevistas = colDef(
            footer = function(values) {
                format(sum(values),  nsmall = 1, big.mark = ",")
            }
        )
    ),
    defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
    
)
