##################################################################################
### 06_labels_cities_names.R
##################################################################################

# load libraries
library(leaflet) # create maps
library(rgdal) # for read shapefile
library(tidyverse) # for data manipulation
library(haven) # for importing data from Stata
library(htmltools) # for html
library(labelled) # for label factor

library(DT)
library(htmlwidgets) # for save html



# 1.0 Importar dados ------------------------------------------------------

dir_name <- "C:/Users/REINALDO ZEZELA/Documents/projects/IAI2020/Reports/anuario_estatistica_agraria_2020/seccao_N/"

# Pesos dos dados de Producao
pesos_producao_tbl <- read_dta(file = "shiny_app/00_data/weightv0.dta")

pesos_producao_tbl %>% glimpse()

# Dados das Seccoes
ibsa_sec_n_dat <- read_dta("shiny_app/00_data/seccao/sec_n1av1.dta")

ibsa_sec_g_dat <- read_dta("shiny_app/00_data/seccao/sec_gv1.dta")

ibsa_sec_dat <- ibsa_sec_g_dat

ibsa_sec_dat %>% glimpse()

# # adicionar nova variavel: prov_dist
# ibsa_sec_n_dat$prov_dist <- ibsa_sec_n_dat$prov*100 + ibsa_sec_n_dat$dist
# 
# ibsa_sec_n_dat %>% glimpse()


# 2.0 Manipulacao de dados ------------------------------------------------

# Juntar as tabelas com base nos campos, "caseid", "prov", "dist", "upa", "af"



# adicionar nova variavel: prov_dist
ibsa_sec_dat$prov_dist <- ibsa_sec_dat$prov*100 + ibsa_sec_dat$dist


ibsa_sec_merged_tbl <- 
    merge(ibsa_sec_dat, pesos_producao_tbl, by = c("caseid", "prov", "dist", "upa", "af"))

ibsa_sec_merged_tbl %>% glimpse()




# Agrupar dados de producao por tipo de producao e usando os respectivos pesos
# Por Provincia

# renomear o campo 6a para tipo de cultura
colnames(ibsa_sec_merged_tbl)[6] <- "tipo_producao"

producao_sec_provincia_tbl <- 
    ibsa_sec_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), tipo_producao) %>%
    
    # # multiplicar a quantidade pelo peso
    # summarise(total_qnt = round(sum(n02*wgt, na.rm = TRUE), 0)) %>% 
    
    # multiplicar a quantidade pelo peso
    summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
    
    ungroup()

producao_sec_provincia_tbl %>% glimpse()



# Por distrito
producao_sec_distrito_tbl <- 
    ibsa_sec_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), prov_dist, tipo_producao) %>% 
    
    # multiplicar o peso pela quantidade 
    summarise(total_qnt = round(sum(n02*wgt, na.rm = TRUE), 0)) %>% 
    
    ungroup()

producao_sec_distrito_tbl %>% glimpse()



# 3.0 Criar Mapa ----------------------------------------------------------

# 2.0 Importing shapefiles
# adding shapefiles
moz_province <- readOGR("shiny_app/00_data/Provincias/Provincias.shp", layer = "Provincias")

moz_province@data %>% glimpse()

moz_district <- readOGR("shiny_app/00_data/Distritos/Distritos.shp", layer = "Distritos")


# moz_district <- suppressWarnings(readOGR("shiny_app/00_data/Distritos/Distritos.shp", layer = "Distritos"))

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

moz_district@data %>% glimpse()


# Verificar se ha correspondencia nos nomes das provincias entre as tabelas
is.element(producao_sec_provincia_tbl$PROVINCIA, moz_province$PROVINCIA) %>% 
    all()

# rename all levels
levels(producao_sec_provincia_tbl$PROVINCIA) <- c("NIASSA", "CABO_DELGADO", "NAMPULA", "ZAMBEZIA", "TETE", "MANICA", "SOFALA", "INHAMBANE", "GAZA", "MAPUTO")


# Tipo de Cultura
producao_sec_provincia_tbl$tipo_producao %>% unique()

# Medir o nivel de producao por tipo de cultura
producao_sec_provincia_tbl %>% 
    group_by(tipo_producao) %>% 
    summarise(total_geral = sum(total_qnt )) %>% 
    arrange(desc(total_geral)) %>% 
    ungroup()


# Provincia: criar tabela por tipo de producao ----------------------------------------

# atribuir dados a variavel
v_producao_tipo <- "GALINHAS LANDIM"


tipo_producao_sec_provincia_tbl <-
    producao_sec_provincia_tbl[producao_sec_provincia_tbl$tipo_producao == v_producao_tipo,]

# Juncao entre shapefiles e dados de producao por tipo de cultura ----
tipo_producao_sec_provincia_merged_tbl <-
    merge(moz_province,
          tipo_producao_sec_provincia_tbl,
          by = "PROVINCIA",
          all.x = F)


# Distrito: criar tabela por tipo de producao
tipo_producao_sec_distrito_tbl <-
    producao_sec_distrito_tbl[producao_sec_distrito_tbl$tipo_producao == v_producao_tipo, ]

# tipo_producao_sec_h_distrito_tbl %>% glimpse()


# Juncao entre shapefiles e dados de producao por tipo de cultura 
# moz_district@data %>% glimpse()

tipo_producao_sec_distrito_merged_tbl <-
    merge(moz_district,
          tipo_producao_sec_distrito_tbl,
          by = "prov_dist",
          all.x = F)


# Labels city name: Province ----------------------------------------------


# Find a center point for each region
centers <- data.frame(rgeos::gCentroid(moz_province, byid = TRUE))
centers$region <- moz_province@data$PROVINCIA

# centers %>% head()


paletteNum <- colorNumeric('Greens', domain = tipo_producao_sec_provincia_merged_tbl$total_qnt)


# labels
producao_Labels <- sprintf('<b>%s</b><br/>%s unid.<br/>%s',
                           tipo_producao_sec_provincia_merged_tbl$PROVINCIA, 
                           prettyNum(round(tipo_producao_sec_provincia_merged_tbl$total_qnt, 0),big.mark = ","),
                           tipo_producao_sec_provincia_merged_tbl$tipo_producao) %>%
    lapply(function(x) HTML(x))


m_province <- leaflet() %>%
    # addMapPane(name = "polygons", zIndex = 410) %>% 
    # addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
    # addProviderTiles("CartoDB.PositronNoLabels") %>%
    # addProviderTiles("CartoDB.PositronOnlyLabels", 
    #                  options = leafletOptions(pane = "maplabels"),
    #                  group = "map labels") %>%
    
    addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    # addProviderTiles(providers$Stamen.TonerLite) %>% 
    # addTiles() %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = tipo_producao_sec_provincia_merged_tbl,
                
                # state border stroke color
                # color = 'white', 
                color = "#660000",
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                fillColor = ~paletteNum(tipo_producao_sec_provincia_merged_tbl$total_qnt),
                
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
    ) %>% 
    addLabelOnlyMarkers(data = centers,
                        lng = ~x, lat = ~y, label = ~region,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
    
    addLegend(pal = paletteNum, values = tipo_producao_sec_provincia_merged_tbl$total_qnt, title = paste0("<small>Campanha Agricola 2020 <br> Producao Pecuaria de ", unique(tipo_producao_sec_provincia_merged_tbl$tipo_producao), " (unid.)<br>(fonte: IAI 2020)</small>"), position = 'bottomright')

# m_province


# Save for HTML: mapa por Provincia
file_name <- paste0(dir_name, "seccao_n_mapa_provincia_", v_producao_tipo, ".html")

saveWidget(m_province, 
           file = file_name,
           selfcontained = F, libdir = "lib")



# Labels city name: Distrito ----------------------------------------------

# Find a center point for each region
centers <- data.frame(rgeos::gCentroid(moz_district, byid = TRUE))
centers$region <- moz_district@data$DISTRITO

# centers %>% head()


# top20 district
top10_district <-
    tipo_producao_sec_distrito_merged_tbl@data[order(tipo_producao_sec_distrito_merged_tbl$total_qnt,
                                                       decreasing = TRUE),] %>% head(n = 10)

# top20_district$DISTRITO

centers <- centers[centers$region %in% top10_district$DISTRITO, ]

# tolower(centers$region) %>% head()

# Mapa por distritos usando Palette ----
paletteNum <- colorNumeric('Greens', domain = tipo_producao_sec_distrito_merged_tbl$total_qnt)


producao_Labels <- sprintf('<b>%s</b><br/>%s unid.<br/>%s',
                           tipo_producao_sec_distrito_merged_tbl$DISTRITO, 
                           prettyNum(round(tipo_producao_sec_distrito_merged_tbl$total_qnt, 0),big.mark = ","),
                           tipo_producao_sec_distrito_merged_tbl$tipo_producao) %>%
    lapply(htmltools::HTML)



m_distritos <- leaflet() %>%
    
    # addTiles() %>% 
    
    # addMapPane(name = "polygons", zIndex = 410) %>% 
    # addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
    # addProviderTiles("CartoDB.PositronNoLabels") %>%
    # addProviderTiles("CartoDB.PositronOnlyLabels", 
    #                  options = leafletOptions(pane = "maplabels"),
    #                  group = "map labels") %>%
    
    
addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    # addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = tipo_producao_sec_distrito_merged_tbl,
                
                # state border stroke color
                # color = 'white', 
                color = "#660000",
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .5, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                fillColor = ~paletteNum(tipo_producao_sec_distrito_merged_tbl$total_qnt),
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
    ) %>% 
    addLabelOnlyMarkers(data = centers,
                        lng = ~x, lat = ~y, label = ~region,
                        labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE)) %>%
    
    addLegend(pal = paletteNum, values = tipo_producao_sec_distrito_merged_tbl$total_qnt, title = paste0("<small>Campanha Agricola 2020 <br> Producao Pecuaria de ", unique(tipo_producao_sec_distrito_merged_tbl$tipo_producao), " (unid.)<br>(fonte: IAI 2020)</small>"), position = 'bottomright')

# m_distritos


# Save for HTML: mapa por distritos
file_name <- paste0(dir_name, "seccao_n_mapa_distritos_", v_producao_tipo, ".html")

saveWidget(
    m_distritos,
    file = file_name,
    selfcontained = F,
    libdir = "lib"
)

# Criar Tabelas -----------------------------------------------------
# library(DT)


# Tabelas: Producao Pecuaria por Provincia

producao_sec_provincia_dta <- 
    tipo_producao_sec_provincia_merged_tbl@data %>% 
    group_by(provincia = PROVINCIA, tipo_producao) %>% 
    summarise(total_qnt = total_qnt) %>% 
    ungroup()


export_producao_provincia_dta <- 
datatable(producao_sec_provincia_dta[ order(producao_sec_provincia_dta$total_qnt, decreasing = T), ],
          extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = 
                  list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                  ))
              
          )) %>%
    formatRound("total_qnt", 0) %>% 
    formatStyle(
        columns = "total_qnt",
        background = styleColorBar(range(producao_sec_provincia_dta$total_qnt), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
    )

# export_producao_provincia_dta

# Save for HTML: tabela por Provincia
file_name <- paste0(dir_name, "seccao_n_tabela_provincia_", v_producao_tipo, ".html")

saveWidget(
    export_producao_provincia_dta,
    file = file_name,
    selfcontained = F,
    libdir = "lib"
)




# tabelas: Producao Pecuaria por distritos
producao_sec_distrito_dta <- 
    tipo_producao_sec_distrito_merged_tbl@data %>% 
    group_by(provincia = PROVINCIA, distrito = DISTRITO, tipo_producao) %>% 
    summarise(total_qnt = total_qnt) %>% 
    ungroup()


export_producao_distrito_dta <- 
datatable(producao_sec_distrito_dta[ order(producao_sec_distrito_dta$total_qnt, decreasing = T), ],
          extensions = 'Buttons', options = list(
              dom = 'Bfrtip',
              buttons = 
                  list('copy', 'print', list(
                      extend = 'collection',
                      buttons = c('csv', 'excel', 'pdf'),
                      text = 'Download'
                  ))
              
          )) %>%
    formatRound("total_qnt", 0) %>% 
    formatStyle(
        columns = "total_qnt",
        background = styleColorBar(range(producao_sec_distrito_dta$total_qnt), 'lightgreen'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
    )    

# Save for HTML: tabela por Distritos
file_name <- paste0(dir_name, "seccao_n_tabela_distritos_", v_producao_tipo, ".html")

saveWidget(
    export_producao_distrito_dta,
    file = file_name,
    selfcontained = F,
    libdir = "lib"
)



# Save html code ----------------------------------------------------------

# library(rio)
# 
# export(html_code,
#        file="html_code_result.html",
#        format="html"
# )
# 
# export(m_province,
#        file = "C:/Users/REINALDO ZEZELA/Documents/projects/IAI2020/Reports/anuario_estatistica_agraria_2020/seccao_N/seccao_n_mapa_SUINOS_provincia.html",
#        format = "html")


# pkgs <- c("hexView", "pzfx", "readODS", "rmatio")
# 
# install.packages(pkgs)




# library(htmlwidgets)


# Provincia
# producao_tipo <- "SUINOS"
# dir_name <- "C:/Users/REINALDO ZEZELA/Documents/projects/IAI2020/Reports/anuario_estatistica_agraria_2020/seccao_N/"


# saveWidget(m_province, 
#            file = "C:/Users/REINALDO ZEZELA/Documents/projects/IAI2020/Reports/anuario_estatistica_agraria_2020/seccao_N/seccao_n_mapa_SUINOS_provincia.html",
#            selfcontained = F, libdir = "lib")


# # mapa por Provincia
# file_name <- paste0(dir_name, "seccao_n_mapa_provincia_", producao_tipo, ".html")
# 
# saveWidget(m_province, 
#            file = file_name,
#            selfcontained = F, libdir = "lib")


# # mapa por distritos
# file_name <- paste0(dir_name, "seccao_n_mapa_distritos_", producao_tipo, ".html")
# 
# saveWidget(
#     m_distritos,
#     file = file_name,
#     selfcontained = F,
#     libdir = "lib"
# )


# # tabela por Provincia
# file_name <- paste0(dir_name, "seccao_n_tabela_provincia_", producao_tipo, ".html")
# 
# saveWidget(
#     export_producao_provincia_dta,
#     file = file_name,
#     selfcontained = F,
#     libdir = "lib"
# )


# # tabela por Distritos
# file_name <- paste0(dir_name, "seccao_n_tabela_distritos_", producao_tipo, ".html")
# 
# saveWidget(
#     export_producao_distrito_dta,
#     file = file_name,
#     selfcontained = F,
#     libdir = "lib"
# )
