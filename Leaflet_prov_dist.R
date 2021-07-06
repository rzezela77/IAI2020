
# 1.0 load libraries ----
library(leaflet) # create maps
library(rgdal) # for read shapefile
library(tidyverse) # for data manipulation
library(haven) # for importing data from Stata
library(htmltools) # for html


# 2.0 Importing shapefiles
# adding shapefiles
moz_province <- readOGR("shiny_app/00_data/Provincias/Provincias.shp")

moz_district <- readOGR("shiny_app/00_data/Distritos/Distritos.shp")


# 2.1 examinar os dados ----
moz_district@data %>% glimpse()



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

# 3.0 Importar dados de Producao ----

# 3.1 Dados da Seccao G ----
ibsa_sec_g_dat <- read_dta("shiny_app/00_data/sec_g.dta")

ibsa_sec_g_dat %>% glimpse()

# adicionar nova variavel: prov_dist

# (ibsa_sec_g_dat$prov*100 + ibsa_sec_g_dat$dist) %>% head()

ibsa_sec_g_dat$prov_dist <- ibsa_sec_g_dat$prov*100 + ibsa_sec_g_dat$dist

ibsa_sec_g_dat %>% glimpse()

# 3.2 Pesos dos dados de Producao
pesos_producao_tbl <- read_dta(file = "shiny_app/00_data/weightv0.dta")

pesos_producao_tbl %>% glimpse()


# 4.0 Manipulacao de dados ----

# 4.1 Juncao de tabelas com base no campo, caseid
# ibsa_sec_g_dat %>% 
#     left_join(pesos_producao_tbl, by = c("caseid", "prov", "dist", "upa", "af"))
# 
# ibsa_sec_g_dat %>% 
#     left_join(pesos_producao_tbl, by.x = c("caseid", "prov"), by.y = c("caseid", "prov") )


ibsa_sec_g_merged_tbl <- 
merge(ibsa_sec_g_dat, pesos_producao_tbl, by = c("caseid", "prov", "dist", "upa", "af"))

ibsa_sec_g_merged_tbl %>% glimpse()

# Agrupar dados de producao por tipo de producao e usando os respectivos pesos
# Por Provincia
producao_por_provincia_tbl <- 
    ibsa_sec_g_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), tipo_cultura = g00) %>%
    
    # multiplicar a quantidade pelo peso
    summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
    
    # convert to tons
    mutate(total_qnt_tons = total_qnt_kg * 0.001102) %>% 
    ungroup()

producao_por_provincia_tbl %>% glimpse()

# Por distrito
producao_por_distrito_tbl <- 
    ibsa_sec_g_merged_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), prov_dist, tipo_cultura = g00) %>% 
    summarise(total_qnt_kg = sum(qntkg*wgt, na.rm = TRUE)) %>% 
    
    # convert to tons
    mutate(total_qnt_tons = total_qnt_kg * 0.001102) %>% 
    ungroup()

producao_por_distrito_tbl %>% glimpse()


# 5.0 Criar Mapa ----


# Verificar se ha correspondencia nos nomes das provincias entre as tabelas
is.element(producao_por_provincia_tbl$PROVINCIA, moz_province$PROVINCIA) %>% 
    all()

# tipo_producao_por_provincia_tbl
unique(producao_por_provincia_tbl$PROVINCIA)

# moz_province
moz_province$PROVINCIA %>% unique()


# rename all levels
levels(producao_por_provincia_tbl$PROVINCIA) <- c("NIASSA", "CABO_DELGADO", "NAMPULA", "ZAMBEZIA", "TETE", "MANICA", "SOFALA", "INHAMBANE", "GAZA", "MAPUTO")



producao_por_provincia_tbl$tipo_cultura %>% unique()

# criar tabela por tipo de cultura
tipo_producao_por_provincia_tbl <-
    producao_por_provincia_tbl[producao_por_provincia_tbl$tipo_cultura == "MILHO", ]

# 5.1 Juncao entre shapefiles e dados de producao por tipo de cultura ----
tipo_producao_por_provincia_merged_tbl <- merge(moz_province, tipo_producao_por_provincia_tbl, by = "PROVINCIA", all.x = F)

tipo_producao_por_provincia_merged_tbl %>% glimpse()


# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Blues", domain=tipo_producao_por_provincia_merged_tbl$total_qnt_tons)
# pal <- colorNumeric("Greens", domain=tipo_producao_provincia_merged_tbl$total_qnt_kg)


# province
m_province <- leaflet() %>% 
    addProviderTiles(providers$Stamen.TonerLite) %>% 
    # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = tipo_producao_por_provincia_merged_tbl,
                # color = "#660000",
                fillColor = ~pal(tipo_producao_por_provincia_merged_tbl$total_qnt_tons), 
                weight = 1,
                smoothFactor = 0.5,
                fillOpacity = 0.7
    )

m_province



# opcao 2
paletteNum <- colorNumeric('Blues', domain = tipo_producao_por_provincia_merged_tbl$total_qnt_tons)

# paletteNum <- colorNumeric('Greens', domain = tipo_producao_provincia_merged_tbl$total_qnt_kg)


# labels
producao_Labels <- sprintf('<b>%s</b><br/>%s ton<br/>%s',
                           tipo_producao_por_provincia_merged_tbl$PROVINCIA, 
                           prettyNum(round(tipo_producao_por_provincia_merged_tbl$total_qnt_tons, 2),big.mark = ","),
                           tipo_producao_por_provincia_merged_tbl$tipo_cultura) %>%
    lapply(function(x) HTML(x))




m_province <- leaflet() %>%
    # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    # addProviderTiles(providers$Stamen.Toner) %>% 
    addTiles() %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = tipo_producao_por_provincia_merged_tbl,
                
                # state border stroke color
                color = 'white', 
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                fillColor = ~paletteNum(tipo_producao_por_provincia_merged_tbl$total_qnt_tons),
                
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
    ) %>% 
    addLegend(pal = paletteNum, values = tipo_producao_por_provincia_merged_tbl$total_qnt_tons, title = '<small>Campanha Agricola 2020: Producao de XYZ <br>(Toneladas| fonte: IAI 2020)</small>', position = 'topright')


m_province


# 6.0 Visualizar a Producao por Distrito ----------------

# criar tabela por tipo de cultura
tipo_producao_por_distrito_tbl <-
    producao_por_distrito_tbl[producao_por_distrito_tbl$tipo_cultura == "MILHO", ]

tipo_producao_por_distrito_tbl %>% glimpse()

# 5.1 Juncao entre shapefiles e dados de producao por tipo de cultura ----
moz_district@data %>% glimpse()

tipo_producao_por_distrito_merged_tbl <- merge(moz_district, tipo_producao_por_distrito_tbl, by = "prov_dist", all.x = F)

tipo_producao_por_distrito_merged_tbl@data %>% glimpse()


# using Bins
# range(tipo_producao_por_distrito_merged_tbl$total_qnt_tons, na.rm = T)
bins_quantile <- quantile(tipo_producao_por_distrito_merged_tbl$total_qnt_tons, na.rm = T)

round(bins_quantile[2], 0)

Bins <- c(0, round(bins_quantile[2],0), round(bins_quantile[3],0), round(bins_quantile[4],0), round(bins_quantile[5],0), Inf)
paletteBinned <- colorBin('YlGnBu', domain = tipo_producao_por_distrito_merged_tbl$total_qnt_tons, bins = Bins)
# paletteBinned <- colorBin('YlOrRd', domain = tipo_producao_provincia_merged_tbl$total_qnt_kg, bins = Bins)


producao_Labels <- sprintf('<b>%s</b><br/>%s ton.<br/>%s',
                           tipo_producao_por_distrito_merged_tbl$DISTRITO, 
                           prettyNum(round(tipo_producao_por_distrito_merged_tbl$total_qnt_tons, 2),big.mark = ","),
                           tipo_producao_por_distrito_merged_tbl$tipo_cultura) %>%
    lapply(htmltools::HTML)


m_distritos <- leaflet() %>%
    addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    # addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = tipo_producao_por_distrito_merged_tbl,
                
                # state border stroke color
                color = 'white', 
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                fillColor = ~paletteBinned(tipo_producao_por_distrito_merged_tbl$total_qnt_tons),
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
    ) %>% 
    addLegend(pal = paletteBinned , values = tipo_producao_por_distrito_merged_tbl$total_qnt_tons, title = '<small>Campanha Agricola 2020: Producao de MILHO<br>(ton| fonte: IAI 2020)</small>', position = 'topright')

m_distritos

