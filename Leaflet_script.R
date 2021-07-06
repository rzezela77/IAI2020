
# 05 Leaflet --------------------------------------------------------------

# load libraries
library(leaflet)
library(rgdal)
library(tidyverse)
library(haven)
library(htmltools)


m <- leaflet() %>% 
    addTiles() %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4)

m


# adding Provider Tiles
m <- leaflet() %>% 
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4)

m


# 5.1 Adding Shapefiles ---------------------------------------------------

moz_province <- readOGR("shiny_app/00_data/Provincias/Provincias.shp")

moz_district <- readOGR("shiny_app/00_data/Distritos/Distritos.shp")

moz_district %>% glimpse()

moz_district@data

# adding Provider Tiles and shapefiles
m <- leaflet() %>% 
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = moz_district,
                color = "#660000",
                weight = 1,
                smoothFactor = 1)

m

moz_district$DISTRITO


# Province

moz_province %>% glimpse()

moz_province %>% head()

moz_province@data

summary(moz_province)




# province
m <- leaflet() %>% 
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = moz_province,
                color = "#660000",
                weight = 1,
                smoothFactor = 1)

m



# 5.2 Producao Agricola ---------------------------------------------------

ibsa_sec_g_dat <- read_dta("shiny_app/00_data/sec_g.dta")

ibsa_sec_g_dat %>% glimpse()



library(readxl)
amostra_harmonizada_tbl <- read_excel("shiny_app/00_data/amostra_harmonizada_v3.xls")

# View(amostra_harmonizada_tbl)

amostra_harmonizada_tbl %>% glimpse()



# 5.3 Juntar as tabelas entre ibsa2020 e Amostra -------------------------


# Joining tables

amostra_modificada_tbl <- amostra_harmonizada_tbl[ , 2:12]

amostra_modificada_tbl <- amostra_modificada_tbl %>% unique()

amostra_modificada_tbl$provincia <- factor(amostra_modificada_tbl$provincia, levels = c("Niassa", "Cabo Delgado", "Nampula", "Zambezia", "Tete", "Manica", "Sofala", "Inhambane", "Gaza", "Maputo Provincia"))


ibsa_sec_g_joined_tbl <-
    ibsa_sec_g_dat %>%
    left_join(amostra_modificada_tbl,
              by = c("upa" = "ibsa_id"))

ibsa_sec_g_joined_tbl %>% glimpse()


# 5.4 Data Manipulation ---------------------------------------------------

library(labelled)

producao_provincia_tbl <- 
ibsa_sec_g_joined_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), tipo_cultura = g00) %>% 
    summarise(total_qnt_kg = sum(qntkg, na.rm = TRUE)) %>% 
    ungroup()

producao_provincia_tbl %>% glimpse()
    


producao_provincia_arroz_tbl <- 
producao_provincia_tbl %>% 
    filter(tipo_cultura == "ARROZ")

producao_provincia_arroz_tbl %>% glimpse()

unique(producao_provincia_arroz_tbl$PROVINCIA)

is.element(producao_provincia_arroz_tbl$PROVINCIA, moz_province$PROVINCIA) %>% 
    all()

unique(producao_provincia_tbl$PROVINCIA)

unique(amostra_modificada_tbl$provincia)

# Joining with shapefiles column name

moz_province$PROVINCIA

setdiff(moz_province$PROVINCIA, producao_provincia_arroz_tbl$provincia_nome)

setdiff(producao_provincia_arroz_tbl$provincia_nome, moz_province$PROVINCIA)

which(producao_provincia_arroz_tbl$provincia_nome == "CABO DELGADO")


# Merge shapefiles with production data files -------

# ?is.element

is.element(producao_provincia_tbl$PROVINCIA, moz_province$PROVINCIA) %>% 
    all()


producao_provincia_tbl %>% glimpse()

# rename all levels
levels(producao_provincia_tbl$PROVINCIA) <- c("NIASSA", "CABO_DELGADO", "NAMPULA", "ZAMBEZIA", "TETE", "MANICA", "SOFALA", "INHAMBANE", "GAZA", "MAPUTO")

is.element(producao_provincia_tbl$PROVINCIA, moz_province$PROVINCIA) %>% 
    all()


# producao_provincia_merged_tbl <- merge(moz_province, producao_provincia_tbl, by = "PROVINCIA")


producao_provincia_arroz_merged_tbl <- merge(moz_province, producao_provincia_arroz_tbl, by = "PROVINCIA", all.x = F)


producao_provincia_merged_tbl[ producao_provincia_merged_tbl$tipo_cultura == "ARROZ", ]


moz_province@data %>% 
    left_join(producao_provincia_tbl, by = "PROVINCIA")



# is.element(producao_provincia_arroz_tbl$provincia_nome, moz_province$PROVINCIA)
# 
# 
# producao_provincia_arroz_tbl[c(2,10), ]
# 
# 
# unique(producao_provincia_tbl$tipo_cultura)
# 
# 
# producao_provincia_tbl$provincia_nome[which(producao_provincia_tbl$provincia_nome == "CABO DELGADO"),] <- as.factor("CABO_DELGADO")
# 
# 
# producao_provincia_tbl[which(producao_provincia_tbl$provincia_nome == "MAPUTO PROVINCIA") ,]
# 
# producao_provincia_tbl$provincia_nome[which(producao_provincia_tbl$provincia_nome == "MAPUTO PROVINCIA"),] <- as"MAPUTO"
# 
# is.element(producao_provincia_tbl$provincia_nome, moz_province$PROVINCIA) %>% 
#     all()
# 
# producao_provincia_tbl[55 , ]
# 
# 
# moz_province$PROVINCIA

# 5.5 Map visualization ---------------------------------------------------

# Creating a color palette based on the number range in the total column
pal <- colorNumeric("Greens", domain=producao_provincia_arroz_tbl$total_qnt_kg)

### Create five colors for fill
mypal <- colorQuantile(palette = "RdYlBu", domain =producao_provincia_arroz_tbl$total_qnt_kg, n = 5, reverse = TRUE)


# province
m <- leaflet() %>% 
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = moz_province,
                # color = "#660000",
                fillColor = ~pal(producao_provincia_arroz_tbl$total_qnt_kg), 
                weight = 1,
                smoothFactor = 0.5,
                fillOpacity = 0.7
                # color = ~colorQuantile("YlOrRd", producao_provincia_arroz_tbl$total_qnt_kg)(producao_provincia_arroz_tbl$total_qnt_kg)
                # color = ~colorNumeric("YlOrRd", producao_provincia_arroz_tbl$total_qnt_kg)(producao_provincia_arroz_tbl$total_qnt_kg),
                # color = ~colorBin("YlOrRd", producao_provincia_arroz_tbl$total_qnt_kg)(producao_provincia_arroz_tbl$total_qnt_kg)
                )

m






# 5.5 R for Journalism: Choropleth Maps -----------------------------------

leaflet(moz_province) %>% 
    addTiles() %>% 
    addPolygons(popup = ~PROVINCIA)




# 5.6 Making Interactive Maps in R with Leaflet ---------------------------

producao_provincia_arroz_merged_tbl <- merge(moz_province, producao_provincia_arroz_tbl, by = "PROVINCIA", all.x = F)


# paletteNum <- colorNumeric('Blues', domain = producao_provincia_arroz_merged_tbl$total_qnt_kg)

paletteNum <- colorNumeric('Greens', domain = producao_provincia_arroz_merged_tbl$total_qnt_kg)

# Prepare the text for tooltips:
mylabels <- paste(
    "Provincia: ", producao_provincia_arroz_merged_tbl$PROVINCIA,"<br/>", 
    "Tipo de Cultura: ", producao_provincia_arroz_merged_tbl$tipo_cultura, "<br/>", 
    "Quantidade em kg: ", round(producao_provincia_arroz_merged_tbl$total_qnt_kg, 2), 
    sep="") %>%
    lapply(htmltools::HTML)

# or
producao_Labels <- sprintf('<b>%s</b><br/>%s kg<br/>%s',
                           producao_provincia_arroz_merged_tbl$PROVINCIA, 
                           prettyNum(round(producao_provincia_arroz_merged_tbl$total_qnt_kg, 2),big.mark = ","),
                           producao_provincia_arroz_merged_tbl$tipo_cultura) %>%
    lapply(function(x) HTML(x))


# using Bins
Bins <- c(0, 50000, 100000, 150000, 200000, Inf)
# paletteBinned <- colorBin('YlGnBu', domain = producao_provincia_arroz_merged_tbl$total_qnt_kg, bins = Bins)
paletteBinned <- colorBin('YlOrRd', domain = producao_provincia_arroz_merged_tbl$total_qnt_kg, bins = Bins)

m <- leaflet() %>%
    # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = producao_provincia_arroz_merged_tbl,
                
                # state border stroke color
                color = 'white', 
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                fillColor = ~paletteNum(producao_provincia_arroz_merged_tbl$total_qnt_kg),
                # fillColor = ~paletteBinned(producao_provincia_arroz_merged_tbl$total_qnt_kg),
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
                ) %>% 
    addLegend(pal = paletteNum, values = producao_provincia_arroz_merged_tbl$total_qnt_kg, title = '<small>Campanha Agricola 2020: Producao de Arroz<br>(kg| fonte: IAI 2020)</small>', position = 'bottomleft')
    # addLegend(pal = paletteBinned , values = producao_provincia_arroz_merged_tbl$total_qnt_kg, title = '<small>Campanha Agricola 2020: Producao de Arroz<br>(kg| fonte: IAI 2020)</small>', position = 'bottomleft')

m



# 5.7 Mapeamento por Distrito ---------------------------------------------

producao_distrito_tbl <- 
    ibsa_sec_g_joined_tbl %>% 
    group_by(PROVINCIA = to_factor(prov), DISTRITO = toupper(distrito), tipo_cultura = g00) %>% 
    summarise(total_qnt_kg = sum(qntkg, na.rm = TRUE)) %>%
    # rename(DISTRITO = toupper(distrito)) %>% 
    ungroup()

producao_distrito_tbl %>% glimpse()

? as.factor

producao_distrito_tbl$DISTRITO <- as.factor(as.character(producao_distrito_tbl$DISTRITO))

moz_district@data %>% glimpse()

moz_district$DISTRITO %>% glimpse()

# capital letters
# is.element(producao_distrito_tbl$distrito, moz_district$DISTRITO)

# split por culturas
producao_distrito_arroz_tbl <- producao_distrito_tbl[ producao_distrito_tbl$tipo_cultura == "ARROZ", ]

producao_distrito_arroz_tbl %>% glimpse()

# check if their matches
is.element(producao_distrito_arroz_tbl$DISTRITO, moz_district$DISTRITO) %>% 
    all()

# using toupper function
is.element(toupper(producao_distrito_arroz_tbl$distrito), moz_district$DISTRITO)

# toupper("Zezela")

producao_distrito_arroz_tbl %>% head()

producao_distrito_arroz_tbl[c(43, 44), ]

moz_district@data[moz_district@data$DISTRITO == toupper("Alto Molocue") , ]

# remove NA values
producao_distrito_arroz_tbl <- producao_distrito_arroz_tbl %>% na.omit()

producao_distrito_arroz_tbl[producao_distrito_arroz_tbl$distrito == "Montepuez",]

producao_distrito_arroz_tbl$distrito[which(producao_distrito_arroz_tbl$distrito == "Montepuez")] <- "Maua"

ibsa_sec_g_joined_tbl %>% glimpse()

ibsa_sec_g_joined_tbl[ibsa_sec_g_joined_tbl$distrito == as.character("Cuamba"), ]


# create maps per district

# producao_distrito_arroz_tbl %>% glimpse()
# 
# producao_distrito_arroz_tbl$DISTRITO <- toupper(producao_distrito_arroz_tbl$distrito)
# 
# producao_distrito_arroz_tbl <- producao_distrito_arroz_tbl[, -2]

moz_district@data

producao_distrito_arroz_merged_tbl <- merge(moz_district, producao_distrito_arroz_tbl, by = "DISTRITO")

producao_distrito_arroz_merged_tbl <- merge(moz_district, producao_distrito_arroz_tbl, by.x = c("DISTRITO", "FIRST_PROV"), by.y = c("DISTRITO", "PROVINCIA"))


producao_distrito_arroz_merged_tbl@data %>% glimpse()


# ?left_join
# ?merge


producao_Labels <- sprintf('<b>%s</b><br/>%s kg<br/>%s',
                           producao_distrito_arroz_merged_tbl$DISTRITO, 
                           prettyNum(round(producao_distrito_arroz_merged_tbl$total_qnt_kg, 2),big.mark = ","),
                           producao_distrito_arroz_merged_tbl$tipo_cultura) %>%
    lapply(htmltools::HTML)


paletteNum <- colorNumeric('Blues', domain = producao_distrito_arroz_merged_tbl$total_qnt_kg)

# using Bins
range(producao_distrito_arroz_merged_tbl$total_qnt_kg, na.rm = T)


Bins <- c(0, 5000, 15000, 25000, 35000, Inf)
# paletteBinned <- colorBin('YlGnBu', domain = producao_provincia_arroz_merged_tbl$total_qnt_kg, bins = Bins)
paletteBinned <- colorBin('YlOrRd', domain = producao_distrito_arroz_merged_tbl$total_qnt_kg, bins = Bins)


m <- leaflet() %>%
    # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    addProviderTiles(providers$Stamen.Toner) %>% 
    setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
    addPolygons(data = producao_distrito_arroz_merged_tbl,
                
                # state border stroke color
                color = 'white', 
                
                # soften the weight of the state borders
                weight = 1, 
                
                # values >1 simplify the polygons' lines for less detail but faster loading
                smoothFactor = .3, 
                
                # set opacity of polygons
                fillOpacity = .75, 
                
                # specify that the each state should be colored per paletteNum()
                # fillColor = ~paletteNum(producao_distrito_arroz_merged_tbl$total_qnt_kg),
                fillColor = ~paletteBinned(producao_distrito_arroz_merged_tbl$total_qnt_kg),
                
                # label = mylabels,
                label = producao_Labels,
                
                labelOptions = labelOptions(
                    style = list(color = 'gray30'),
                    textsize = '12px'),
                highlightOptions = highlightOptions(
                    weight = 3,
                    color = 'dodgerblue')
    ) %>% 
    # addLegend(pal = paletteNum, values = producao_distrito_arroz_merged_tbl$total_qnt_kg, title = '<small>Campanha Agricola 2020: Producao de Arroz<br>(kg| fonte: IAI 2020)</small>', position = 'topright')
 addLegend(pal = paletteBinned , values = producao_distrito_arroz_merged_tbl$total_qnt_kg, title = '<small>Campanha Agricola 2020: Producao de Arroz<br>(kg| fonte: IAI 2020)</small>', position = 'topright')

m
