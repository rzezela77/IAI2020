#########################################################################################
### 04_svr_mapa_tab.R
#########################################################################################

filtered_provincia_data <- reactive({
    
    tipo_producao_sec_h_provincia_tbl <- 
    producao_sec_h_provincia_tbl[producao_sec_h_provincia_tbl$tipo_cultura == input$input_tipo_produto, ]
    
    # Juncao entre shapefiles e dados de producao por tipo de cultura ----
    result <-
        merge(moz_province,
              tipo_producao_sec_h_provincia_tbl,
              by = "PROVINCIA",
              all.x = F)
    
    return(result)
    
})





# criar mapa --------------------------------------------------------------

# output$producao_mapa <- renderLeaflet({
# 
#    # leaflet() %>%
#    #      addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
#    #      # addProviderTiles(providers$Stamen.TonerLite) %>%
#    #      # addTiles() %>%
#    #      setView(lng = 35.529562, lat = -18.665695, zoom = 4)
# 
# 
# 
#     paletteNum <- colorNumeric('Greens', domain = tipo_producao_sec_h_provincia_merged_tbl$total_qnt_kg/1000)
# 
# 
#     # labels
#     producao_Labels <- sprintf('<b>%s</b><br/>%s ton<br/>%s',
#                                tipo_producao_sec_h_provincia_merged_tbl$PROVINCIA,
#                                prettyNum(round(tipo_producao_sec_h_provincia_merged_tbl$total_qnt_kg/1000, 2),big.mark = ","),
#                                tipo_producao_sec_h_provincia_merged_tbl$tipo_cultura) %>%
#         lapply(function(x) HTML(x))
# 
# 
#     m_province <- leaflet() %>%
#         addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
#         # addProviderTiles(providers$Stamen.TonerLite) %>%
#         # addTiles() %>%
#         setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>%
#         addPolygons(data = tipo_producao_sec_h_provincia_merged_tbl,
# 
#                     # state border stroke color
#                     # color = 'white',
#                     color = "#660000",
# 
#                     # soften the weight of the state borders
#                     weight = 1,
# 
#                     # values >1 simplify the polygons' lines for less detail but faster loading
#                     smoothFactor = .3,
# 
#                     # set opacity of polygons
#                     fillOpacity = .75,
# 
#                     # specify that the each state should be colored per paletteNum()
#                     fillColor = ~paletteNum(tipo_producao_sec_h_provincia_merged_tbl$total_qnt_kg/1000),
# 
# 
#                     # label = mylabels,
#                     label = producao_Labels,
# 
#                     labelOptions = labelOptions(
#                         style = list(color = 'gray30'),
#                         textsize = '12px'),
#                     highlightOptions = highlightOptions(
#                         weight = 3,
#                         color = 'dodgerblue')
#         ) %>%
#         addLegend(pal = paletteNum, values = tipo_producao_sec_h_provincia_merged_tbl$total_qnt_kg/1000, title = paste0("<small>Campanha Agricola 2020 <br> Producao de ", unique(tipo_producao_sec_h_provincia_merged_tbl$tipo_cultura), " (ton.)<br>(fonte: IAI 2020)</small>"), position = 'bottomright')
# 
#     m_province
# 
# 
# })



output$mapa_provincia <- renderLeaflet({
    
    
    dataset <- filtered_provincia_data()
    
    
    paletteNum <- colorNumeric('Greens', domain = dataset$total_qnt_kg/1000)
    
    
    # labels
    producao_Labels <- sprintf('<b>%s</b><br/>%s ton<br/>%s',
                               dataset$PROVINCIA,
                               prettyNum(round(dataset$total_qnt_kg/1000, 2),big.mark = ","),
                               dataset$tipo_cultura) %>%
        lapply(function(x) HTML(x))
    
    
    m_province <- leaflet() %>%
        # addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        # addTiles() %>%
        setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>%
        addPolygons(data = dataset,
                    
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
                    fillColor = ~paletteNum(dataset$total_qnt_kg/1000),
                    
                    
                    # label = mylabels,
                    label = producao_Labels,
                    
                    labelOptions = labelOptions(
                        style = list(color = 'gray30'),
                        textsize = '12px'),
                    highlightOptions = highlightOptions(
                        weight = 3,
                        color = 'dodgerblue')
        ) %>%
        addLegend(pal = paletteNum, values = dataset$total_qnt_kg/1000, title = paste0("<small>Campanha Agricola 2020 <br> Producao de ", unique(dataset$tipo_cultura), " (ton.)<br>(fonte: IAI 2020)</small>"), position = 'bottomleft')
    
    m_province
    
    
})




# # reactive data
# output$producao_mapa <- renderLeaflet({
# 
# 
#     # observe({
#     #
#     dataset <- filtered_data()
#     
#     paletteNum <-
#         colorNumeric('Greens', domain = dataset$total_qnt_kg / 1000)
#     
#     
#     # labels
#     producao_Labels <- sprintf(
#         '<b>%s</b><br/>%s ton<br/>%s',
#         dataset$PROVINCIA,
#         prettyNum(round(dataset$total_qnt_kg / 1000, 2), big.mark = ","),
#         dataset$tipo_cultura
#     ) %>%
#         lapply(function(x)
#             HTML(x))
#     
#     
#     # leafletProxy("producao_mapa", data = filtered_data())  %>%
#     #     clearShapes() %>%
#     #     addPolygons(data = dataset,
#     #                 fillColor =  "red",
#     #                 popup = state_popup,
#     #                 color = "#BDBDC3",
#     #                 fillOpacity = 1,
#     #                 weight = 1)
#     
#     
#     addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
#         # addProviderTiles(providers$Stamen.TonerLite) %>%
#         # addTiles() %>%
#         setView(lng = 35.529562,
#                 lat = -18.665695,
#                 zoom = 4) %>%
#         
#         ## clear shapes
#         # clearShapes() %>%
#         
#         addPolygons(
#             data = dataset,
#             #data = dataset,
#             
#             # state border stroke color
#             # color = 'white',
#             color = "#660000",
#             
#             # soften the weight of the state borders
#             weight = 1,
#             
#             # values >1 simplify the polygons' lines for less detail but faster loading
#             smoothFactor = .3,
#             
#             # set opacity of polygons
#             fillOpacity = .75,
#             
#             # specify that the each state should be colored per paletteNum()
#             fillColor = ~ paletteNum(dataset$total_qnt_kg / 1000),
#             
#             
#             # label = mylabels,
#             label = producao_Labels,
#             
#             labelOptions = labelOptions(style = list(color = 'gray30'),
#                                         textsize = '12px'),
#             highlightOptions = highlightOptions(weight = 3,
#                                                 color = 'dodgerblue')
#         ) %>%
#         addLegend(
#             pal = paletteNum,
#             values = dataset$total_qnt_kg / 1000,
#             title = paste0(
#                 "<small>Campanha Agricola 2020 <br> Producao de ",
#                 unique(dataset$tipo_cultura),
#                 " (ton.)<br>(fonte: IAI 2020)</small>"
#             ),
#             position = 'bottomleft'
#         )
#     
# 
# })


# # Mapa de producao por Provincia e por tipo de culturas -------------------
#
#
# color_palette <- reactive({
#
#     dataset <- filtered_data()
#
#     colorNumeric('Greens', domain = dataset$total_qnt_kg/1000)
# })
#
#
# labels_map <- reactive({
#     
#     dataset <- filtered_data()
#     
#     sprintf('<b>%s</b><br/>%s ton<br/>%s',
#             dataset$PROVINCIA, 
#             prettyNum(round(dataset$total_qnt_kg/1000, 2),big.mark = ","),
#             dataset$tipo_cultura) %>%
#         lapply(function(x) HTML(x))
#     
# })
# 
# observe({
#     
#     dataset <- filtered_data()
#     
#     
#     paletteNum <- color_palette()
#     
#     
#     # labels
#     producao_Labels <- labels_map()
#     
#     
#     leafletProxy("producao_mapa") %>% 
#         clearControls() %>%
#         # addPolygons(data = dataset, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1) 
#         addPolygons(
#             # data = dataset,
# 
#                                     # state border stroke color
#                                     # color = 'white',
#                                     color = "#660000",
# 
#                                     # soften the weight of the state borders
#                                     weight = 1,
# 
#                                     # values >1 simplify the polygons' lines for less detail but faster loading
#                                     smoothFactor = .3,
# 
#                                     # set opacity of polygons
#                                     fillOpacity = .75,
# 
#                                     # specify that the each state should be colored per paletteNum()
#                                     fillColor = ~paletteNum(dataset$total_qnt_kg/1000),
# 
# 
#                                     # label = mylabels,
#                                     label = producao_Labels(),
# 
#                                     labelOptions = labelOptions(
#                                         style = list(color = 'gray30'),
#                                         textsize = '12px'),
#                                     highlightOptions = highlightOptions(
#                                         weight = 3,
#                                         color = 'dodgerblue')
#                         )%>%
#                         addLegend(pal = paletteNum, values = dataset$total_qnt_kg/1000, title = paste0("<small>Campanha Agricola 2020 <br> Producao de ", unique(dataset$tipo_cultura), " (ton.)<br>(fonte: IAI 2020)</small>"), position = 'bottomright')
#     
#     
#     
# })




# datatable Output --------------------------------------------------------

producao_provincia_dta <- reactive({
    
    dataset <- filtered_provincia_data()
    
    result <- 
        dataset@data %>% 
        group_by(provincia = PROVINCIA, tipo_cultura) %>% 
        summarise(total_qnt_tons = total_qnt_kg/1000) %>% 
        ungroup()
    
    return(result)
    
})



producao_distrito_dta <- reactive({
    
    dataset <- filtered_distrito_data()
    
    result <- 
        dataset@data %>% 
        group_by(provincia = PROVINCIA, distrito = DISTRITO, tipo_cultura) %>% 
        summarise(total_qnt_tons = total_qnt_kg/1000) %>% 
        ungroup()
    
    return(result)
    
})


output$producao_dta <- renderDataTable({
    
    
    # switch (input$btn_prov_dist,
    #         
    #         # criar mapa de Provincia
    #         provincia_viz = leafletOutput("mapa_provincia"),
    #         
    #         
    #         # criar mapa de Distrito
    #         distrito_viz = leafletOutput("mapa_distrito")
    #         
    #         
    # )
    
    if (input$btn_prov_dist == "provincia_viz"){
        
        dataset <- producao_provincia_dta()
        
    } else{
        
        dataset <- producao_distrito_dta()
        
    }
    
    
    
    datatable(dataset[ order(dataset$total_qnt_tons, decreasing = T), ]) %>%
        formatRound("total_qnt_tons") %>% 
        formatStyle(
            columns = "total_qnt_tons",
            background = styleColorBar(range(dataset$total_qnt_tons), 'lightgreen'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
        )
    
    
})




# Mapa de Distrito --------------------------------------------------------

filtered_distrito_data <- reactive({
    
    # criar tabela por tipo de cultura
    tipo_producao_sec_h_distrito_tbl <-
        producao_sec_h_distrito_tbl[producao_sec_h_distrito_tbl$tipo_cultura == input$input_tipo_produto, ]
    
    
    # Juncao entre shapefiles e dados de producao por tipo de cultura ----
    result <-
        merge(moz_district,
              tipo_producao_sec_h_distrito_tbl,
              by = "prov_dist",
              all.x = F)
    
    return(result)
    
})



output$mapa_distrito <- renderLeaflet({
    
    # leaflet() %>%
    #     addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
    #     # addProviderTiles(providers$Stamen.TonerLite) %>%
    #     # addTiles() %>%
    #     setView(lng = 35.529562,
    #             lat = -18.665695,
    #             zoom = 4) %>%
    # addPolygons(
    #     data = moz_district,
    #     fillColor =  "red",
    #     # popup = state_popup,
    #     color = "#BDBDC3",
    #     fillOpacity = 1,
    #     weight = 1
    # )
    
    
    dataset <- filtered_distrito_data()
    
    # Mapa por distritos usando Palette ----
    paletteNum <- colorNumeric('Greens', domain = dataset$total_qnt_kg/1000)
    
    
    producao_Labels <- sprintf('<b>%s</b><br/>%s ton.<br/>%s',
                               dataset$DISTRITO, 
                               prettyNum(round(dataset$total_qnt_kg/1000, 2),big.mark = ","),
                               dataset$tipo_cultura) %>%
        lapply(htmltools::HTML)
    
    
    m_distritos <- leaflet() %>%
        addProviderTiles(providers$CartoDB.PositronNoLabels)  %>%
        # addProviderTiles(providers$Stamen.Toner) %>% 
        setView(lng = 35.529562, lat = -18.665695, zoom = 4) %>% 
        addPolygons(data = dataset,
                    
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
                    fillColor = ~paletteNum(dataset$total_qnt_kg/1000),
                    
                    # label = mylabels,
                    label = producao_Labels,
                    
                    labelOptions = labelOptions(
                        style = list(color = 'gray30'),
                        textsize = '12px'),
                    highlightOptions = highlightOptions(
                        weight = 3,
                        color = 'dodgerblue')
        ) %>% 
        addLegend(pal = paletteNum, values = dataset$total_qnt_kg/1000, title = paste0("<small>Campanha Agricola 2020 <br> Producao de ", unique(dataset$tipo_cultura), " (ton.)<br>(fonte: IAI 2020)</small>"), position = 'bottomleft')
    
    m_distritos
    
    
})




# mapa_UI -----------------------------------------------------------------


output$default <- renderText({ 
    
    dataset <- filtered_data()
    
    dataset@data
    
    })


output$mapa_output <- renderUI({
    
    switch (input$btn_prov_dist,
            
            # criar mapa de Provincia
            provincia_viz = leafletOutput("mapa_provincia"),
            
            
            # criar mapa de Distrito
            distrito_viz = leafletOutput("mapa_distrito")
            
            
    )
    
   
    
})


mapa_card <- tablerCard(
    uiOutput(outputId = "mapa_output"),
    title = "Producao Agricola",
    width = 12,
    status = "success",
    statusSide = "left",
    options = tagList(
        
        pickerInput(
            inputId = "input_tipo_produto",
            label = strong("Tipo de Produto:"),
            choices = producao_sec_h_provincia_tbl$tipo_cultura %>% unique(),
            # selected = levels(dataframeTotal$countryName),
            # multiple = TRUE,
            # selected = "All",
            width = "100%",
            options = list(`live-search` = TRUE
                           # `select-all-text` = "Yeah, all !"
            ),
            inline = F
        )
        
       
    ),
    footer = tagList(
        column(
            width = 6,
            
            prettyRadioButtons(
                inputId = "btn_prov_dist",
                label = "Visualizar por:",
                choiceNames = c("Provincia", "Distrito"),
                choiceValues = c("provincia_viz", "distrito_viz"),
                shape = "round",
                # shape = c("round", "square", "curve"),
                status = "danger",
                fill = TRUE,
                inline = TRUE
            )
            
        )
        
    )
    
)






output$mapa_UI <- renderUI({
    
    tagList(
        
        # mapa
        fluidRow(
            
            mapa_card
            
            
            # # criar mapa
            # leafletOutput("producao_mapa"),
            # 
            # # painel para seleccionar
            # # absolutePanel(id = "input_control",class = "panel panel-default",bottom = 60, left = 10, draggable = F, width = 330, height = "auto",
            # 
            # absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
            #               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
            #               width = 330, height = "auto",
            # 
            # 
            #               h6("Producao Agricola"),
            # 
            #               # seleccionar os Itens
            #               pickerInput(
            #                   inputId = "input_tipo_produto",
            #                   label = strong("Tipo de Produto:"),
            #                   choices = producao_sec_h_provincia_tbl$tipo_cultura %>% unique(),
            #                   # selected = levels(dataframeTotal$countryName),
            #                   # multiple = TRUE,
            #                   # selected = "All",
            #                   width = "100%",
            #                   options = list(`live-search` = TRUE
            #                                  # `select-all-text` = "Yeah, all !"
            #                   ),
            #                   inline = F
            #               ),
            #               checkboxInput("legend", "Show legend", TRUE)
            # )
        ),
        
        # datatable
        fluidRow(
            
            dataTableOutput(outputId = "producao_dta", width = "100%", height = "auto")
            
        )
    )
})
