######################################
## 01_svr_dashboard_tab.R
######################################


# 1.0 dashboard_UI --------------------------------------------------------


get_num_total_entrevistas <- reactive({
    
    # No. total de entrevistas
    result <- 
    daily_interview_tbl %>% 
        summarise(no_total_entrevistas = sum(no_entrevistas))
    
    return(result$no_total_entrevistas)
    
})


get_total_tipo_exploracao_tbl <- reactive({

    result <-
    tipo_exploracao_tbl %>%
        group_by(a06) %>%
        summarise(no_total_tipo_exploracao = sum(n)) %>%
        ungroup()

    return(result)

})



output$apex_out_total_entrevistas_provincia <- renderApexchart({
    
    # plot No. total de entrevistas por provincia
    apex(
        data = total_entrevistas_provincia_tbl,
        type = "column",
        aes(x = provincia, y = no_total_entrevistas)
    ) %>%
        ax_title(text = "Total de entrevistas por Provincia") %>%
        ax_subtitle(text = "IAI 2020") %>%
        ax_dataLabels(enabled = TRUE) %>% 
        ax_xaxis(title = list(text = "Provincia")) %>% 
        ax_yaxis(title = list(text = "Numero de Questionario"))
    # ax_dataLabels(enabled = TRUE, dropShadow(enabled=TRUE))
})

# No. total de entrevistas por provincia
total_entrevistas_provincia_card <- tablerCard(
    apexchartOutput(outputId = "apex_out_total_entrevistas_provincia", height = "400px"),
    title = "Questionarios por Provincia",
    width = 6,
    status = "success",
    statusSide = "left"
    )


# performance card ------------------------------------------

output$plot_apex_ranking_provincia <- renderApexchart({
    
    apex(
        data = upa_entrevistas_performance_tbl[order(upa_entrevistas_performance_tbl$percent, decreasing = TRUE),],
        type = "bar",
        aes(x = provincia, y = percent)
    ) %>%
        ax_title(text = "no. AE's entrevistadas vs no. AE's planificadas") %>%
        ax_subtitle(text = "IAI 2020") %>%
        ax_dataLabels(enabled = TRUE) %>% 
        ax_xaxis(title = list(text = "Percentagem de Execucao (%)"))
    
})



output$plot_apex_comparacao_media_geral <- renderApexchart({
    
    # Comparação com a Média Geral
    apex(
        data = upa_entrevistas_performance_tbl,
        type = "column",
        aes(x = provincia, y = percent)
    ) %>%
        add_hline(value = mean(upa_entrevistas_performance_tbl$percent),
                  label = "Media Geral",
                  dash = 2) %>% 
        ax_title(text = "No. AE's entrevistadas vs no. AE's planificadas") %>%
        ax_subtitle(text = "IAI 2020") %>%
        ax_dataLabels(enabled = TRUE) %>% 
        ax_xaxis(title = list(text = "Percentagem de Execucao (%)"))
})


output$dt_tabela_performance <- renderDataTable({
    
    
    
    datatable(upa_entrevistas_performance_tbl[ order(upa_entrevistas_performance_tbl$percent, decreasing = TRUE), ]) %>%
        formatStyle(
            columns = "percent",
            background = styleColorBar(range(upa_entrevistas_performance_tbl$percent), 'lightblue'),
            backgroundSize = '98% 88%',
            backgroundRepeat = 'no-repeat',
            backgroundPosition = 'center'
        )
})

output$performance_output <- renderUI({
    switch (
        input$btn_performance,
        
        
        ranking_provincia = apexchartOutput(
            outputId = "plot_apex_ranking_provincia",
            width = "100%",
            height = "400px"
        ),
        
        comparacao_media_geral = apexchartOutput(
            outputId = "plot_apex_comparacao_media_geral",
            width = "100%",
            height = "400px"
        ),
        
        # tabela performance
        tabela_performance = DT::dataTableOutput(outputId = "dt_tabela_performance",
                                                 width = "100%",
                                                 height = "400px")
    )
})


# performance
performance_card <- tablerCard(
    uiOutput(outputId = "performance_output"),
    title = "Performance",
    width = 6,
    status = "success",
    statusSide = "left",
    options = tagList(
        prettyRadioButtons(
            inputId = "btn_performance",
            label = "Opcao:",
            choiceNames = c("Ranking", "Comparacao", "Tabela"),
            choiceValues = c("ranking_provincia", "comparacao_media_geral", "tabela_performance"),
            shape = "round",
            # shape = c("round", "square", "curve"),
            status = "danger",
            fill = TRUE,
            inline = TRUE
        )
    )
    
)



# No. de entrevistas diariamente por provincia -----------------------------------------------------

output$plot_apex_daily_interview_all_country <- renderApexchart({
    
    plot_country_tbl <- 
        daily_interview_tbl %>% 
        group_by(created_at) %>% 
        summarise(total_entrevistas = sum(no_entrevistas)) %>% 
        ungroup()
    
    
    apex(plot_country_tbl, aes(x =created_at, y= total_entrevistas), "column", height = "400px") %>% 
        # ax_dataLabels(enabled = TRUE, dropShadow = TRUE) %>%
        ax_dataLabels(enabled = TRUE) %>% 
        ax_title(text = "No. diario de Entrevistas em todo Pais") %>%
        ax_subtitle(text = "IAI 2020") %>%
        ax_xaxis(title = list(text = "Data")) %>% 
        ax_yaxis(title = list(text = "Numero de Entrevistas")) %>% 
        add_hline(value = mean(plot_country_tbl$total_entrevistas),
                  label = "Media diaria",
                  dash = 2)
})



output$plot_apex_daily_interview <- renderApexchart({
    
    get_plot_apex_daily_interview(input$in_nome_provincia)
    
    
})



# Tendencia por Provincia -------------------------------------------------

output$plot_trend <- renderPlot({
    
    ggplot(data = daily_interview_tbl, aes(x = created_at, y = no_entrevistas, fill = provincia)) +
        
        # geometries
        geom_col() +
        # geom_label(aes(label = scales::comma(no_entrevistas))) +
        geom_smooth(method = 'lm', se = FALSE) +
        
        # facet
        facet_wrap( ~ provincia, scales = "free_y") +
        
        # formatting
        scale_y_continuous(labels = scales::comma)
    
})


# # tendencia
# trend_card <- tablerCard(
#     plotOutput("plot_trend"),
#     title = "Tendencia",
#     width = 6,
#     status = "success",
#     statusSide = "left"
# )



output$daily_interview_output <- renderUI({
    
    switch (input$btn_trend,
            
            # no. de entrevista diariamente
            no_entrevistas = apexchartOutput(outputId = "plot_apex_daily_interview", width = "100%", height = "400px"),
            
            # tendencia
            tendencia =   plotOutput("plot_trend")
    )
    
    # get_plot_apex_daily_interview(input$in_nome_provincia)
    
    # v_provincia <<- input$in_nome_provincia
    
    # switch (v_provincia,
    #         "Todas Provincias" = apexchartOutput(outputId = "plot_apex_daily_interview_all_country", width = "100%", height = "400px"),
    #         apexchartOutput(outputId = "plot_apex_daily_interview", width = "100%", height = "400px")
    # )
    
    # if (input$in_nome_provincia == "Todas Provincias"){
    # 
    #     apexchartOutput(outputId = "plot_apex_daily_interview_all_country", width = "100%", height = "400px")
    # 
    # } else {
    # 
    #     apexchartOutput(outputId = "plot_apex_daily_interview", width = "100%", height = "400px")
    # }
    
    
})

daily_interview_card <- tablerCard(
    uiOutput(outputId = "daily_interview_output"),
    title = "Evolucao ao longo do tempo",
    width = 12,
    status = "success",
    statusSide = "left",
    options = tagList(
        prettyRadioButtons(
            inputId = "btn_trend",
            label = "Opcao:",
            choiceNames = c("No. de Entrevistas", "Tendencia"),
            choiceValues = c("no_entrevistas", "tendencia"),
            shape = "round",
            # shape = c("round", "square", "curve"),
            status = "danger",
            fill = TRUE,
            inline = TRUE
        )
    ),
    footer = tagList(
        column(
            width = 6,
            
            pickerInput(
                inputId = "in_nome_provincia",
                label = strong("Seleccionar a Provincia:"),
                choices = c("Todas Provincias", levels(upa_provincia_entrevistadas_tbl$provincia)),
                # selected = levels(dataframeTotal$countryName),
                # multiple = TRUE,
                selected = "Todas Provincias",
                width = "100%",
                options = list(`live-search` = TRUE
                               # `select-all-text` = "Yeah, all !"
                ),
                inline = F
            )
            
        )
        
    )
    
)




# Tipo de Exploracao ------------------------------------------------------

output$plot_apex_pie_chart_tipo_exploracao <- renderApexchart({
    
    
    
    # Pie
    apex(
        data = count(ibsa_sec_a_dat, a06),
        mapping = aes(x = to_factor(a06), y = n),
        type = "pie", height = "400px"
    ) %>%
        ax_plotOptions(
            pie = pie_opts(customScale = 0.5)
        ) %>% 
        ax_title(text = "Tipo de Exploracao em todo Pais") %>% 
        ax_subtitle(text = "IAI 2020")
})


output$plot_hc_pie_chart_tipo_exploracao <- renderHighchart({
    
    # count(ibsa_sec_a_dat, a06) %>% 
    count(ibsa_sec_a_joined_tbl, a06) %>% 
        hchart(
            "pie", hcaes(x = to_factor(a06), y = (n/sum(n))*100),
            name = "Tipo de Exploracao"
        ) %>% 
        hc_tooltip(crosshairs=TRUE, borderWidth=5, sort=TRUE, shared=TRUE, table=TRUE,
                   pointFormat=paste('<br><b>Tipo de Exploracao: {point.percentage:.1f}%</b><br>{point.text}'))
    
})

tipo_exploracao_pie_chart_card <- tablerCard(
    # apexchartOutput(
    #     outputId = "plot_apex_pie_chart_tipo_exploracao",
    #     width = "100%",
    #     height = "400px"
    # ),
    highchartOutput(outputId = "plot_hc_pie_chart_tipo_exploracao",
                    width = "100%",
                    height = "400px"),
    title = "Tipo de Exploracao",
    width = 5,
    status = "success",
    statusSide = "left"
    
)


output$plot_apex_tipo_exploracao_provincia <- renderApexchart({
    
    tipo_exploracao_tbl <-
        ibsa_sec_a_dat %>%
        count(prov, a06) 
    
    # Column
    apex(
        data = tipo_exploracao_tbl,
        mapping = aes(x = to_factor(prov), y = n, group = to_factor(a06)),
        type = "column", height = "400px"
    ) %>% 
        # ax_dataLabels(enabled = TRUE) %>% 
        ax_title(text = "Tipo de Exploracao por Provincia") %>% 
        ax_subtitle(text = "IAI 2020")
})

tipo_exploracao_provincia_card <- tablerCard(
    apexchartOutput(
        outputId = "plot_apex_tipo_exploracao_provincia",
        width = "100%",
        height = "400px"
    ),
    title = "Tipo de Exploracao",
    width = 7,
    status = "success",
    statusSide = "left"
    
)


output$table_out_No_entrevistas_AE <- renderReactable({
    
    ibsa_sec_a_joined_tbl %>% 
        count(provincia, distrito, upa) %>% 
        reactable(groupBy = "provincia")
})

No_entrevistas_AE_card <- tablerCard(
    
reactableOutput(outputId = "table_out_No_entrevistas_AE"),
    title = "No. de entrevistas por AE",
    width = 6,
    status = "success",
    statusSide = "left"
    )

# dashboard_UI ------------------------------------------------------------

output$dashboard_UI <- renderUI({
    
    total_tipo_exploracao_tbl <- get_total_tipo_exploracao_tbl()
    
    
    tagList(
        
       
        
        h5(strong("Seccao A - Identificacao da Exploracao")),
        # br(),
        
        
        
        fluidRow(
            
            # tablerInfoCard(
            #     value = "132 sales",
            #     status = "danger",
            #     icon = "dollar-sign",
            #     description = "12 waiting payments",
            #     width = 3
            # ),
            
            tablerStatCard(
                # value = prettyNum(paste("$", get_accumulated_ThisYear()), big.mark = ",") ,
                value = prettyNum(get_num_total_entrevistas(), big.mark = ","),
                title = "Total de Entrevistas (PE + ME)",
                # trend = round(22.75, 2),
                width = 3
                # ,icon("dollar-sign",lib='font-awesome')
            ),
            
            tablerStatCard(
                # value = prettyNum(get_quantidade_recarregamento(), big.mark = ","),
                value = prettyNum(total_tipo_exploracao_tbl[ total_tipo_exploracao_tbl$a06 == 1, 2], big.mark = ",") ,
                title = "PE: no. total de Entrevistas",
                # trend = 0,
                width = 3
            ),
            
            tablerStatCard(
                # value = scales::comma(lastDay_prep_subscribers_tbl()$Activos, big.mark = ','),
                value = prettyNum(total_tipo_exploracao_tbl[ total_tipo_exploracao_tbl$a06 == 2, 2], big.mark = ","),
                title = "ME: no. total de Entrevistas",
                # trend = 0,
                width = 3
            ),
            
            tablerStatCard(
                value = 0,
                title = "GE: no. total de Entrevistas",
                # trend = 0,
                width = 3)
        ),
        
        fluidRow(
            # cards: No. total de entrevistas por provincia
            total_entrevistas_provincia_card,
            
            # performance
            performance_card 
            
        ),
        
        fluidRow(
            # no. de entrevistas diariamente por provincia
            daily_interview_card
            
            
            # # tendencia por provincia
            # trend_card
            
        ),
        
        fluidRow(
            # pie chart
            tipo_exploracao_pie_chart_card,
            
            # tipo de exploracao por provincia
            tipo_exploracao_provincia_card
            
        ),
        
        fluidRow(
            
            No_entrevistas_AE_card
        )
        
    )
    
    
})