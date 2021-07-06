#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tablerDash)
library(shinyEffects)
# library(echarts4r)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)



source(file = "global.R", local = TRUE)
source(file = "get_functions.R", local = TRUE)
# 
# # elements
source(file = "ui/dashboard_tab.R", local = TRUE)
source(file = "ui/grandes_exploracoes_tab.R", local = TRUE)
source(file = "ui/producao_tab.R", local = TRUE)
source(file = "ui/mapa_tab.R", local = TRUE)
# source(file = "ui/map_tab.R", local = TRUE)




shiny::shinyApp(
    ui = tablerDashPage(
        
        # theme = bslib::bs_theme(bootswatch = "darkly"),
        
        navbar = tablerDashNav(
            id = "mymenu",
            src = "https://preview-dev.tabler.io/static/logo.svg",
            # src = "~/projects/IAI2020/image/img01_plano_nacional_producao.PNG",
            navMenu = tablerNavMenu(
                tablerNavMenuItem(
                    tabName = "dashboard",
                    icon = "home",
                    "Dashboard"
                )
                ,tablerNavMenuItem(
                    tabName = "GE",
                    icon = "box",
                    "Grandes Explorações"
                )
                ,tablerNavMenuItem(
                    tabName = "producao",
                    icon = "box",
                    "Produção"
                )
                ,tablerNavMenuItem(
                    tabName = "mapa",
                    icon = "box",
                    "Mapa"
                )
            )
        ),
        
        footer = tablerDashFooter(
            tablerIcon(name = "maestro", lib = "payment"),
            tablerIcon(name = "mastercard", lib = "payment"),
            copyrights = "@Reinaldo Zezela, 2021"
        ),
        
        title = "Inquérito Agrário Integrado 2020",
        
        body = tablerDashBody(
            # tags$link(
            #     rel = "stylesheet",
            #     type = "text/css",
            #     href = "meu.css"
            # ),
            # tags$link(
            #     rel = "stylesheet",
            #     href= "https://fonts.googleapis.com/css2?family=Sansita+Swashed:wght@500&display=swap"
            # ),
            
            setZoom(class = "card"),
            chooseSliderSkin("Nice"),
            
            tablerTabItems(
                dashboard_tab,
                grandes_exploracoes_tab,
                producao_tab,
                mapa_tab
            )
        )
    ),
    
    server = function(input, output) {
        
        source(file = "server/01_svr_dashboard_tab.R", local = TRUE)
        
        source(file = "server/04_svr_mapa_tab.R", local = TRUE)
    }
)