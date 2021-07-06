#######################################
## mapa_tab.R
##########################################

tabText4 <- "Mapa"

mapa_tab <- tablerTabItem(
    tabName = "mapa",
    # tabText4
    uiOutput("mapa_UI") %>% withSpinner()
    
)