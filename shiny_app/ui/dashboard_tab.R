#######################################
## dashboard_tab.R
##########################################

tabText1 <- "Dashboard"

dashboard_tab <- tablerTabItem(
    tabName = "dashboard",
    # tabText1
    uiOutput("dashboard_UI") %>% withSpinner()
    
)
