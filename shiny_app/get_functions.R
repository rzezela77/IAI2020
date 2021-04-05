##############################################################
### get_functions.R
##############################################################

library(apexcharter)


get_plot_apex_daily_interview <- function(provincia) {
    # dataset <- data
    
    # v_provincia <- as.character(provincia)
    
    v_provincia <- provincia
    
    # v_title <- paste("No. diario de Entrevistas:", v_provincia)
    
    if (v_provincia == "Todas Provincias") {
        plot_country_tbl <-
            daily_interview_tbl %>%
            group_by(created_at) %>%
            summarise(total_entrevistas = sum(no_entrevistas)) %>%
            ungroup()
        
        
        
        apex_out <-
            apex(
                plot_country_tbl,
                aes(x = created_at, y = total_entrevistas),
                "column",
                height = "400px"
            ) %>%
            # ax_title(text = "No. diario de Entrevistas em todo Pais") %>% 
            # ax_dataLabels(enabled = TRUE, dropShadow = TRUE) %>%
            ax_dataLabels(enabled = TRUE) %>%
            add_hline(
                value = mean(plot_country_tbl$total_entrevistas),
                label = "Media diaria",
                dash = 2
            ) 
            
        
    } else{
        plot_data <-
            daily_interview_tbl[daily_interview_tbl$provincia == v_provincia &
                                    daily_interview_tbl$created_at <= Sys.Date(),]
        
        
        apex_out <-
            apex(plot_data,
                 aes(x = created_at, y = no_entrevistas),
                 "column",
                 height = "400px") %>%
            # ax_title(text = paste("No. diario de Entrevistas:", v_provincia)) %>% 
            # ax_dataLabels(enabled = TRUE, dropShadow = TRUE) %>%
            ax_dataLabels(enabled = TRUE) %>%
            add_hline(
                value = mean(plot_data$no_entrevistas),
                label = "Media diaria",
                dash = 2
            ) 
            
        
    }
    
    apex_out %>%
        # ax_title(text = v_title) %>%
        ax_title(text = "No. diario de Entrevistas") %>%
        ax_subtitle(text = "IAI 2020") %>%
        ax_xaxis(title = list(text = "Data")) %>%
        ax_yaxis(title = list(text = "Numero de Entrevistas"))
    
}
