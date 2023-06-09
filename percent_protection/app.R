#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)

##Data for app
##Root results
root = read_sf('data', layer = '06_prot_corl_30000ha_all') %>% st_transform(4326)
coastline = read_sf('data', layer = 'mar_coastline') %>% st_transform(4326)
#replenishment = read_sf('data', layer = 'bz_fish_replenishment_areas') %>% st_transform(4326)
replenishment = read_sf('data', layer = 'exsiting_HPCZ') %>% st_transform(4326)
reef = read_sf('data', layer = 'Reef_habitats_Belize') %>% st_transform(4326)


##Biomass and catch estimates
bio <- read_csv("data/bio_catch_estimates.csv") %>% 
  dplyr::select(SDU_ID, bio, bio_kg_ha, bio_total, est_catch, est_catch_rs)

root = root %>%
  left_join(bio)

##Connectivity
connectivity <- read_csv("data/connectivity.csv")

root = root %>%
  left_join(connectivity) %>% 
  mutate(tot_score = if_else(is.na(tot_score), 0, tot_score))

##Agreement2
agreement2 <- read_csv("data/agreement2.csv")

root = root %>%
  left_join(agreement2)

##Dta
root_dta = root %>% 
  st_drop_geometry()

##Calculate lobster fisheries benefits
root_dta2 = root_dta %>% 
  mutate(total = sum(root_dta$lob),
         fish = total-lob)

lob_benefit = root_dta2 %>% 
  mutate(fis_rs = (fish-min(root_dta2$fish))/(max(root_dta2$fish) - min(root_dta2$fish))) %>% 
  dplyr::select(SDU_ID, total, fish, fis_rs)

root = root %>% 
  left_join(lob_benefit)

##Dta
root_dta = root %>% 
  st_drop_geometry()

##Maps
# Base theme
base_theme <- theme(legend.title = element_text(size = 10),
                    legend.text = element_text(size = 10),
                    plot.title = element_text(size = 13),
                    axis.text=element_blank(),
                    axis.title=element_blank(),
                    strip.text=element_blank(),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    #legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Agreement maps"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      checkboxInput("checkbox", "Show reef layer", value = FALSE),
      
      selectInput("var", 
                  label = "Reef protection target",
                  choices = c("No target",
                              "5% protection", 
                              "10% protection",
                              "15% protection", 
                              "20% protection",
                              "25% protection", 
                              "30% protection"),
                  selected = "10% protection"),
      
      helpText("Define importance of each ecosystem service"),
      
      sliderInput("bio",
                  "Reef fish biomass:",
                  min = 0,
                  max = 10,
                  value = 1),
      
      sliderInput("cp",
                  "Coastal protection:",
                  min = 0,
                  max = 10,
                  value = 1),
      
      sliderInput("rec",
                  "Tourism:",
                  min = 0,
                  max = 10,
                  value = 1),
      
      sliderInput("con",
                  "Connectivity:",
                  min = 0,
                  max = 10,
                  value = 1),
      
      sliderInput("lob",
                  "Lobster fisheries:",
                  min = 0,
                  max = 10,
                  value = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$var == "No target"){
      
      root2 = root %>% 
        mutate(agreement = 
                 input$cp*cv_rs + 
                 input$rec*rec_rs + 
                 input$lob*fis_rs + 
                 input$bio*targ_rs + 
                 input$con*connect_rs)
      
      # plot map
      if(input$checkbox == "TRUE"){
      
      ggplot() +
        geom_sf(data = coastline) +
        geom_sf(data = reef, alpha = 0.2, color = "grey") +
        geom_sf(data = replenishment, alpha = 0.5, aes(colour = "No-take\nzone")) +
        geom_sf(data = root2, mapping = aes(fill = agreement), alpha = 0.5) +
        scale_fill_gradient(name="Priority areas\n ",
                            breaks = c(min(root2$agreement), max(root2$agreement)),
                            labels = c("Low", "High"),
                            low="white", high="darkred") +
        scale_colour_manual(values = "darkblue", name = "") +
        coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
        #labs(fill = "Biomass (non-MPA)") +
        theme_bw() + base_theme +
          theme(plot.margin = unit(c(0,0,0,0), "cm"),
                legend.text = element_text(size = 18),
                legend.title = element_text(size = 18))}
        else{
          ggplot() +
            geom_sf(data = coastline) +
            #geom_sf(data = reef, alpha = 0.2, color = "grey") +
            geom_sf(data = replenishment, alpha = 0.5, aes(colour = "No-take\nzone")) +
            geom_sf(data = root2, mapping = aes(fill = agreement), alpha = 0.5) +
            scale_fill_gradient(name="Priority areas\n ",
                                breaks = c(min(root2$agreement), max(root2$agreement)),
                                labels = c("Low", "High"),
                                low="white", high="darkred") +
            scale_colour_manual(values = "darkblue", name = "") +
            coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
            #labs(fill = "Biomass (non-MPA)") +
            theme_bw() + base_theme +
            theme(plot.margin = unit(c(0,0,0,0), "cm"),
                  legend.text = element_text(size = 18),
                  legend.title = element_text(size = 18)) 
        }
        
    }else{
      
    # generate bins based on input$bins from ui.R
    root2 = root %>% 
      mutate(agreement = 
               input$cp*cv_rs + 
               input$rec*rec_rs + 
               input$lob*fis_rs + 
               input$bio*targ_rs + 
               input$con*connect_rs) %>% 
      arrange(desc(agreement)) %>% 
      mutate(row_order = 1:nrow(root),
             is_priority = if_else(input$var == "5% protection" & row_order<=round(0.05*602), "High",
                                   if_else(input$var == "10% protection" & row_order<=round(0.1*602), "High",
                                           if_else(input$var == "15% protection" & row_order<=round(0.15*602), "High", 
                                                   if_else(input$var == "20% protection" & row_order<=round(0.2*602), "High",
                                                           if_else(input$var == "25% protection" & row_order<=round(0.25*602), "High", 
                                                                   if_else(input$var == "30% protection" & row_order<=round(0.3*602), "High", "Low"))))))) %>% 
      filter(is_priority == "High")
    
    # root2 = root %>%
    #   mutate(agreement =
    #                    cv_rs +
    #                    rec_rs +
    #                    #lob_rs +
    #                    targ_rs +
    #                    connect_rs) %>%
    #   arrange(desc(agreement)) %>%
    #   mutate(row_order = 1:nrow(root),
    #                  is_priority = if_else(var1 == "5% protection" & row_order<=round(0.05*602), "High",
    #                                                       if_else(var1 == "10% protection" & row_order<=round(0.1*602), "High",
    #                                                                                                    if_else(var1 == "15% protection" & row_order<=round(0.15*602), "High",
    #                                                                                                     if_else(var1 == "20% protection" & row_order<=round(0.2*602), "High",
    #                                                                                                      if_else(var1 == "25% protection" & row_order<=round(0.25*602), "High",
    #                                                                                                       if_else(var1 == "30% protection" & row_order<=round(0.3*602), "High", "Low"))))))) %>%
    # filter(is_priority == "High")

    perc_nt = as.data.frame(st_intersects(root2, replenishment))
    perc_nt2 = round(100*nrow(perc_nt)/nrow(root2))
    
    # plot map
    if(input$checkbox == "TRUE"){
      ggplot() +
        geom_sf(data = coastline) +
        geom_sf(data = reef, alpha = 0.2, color = "grey") +
        geom_sf(data = replenishment, alpha = 0.5, aes(colour = "No-take\nzone")) +
        geom_sf(data = root2, mapping = aes(fill = is_priority), alpha = 0.6) +
        coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
        scale_colour_manual(values = "darkblue", name = "") +
        annotate("text", x=-88.65, y=18, label= paste(perc_nt2, "% within current\n no-take zones",sep = ""), size = 5) +
        labs(fill = "Priority") +
        theme_bw() + base_theme +
        theme(plot.margin = unit(c(0,0,0,0), "cm"),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 18))
    }else{
      ggplot() +
        geom_sf(data = coastline) +
        #geom_sf(data = reef, alpha = 0.2) +
        geom_sf(data = replenishment, alpha = 0.5, aes(colour = "No-take\nzone")) +
        geom_sf(data = root2, mapping = aes(fill = is_priority), alpha = 0.6) +
        coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
        scale_colour_manual(values = "darkblue", name = "") +
        annotate("text", x=-88.65, y=18, label= paste(perc_nt2, "% within current\n no-take zones",sep = ""), size = 5) +
        labs(fill = "Priority") +
        theme_bw() + base_theme +
        theme(plot.margin = unit(c(0,0,0,0), "cm"),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 18))
    }

  }}, height = 950, width = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
