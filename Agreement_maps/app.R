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
         sliderInput("bio",
                     "Biomass:",
                     min = 0,
                     max = 10,
                     value = 1),
         sliderInput("cp",
                     "Coastal Protection:",
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
                     "Lobster:",
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

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      root2 = root %>% 
        mutate(agreement = input$cp*cv_rs + input$rec*rec_rs + input$lob*lob_rs + input$bio*targ_rs + input$con*connect_rs)
      
      # draw the histogram with the specified number of bins
      ggplot() +
        geom_sf(data = coastline) +
        geom_sf(data = root2, mapping = aes(fill = agreement), alpha = 0.5) +
        scale_fill_gradient(name="Priority areas",
                             #breaks = c(50, 950),
                             #labels = c("Low", "High"),
                             low="white", high="darkred") +
        coord_sf(y=c(15.8, 18.5), x = c(-89, -87.3)) +
        #labs(fill = "Biomass (non-MPA)") +
        theme_bw() + base_theme 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

