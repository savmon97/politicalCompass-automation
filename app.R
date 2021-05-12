# App for 531 googlesheets

# Library
library(shiny)
library(data.table)
library(ggplot2)
library(shinythemes)
library(rgdal)
library(plyr)
library(leaflet)
library(dplyr)
library(ggthemes)
library(tidyr)
library(leaflet)
library(rsconnect)
library(googlesheets4)
library(googledrive)
library(readxl)
library(janitor)


server <- function(input, output) {  
  # read in data from google sheet
  # Tell R that you are going to use a permission that
  # is saved in the file 'secrets'
  options(gargle_oauth_cache = ".secrets",
          gargle_oauth_email = TRUE)
  
  # authorize the scope to 'read only' - for privacy!
  sheets_auth(token = drive_token(),
              scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
  
  # Now we can open up that sheet
  sheets_get(
    "https://docs.google.com/spreadsheets/d/10qkioll_tIZnX55D1zQcfKi7hmZgAaH9tcDErv76mdY/edit?resourcekey#gid=1768506437"
             )
  
  # Bring it into the object gafl
  gafl <-read_sheet(
      "https://docs.google.com/spreadsheets/d/10qkioll_tIZnX55D1zQcfKi7hmZgAaH9tcDErv76mdY/edit?resourcekey#gid=1768506437",
      sheet = 1,
      col_types = "c",
      na = ""
      ) %>% as_tibble() %>% pivot_longer(cols = 3:64) %>% select(-c(1)) %>% 
    rename('initials' = 'Your Initials')
  
  ### Read in codebook
  codebook <- read_xlsx("codebook.xlsx") %>% as_tibble() %>% rename('name' = 'question')
 
  ## here we use tidyverse commands to apply weights based on responses
  gafl <- left_join(gafl, codebook, by = 'name' )
  gafl$score <- with(gafl, ifelse(value =='Strongly Disagree',-2, 
                                 ifelse( value == "Disagree", -1,
                                         ifelse(value == 'Agree', 1, 2))))
  gafl$weight <- with(gafl, ifelse(type == 'Economic', 1.5, 1)) #fixing for having more social compass questions
  gafl$final <- gafl$agree * gafl$score * gafl$weight
  
  gafl <- gafl %>% group_by(initials, type) %>% 
    summarize(final.x = sum(final)) %>% pivot_wider(names_from = type, values_from = final.x)
  
  
  
  
  #-------------------
  # Compass
  #-------------------

  compass<- ggplot(gafl, mapping = aes(x = Economic, y = Social)) +
    annotate("rect", xmin = 60, xmax = 0, 
             ymin = 60, ymax = 0, fill= "#60acff", alpha = 0.5)  + 
    annotate("rect", xmin = -60, xmax = 0, 
             ymin = -60, ymax = 0 , fill= "#f97c7c", alpha = 0.5) + 
    annotate("rect", xmin = 0, xmax = 60, 
             ymin = 0, ymax = -60, fill= "#fdfd96", alpha = 0.5) + 
    annotate("rect", xmin = 0, xmax = -60, 
             ymin = 60, ymax = 0, fill= "#a0e7a0", alpha = 0.5) + 
    annotate("segment", x = 0, xend = 0, y = -60, yend = 60, color = 'black') +
    annotate("segment", x = -60, xend = 60, y = 0, yend = 0, color = 'black')+
    geom_point() + theme_bw() + 
    geom_text(label=gafl$initials,
              position=position_jitter(width=3,height=2)) +
    theme(panel.grid = element_line(colour = "gray", size = 0.5)) + 
    xlab('Libertarian') + ylab('Socialist')+
    scale_y_continuous(expand = c(0, 0), breaks = seq(-60,60,10), 
                       sec.axis = dup_axis(name = 'Capitalist')) + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(-60,60,10), 
                       sec.axis = dup_axis(name = 'Authoritarian'))
  compass
  #compass_image <- png("compass_image.png")
  
  output$compass <- renderPlot(compass)
  
  
  }



#-----------------------------------#
# UI #
#-----------------------------------#
library(shinythemes)
library(leaflet)

ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Political Compass",
    tabPanel("Map",
             plotOutput(
               "compass", width = "750px", height = "750px"
             ))
  )
))

shinyApp(ui = ui, server = server)
