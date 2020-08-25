library(tidyverse)
library(httr)
library(rvest)
library(lubridate)
library(xml2)
library(xslt)
library(tm)
library(qdapRegex)
library(SnowballC)
library(RColorBrewer)
library(wordcloud2)
library(shiny)

#### R shiny app #### 
ui <- navbarPage("Band Analyzer", fluid = FALSE,    
      
      # Tab 1 title #########################################################
      tabPanel("Lyrics wordclouds", 
      
      # Sidebar
      sidebarLayout(   
        
        # Define the sidebar with inputs (2 Bands are choosable)
        sidebarPanel(selectInput("band1", "Compare ...", 
                                 choices= names(FLL), 
                                 selected = "Elvis Presley"), 
          
                     selectInput("band2", "with ...", 
                                 choices= names(FLL), 
                                 selected = "AC/DC"),
          
          #horizontal line
          hr(),
          
          helpText("Some notes: Lyrics have been scraped automatically from ",
                   tags$a(href="https://www.lyrics.com/", "Lyrics.com.",), "'Stopwords'
                   (frequently used english words) have been removed from all lyrics. 
                   Lyrics have not been 'stemmed' due to erroneous results"),
          
          hr(),
          
          helpText("Source codes available ",
                   tags$a(href="https://github.com/TobiasAnh/band_analyzer", "here.")),
          
          width = 3 
          
          ),
        
        # Create wordclouds 1 and 2
        mainPanel(
          wordcloud2Output("wordcloud_1"),
          hr(),
          wordcloud2Output("wordcloud_2"),
          width = 6)
                 )
        
                  
                 # sidebar 1 closes
               ),
               # tab 1 closes#
       tabPanel("Band metrics", # tab 2 opens
                
                # Sidebar tab 2
                sidebarLayout(      
                  
                  # Define the sidebar input (2 Bands are choosable)
                  sidebarPanel(radioButtons("metric", "Choose metric:", 
                                           selected = "count",
                                           choiceNames = c("Number of songs", "Lexical diversity (TTR)", "Song duration"),
                                           choiceValues = names(plot_list)),
                               #horizontal line
                               hr(),
                               
                               helpText("Source: ",
                                        tags$a(href="https://www.lyrics.com/", "Lyrics.com")),
                               width = 3
                              ),
                  
                  # Create a spot for the barplot
                  mainPanel(
                    plotOutput("metric_plot"), width = 6)
                   
                  
                             )
                
                
                
                
                
                
                
                )
)



server <- function(input, output, session) {
  
  # render wordcloud ONE with 'wordcloud2' using 'band1'
  output$wordcloud_1 <- renderWordcloud2({
    
    clouds[[input$band1]] %>% top_n(500) %>%                              # using only 500 most common words
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri",
                                         #shape = "star",
                                         ellipticity = 0.3,
                                         size = 1.5)
  })
  
  
  # render wordcloud TWO with 'wordcloud2' using 'band2'
  output$wordcloud_2 <- renderWordcloud2({
    
    clouds[[input$band2]] %>% top_n(500) %>%                              # using only 500 most common words
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri",
                                         #shape = "star",
                                         ellipticity = 0.3,
                                         size = 1.5)
  })
  
  # render metric plot 
  output$metric_plot <- renderPlot({ plot_list[[input$metric]]  })
}

shinyApp(ui, server)




