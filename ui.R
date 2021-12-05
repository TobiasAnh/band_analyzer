
library(wordcloud2)
library(ggcorrplot)
library(dplyr)
library(forcats)
library(ggplot2)
library(shiny)
library(shinythemes)
library(ggridges)

#### R shiny app #### 
ui <- navbarPage("Band Analyzer", fluid = F, theme = shinytheme("flatly"),  
      
      # Tab 1 title #########################################################
      tabPanel("Lyrics wordclouds", icon= icon("microphone-alt"),
      
      # Sidebar
      sidebarLayout(   
        
        # Define the sidebar with inputs (1 Band is choosable)
        sidebarPanel(width = 4,
                     selectInput("band1", "Choose band/artist", 
                                 choices= names(FLL), 
                                 selected = "Elvis Presley"),
                     
                     #horizontal line
                     hr(),
                     
                     helpText("Lyrics have been web-scraped automatically from ",
                              tags$a(href="https://www.lyrics.com/", "Lyrics.com."), "'Stopwords'
                              (frequently used english words) have been removed from all lyrics. 
                              "),
                     
                     hr(),
                     
                     helpText("Source codes available ", tags$a(href="https://github.com/TobiasAnh/band_analyzer", "here."))
          
           
          
                    ),
        
        # Create wordcloud and similarity matrix
        mainPanel(column(align="center", width = 8,
                         wordcloud2Output("wordcloud_1"),
                  
                         sliderInput("maxwords1", 
                                     "Max. words",
                                     min = 50,
                                     max = 500,
                                     value = 100,
                                     step = 50),
                        
                         hr())
                  )
               # sidebar 1 closes
               )),
               # tab 1 closes#
       
      tabPanel("Lyrics clustering", icon= icon("project-diagram"),# tab 2 opens
               
               # Sidebar tab 2
               sidebarLayout(      
                 
                 # Define the sidebar input (2 Bands are choosable)
                 sidebarPanel(radioButtons("vis_type", "Choose visualization:", 
                                           selected = "phylogenic",
                                           choiceNames = c("rectangle", "circular", "phylogenic"),
                                           choiceValues = c("rectangle", "circular", "phylogenic")),
                              #horizontal line
                              hr(),
                              
                              helpText("Source: ",
                                       tags$a(href="https://www.lyrics.com/", "Lyrics.com")),
                              width = 3
                 ),
                 
                 # Create a spot for the barplot
                 mainPanel(width = 6,
                           plotOutput("similarity_matrix"),
                           
                           hr(),
                           
                           plotOutput("lyrics_hclust")
                           
                          )
                 
                 
               ),
      ),
      
      
      
       tabPanel("Band metrics", icon= icon("sliders-h"),# tab 3 opens
                
                # Sidebar tab 3
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
                  mainPanel(width = 6,
                            plotOutput("metric_plot"))
                   
                  
                           )
            
                )
      
               
      )
      

