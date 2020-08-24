library(shiny)
#### R shiny app #### 
ui <- fluidPage(    
  
  # Page title
  titlePanel("Band Analyzer"),
  
  # Sidebar
  sidebarLayout(      
    
    # Define the sidebar with inputs (2 Bands are choosable)
    sidebarPanel(
      
      selectInput("band1", "Band 1:", 
                  choices= names(FLL), 
                  selected = "ABBA"), 
      
      selectInput("band2", "Band 2:", 
                  choices= names(FLL), 
                  selected = "Eminem"),
      
      #horizontal line
      hr(),
      
      helpText("Source: ",
               tags$a(href="https://www.lyrics.com/", "Lyrics.com"))
    ),
    
    
    # Create a spot for the barplot
    mainPanel(
      wordcloud2Output("wordcloud_1"),
      hr(),
      wordcloud2Output("wordcloud_2")
    )
    
  )
)



server <- function(input, output, session) {
  
  # Fill in the spot we created for a plot
  output$wordcloud_1 <- renderWordcloud2({
    
    clouds[[input$band1]] %>% top_n(300) %>%                              # create wordcloud from specific word_matrix_df
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri",
                                         #shape = "star",
                                         ellipticity = 0.2)
  })
  
  
  # Fill in the spot we created for a plot
  output$wordcloud_2 <- renderWordcloud2({
    
    # normierte Infektionszahlen
    
    clouds[[input$band2]] %>% top_n(300) %>%                              # create wordcloud from specific word_matrix_df
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri",
                                         #shape = "star",
                                         ellipticity = 0.2)
  })
}

shinyApp(ui, server)




