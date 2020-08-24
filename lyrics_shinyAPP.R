library(shiny)
#### R shiny app #### 
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("Band Analyzer"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      
      selectInput("band1", "Band 1:", 
                  choices= names(FLL), 
                  selected = "ABBA"), 
      
      selectInput("band2", "Band 2:", 
                  choices= names(FLL), 
                  selected = "Eminem"),
      
      
      hr(),
      
      helpText("Source: Lyrics.com (",
               tags$a(href="https://www.lyrics.com/", ")"))
    ),
    
    
    # Create a spot for the barplot
    mainPanel(
      wordcloud2Output("wordcloud_1", width = "20%"),
      hr(),
      wordcloud2Output("wordcloud_2", width = "20%")
    )
    
  )
)



server <- function(input, output, session) {
  
  # Fill in the spot we created for a plot
  output$wordcloud_1 <- renderWordcloud2({
    
    clouds[[input$band1]] %>% top_n(300) %>%                              # create wordcloud from specific word_matrix_df
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri")
  })
  
  
  # Fill in the spot we created for a plot
  output$wordcloud_2 <- renderWordcloud2({
    
    # normierte Infektionszahlen
    
    clouds[[input$band2]] %>% top_n(300) %>%                              # create wordcloud from specific word_matrix_df
                              wordcloud2(color = "random-dark", 
                                         fontFamily = "Calibri")
  })
}

shinyApp(ui, server)




