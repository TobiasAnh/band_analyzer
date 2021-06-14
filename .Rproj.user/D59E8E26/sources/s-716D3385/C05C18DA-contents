server <- function(input, output, session) {
  
  # render wordcloud ONE with 'wordcloud2' using 'band1'
  output$wordcloud_1 <- renderWordcloud2({
    
    clouds[[input$band1]] %>% top_n(input$maxwords1) %>%                              
      wordcloud2(color = "random-dark", 
                 fontFamily = "Calibri",
                 #shape = "star",
                 ellipticity = 0.8,
                 size = 0.8)
  })
  
  
  # render similarity matrix plot
  output$similarity_matrix <- renderPlot({
    
    ggcorrplot(corr = 1-diss_df, 
               method = "circle", 
               #type = "lower", 
               title = "Lyrics similarity among bands", 
               show.diag = T, tl.srt = 90, ) + 
      scale_fill_gradient2(limit = c(0,0.9), low = "white", high = "darkgreen")
  })
  
  # render metric plot 
  output$metric_plot <- renderPlot({ plot_list[[input$metric]]  })
}

#shinyApp(ui, server)