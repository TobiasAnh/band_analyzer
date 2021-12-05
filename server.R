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
    
    corrplot::corrplot(as.matrix(1-diss_df), 
                       type = "upper", 
                       is.corr = F, 
                       tl.col = "black", tl.cex = 0.8, cl.cex = 0.8, 
                       number.cex = 0.6, 
                       addgrid.col = "darkgray", 
                       diag = F, 
                       title = element_blank())
  })
  
  output$lyrics_hclust <- renderPlot({
    
    factoextra::fviz_dend(clustered,
                          type = input$vis_type,
                          main = "Band lyrics dendrogram",  #title
                          k = clusters,                     #predefined clusters
                          k_colors = "black",               #cluster colors
                          color_labels_by_k = T,                                         
                          rect = TRUE,                      #add rectangles
                          rect_border = k_color_palettes,  #rectangle colors
                          rect_fill = T,
                          ylab = "")
    
  })
  
  
  # render metric plot 
  output$metric_plot <- renderPlot({ plot_list[[input$metric]]  })
}

#shinyApp(ui, server)