library(shiny)

shinyServer(function(input, output) {
  
  output$banner <- renderImage({
    list(src = './viz/postcards.png',
         alt = "Awaygoing Banner",
         width=800)
    
  }, deleteFile = FALSE)
  
  output$tampa <- renderImage({
    list(src = './viz/tampa.slice.jpg',
         alt = "Downtown Tampa",
         width=200)
    
  }, deleteFile = FALSE)
  
  output$map <- renderImage({
    file.name <- strsplit(input$City, ', ')
    list(src = paste('./viz/maps/', file.name[[1]][1],'.', file.name[[1]][2], '.png', sep=''),
         alt = input$City,
         width=650,
         height=500)
    
  }, deleteFile = FALSE)
  
  output$summary <- renderText({
    "hello"
  })
  
  output$random.forest <- renderDataTable({
    random.forest
  })
  
})