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
  
  output$map <- renderPlot({
    sf.1 <- filter(joind, city=='San Francisco')
    dc.1 <- filter(joind, city=='Washington')
    dul.1 <- filter(joind, city==input$City | city=='Washington' | city=='San Francisco')
    earth.dist(sf.1$long, sf.1$lat, dul.1$long, dul.1$lat)
    earth.dist(dc.1$long, dc.1$lat, dul.1$long, dul.1$lat)
    map<- get_map(location = 'US', zoom = 4)
    ggmap(map)
    sf <- filter(joind, city=='San Francisco' | city==input$City)
    dc <- filter(joind, city=='Washington' | city==input$City)
    ggmap(map)+geom_point(data=joind.not.awaygoing,alpha = .7, aes(x=long, y=lat,size =pop, fill=rank))+
      ggtitle("Unspoiled Awaygoing Cities") +
      geom_point(data=sf, aes(x=long, y=lat),color="black") + geom_line(data=sf, aes(x=long, y=lat), color="black", size=2)+
      geom_point(data=dc, aes(x=long, y=lat),color="black") + geom_line(data=dc, aes(x=long, y=lat), color="orange", size=2) +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),legend.position="none",
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    
  })
  
  output$summary <- renderText({
    "hello"
    #summary(filter(joind, city==input$City))
  })
  
  output$table <- renderText({
    "Mork"
    #summary(filter(joind, city==input$City))
  })
  
})