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
  
  output$rankings <- renderUI({
    df <- filter(joind.not.awaygoing, paste(city, state, sep=', ') == input$City)
    hate.df <- filter(hate.per.city, city == input$City)
    HTML(paste("<h3>Awaygoing Suitability Ranking: <span style=\"color:", kpi.color(df$rank), "\">",
               df$rank, "</span></h3>", sep=''),
         paste("<h5>Arts: <span style=\"color:", kpi.color(df$arts.rank), "\">",
               df$arts.rank, "</span></h5>", sep=''),
         paste("<h5>Climate: <span style=\"color:", kpi.color(df$climate.rank), "\">",
               df$climate.rank, "</span></h5>", sep=''),
         paste("<h5>Crime: <span style=\"color:", kpi.color(df$crime.rank), "\">",
               df$crime.rank, "</span></h5>", sep=''),
         paste("<h5>Economy: <span style=\"color:", kpi.color(df$econ.rank), "\">",
               df$econ.rank, "</span></h5>", sep=''),
         paste("<h5>Education: <span style=\"color:", kpi.color(df$educ.rank), "\">",
               df$educ.rank, "</span></h5>", sep=''),
         paste("<h5>Healthcare: <span style=\"color:", kpi.color(df$hlthcare.rank), "\">",
               df$hlthcare.rank, "</span></h5>", sep=''),
         paste("<h5>Housing Cost: <span style=\"color:", kpi.color(df$housingcost.rank), "\">",
               df$housingcost.rank, "</span></h5>", sep=''),
         paste("<h5>Recreation: <span style=\"color:", kpi.color(df$recreat.rank), "\">",
               df$recreat.rank, "</span></h5>", sep=''),
         paste("<h5>Transportation: <span style=\"color:", kpi.color(df$transp.rank), "\">",
               df$transp.rank, "</span></h5>", sep=''),
         paste("<h5>Distance from San Francisco: <span style=\"color:green\">",
               round(df$sf.dists, 2), " miles</span></h5>", sep=''),
         paste("<h5>Distance from Washington, DC: <span style=\"color:green\">",
               round(df$dc.dists, 2), " miles</span></h5>", sep=''),
         paste("<h5>According to the <strong>Southern Poverty Law Center</strong>, as of 2014, there were <strong><span style=\"color:red\">",
               nrow(hate.df), " hate groups</span></strong> operating within 100 miles of ", input$City,
              ".</h5>", sep='')
    )
  })
  
  output$cluster <- renderUI({
    df <- filter(joind.not.awaygoing, paste(city, state, sep=', ') == input$City)
    similars <- filter(joind.not.awaygoing, cluser = df$cluster)
    HTML()
  })
  
  output$random.forest <- renderDataTable({
    random.forest
  })
  
})