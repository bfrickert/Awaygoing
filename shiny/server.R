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
  
  output$city.name <- renderUI({
    HTML(paste("<h2><strong>", input$City, "</strong></h2>"))
  })
  
  output$map.city.name <- renderUI({
    HTML(paste("<h2><strong>", input$City, "</strong></h2>"))
  })
  
  output$map.text <- renderUI({
    df <- filter(joind.not.awaygoing, city.full == input$City)
    
    HTML(paste("<h5><strong>", input$City, "</strong> (population <strong>", prettyNum(df$pop,big.mark=",",scientific=FALSE), "</strong>) is <strong><span style=\"color:green\">",
               round(df$dc.dists, 2), " miles</span></strong> from <strong>Washington, DC</strong> and 
               <strong><span style=\"color:green\">",
               round(df$sf.dists, 2), " miles</span></strong> from <strong>San Francisco</strong>.</h5>", sep=''))
    })
  
  output$map <- renderImage({
    file.name <- strsplit(input$City, ', ')
    list(src = paste('./viz/maps/', file.name[[1]][1],'.', file.name[[1]][2], '.jpg', sep=''),
         alt = input$City,
         width=400)
  }, deleteFile = FALSE)
  
  output$rankings <- renderUI({
    df <- filter(joind.not.awaygoing, city.full == input$City)
    hate.df <- filter(hate.per.city, city == input$City)
    HTML(paste("<h3>Awaygoing Suitability Ranking<strong><span style='font-family:Arial'>:</span> <span style=\"color:", kpi.color(df$rank), "\">",
               df$rank, "</span></strong></h3>", sep=''),
         paste("<h5>Arts: <strong><span style=\"color:", kpi.color(df$arts.rank), "\">",
               df$arts.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Climate: <strong><span style=\"color:", kpi.color(df$climate.rank), "\">",
               df$climate.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Crime: <strong><span style=\"color:", kpi.color(df$crime.rank), "\">",
               df$crime.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Economy: <strong><span style=\"color:", kpi.color(df$econ.rank), "\">",
               df$econ.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Education: <strong><span style=\"color:", kpi.color(df$educ.rank), "\">",
               df$educ.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Healthcare: <strong><span style=\"color:", kpi.color(df$hlthcare.rank), "\">",
               df$hlthcare.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Housing Cost: <strong><span style=\"color:", kpi.color(df$housingcost.rank), "\">",
               df$housingcost.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Recreation: <strong><span style=\"color:", kpi.color(df$recreat.rank), "\">",
               df$recreat.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Transportation: <strong><span style=\"color:", kpi.color(df$transp.rank), "\">",
               df$transp.rank, "</span></strong></h5>", sep=''),
         paste("<h5>Distance from San Francisco: <strong><span style=\"color:green\">",
               round(df$sf.dists, 2), " miles</span></strong></h5>", sep=''),
         paste("<h5>Distance from Washington, DC: <strong><span style=\"color:green\">",
               round(df$dc.dists, 2), " miles</span></strong></h5>", sep=''),
         paste("<h5>According to the <strong>Southern Poverty Law Center</strong>, as of 2014, ",
               hate.kpi.color(df$hate.cnt), " operating within 100 miles of 
               <strong>", input$City,
              "</strong>.</h5>", sep='')
    )
  })
  
  output$cluster <- renderUI({
    df <- filter(joind.not.awaygoing, city.full == input$City)
    similars <- filter(joind.not.awaygoing, cluster == df$cluster & !city.full == input$City)
    similars.output <- apply(arrange(select(similars, city, state), city), 1, function(x){
      return(paste(x[1], ', ', x[2], '<br />', sep=''))
    })
    similars.output <- similars.output[sample(1:length(similars.output),min(3, length(similars.output)))]
    HTML(paste('<strong>',unlist(similars.output), '</strong>',sep=''))
  })
    
  output$cluster.text <- renderUI({
    HTML(paste("<h5><i>K-Means Clustering</i> has identified cities that are similar to <strong>",
        input$City, "</strong>. Among them are:</h5>", sep=''))
  })
  
  output$rf.text <- renderUI({
    HTML("<h5>These lucky cities were predicted by a random forest machine learning algorithm to be <strong>future Awaygoing destinations</strong>.
         Featured along with each city is it's <i>Awaygoing Suitability Ranking</i>. You might notice that some of these predicted destinations
         aren't exactly '<i>suitable</i>'. This is a function of only being able to train the algorithm on a small subset of the data.<br /><br />Therefore,
         one is well advised to factor suitability <strong>AND</strong> its appearance here when choosing a destination.</h5>")
  })
  
  output$random.forest <- renderUI({
    rf.output <- apply(random.forest, 1, function(x){
      df <- filter(joind.not.awaygoing, city.full == paste(x[1], x[2], sep=', '))
      
      return(paste("<strong>", x[1], ', ', x[2], '</strong> (<strong><span style=\'color:', kpi.color(df$rank), "'>", df$rank, '</span></strong>)<br />', sep=''))
    })
    HTML(unlist(sort(rf.output)))
  })
  
})