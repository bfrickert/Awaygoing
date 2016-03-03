library(shiny)
source('helper.R')

shinyUI(
  fixedPage(theme='bootstrap.css',
            titlePanel("Awaygoing Data Analytics"),
            imageOutput('banner'),
                  br(),br(),br(),br(),br(),
                  sidebarLayout(fluid=F,
                     sidebarPanel(fluid=F,
                      selectInput('City','Potential Awaygoing Destination',
                                  with(arrange(joind.not.awaygoing, city), 
                                       paste(city, state, sep=', ')), selected='St. Louis, MO-IL'),
                      imageOutput('tampa'),
                      br(),br(),br(),br(),br()
                      ),
                    mainPanel(
                        tabsetPanel(
                          tabPanel("City Rankings", uiOutput("city.name"), uiOutput("rankings"), 
                                   tags$h3('Similar Cities'),
                                   uiOutput('cluster.text'),
                                   uiOutput('cluster')), 
                          tabPanel("US Map", uiOutput('map.text'), tags$hr(), imageOutput('map')),
                          tabPanel("Predicted Awaygoing Cities", HTML("<h2><strong>Awaygoing Cities of the Future</strong></h2>"), uiOutput("rf.text"), 
                                   tags$hr(), uiOutput("random.forest"), tags$br(), tags$br())
                        )
                      )
                    )
))