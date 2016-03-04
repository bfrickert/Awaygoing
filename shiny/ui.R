library(shiny)
source('helper.R')

shinyUI(
  fixedPage(theme='bootstrap.css',
            tags$h1(class="title","Awaygoing Data Analytics"),
            imageOutput('banner'),
                  br(),br(),br(),br(),br(),
                  fixedRow(column(width=4,
                                  tags$h3('Potential Awaygoing City'),
                      selectInput('City', label=NULL,
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
                                   uiOutput('cluster'),br(),br()), 
                          tabPanel("US Map", uiOutput("map.city.name"), uiOutput('map.text'), tags$hr(), imageOutput('map')),
                          tabPanel("Predicted Awaygoing Cities", HTML("<h2><strong>Awaygoing Cities of the Future</strong></h2>"), uiOutput("rf.text"), 
                                   tags$hr(), uiOutput("random.forest"), tags$br(), tags$br())
                        )
                      )
                    )
))
