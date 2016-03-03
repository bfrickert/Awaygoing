library(shiny)
source('helper.R')

shinyUI(
  fixedPage(theme='bootstrap.css',
            titlePanel("Awaygoing Data Analytics"),
            imageOutput('banner'),
                  br(),br(),br(),br(),br(),
                  sidebarLayout(fluid=F,
                     sidebarPanel(fluid=F,
                      selectInput('City','',
                                  with(arrange(joind.not.awaygoing, city), 
                                       paste(city, state, sep=', ')), selected='St. Louis, MO-IL'),
                      imageOutput('tampa'),
                      br(),br(),br(),br(),br()
                      ),
                    mainPanel(
                        tabsetPanel(
                          tabPanel("City Rankings", uiOutput("rankings"), 
                                   tags$h3('Similar Cities'),
                                   uiOutput('cluster.text'),
                                   uiOutput('cluster')), 
                          tabPanel("US Map", imageOutput('map')),
                          tabPanel("Predicted Awaygoing Cities", uiOutput("random.forest"))
                        )
                      )
                    )
))