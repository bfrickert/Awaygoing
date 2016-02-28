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
                      br(),br(),br(),br(),br(), br(),br(),br(),br(),br()
                      ),
                    mainPanel(
                        tabsetPanel(
                          tabPanel("City Rankings", textOutput("summary")), 
                          tabPanel("US Map", imageOutput('map')),
                          tabPanel("Predicted Awaygoing Cities", dataTableOutput("random.forest"))
                        )
                      #plotOutput('map',width="500px", heigh='500px')
                      )
                    )
))