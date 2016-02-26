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
                                  c(arrange(joind.not.awaygoing, city)$city), selected='St. Louis'),
                      imageOutput('tampa'),
                      br(),br(),br(),br(),br(), br(),br(),br(),br(),br()
                      ),
                    mainPanel(
                        tabsetPanel(
                          tabPanel("City Rankings", textOutput("summary")), 
                          tabPanel("US Map", plotOutput('map',width="500px", heigh='500px')),
                          tabPanel("Predicted Awaygoing Cities", dataTableOutput("random.forest"))
                        )
                      #plotOutput('map',width="500px", heigh='500px')
                      )
                    )
))