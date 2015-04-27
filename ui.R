
library(shiny)
library(XML)
# teampage <- htmlTreeParse("http://socialbostonsports.com/leagues/45120/teams/208585",useInternalNodes=T)
# teampage.parse <- xpathApply(teampage, "//a/@class")


shinyUI(fluidPage(
    tags$head(includeScript("googleanalytics.js")),
  # Application title
  titlePanel(strong("Dodgeball or whatever")),
    fluidRow(column(4,
       radioButtons("radio", label = h3("Display"),
            choices = list(
                "Current Season" = 1,"Season Archive" = 4, "Player" = 2, "Players Overall" = 3, 
                "Network"=5), selected = 5))),
    uiOutput("searchtype"),
  
    fluidRow(
        column(5,
            uiOutput("chart1")
        ),
        column(6,
            uiOutput("chart2")
        )      
    ),
  fluidRow(
      column(10,
             uiOutput("NetChart")))
  )
)
