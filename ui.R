#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
library(shinydashboard)
library(DT)
plot_height="500px"


dashboardPage(
  dashboardHeader(title="Fantasy Dashboard"),
  dashboardSidebar(collapsed=T, disable=T),
  dashboardBody(
    fluidRow(
      column(width = 8, div(DT::dataTableOutput('player.table'), style = "font-size:80%")),
      column(width=4, 
             h4("Player Notes"),
             textOutput("notes"))
    ),
    fluidRow(
      column(width=8, plotOutput("rankPlot", height=plot_height)),
      column(width=4, plotOutput("vorPlot", height=plot_height))
    )
  )
)
