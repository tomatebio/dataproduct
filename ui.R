#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Vacination"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
                checkboxInput("Polio", "Polio", value = TRUE),
                checkboxInput("Measles", "Measles", value = TRUE),
                checkboxInput("Mumps", "Mumps", value = TRUE),
                checkboxInput("Other", "Other", value = TRUE),
                checkboxInput("Rubella", "Rubella", value = TRUE),
                checkboxInput("Whooping_Cough", "Whooping Cough", value = TRUE),
                h3("How long prevision"),
                sliderInput("slider1", "Time (in months)", 1, 500, 1),
                submitButton("Refresh"),
                h3("How use"),
                p("Choose one disease and click in submit. So you can see the historical data, geographic data
                  and also de prevision of new outbreaks."),
                p("This is one exercise for Coursera class. The actual data and explanation is in https://www.cfr.org/interactives/GH_Vaccine_Map/"),
                p("This is NOT a expertise site.")
                ) ,           
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Overview", br(), textOutput("Overview")), 
                  tabPanel("Cases", br(), plotlyOutput("Cases")) , 
                  #tabPanel("Fatal", br(), plotlyOutput("Fatal")), 
                  tabPanel("Maps", br(), leafletOutput("Map")),
                  tabPanel("Prevision", br(), plotOutput("prev"))
      )
    )
  )
))
