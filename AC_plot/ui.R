library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)
options(warn = -1)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Anti-coagulant Patterns for Patients After Index VTE"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      helpText("Please select patient ID's to be plotted."),
      uiOutput('select_patid'),
      # radioButtons("size_copay",
      #              label = "Show size of copay",
      #              choices = c("Yes" = "Copay_sum", "No" = "NULL"),
      #              selected = NULL)
      # # verbatimTextOutput("hover"),
      # verbatimTextOutput("click"),
      # verbatimTextOutput("brush")
      width = 3
    ),
    
    
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           div(style = "position:relative",
                               plotlyOutput("patternPlot")),
                           width = 7),
                  tabPanel("Patient Information", dataTableOutput("select_patid_disp")),
                  tabPanel("INR Information", dataTableOutput("select_inr_disp")),
                  tabPanel("Confinement Information", dataTableOutput("select_conf_disp")),
                  tabPanel("Insurance Enrollment Information", dataTableOutput("select_enroll_disp"))
      ),
      
      p('You may hover over the points to view more information.')
    )
  )
))
