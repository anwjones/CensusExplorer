
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

var.names <- read.csv('VAR_NAMES.csv', strip.white=TRUE, stringsAsFactors = FALSE)

shinyUI(fluidPage(

  # Application title
  titlePanel("Australian Mortality by Census District"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        selectInput("X.Axis", 
                    label = "Horizontal Axis",
                    choices = var.names$DISPLAY,
                    selected = 'Longitude'),
        
        selectInput("Y.Axis", 
                    label = "Vertical Axis",
                    choices = var.names$DISPLAY,
                    selected = 'Latitude')
        
    ),

    # Show a plot of the generated distribution
    mainPanel(
        a('Open the documentation for this app', href='index.html')
      ,h3('Scatter Plot of Actual/Expected Deaths for each Census District')
      ,p('Each point represents a single Census District with the colour showing actual v expected deaths')
      ,plotOutput("scatter")
      ,h3('Grouping similar Census District')
      ,p('Where the axis data is continuous with a large range of values it has been grouped into
         deciles.  The gradation of color across the chart allows you to judge if mortality is better 
         (greener) or worse (redder) across the values of the axis.')
      ,plotOutput("group")
    )
  )
))
