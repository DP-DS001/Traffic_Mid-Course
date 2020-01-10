#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = "Traffic Congestion"),
    dashboardSidebar(
        sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Widgets", tabName = "widgets", icon = icon("th")),
        menuItem("Charts", tabName = "Charts", icon = icon("charts")))),

    dashboardBody(
        tabItems(
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 50, 25)
                            ) #box
                        ) #fluidrow
                    ), #tabitem
tabItem(tabName = "widgets",
        h2("Widgets tab content")
), #tabitem
tabItem(tabName = "Charts", fluidRow(plotOutput("plot2", height = 250)), box(title = "Years", sliderInput("slider2","Number of Years:", 2011, 2018, 2011, sep="")),
        h2("Congestion Over Time"))
) #tabitems
) #dashboard body
) #dashboard page


