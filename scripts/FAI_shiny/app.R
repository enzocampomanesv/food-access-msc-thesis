# Main script for the food access index scenario dev't Shiny application
# 
# Florencio Campomanes V
# 2022-04-04
#
# This script requires the ff R package/s:
#   shiny - application framework
#   randomForest - FAI prediction using RF
#   mgcv - FAI prediction using GAM
#
# This script requires the RFGAM_dataPrep_v3.RData created in the helper script.
# Some features written in the script are unused in the actual application like:
#   bee swarm plots - written for future use and improvements.
#   waffle plots - used bar plots for now since the package for waffle plots are
#                 not on CRAN, so it is not possible to upload on shiny.io.
#
# Notes:
# (a) All file paths and names should be changed as needed
#

library(rstudioapi)
# setwd(dirname(getActiveDocumentContext()$path))

rm(list=ls(all=TRUE))
library(shiny)
library(shinyjs)
library(shinythemes)
library(randomForest)
library(mgcv)
library(cowplot)
library(gridGraphics)
require(plotly)
library(ggthemr)
library(tidyverse)
library(ggpubr)
library(stringr)
library(gglorenz)
library(RColorBrewer)
library(scales)
library(ineq)
library(viridis)
ggthemr("fresh")

# loadfonts(device="win",quiet=TRUE)
local({
  load("RFGAM_dataPrep_v3.RData", .GlobalEnv)
  ls()
})


ui <- fluidPage(
  theme=shinytheme("lumen"),
  useShinyjs(),
  headerPanel("Food Accessibility Scenario Development"),
  fluidRow(
    column(3,
         wellPanel(
           column(6,uiOutput("zoneSelect")),
           column(6,uiOutput("methodSelect")),
           sliderInput(inputId="precip",
                       label = "%Change in Precipitation of Coldest Quarter",
                       value = 0, min = -200, max= 200),
           sliderInput(inputId="cerPrMag",
                       label = "%Change in Cereal Prices",
                       value = 0, min = -200, max= 200),
           "Shocks:",
           checkboxInput(inputId="cf",
                         label = "Conflict"),
           disabled(
             selectInput(inputId="mode",
                         label = "Select a map to display",
                         choices=c('Baseline','Simulated',
                                   '%Difference','Absolute Difference')
             )
           )
         ),
         plotlyOutput("pop")
         # plotlyOutput("pop_waf")
      ),
    column(5,align="center",
           uiOutput("s_fai",style='border: 1px solid black'),
           # uiOutput("s_bee_fai")
           uiOutput("s_urca_fai")
    ),
    column(4,align="center",
           plotlyOutput("ineq"),
           plotlyOutput("lorenz"))
    )
)

server <- function(input, output, session){
  toListen <- reactive({
    list(input$cerPrMag,input$precip,input$cf, input$mode)
  })
  
  observeEvent(toListen(), {
    if(input$cerPrMag!=0 | input$precip!=0 | input$cf==TRUE){
      enable("mode")
      if(input$mode =='Difference'){
        updateSelectInput(session,"mode",
                          selected='Difference')
      }else if(input$mode=='Simulated'){
        updateSelectInput(session,"mode",
                          selected='Simulated')
      }
      
    }else{
      updateSelectInput(session,"mode",
                        selected='Baseline')
      disable("mode")
    }
  })
  
  output$zoneSelect <- renderUI({
    options <- sort(unique(test$NAME_2))
    selectizeInput(
      inputId = "zone",
      label = "Select a zone",
      choices = c("All", options)
      ,
      selected = "Addis Abeba" # Default value is Addis Ababa
    )
  })
  output$methodSelect <- renderUI({
    options <- c("RF","GAM")
    selectizeInput(
      inputId = "method",
      label = "Select a method",
      choices = c(options)
      ,
      selected = "RF" # Default value is RF
    )
  })
  base <- reactive({
    base_FAI(input$zone, input$method)
  })
  pred_react <- reactive({
    predict_FAI(test,
                input$cerPrMag,
                input$precip,
                input$cf,
                input$zone,
                input$method)
  })
  
  output$s_fai <- renderUI({
    plotlyOutput("fai", height=515)
  })
  output$fai <- renderPlotly({
    plot_map(test, pred_react(),base(), input$mode)
  })
  output$urca_fai <- renderPlotly({
    plot_urca(test, pred_react(), base(), 
              input$cerPrMag,input$cf,input$precip)
  })
  output$s_urca_fai <- renderUI({
    plotlyOutput("urca_fai", height=450)
  })
  output$bee_fai <- renderPlotly({
    plot_beeswarm(test, pred_react(), base(), 
              input$cerPrMag,input$cf,input$precip)
  })
  output$s_bee_fai <- renderUI({
    plotlyOutput("bee_fai", height=450)
  })
  output$lorenz <- renderPlotly({
    plot_lorenz(test, pred_react())
  })
  output$pop <- renderPlotly({
    plot_pop(test, pred_react(), base(), 
             input$cerPrMag,input$cf,input$precip)
  })
  # output$pop_waf <- renderPlotly({
  #   plot_pop_waf(test, pred_react(), base(), 
  #            input$cerPrMag,input$cf,input$precip)
  # })
  output$ineq <- renderPlotly({
    plot_ineq(test, pred_react(), base(), 
              input$cerPrMag,input$cf,input$precip)
  })
}

shinyApp(ui=ui, server=server)
