library(shiny)
library(formattable)
library(shinyjs)

shinyUI(fluidPage(
  
  shinyjs::useShinyjs(),
  tags$script(
    HTML(
      'setInterval(function(){ $("#hiddenButton").click(); }, 1000*30);'
    )
  ),
  tags$footer(shinyjs::hidden(
    actionButton(inputId = "hiddenButton", label = "hidden")
  )),
  titlePanel("Data quality"),
  
  sidebarPanel(
    sliderInput("wscale", "Plot width scale", 0, 3, 1, 0.2),
    sliderInput("hscale", "Plot height scale", 0, 3, 1, 0.2)
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Overview",
        uiOutput("select_col"),
        formattableOutput("diagnose_table")
      ),
      tabPanel(
        "Missing data",
        uiOutput("reacOut"),
        uiOutput("reacOut_2")
      ),
      tabPanel(
        "Data transformation",
        numericInput("max_na_prop_row", "Maximum missing proportion (per row)", value = 0, min = 0, max = 1),
        numericInput("max_na_prop_col", "Maximum missing proportion (per column)", value = 0, min = 0, max = 1),
        checkboxInput("imputena", "Impute remaining NAs", value = FALSE),
        h5("Click below to send data back to Tercen"),
        actionButton("button", "Transform data!")
        
      )
      
    )
    
  )))
