library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Data quality"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 800),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500)
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Overview",
        formattableOutput("diagnose_table")
      ),
      tabPanel(
        "Missing data",
        uiOutput("reacOut")
      ),
      tabPanel(
        "Data transformation",
        h4("Check / change data types"),
        h4("Remove rows with too many missing values"),
        h4("Remove columns with too many missing values"),
        h4("Impute remaining NAs"),
        h4("Impute remaining NAs"),
        h4("Log transform"),
        h4("button to run transformation"),
        actionButton("button", "Transform data!")
        
      )
      
    )
    
  )))
