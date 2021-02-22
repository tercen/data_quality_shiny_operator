library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(dlookr)
library(DT)
library(formattable)

# need latticeExtra for R < 3.6
# install.packages("https://cran.r-project.org/src/contrib/Archive/latticeExtra/latticeExtra_0.6-28.tar.gz",
#                  repos = NULL,
#                  type = "source") 
# install.packages("dlookr")
  
############################################
#### This part should not be included in ui.R and server.R scripts
getCtx <- function(session) {
  ctx <- tercenCtx(stepId = "f0f7c34d-e438-459f-be32-fcde2a205abe",
                   workflowId = "2553cb89b6ec3bc593e238e0df047713")
  return(ctx)
}
####
############################################

ui <- shinyUI(fluidPage(
  
  titlePanel("Data quality"),
  
  sidebarPanel(
    sliderInput("plotWidth", "Plot width (px)", 200, 2000, 800),
    sliderInput("plotHeight", "Plot height (px)", 200, 2000, 500)
  ),
  
  mainPanel(tabsetPanel(type = "tabs",
    tabPanel("Overview",
      dataTableOutput("diagnose_table")
    ),
    tabPanel("Missing data",
      uiOutput("reacOut")
    ),
    tabPanel("Data transformation",
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

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })
  
  output$reacOut <- renderUI({
    plotOutput(
      "na_plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  })

  output$na_plot <- renderPlot({
    d <- dataInput()
    dlookr::plot_na_pareto(d)
    # dlookr::plot_na_hclust(d)
  })
  
  output$diagnose_table <- renderDataTable({
    d <- dataInput()
    ## overview
    d_out <- dlookr::diagnose(d)
    dd <- formattable::formattable(d_out,
                             list(missing_percent = color_tile("white", "orange")))
    as.datatable(dd)
  })
  
  observeEvent(input$button, {
    d <- dataInput()
    d$.ci <- 0
    ctx <- getCtx(session)
    d %>% ctx$addNamespace() %>% ctx$save()
  })

})

getValues <- function(session){
  ctx <- getCtx(session)

  ## load input table
  documentId <- ctx$cselect()[[1]]
  client = ctx$client
  schema = client$tableSchemaService$get(documentId)
  df <- as_tibble(client$tableSchemaService$select(schema$id, list(), 0, schema$nRows))
  
  return(df)
}

runApp(shinyApp(ui, server))  

