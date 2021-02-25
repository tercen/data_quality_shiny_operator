library(shiny)
library(tercen)
library(dplyr)
library(tidyr)
library(dlookr)
library(formattable)
library(shinyjs)

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

server <- shinyServer(function(input, output, session) {
  
  dataInput <- reactive({
    getValues(session)
  })

  overviewTable <- reactive({
    d <- dlookr::diagnose(dataInput())
    d$missing_percent <- round(d$missing_percent, 1)
    d$unique_rate <- round(100 * d$unique_rate, 1)
    colnames(d) <- c("Variable", "Type", "# missing", "% missing", "# unique", "% unique")
    d
  })
  
  output$select_col <- renderUI({
    cl <- colnames(overviewTable())
    selectInput(inputId = "overview_sort", label = "Sort by", selected = cl[1], choices = cl)
  })
    
  output$reacOut <- renderUI({
    plotOutput(
      "na_plot",
      height = input$hscale * 500,
      width = input$wscale * ncolumns() * 30
    )
  })
  output$reacOut_2 <- renderUI({
    plotOutput(
      "na_plot_2",
      height = input$hscale * ncolumns() * 15,
      width = input$wscale * 800
    )
  })
  output$na_plot <- renderPlot({
    d <- dataInput()
    dlookr::plot_na_pareto(d)
  })
  output$na_plot_2 <- renderPlot({
    d <- dataInput()
    dlookr::plot_na_hclust(d)
  })
  
  ncolumns <- reactive({
    ncol(dataInput())
  })
  
  
  output$diagnose_table <- renderFormattable({

    ## overview
    d_out <- overviewTable()
    if(!is.null(input$overview_sort)) d_out <- d_out[order(d_out[[input$overview_sort]], decreasing = TRUE), ]
    dd <- formattable::formattable(
      d_out,
      list(
        `% missing` = color_tile("white", "orange"),
        `% unique` = color_tile("white", "lightblue")
      )
    )
    (dd)
  })
  
  observeEvent(input$button, {
    
    #shinyjs::disable("button")
    
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

