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
#### This part should not be modified
getCtx <- function(session) {
  # retreive url query parameters provided by tercen
  query <- parseQueryString(session$clientData$url_search)
  token <- query[["token"]]
  taskId <- query[["taskId"]]
  
  # create a Tercen context object using the token
  ctx <- tercenCtx(taskId = taskId, authToken = token)
  return(ctx)
}
####
############################################

shinyServer(function(input, output, session) {
  
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
  
  output$diagnose_table <- renderFormattable({
    d <- dataInput()
    ## overview
    d_out <- dlookr::diagnose(d)
    dd <- formattable::formattable(d_out,
                                   list(missing_percent = color_tile("white", "orange")))
    (dd)
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
