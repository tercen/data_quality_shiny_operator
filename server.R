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
      "main.plot",
      height = input$plotHeight,
      width = input$plotWidth
    )
  }) 
  
  output$main.plot <- renderPlot({
    values <- dataInput()
    data <- values$data$.y
    hist(data)
  })
  
})

getValues <- function(session){
  ctx <- getCtx(session)
  values <- list()
  
  values$data <- ctx %>% select(.y, .ri, .ci) %>%
    group_by(.ci, .ri) %>%
    summarise(.y = mean(.y)) # take the mean of multiple values per cell
  
  return(values)
}

