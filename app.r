source("Survey.r")
library("shiny")
library("shinydashboard")
library("DT")
library(rsconnect)
#rsconnect::setAccountInfo(name='emblazion', token='C2FB99087F21181B7E76B0639589C266', secret='b534A3L410t2yVjF/7Kr8JtY0IqTrBWXoNyjV6fA')
#rsconnect::deployApp('/home/emblaze/RCode/SurveyApp')


if (FALSE){
  
  ui <- fluidPage(
    tabsetPanel(
      tabPanel(title = "Responses", 
               DT::dataTableOutput(outputId = "table")
      ),
      tabPanel(title = "Central Limit Theorem",
               sliderInput(inputId = "iterations", label = "Iterations", value = 100, min = 1, max = 10000),
               sliderInput(inputId = "size", label = "Sample Size", value = 10, min = 1, max = length(temp[[11]])),
               actionButton(inputId = "click", label = "Update"),
               plotOutput(outputId = "plot")
      ), 
      tabPanel(title = "Cochran's Formula",
               DT::dataTableOutput(outputId = "cochran")
      )
    )
  )
  
  
  server <- function(input, output){
    output$table <- DT::renderDataTable({
      temp
    })
    
    cochransTable <- reactive({
      cochranFormula()
    })
    
    
    output$cochran <- DT::renderDataTable(({
      cochranFormula()
      #View(cochranFormula())
    }))
    
    #observeEvent(input$click, {
    # data <- reactive({ list("sample size" = input$size, "iterations" =  input$iterations) })  
    #})
    
    data <- eventReactive(input$click, {
      list("sample size" = input$size, "iterations" =  input$iterations)
    })
    
    output$plot <- renderPlot({
      someExtraPlotting(sampleSize = data()[["sample size"]], iter = data()[["iterations"]])
    }
    #cacheKeyExpr = { list(input$size, input$iterations) }
    )
  }
  
}
###################################################################################################
#where the REAL CODE starts

df <- temp[, 3:22]

ui <- dashboardPage(
  dashboardHeader(title = "Inference Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("DashBoard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Spending Vs Rating", tabName = "SpendRate", icon = icon("chart-line")),
      menuItem("Survey Data", tabName = "data", icon = icon("table")),
      menuItem("Cochran's Formula Tabulated", tabName = "cochran", icon = icon("table")),
      menuItem("Central Limit Theroem", tabName = "CLT", icon = icon("chart-bar")),
      menuItem("Bayesian Inference", tabName = "bayes", icon = icon("chart-line")),
      menuItem("Distributions", tabName = "distri", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(title = "Controls",
                    sliderInput(inputId = "slider", label = "Number of observations", min = 1, max = 100, value = 50)
                )
              )
      ),
      tabItem(tabName = "SpendRate",
              box(plotOutput("spendVRate"), height = 420, width = 500, status = "warning")
      ),
      tabItem(tabName = "CLT",
              fluidRow(
                box(sliderInput(inputId = "iter", label = "Number of observations", min = 1, max = 10000, value = 10000)),
                box(sliderInput(inputId = "size", label = "Sample Size", min = 1, max = length(temp[[1]]), value = 10))
              ),
              fluidRow(
                actionButton(inputId = "update", label = "Generate New Data"),
                column(radioButtons(
                  inputId = "choice", label = "Canteen & Chill Spending/Rating Data",
                  choices = c("Spending Data" = "spend", "Rating Data" = "rate")          
                ), width = 5) 
              ),
              box(plotOutput("CLTPlot"), height = 400, width = 400, status = "warning")
      ),
      tabItem(tabName = "bayes",
              fluidRow(
                column(
                  box(radioButtons(
                    inputId = "choice2", label = "Data",
                    choices = c("Gender" = "3", "Academic Section" = "4", "Academic Year" = "5",
                                "Eating Means" = "6", "Frequency Canteen" = "7",
                                "Frequency Chill" = "8")
                  ))
                  , width = 6), 
                column(
                  box(uiOutput("columns")),
                  width = 6
                )
              ),
              box(plotOutput("BayesPlot"), height = 400, width = 500, status = "warning")
      ),
      tabItem(tabName = "data",
              DT::dataTableOutput(outputId = "data")
      ),
      tabItem(tabName = "cochran",
              DT::dataTableOutput(outputId = "cochran")
      ),
      tabItem(tabName = "distri",
              fluidRow(
                column(
                  box(radioButtons(
                    inputId = "dist", label = "Random Variates",
                    choices = c("Variance" = "var", "Max" = "max", "Min" = "min",
                                "Range" = "range")
                  ), width = 12)
                , width = 3),
                column(
                  box(sliderInput(inputId = "iter2", label = "Number of observations", min = 1, max = 10000, value = 10000)),
                  box(sliderInput(inputId = "size2", label = "Sample Size", min = 1, max = length(temp[[1]]), value = 10))
                  ,width = 9
                )
              ),
              box(plotOutput("Distribution"), height = 400, width = 500, status = "warning")
      ))
  )
)

server <- function(input, output){
  histdata <- rnorm(100)
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  CLTData <- eventReactive(input$update, {
    list("sample size" = input$size, "iter" =  input$iter)
  })
  
  output$CLTPlot <- renderPlot({
    CLTCols <- switch(input$choice,
                      spend = list("Canteen" = temp[[11]], "Chill" = temp[[12]]),
                      rate = list("Canteen" = temp[[13]], "Chill" = temp[[14]])
    )
    someExtraPlotting(col1 = CLTCols[["Canteen"]], col2 = CLTCols[["Chill"]], sampleSize = CLTData()[["sample size"]], iter = CLTData()[["iter"]])
  })
  
  output$data <- DT::renderDataTable({
    DT::datatable(data = df, options = list(scrollX = TRUE))  
  })
  
  output$cochran <- DT::renderDataTable({
    table <- cochranFormula()
    DT::datatable(data = table, options = list(scrollX = TRUE))
  })
  
  choice2 <- eventReactive(input$choice2, {
    switch(input$choice2,
          "3" = temp[[3]], "4" = temp[[4]], "5" = temp[[5]],"6" = temp[[6]],
          "7" = temp[[7]],"8" = temp[[8]])
  })
  

  output$columns <- renderUI({
      
    tagList(
      radioButtons(inputId = "colChoice", label = "Levels",
                   choices = c(names(table(choice2()))), selected = character(0)
      )
    )
    
  })
  
  colChoice <- reactive({
    input$colChoice
  })
  
    output$BayesPlot <- renderPlot({
      validate({
        need(colChoice(), "Please Select a Level" )
        need((colChoice() %in% names(table(choice2()))), "Please Select a Level")
      })
      
      beta_bfsComparison( table(choice2()), 
                          successName = colChoice(), 
                          hypotheses = hypotheses)
  
    }#,
    #cacheKeyExpr = { list(input$colChoice, choice2(), input$choice2) }
    )

  
  output$spendVRate <- renderPlot({
    SpendingVsRating()
  })
  
  fun <- eventReactive(input$dist, {
    switch (input$dist,
      "var" = var,
      "max" = max,
      "min" = min,
      "range" = R
    )
  })

  
    
  output$Distribution <- renderPlot({
    distribution(pop = temp[[11]], sampleSize = input$size2, iter = input$iter2, f = fun())
  })
  
}

shinyApp(ui = ui, server = server)
