source("Survey.r")
library(shiny)
library(shinydashboard)
library(DT)
library(rsconnect)
library(dashboardthemes)


df <- temp[, 3:22]
myLogo <- shinyDashboardLogoDIY(
  boldText = "Inference Board"
  ,mainText = ""
  ,textSize = 16
  ,badgeText = "1.0"
  ,badgeTextColor = "Black"
  ,badgeTextSize = 3
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
)

  # Ui functions ------------------------------------------------------------
uiChangeThemeDropdown <- function(dropDownLabel = "Change Theme", defaultTheme = "grey_dark"){
  changeThemeChoices <- c(
    "Blue gradient" = "blue_gradient",
    "Plane website" = "boe_website",
    "Grey light" = "grey_light",
    "Grey dark" = "grey_dark",
    "OneNote" = "onenote",
    "Poor man's Flatly" = "poor_mans_flatly",
    "Purple gradient" = "purple_gradient"
  )
  ns <- NS("moduleChangeTheme")
  dropdown <- tagList(
    selectizeInput(
      inputId = ns("dbxChangeTheme"),
      label = dropDownLabel,
      choices = changeThemeChoices,
      selected = defaultTheme
    )
  )
  return(dropdown)
}

uiChangeThemeOutput <- function(){
  ns <- NS("moduleChangeTheme")
  themeOutput <- tagList(
    uiOutput(ns("uiChangeTheme"))
  )
  return(themeOutput)
}


  # Server functions --------------------------------------------------------
serverChangeTheme <- function(input, output, session){
  observeEvent(
    input$dbxChangeTheme, 
    {
      output$uiChangeTheme <- renderUI({
        shinyDashboardThemes(theme = input$dbxChangeTheme)
      })
    }
  )
}


ui <- dashboardPage(
  dashboardHeader(title = "Inference Board"),
  dashboardSidebar(
    sidebarMenu(
      #menuItem("DashBoard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Themes", tabName = "tabThemes", icon = icon("chart-line")),
      menuItem("Spending Vs Rating", tabName = "SpendRate", icon = icon("chart-line")),
      menuItem("Survey Data", tabName = "data", icon = icon("table")),
      menuItem("Cochran's Formula(Table)", tabName = "cochran", icon = icon("table")),
      menuItem("Central Limit Theorem", tabName = "CLT", icon = icon("chart-bar")),
      menuItem("Bayesian Inference", tabName = "bayes", icon = icon("chart-line")),
      menuItem("Distributions", tabName = "distri", icon = icon("chart-bar")),
      menuItem("Tests", tabName = "tests", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    uiChangeThemeOutput(),
    tabItems(
      tabItem(
        tabName = "tabThemes",
          fluidRow(
            column(
              width = 12,
              uiChangeThemeDropdown()
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

                actionButton(inputId = "update", label = "Generate New Data", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                column(box(radioButtons(
                  inputId = "choice", label = "Canteen & Chill Spending/Rating Data",
                  choices = c("Spending Data" = "spend", "Rating Data" = "rate", 
                  "Spending*Rating Ratio (Canteen and Chill)" = "Mul",
                  "Spending/Rating Ratio (Canteen and Chill)" = "Div")          
                )), width = 5) 
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
                                "Range" = "range", "Median" = "med")
                  ), width = 12)
                , width = 3),
                column(
                  box(sliderInput(inputId = "iter2", label = "Number of observations", min = 1, max = 10000, value = 10000)),
                  box(sliderInput(inputId = "size2", label = "Sample Size", min = 1, max = length(temp[[1]]), value = 10))
                  ,width = 9
                )
              ),
              box(plotOutput("Distribution"), height = 400, width = 500, status = "warning")
      ),
      tabItem(tabName = "tests",
              tabsetPanel(
                tabPanel(title = "Confidence Interval",
                        box(radioButtons(
                          inputId = "CIChoice", label = "Data",
                          choices = c("Names Given" = "2","Gender" = "3", "Academic Section" = "4", "Academic Year" = "5",
                                      "Eating Means" = "6", "Frequency Canteen" = "7",
                                      "Frequency Chill" = "8"))),
                        DT::dataTableOutput(outputId = "confidenceInterval")                   
                ),
                tabPanel(title = "Non Parametric Tests",
                  verbatimTextOutput(outputId = "NonPara")
                ),
                tabPanel(title = "Chi-Square test of Independence",
                  fluidRow(
                    box(radioButtons(
                      inputId = "ChiIndieChoice", label = "Data",
                      choices = c("Eating Means" = "6", "Frequency Canteen" = "7",
                                  "Frequency Chill" = "8", "Spending Canteen" = "11",
                                  "Spending Canteen" = "12", "Rating Canteen" = "13",
                                  "Rating Chill" = "14", "Enough Campus Chills" = "17",
                                  "Canteen during Free Hours" = "21")))
                  ),        
                  verbatimTextOutput(outputId = "chi")
                ),
                tabPanel(title = "Linear Regression and ANOVA",
                  verbatimTextOutput(outputId = "anova")                                            
                )
              )
      )
    )
  )
)

server <- function(input, output, session){

  callModule(module = serverChangeTheme, id = "moduleChangeTheme")

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
                      rate = list("Canteen" = temp[[13]], "Chill" = temp[[14]]),
                      Mul = list("Canteen" = round(temp[[11]]*temp[[13]], digits = 2), "Chill" = round(temp[[12]]*temp[[14]], digits = 2)),
                      Div = list("Canteen" = round(temp[[11]]/temp[[13]], digits = 2), "Chill" = round(temp[[12]]/temp[[14]], digits = 2))
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
      "range" = R,
      "med" = median
    )
  })

  output$Distribution <- renderPlot({
    distribution(pop = temp[[11]], sampleSize = input$size2, iter = input$iter2, f = fun())
  })
  
  CIchoice <- eventReactive(input$CIChoice, {
    switch(input$CIChoice,
          "2" = isGiven(), "3" = temp[[3]], "4" = temp[[4]], "5" = temp[[5]],
          "6" = temp[[6]], "7" = temp[[7]],"8" = temp[[8]])
  })


  output$confidenceInterval <- DT::renderDataTable({
    DT::datatable(data = confidenceInterval(table(CIchoice())), options = list(scrollX = TRUE))
  })
  
  ChiIndieChoice <- eventReactive(input$ChiIndieChoice, {
    switch(input$ChiIndieChoice,
          "6" = temp[[6]], "7" = temp[[7]], "8" = temp[[8]],
          "11" = temp[[11]], "12" = temp[[12]], "13" = temp[[13]],
          "14" = temp[[14]], "17" = temp[[17]], "21" = temp[[21]])
  })
  

  output$chi <- renderPrint({
    chiSquareTestIndependence(table = table(temp[[3]], ChiIndieChoice()))
    
  })

  output$anova <- renderPrint({
    regressionIndependenceTest(temp[[11]] ~ temp[[12]] * temp[[13]] , temp)
    regressionIndependenceTest(temp[[12]] ~ temp[[11]] * temp[[14]] , temp)
  })

  output$NonPara <- renderPrint({
    bothNonPara <- function(){
      print(wilcox.test(temp[["Average amount of money spent in the Canteen per Week"]], temp[["Average amount of money spent in the Campus Chill per Week"]]))
      print(wilcox.test(temp[["Average amount of money spent in the Canteen per Week"]], temp[["Average amount of money spent in the Campus Chill per Week"]], paired = TRUE, correct = FALSE))
      print(wilcox.test(temp[["Rate the Canteen"]], temp[["Rate the Campus Chill (Xavier/IT Block)"]]))
      print(wilcox.test(temp[["Rate the Canteen"]], temp[["Rate the Campus Chill (Xavier/IT Block)"]], paired = TRUE, correct = FALSE))
    }
    
    bothNonPara()
      
  })
}

shinyApp(ui = ui, server = server)
