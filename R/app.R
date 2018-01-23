task = bh.task

ui <- dashboardPage(
  dashboardHeader(title="shapley value"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
      menuItem("single Value",tabName = "single",icon=icon("single")),
      menuItem("multi Value",tabName = "multi",icon=icon("multi")),
      menuItem("features",tabName = "features",icon=icon("features"))
    )
  ),
  dashboardBody(
    #Boxes need to be put in a row (or colum)
    tabItems(
      tabItem(tabName = "dashboard",
              h2("shaply value"),
              fluidRow(
                box(tableOutput('table'),
                    height = 250,
                    width = 50),
                box(
                  title = "choose the observation rows",
                  sliderInput("row", "the Range of rows:",
                              min = 1, max = nrow(getTaskData(task)),
                              value = c(1,1)),
                  width = 50
                )
              )
      ),
      tabItem(tabName = "single",
              h2("single tab content"),
              fluidRow(
                box(plotOutput("plot1")),
                box(
                  title = "choose the observation row",
                  sliderInput("slider","Row number :",1,nrow(getTaskData(task)),50)
                )
              )
      ),
      tabItem(tabName = "multi",
              h2("multi tab content"),
              fluidRow(
                box(plotOutput("plot2")),
                box(
                  title = "choose the observation rows",
                  sliderInput("range", "the Range of rows:",
                              min = 1, max = nrow(getTaskData(task)),
                              value = c(1,50))
                )
              )
      ),
      tabItem(tabName = "features",
              h2("features tab content"),
              fluidRow(
                box(plotOutput("plot3")),
                box(
                  title = "choose the observation rows",
                  sliderInput("range1", "the Range of rows:",
                              min = 1, max = nrow(getTaskData(task)),
                              value = c(1,50)),
                  selectInput(inputId = "feat",label="choose the observation feature",
                              getTaskFeatureNames(task),multiple=TRUE,selectize=TRUE)
                )
              )
      )
    )

  )

)

server <- function(input, output) {
  output$table <- renderTable({
    s = input$row
    t = s[1]:s[2]
    shapley(t)
  })

  output$plot1 <- renderPlot({

    plot.shapley.singleValue(input$slider)

  })
  output$plot2 <- renderPlot({

    s = input$range
    t = s[1]:s[2]
    plot.shapley.multipleValues(t)

  })

  output$plot3 <- renderPlot({

    s = input$range1
    t = s[1]:s[2]

    f = input$feat
    if(is.null(f))
      f = sample(getTaskFeatureNames(task),1)

    plot.shapley.multipleFeatures(t,features = f)

  })

}

shinyApp(ui, server)
