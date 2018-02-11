task = bh.task

ui = dashboardPage(
  dashboardHeader(title="shapley value"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("single Value", tabName = "single", icon = icon("single")),
      menuItem("multi Value", tabName = "multi", icon = icon("multi")),
      menuItem("features", tabName = "features", icon = icon("features"))
    )
  ),
  dashboardBody(
    tabItems(
      # shapley value table
      tabItem(tabName = "dashboard", h2("shaply value"),
        fluidRow(
          box(title = "choose the observation rows", width = 50,
            sliderInput("row", "the Range of rows:", min = 1,
              max = nrow(getTaskData(task)), value = c(1, 1))
          ),
          box(tableOutput('table'), width = 50)
        )
      ),
      # single value plot
      tabItem(tabName = "single", h2("single tab content"),
        fluidRow(
          box(plotOutput("plot1")),
          box(title = "choose the observation row",
            sliderInput("slider","Row number :",1, nrow(getTaskData(task)),50)
          )
        )
      ),
      # multiple values plot
      tabItem( tabName = "multi", h2("multi tab content"),
        fluidRow(box(
          plotOutput("plot2")),
          box( title = "choose the observation rows",
            sliderInput("range", "the Range of rows:",
              min = 1, max = nrow(getTaskData(task)), value = c(1,50))
          )
        )
      ),
      # features plot
      tabItem( tabName = "features", h2("features tab content"),
        fluidRow(
          box(plotOutput("plot3")),
          box( title = "choose the observation rows",
            sliderInput("range1", "the Range of rows:",
              min = 1, max = nrow(getTaskData(task)), value = c(1,50)),
            selectInput(inputId = "feat", label="choose the observation feature",
              getTaskFeatureNames(task), multiple=TRUE, selectize=TRUE)
          )
        )
      )
    )
  )
)
server = function(input, output) {
  output$table = renderTable({
    shapley(input$row[1]:input$row[2])
  })

  output$plot1 = renderPlot({
    plot.shapley.singleValue(input$slider)
  })

  output$plot2 = renderPlot({
    plot.shapley.multipleValues(input$range[1]:input$range[2])
  })

  output$plot3 = renderPlot({
    if(is.null(input$feat))
      input$feat = sample(getTaskFeatureNames(task),1)

    plot.shapley.multipleFeatures(input$range1[1]:input$range1[2],
      features = input$feat)
  })
}

shinyApp(ui, server)
