library(shiny)

ui <- fluidPage(
  selectInput("dataset", label = "데이터셋",
              choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session)
{
  output$summary <- renderPrint({
    dataset <- get(input$dataset, "package:datasets")
    summary(dataset)
  })
  
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    dataset
  })
  # do something here
}

shinyApp(ui = ui ,server = server)
