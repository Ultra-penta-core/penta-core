#install.packages("shiny")
library(shiny)

ui <- fluidPage(
  h1("hello, shiny"),
  img(src="https://d33wubrfki0l68.cloudfront.net/0c97eee3d8fc820f3a8d670c08b286e8a524257b/e426c/cover.png")
)

server <- function(input, output, session)
{
  
  # do something here
}
  
shinyApp(ui = ui ,server = server)
