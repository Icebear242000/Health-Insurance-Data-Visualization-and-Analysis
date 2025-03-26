install.packages(c("shiny", "plotly"))

library(shiny)
library(plotly)

# Define the UI
ui <- fluidPage(
  titlePanel("Insurance Dataset Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("x_axis", "Choose X-axis variable", choices = names(insurance_data)),
      selectInput("y_axis", "Choose Y-axis variable", choices = names(insurance_data)),
      sliderInput("point_size", "Select Point Size", min = 1, max = 10, value = 5),
      checkboxInput("show_grid", "Show Grid", value = TRUE)
      
    ),
    mainPanel(
      plotlyOutput("scatter_plot")
    )
  )
)

# Define the server
server <- function(input, output) {
  output$scatter_plot <- renderPlotly({
    plot_ly(data = insurance_data, x = ~get(input$x_axis), y = ~get(input$y_axis), mode = "markers") %>%
      layout(title = paste("Scatter Plot of", input$y_axis, "vs.", input$x_axis),
             xaxis = list(title = input$x_axis),
             yaxis = list(title = input$y_axis))
  })
}

# Run the Shiny app
shinyApp(ui, server)
