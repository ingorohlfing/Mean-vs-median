library(shiny)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Mittelwert und Median"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs5", "Wert der 5. Beobachtung (rot):",
                  min = 5, max = 50, value = 10),
      br(),
      textOutput("mean_output"),
      textOutput("median_output")
    ),
    mainPanel(
      plotOutput("obs_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Calculate mean and median based on the 9th observation value
  observe({
    sample <- c(1:4, input$obs5)
    mean_val <- mean(sample)
    median_val <- median(sample)

    output$mean_output <- renderText(paste("Mittelwert: ", mean_val))
    output$median_output <- renderText(paste("Median: ", median_val))
  })

  # Plot the observations using ggplot2
  output$obs_plot <- renderPlot({
    sample <- c(1:4, input$obs5)
    df <- data.frame(hor = sample, vert = 1)

    ggplot() +
      geom_point(data = df[1:4, ], aes(x = hor, y = vert), size = 3) +
      geom_point(data = df[5, ], aes(x = hor, y = vert), size = 3, color = "red") +
      geom_vline(xintercept = mean(sample), color = "black", linetype = 5) +
      geom_vline(xintercept = median(sample), color = "blue", linetype = 5) +
      scale_x_continuous(breaks = 1:input$obs5) +
      scale_y_discrete(breaks = NULL) +
      theme_minimal() +
      labs(x = "Wert", y = "", title = "Fünf Beobachtungen",
           caption = "Blaue Vertikale: Median
           Schwarze Vertikale: Mittelwert") +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 14),
            plot.title = element_text(size = 16),
            plot.caption = element_text(size = 14))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
