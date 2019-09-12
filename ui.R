require(shiny)
require(shinythemes)

shinyUI(fluidPage(theme = shinytheme("united"),
    tags$head(
      tags$title('QPRD by Lytze'),
      tags$link(rel = "stylesheet", href = "lib/font-awesome/css/font-awesome.min.css")
    ),
    titlePanel(
      h2("Quantile / Probability / Random Distributoin")
    ),
    sidebarLayout(position = "right",
        sidebarPanel(
            selectInput(
                inputId = "I_select_distribution",
                label = "Select Distribution",
                choices = c("Standarized Normal" = "Z", "Student's T" = "T"),
                selected = "Normal"
            ),
            tags$hr(),
            conditionalPanel(
                condition = "input.I_select_distribution == 'Z'",
                sliderInput(
                    label = "Quantile",
                    inputId = "I_Z_quantile",
                    value = 0.50, min = 0.00, max = 1.00, step = 0.001
                ),
                sliderInput(
                    label = "Statistic (Z)",
                    inputId = "I_Z_stat",
                    value = 0.00, min = -5, max = 5, step = 0.001
                )
            ),
            conditionalPanel(
                condition = "input.I_select_distribution == 'T'",
                numericInput(
                    label = "Degree of Freedom",
                    inputId = "I_T_df",
                    value = 10, min = 2, max = Inf, step = 1
                ),
                numericInput(
                    label = "Quantile",
                    inputId = "I_T_quantile",
                    value = 0.50, min = 0.00, max = 1.00, step = 0.001
                ),
                numericInput(
                    label = "Statistic (T)",
                    inputId = "I_T_stat",
                    value = 0.00, min = -Inf, max = Inf, step = 0.01
                )
            )
        ),
        mainPanel(
            conditionalPanel(condition = "input.I_select_distribution == 'Z'",
                             plotOutput(outputId = "O_Z_distribution")),
            conditionalPanel(condition = "input.I_select_distribution == 'T'",
                             plotOutput(outputId = "O_T_distribution"))
        )
                
    )
))