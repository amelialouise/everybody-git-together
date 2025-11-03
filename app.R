library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(purrr)
library(DT)

# Generate synthetic data
set.seed(123)
generate_data <- function() {
  insurers <- paste0("Insurer_", LETTERS[1:50])
  years <- 2020:2024

  data <- expand.grid(insurer = insurers, year = years) %>%
    mutate(
      # Generate premium written with some variation by year and insurer
      base_premium = runif(n(), 2, 13.6),
      year_factor = case_when(
        year == 2020 ~ 0.85,
        year == 2021 ~ 0.92,
        year == 2022 ~ 1.0,
        year == 2023 ~ 1.05,
        year == 2024 ~ 1.1
      ),
      premium_written = base_premium * year_factor * runif(n(), 0.8, 1.2),

      # Generate LCM with some correlation to insurer size (larger insurers tend to have lower LCM)
      size_factor = (premium_written - min(premium_written)) /
        (max(premium_written) - min(premium_written)),
      lcm = 0.75 + (1.25 * (1 - size_factor * 0.6)) + rnorm(n(), 0, 0.15),
      lcm = pmax(0.75, pmin(2.0, lcm)) # Bound between 0.75 and 2.0
    ) %>%
    select(insurer, year, premium_written, lcm) %>%
    arrange(year, desc(premium_written))

  return(data)
}

# Generate the data
insurance_data <- generate_data()

ui <- page_sidebar(
  title = "Loss Cost Multiplier Sensitivity Analysis",
  sidebar = sidebar(
    width = 350,
    h4("Analysis Controls"),

    selectInput(
      "selected_year",
      "Select Year:",
      choices = 2020:2024,
      selected = 2024
    ),

    sliderInput(
      "top_n",
      "Number of Top Insurers (by Premium):",
      min = 5,
      max = 50,
      value = 30,
      step = 1
    ),

    hr(),

    h5("Current Analysis Summary:"),
    verbatimTextOutput("summary_stats")
  ),

  layout_columns(
    card(
      card_header("Weighted Average LCM by Number of Insurers"),
      plotlyOutput("sensitivity_plot", height = "400px")
    ),

    card(
      card_header("Premium Distribution"),
      plotlyOutput("premium_plot", height = "400px")
    ),

    col_widths = c(6, 6)
  ),

  layout_columns(
    card(
      card_header("Insurers Included in Calculation"),
      DTOutput("insurers_table")
    ),

    col_widths = 12
  )
)

server <- function(input, output, session) {
  # Reactive data for selected year
  year_data <- reactive({
    insurance_data %>%
      filter(year == input$selected_year) %>%
      arrange(desc(premium_written))
  })

  # Calculate weighted average for different numbers of top insurers
  sensitivity_data <- reactive({
    data <- year_data()

    results <- map_dfr(5:50, function(n) {
      top_n_data <- data %>% slice_head(n = n)

      weighted_avg <- sum(top_n_data$lcm * top_n_data$premium_written) /
        sum(top_n_data$premium_written)

      total_premium <- sum(top_n_data$premium_written)

      tibble(
        n_insurers = n,
        weighted_lcm = weighted_avg,
        total_premium = total_premium,
        premium_coverage = total_premium / sum(data$premium_written) * 100
      )
    })

    return(results)
  })

  # Current calculation based on slider input
  current_calculation <- reactive({
    data <- year_data() %>% slice_head(n = input$top_n)

    weighted_avg <- sum(data$lcm * data$premium_written) /
      sum(data$premium_written)
    total_premium <- sum(data$premium_written)

    list(
      weighted_lcm = weighted_avg,
      total_premium = total_premium,
      n_insurers = input$top_n,
      data = data
    )
  })

  # Summary stats output
  output$summary_stats <- renderText({
    calc <- current_calculation()
    paste0(
      "Weighted Avg LCM: ",
      round(calc$weighted_lcm, 3),
      "\n",
      "Total Premium: $",
      round(calc$total_premium, 1),
      "M\n",
      "Number of Insurers: ",
      calc$n_insurers,
      "\n",
      "Premium Coverage: ",
      round(calc$total_premium / sum(year_data()$premium_written) * 100, 1),
      "%"
    )
  })

  # Sensitivity plot
  output$sensitivity_plot <- renderPlotly({
    sens_data <- sensitivity_data()
    current <- current_calculation()

    p <- ggplot(sens_data, aes(x = n_insurers, y = weighted_lcm)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(color = "steelblue", size = 2) +
      geom_point(
        data = data.frame(x = current$n_insurers, y = current$weighted_lcm),
        aes(x = x, y = y),
        color = "red",
        size = 4
      ) +
      labs(
        title = paste("Sensitivity Analysis for", input$selected_year),
        x = "Number of Top Insurers Included",
        y = "Weighted Average LCM"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    ggplotly(p, tooltip = c("x", "y"))
  })

  # Premium distribution plot
  output$premium_plot <- renderPlotly({
    data <- year_data() %>%
      slice_head(n = input$top_n) %>%
      mutate(rank = row_number())

    p <- ggplot(data, aes(x = rank, y = premium_written, fill = lcm)) +
      geom_col() +
      scale_fill_gradient2(
        low = "green",
        mid = "yellow",
        high = "red",
        midpoint = 1.375,
        name = "LCM"
      ) +
      labs(
        title = paste("Premium Distribution - Top", input$top_n, "Insurers"),
        x = "Insurer Rank (by Premium)",
        y = "Premium Written ($M)"
      ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 12))

    ggplotly(p)
  })

  # Insurers table
  output$insurers_table <- renderDT({
    current_calculation()$data %>%
      mutate(
        rank = row_number(),
        premium_written = round(premium_written, 2),
        lcm = round(lcm, 3),
        weight = premium_written / sum(premium_written),
        weighted_contribution = lcm * weight
      ) %>%
      select(
        Rank = rank,
        Insurer = insurer,
        `Premium ($M)` = premium_written,
        LCM = lcm,
        `Weight (%)` = weight,
        `Weighted Contrib` = weighted_contribution
      ) %>%
      datatable(
        options = list(
          pageLength = 15,
          scrollY = "400px",
          scrollCollapse = TRUE
        )
      ) %>%
      formatPercentage("Weight (%)", digits = 1) %>%
      formatRound(c("Weighted Contrib"), digits = 4)
  })
}

shinyApp(ui = ui, server = server)
