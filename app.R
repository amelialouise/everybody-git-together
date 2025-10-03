library(shiny)
library(bslib)
library(ggplot2)

# Define UI
ui <- page_sidebar(
  title = "Sugar Skull Designer",
  sidebar = sidebar(
    h4("Customize Your Sugar Skull"),
    selectInput("skull_color", "Skull Base Color:",
                choices = c("White" = "white", "Cream" = "ivory", "Light Pink" = "lightpink", 
                           "Light Blue" = "lightblue", "Light Yellow" = "lightyellow"),
                selected = "white"),
    
    selectInput("eye_color", "Eye Socket Color:",
                choices = c("Black" = "black", "Dark Blue" = "darkblue", "Purple" = "purple", 
                           "Dark Green" = "darkgreen", "Maroon" = "maroon"),
                selected = "black"),
    
    selectInput("flower_color", "Flower Color:",
                choices = c("Red" = "red", "Pink" = "hotpink", "Orange" = "orange", 
                           "Yellow" = "gold", "Purple" = "mediumorchid", "Blue" = "dodgerblue"),
                selected = "red"),
    
    selectInput("pattern_color", "Pattern Color:",
                choices = c("Black" = "black", "Dark Blue" = "darkblue", "Purple" = "purple", 
                           "Dark Green" = "darkgreen", "Red" = "red", "Gold" = "gold"),
                selected = "black"),
    
    sliderInput("pattern_size", "Pattern Size:",
                min = 0.5, max = 2, value = 1, step = 0.1),
    
    checkboxInput("show_flowers", "Show Flowers", value = TRUE),
    checkboxInput("show_patterns", "Show Decorative Patterns", value = TRUE),
    
    actionButton("randomize", "Randomize Colors", class = "btn-primary")
  ),
  
  card(
    card_header("Your Sugar Skull"),
    plotOutput("skull_plot", height = "600px")
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive values for colors (to support randomization)
  skull_colors <- reactiveValues(
    skull = "white",
    eye = "black", 
    flower = "red",
    pattern = "black"
  )
  
  # Update reactive values when inputs change
  observe({
    skull_colors$skull <- input$skull_color
    skull_colors$eye <- input$eye_color
    skull_colors$flower <- input$flower_color
    skull_colors$pattern <- input$pattern_color
  })
  
  # Randomize colors
  observeEvent(input$randomize, {
    colors <- c("red", "blue", "green", "purple", "orange", "pink", "yellow", "cyan", "magenta")
    dark_colors <- c("black", "darkblue", "darkgreen", "purple", "maroon", "darkred")
    light_colors <- c("white", "ivory", "lightpink", "lightblue", "lightyellow", "lavender")
    
    updateSelectInput(session, "skull_color", selected = sample(light_colors, 1))
    updateSelectInput(session, "eye_color", selected = sample(dark_colors, 1))
    updateSelectInput(session, "flower_color", selected = sample(colors, 1))
    updateSelectInput(session, "pattern_color", selected = sample(dark_colors, 1))
  })
  
  output$skull_plot <- renderPlot({
    # Create the sugar skull plot
    p <- ggplot() + 
      theme_void() +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      ) +
      coord_fixed(ratio = 1) +
      xlim(-10, 10) +
      ylim(-12, 8)
    
    # Skull base (main shape)
    skull_x <- c(-6, -6, -5, -3, 3, 5, 6, 6, 5, 3, 2, 1, -1, -2, -3, -5)
    skull_y <- c(-2, 4, 6, 7, 7, 6, 4, -2, -4, -6, -8, -10, -10, -8, -6, -4)
    
    p <- p + geom_polygon(aes(x = skull_x, y = skull_y), 
                         fill = skull_colors$skull, 
                         color = skull_colors$pattern, 
                         linewidth = 1.5)
    
    # Eye sockets
    p <- p + 
      geom_point(aes(x = -2.5, y = 2), size = 25, color = skull_colors$eye, shape = 16) +
      geom_point(aes(x = 2.5, y = 2), size = 25, color = skull_colors$eye, shape = 16)
    
    # Eye centers (decorative dots)
    p <- p +
      geom_point(aes(x = -2.5, y = 2), size = 8, color = skull_colors$flower, shape = 16) +
      geom_point(aes(x = 2.5, y = 2), size = 8, color = skull_colors$flower, shape = 16)
    
    # Nasal cavity (heart shape)
    nose_x <- c(0, -0.8, -0.5, 0, 0.5, 0.8, 0)
    nose_y <- c(-1, 0.2, 1, 0.5, 1, 0.2, -1)
    p <- p + geom_polygon(aes(x = nose_x, y = nose_y), 
                         fill = skull_colors$eye, 
                         color = skull_colors$pattern)
    
    # Mouth (stitched smile)
    mouth_x <- seq(-3, 3, length.out = 20)
    mouth_y <- -4 + 0.3 * sin(mouth_x)
    p <- p + geom_line(aes(x = mouth_x, y = mouth_y), 
                      color = skull_colors$pattern, 
                      linewidth = 2 * input$pattern_size)
    
    # Mouth stitches
    for(i in seq(-2.5, 2.5, by = 1)) {
      p <- p + geom_segment(aes(x = i, y = -4 + 0.3 * sin(i) - 0.3, 
                               xend = i, yend = -4 + 0.3 * sin(i) + 0.3),
                           color = skull_colors$pattern, 
                           linewidth = 1 * input$pattern_size)
    }
    
    # Add decorative patterns if enabled
    if(input$show_patterns) {
      # Forehead pattern (swirls)
      t <- seq(0, 4*pi, length.out = 100)
      spiral_x <- 0 + 1.5 * cos(t) * exp(-t/10)
      spiral_y <- 4.5 + 1.5 * sin(t) * exp(-t/10)
      p <- p + geom_path(aes(x = spiral_x, y = spiral_y), 
                        color = skull_colors$pattern, 
                        linewidth = 2 * input$pattern_size)
      
      # Side decorations (dots and lines)
      for(i in 1:5) {
        p <- p + 
          geom_point(aes(x = -4.5, y = 1 - i*0.8), 
                    size = 3 * input$pattern_size, 
                    color = skull_colors$pattern) +
          geom_point(aes(x = 4.5, y = 1 - i*0.8), 
                    size = 3 * input$pattern_size, 
                    color = skull_colors$pattern)
      }
    }
    
    # Add flowers if enabled
    if(input$show_flowers) {
      # Left flower
      flower_angles <- seq(0, 2*pi, length.out = 6)
      left_flower_x <- -4 + 0.8 * cos(flower_angles)
      left_flower_y <- 5 + 0.8 * sin(flower_angles)
      
      for(i in 1:5) {
        p <- p + geom_point(aes(x = left_flower_x[i], y = left_flower_y[i]), 
                           size = 8, color = skull_colors$flower, shape = 16)
      }
      p <- p + geom_point(aes(x = -4, y = 5), size = 6, color = "yellow", shape = 16)
      
      # Right flower
      right_flower_x <- 4 + 0.8 * cos(flower_angles)
      right_flower_y <- 5 + 0.8 * sin(flower_angles)
      
      for(i in 1:5) {
        p <- p + geom_point(aes(x = right_flower_x[i], y = right_flower_y[i]), 
                           size = 8, color = skull_colors$flower, shape = 16)
      }
      p <- p + geom_point(aes(x = 4, y = 5), size = 6, color = "yellow", shape = 16)
    }
    
    # Add some sparkle points around the skull
    sparkle_x <- c(-7, -6, -8, 7, 6, 8, 0, -1, 1)
    sparkle_y <- c(3, 1, -1, 3, 1, -1, 7, 6.5, 6.5)
    p <- p + geom_point(aes(x = sparkle_x, y = sparkle_y), 
                       size = 4, color = skull_colors$flower, shape = 8)
    
    print(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
