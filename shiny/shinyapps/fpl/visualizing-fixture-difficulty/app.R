library(tidyverse)
library(jsonlite)
library(plotly)
library(shiny)
library(ggthemes)
library(RColorBrewer)

# Load data
# ------------------------------------------------------------------------------
app_data <- readRDS("./app_data.RDS")

# Define UI
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$h4(
    style = "text-align:center;font-weight:bold;margin-bottom:2em",
    "Fixture difficulty (Premier League 2019-20)"     
  ),
  
  fluidRow(
    column(
      width = 3,
      offset = 3,
      selectInput(
        inputId = "fill_variable",
        label = "Select variable to display:",
        choices = list(
          "Opponent overall strength" = "opponent_strength_overall",
          "Opponent attack strength" = "opponent_strength_attack",
          "Opponent defence strength" = "opponent_strength_defence"
        )
      )  # End selectInput
    ),  # End column
    column(
      width = 6,
      radioButtons(
        inputId = "fill_mode",
        label = "Display options:",
        choices = list(
          "Show opponent strength" = "abs",
          "Show strength difference" = "diff"
        )
      )  # End radioButtons
    )  # End column
  ),  # End fluidRow
  fluidRow(
    column(
      width = 12,
      plotlyOutput(outputId = "plot")
    )  # End column
  )  # End fluidRow
)  # End fluidPage

# Define server function
# ------------------------------------------------------------------------------
server <- function(input, output) {
  # Function for getting fill variable based on inputs
  get_fill_var <- function(variable = input$fill_variable, mode = input$fill_mode) {
    ifelse(
      mode == "diff",
      str_replace(variable, "opponent", "difference"),
      variable
    ) %>% 
      return()
  }
  
  # Prepare data for plot based on inputs
  gen_plot_data <- reactive({
    app_data %>% 
      mutate(
        fill_variable := !!sym(get_fill_var())
      )
  })
  
  # Make the plot
  gen_plot <- function(data = gen_plot_data()) {
    p <- data %>% 
      ggplot(aes(x = gameweek, y = team, fill = fill_variable)) +
      suppressWarnings(
        geom_raster(
          aes(text = paste0(
            # Line 1
            "<b>", team, " (GW", gameweek, ifelse(is_home, " Home", " Away"), ")</b><br>",
            # Line 2
            case_when(
              str_detect(input$fill_variable, "overall") ~ "&nbsp;&nbsp;Overall strength: ",
              str_detect(input$fill_variable, "attack") ~ "&nbsp;&nbsp;Defence strength: ",         
              str_detect(input$fill_variable, "defence") ~ "&nbsp;&nbsp;Attack strength: "         
            ),
            case_when(
              str_detect(input$fill_variable, "overall") ~ team_strength_overall,         
              str_detect(input$fill_variable, "attack") ~ team_strength_defence,
              str_detect(input$fill_variable, "defence") ~ team_strength_attack         
            ),
            "<br><br>",
            # Line 3
            "<b>vs. ", opponent_team, "</b><br>",
            # Line 4
            case_when(
              str_detect(input$fill_variable, "overall") ~ "&nbsp;&nbsp;Overall strength: ",
              str_detect(input$fill_variable, "attack") ~ "&nbsp;&nbsp;Attack strength: ",         
              str_detect(input$fill_variable, "defence") ~ "&nbsp;&nbsp;Defence strength: "              
            ),
            case_when(
              str_detect(input$fill_variable, "overall") ~ opponent_strength_overall,
              str_detect(input$fill_variable, "attack") ~ opponent_strength_attack,
              str_detect(input$fill_variable, "defence") ~ opponent_strength_defence
            ),
            "<br><br>",
            # Line 5
            "Color value: ", fill_variable, "<br><br>",
            # Line 6 - 7
            "Kickoff time (GMT):<br>", kickoff_time
          ))
        )
      ) +
      scale_x_continuous(
        limits = c(0, 39),
        breaks = seq(2, 38, 2),
        expand = c(0, 0)
      ) +
      scale_fill_distiller(palette = "YlOrRd", direction = 1) +
      theme_tufte(base_family = "sans-serif") +
      theme(legend.position = "bottom") +
      labs(
        x = "Gameweek",
        y = "",
        fill = "",
        title = case_when(
          input$fill_mode == "abs" & input$fill_variable %>% str_detect("overall") ~ "Opponent overall strength",
          input$fill_mode == "abs" & input$fill_variable %>% str_detect("attack") ~ "Opponent attack strength",           
          input$fill_mode == "abs" & input$fill_variable %>% str_detect("defence") ~ "Opponent defence strength",           
          input$fill_mode == "diff" & input$fill_variable %>% str_detect("overall") ~ "Opponent overall strength - Team overall strength",           
          input$fill_mode == "diff" & input$fill_variable %>% str_detect("attack") ~ "Opponent attack strength - Team defence strength",                      
          input$fill_mode == "diff" & input$fill_variable %>% str_detect("defence") ~ "Opponent defence strength - Team attack strength"
        ) %>% paste("Color logic:", .)
      )
    
    
    ggplotly(p, tooltip = "text") %>% 
      # Disable unneccessary plotly functionality
      layout(
        legend = list(orientation = "h", x = -0.5, y = -1),
        xaxis = list(fixedrange = T),  
        yaxis = list(fixedrange = T)
      ) %>% 
      config(displayModeBar = F) 
  }
  
  # Render the plot
  output$plot <- renderPlotly(gen_plot())
  
}  # End server

shinyApp(ui, server)
