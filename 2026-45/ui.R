library(shiny)
library(tidyverse)
library(tidytuesdayR)
library(DT)
library(plotly)

fluidPage(
  titlePanel(str_to_title("evidence-graded power rankings of fictional characters")),
  
  selectInput("franchaise_opt", label = "Franchaise", choices = c("All Franchises",  tt_load(2026, week = 45)$fiction_rankings$franchise |> unique()),
              selected = "All Franchises"),
  
  textOutput("filter_text"),
  br(),
  DTOutput("table_filter"),
  br(),
  plotlyOutput("bp", height = "880px")
  # plotOutput("boxplot"),
  # br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),
  # plotlyOutput("plotly_box", height = "880px")

  # actionButton("select_all", "Select all filtered rows"),
  # actionButton("clear_selection", "Clear selection")
)