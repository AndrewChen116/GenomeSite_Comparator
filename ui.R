library(shiny)
library(tidyverse)
library(parallel)
library(bslib)
library(DT)

css <- "mark{
          padding: 0;
          background-color:#FFFF93;
          color:#930000;
        }"

ui <- navbarPage(
  # Title 
  titlePanel(
    h3("GenomeSite Comparator"),
    windowTitle = "GenomeSite_Comparator"
  ),
  theme = bs_theme(
    bootswatch = "darkly"
  ),
  
  sidebarLayout(
    # side panel
    sidebarPanel(
      fileInput(
        "target_file", h6(strong("Target file"), style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      #htmlOutput("print_A") %>% h6(.,align="center",style = "color:#00BB00"),
      fileInput(
        "anchor_file", h6(strong("Anchor file"),style = "color:#97CBFF"),
        accept = ".tsv", multiple = F ,width = "100%",
        placeholder = "Please input tsv data "
      ),
      #htmlOutput("print_T") %>% h6(.,align="center",style = "color:#00BB00"),
      fluidRow(
        column(
          numericInput(
            "f_nt",h6(strong("Forward range (nt)"),style = "color:#97CBFF"),
            value = 20, min = 1, max = 100000
          ),
          width = 6
        ),
        column(
          numericInput(
            "b_nt",h6(strong("Backward range (nt)"),style = "color:#97CBFF"),
            value = 20, min = 1, max = 100000
          ),
          width = 6
        )
      ),
      fluidRow(
        column(
          selectInput(
            "DoParallel",h6(strong("Parallel calculating"),style = "color:#97CBFF"),
            choices = c("TRUE", "FALSE")
          ),
          width = 6
        ),
        column(
          sliderInput(
            "ncore",h6(strong("Cores"),style = "color:#97CBFF"),
            value = 4, min = 2, max = floor(detectCores()*0.8)
          ),
          width = 6
        )
      ),
      br(),
      fluidRow(
        column(
          actionButton(inputId="DoSearch", label=" Search",
                       icon=icon(name = "compass")),
          width = 3
        ),
        column(
          actionButton(inputId="DoExample", label=" Example",
                       icon=icon(name = "table")),
          width = 3
        ),
        column(
          width = 6
          
        )
      ),
      htmlOutput("print_time") %>% h6(.,align="center",style = "color:#B3D9D9"),
      br(),
      br(),
      br(),
      br(),
      h6("20220315_KLC_v0.1.0",align="right",style = "color:#6C6C6C"),
      width = 3
      
    ),
    # main panel
    mainPanel(
      tags$head(tags$style(HTML(css))),
      
      tabsetPanel(
        tabPanel(
          h5("Comparison result",style = "color:#97CBFF"),
          dataTableOutput("output_table"),
          width = 1
        )
      ),
      width = 9
    )
  )
)