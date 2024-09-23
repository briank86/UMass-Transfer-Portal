library(shiny)
library(tidyverse)
library(DT)
library(googlesheets4)
library(rsconnect)

gs4_deauth()

hitters <- read_sheet('https://docs.google.com/spreadsheets/d/1rzIV3JBkimw9iJ9zTSETHAQUmAPukoS7DPBRbadQ-FY/edit#gid=0', "POSITION PLAYERS")

pitchers <- read_sheet('https://docs.google.com/spreadsheets/d/1rzIV3JBkimw9iJ9zTSETHAQUmAPukoS7DPBRbadQ-FY/edit#gid=1040640301', "PITCHERS")

hitters <- hitters %>% unite(NAME, FIRST, LAST, sep = " ")
pitchers <- pitchers %>% unite(NAME, FIRST, LAST, sep = " ")

ui <- fluidPage(
  column(10, offset = 1,
         titlePanel("UMass Baseball Transfer Portal Dashboard"),
         hr(style="border-color: black;"), 
         fluidRow(
           column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(pitchers$NAME)))),
           column(2, selectInput(inputId = "HitterInput", label = "Select Hitter", choices = sort(unique(hitters$NAME))))
         ),
         hr(style="border-color: black;"),
         wellPanel(style = "background: white; border-color:black; border-width:2px",
                   fluidRow(
                     column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"), h2(strong(textOutput("selected_hitter"))), style = "padding-right:0px;"),
                     column(8, h2("Player Dashboard"), hr(style="border-color: black;"), align = "right", style = "padding-left:0px;")),
                   hr(style="border-color: black;"), 
                   fluidRow(
                     column(10, offset = 1, h3(strong("Pitcher Summary")), dataTableOutput("pitcher_summary_table"), align = "center"), 
                     column(10, offset = 1, h3(strong("Hitter Summary")), dataTableOutput("hitter_summary_table"), align = "center")
                   ), br(), br(),  br(), br(), br()
         ), br(),
         p(em("If the contents of this page appear distorted, decrease zoom to 50%-67%"), align = "center")
         
  )
)



server <- function(input, output, session) {
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)})
  
  output$selected_hitter <- renderText({paste(input$HitterInput)})
  
  
  output$pitcher_summary_table <- renderDataTable({
    table <- pitchers %>%
      filter(NAME == input$PitcherInput)
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17), `border-left` = "solid 1px")
    
  })
  
  
  
  output$hitter_summary_table <- renderDataTable({
    table <- hitters %>%
      filter(NAME == input$HitterInput)
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  
}

shinyApp(ui = ui, server = server)