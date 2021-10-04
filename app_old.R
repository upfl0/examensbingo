library(shiny)
library(tidyverse)
library(shinyjs)


# Define UI for application that draws a histogram
ui <- navbarPage(
  "STEXbingo",
  tabPanel("Join",
           fluidPage(
             
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 tags$head(tags$script
                           ('
                                                    $(document).on("shiny:connected", function(e) {
                                                        Shiny.onInputChange("innerWidth", window.innerWidth);
                                                    });
                                                    $(window).resize(function(e) {
                                                        Shiny.onInputChange("innerWidth", window.innerWidth);
                                                    });
                                                    '),
                           tags$style(HTML('
                                                    .navbar-brand{
                                                    font-family: "Georgia", Times, "Times New Roman", serif;
                                                    font-weight: bold;
                                                    font-size: 24px;
                                                    }'))
                 ),
                 textInput(inputId = "name_1",
                             label = "Vorname:"),
                 textInput(inputId = "name_2",
                           label = "Nachname:"),
                 textInput(inputId = "email",
                           label = "E-Mail Adresse:"),
                 checkboxInput(inputId = "datenschutz",
                               label = "Mit der Speicherung meiner Daten bin ich einverstanden. Weitere Informationen unter Datenschutz und Sicherheit."),
                 textInput(inputId = "bingo_1",
                           label = "Thema 1:"),
                 textInput(inputId = "bingo_2",
                           label = "Thema 2:"),
                 textInput(inputId = "bingo_3",
                           label = "Thema 3:"),
                 textInput(inputId = "bingo_4",
                           label = "Thema 4:"),
                 textInput(inputId = "bingo_5",
                           label = "Thema 5:"),
                 textInput(inputId = "bingo_6",
                           label = "Thema 6:"),
                 textInput(inputId = "bingo_7",
                           label = "Thema 7:"),
                 textInput(inputId = "bingo_8",
                           label = "Thema 8:"),
                 textInput(inputId = "bingo_9",
                           label = "Thema 9:"),
                 textInput(inputId = "bingo_10",
                           label = "Thema 10:"),
                 textInput(inputId = "bingo_11",
                           label = "Thema 11:"),
                 textInput(inputId = "bingo_12",
                           label = "Thema 12:"),
                 textInput(inputId = "bingo_13",
                           label = "Thema 13:"),
                 textInput(inputId = "bingo_14",
                           label = "Thema 14:"),
                 textInput(inputId = "bingo_15",
                           label = "Thema 15:"),
                 textInput(inputId = "bingo_16",
                           label = "Thema 16:"),
                 textInput(inputId = "bingo_17",
                           label = "Thema 17:"),
                 textInput(inputId = "bingo_18",
                           label = "Thema 18:"),
                 textInput(inputId = "bingo_19",
                           label = "Thema 19:"),
                 textInput(inputId = "bingo_20",
                           label = "Thema 20:"),
                 textInput(inputId = "bingo_21",
                           label = "Thema 21:"),
                 textInput(inputId = "bingo_22",
                           label = "Thema 22:"),
                 textInput(inputId = "bingo_23",
                           label = "Thema 23:"),
                 textInput(inputId = "bingo_24",
                           label = "Thema 24:"),
                 textInput(inputId = "bingo_25",
                           label = "Thema 25:")
               ),
               
               mainPanel(
                 fluidRow(h3("Willkommen zum Stexbingo"), align = "center"),
                 br(),
                 br(),
                 fluidRow(h4("Mitmachen ist ganz einfach. Genaue Infos findet ihr unter 'About'. Folgende Regeln gelten:"), align="center"),
                 br(),
                 fluidRow(tableOutput('bingo_table'), align = "center"),
                 br(),
                 fluidRow(downloadButton("downloadData", "Download Bingo (Für Teilnahme verpflichtend!)"), align = "center"),
                 br()
                 
               )))),
  
  tabPanel("About",
           fluidPage(
             fluidRow(div(HTML("<em>Wait! That's illegal</em> - Netter's Neurology")), align = "center"),
             br(),
             
             
             navlistPanel(widths = c(3, 9),
                          "Dashboard",
                          tabPanel("Creators",
                                   fluidRow(
                                     br(),
                                     br(),
                                     br(), 
                                     div(id = "authors",
                                         "Bingo package by", a("Jenny Bryan", href = "https://twitter.com/jennybryan"),
                                         "and", a("Dean Attali", href = "http://deanattali.com"),
                              
                                     div(id = "modifier"),
                                         "Modified by", a("Florian Rumpf", href = ""), ".",
                                         "Code", a("on GitHub", href = "https://github.com/upfl0/examensbingo")
                                     ),
                                     align = "center"   
                                   )
                          ),
                          tabPanel("Shiny"),
                          "STEX 101",
                          tabPanel("Stexmemes",
                                   fluidRow(
                                     br(),
                                     br(),
                                     br(),
                                     uiOutput("link_memes"), 
                                     align = "center"
                                   )
                          ),
                          tabPanel("Amboss Ergebnisse",
                                   fluidRow(
                                     br(),
                                     br(),
                                     br(),
                                     uiOutput("link_amboss"), 
                                     align = "center"
                                   )
                          )
             )
             
           )
  ),
  
  tabPanel("Donate"
  )
  
  
)


make_bingo_vector <- function(input){
  return(sample(c(input$bingo_1, input$bingo_2, input$bingo_3, input$bingo_4, input$bingo_5,
                         input$bingo_6, input$bingo_7, input$bingo_8, input$bingo_9, input$bingo_10,
                         input$bingo_11, input$bingo_12, input$bingo_13, input$bingo_14, input$bingo_15,
                         input$bingo_16, input$bingo_17, input$bingo_18, input$bingo_19, input$bingo_20,
                         input$bingo_21, input$bingo_22, input$bingo_23, input$bingo_24, input$bingo_25))
  )
}


prep_bingo <- function(input){
  bingo_list <- make_bingo_vector(input)
  bingo_mat <- rbind(bingo_list[1:5], bingo_list[6:10], bingo_list[11:15], bingo_list[16:20], bingo_list[21:25])
  
  nrows <- 5
  ncols <- 5
  
  return(bingo_mat)
}


# Define server logic 
server <- function(input, output, session) {
  
  ###Bingo Table
  output$bingo_table <- renderTable({prep_bingo(input)},  
                            bordered = TRUE,  
                            spacing = 'l',  
                            align = 'c',  
                            colnames = FALSE)
  
  ###Download Bingo
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("STEXbingo_", input$name_1, ".pdf", sep = "")
    },
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', pdf_document())
      file.rename(out, file)
      
      #write.csv(prep_bingo(input), file, row.names = FALSE, col.names = FALSE)
    }
  )
  
  ###Methoden
  #Links
  output$github_link <- renderUI({
    tagList("R Code is available on", a("GitHub", href="https://github.com/Superfl0/Kvir"))
  })
  output$link_memes <- renderUI({
    tagList(a("Lebensretter", href="https://www.instagram.com/stexmemes/"), "an den Examenstagen")
  })
  output$link_amboss <- renderUI({
    tagList(a("Ergebnisse", href="https://next.amboss.com/de/"), "eintragen.")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)




