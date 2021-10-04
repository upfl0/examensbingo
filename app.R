library(shiny)
library(shinyjs)
library(ggplot2)


# Given a string with comma-separated phrases, extract the phrases
getWordsText <- function(text) {
  trimws(strsplit(text, ",")[[1]])
}

# Given a file with comma-separated or newline-separated phrases,
# extract the phrases
getWordsFile <- function(file) {
  getWordsText(paste(readLines(file), collapse = ","))
}


# Make sure there are enough phrases to fill at least one card
validateSize <- function(words, size) {
  if (length(words) < (size * size - 1)) {
    stop(sprintf("You need at least %s phrases to fill a %sx%s bingo card (you provided %s)",
                 size * size - 1, size, size, length(words)))
  }
}

# Create CSS that is needed to customize the appearance of the bingo cards
generateCardCSS <- function(length = 10, textSize = 16) {
  tags$style(paste0(
    "table.bingo-card {",
    "  width: ", length, "cm;",
    "  height: ", length, "cm;",
    "  font-size: ", textSize, "px;",
    "}"
  ))
}

# Generate HTML for a bingo card
generateCard <- function(words = LETTERS, size = 5) {
  # Randomly select phrases and insert "FREE" in the middle
  words <- sample(words, size * size - 1)
  middle <- size * size / 2 + 1
  middleWord <- words[middle]
  words[middle] <- "FREE"
  words[length(words) + 1] <- middleWord
  # Just for convenience, make the phrases a 2D matrix instead of 1D vector
  dim(words) <- c(size, size)
  
  tags$table(
    class = "bingo-card",
    tags$tbody(
      lapply(seq(size), function(row) {
        tags$tr(
          lapply(seq(size), function(col) {
            tags$td(words[row, col])
          })
        )
      })
    )
  )
}


ui <- navbarPage(

"STEXbingo",
tabPanel("Mitmachen",
  
  fluidPage(
    
  shinyjs::useShinyjs(),
  
  sidebarLayout(
    sidebarPanel(width = 5,
    tags$head(tags$script
            ('
                                                    $(document).on("shiny:connected", function(e) {
                                                        Shiny.onInputChange("innerWidth", window.innerWidth);
                                                    });
                                                    $(window).resize(function(e) {
                                                        Shiny.onInputChange("innerWidth", window.innerWidth);
                                                    });
                                                    '),
            tags$style(HTML('                       #bulletpoints{
                                                           padding: 20px;
                                                           line-height: 30px;
                                                           font-size: 18px;
                                                           font-weight: bold;
                                                    }
                                                    #quotes {
                                                           padding: 20px;
                                                           line-height: 30px;
                                                           font-size: 18px;
                                                           font-style: italic;
                                                           text-align: center;
                                                    }
                                                    #textblock {
                                                           padding: 20px;
                                                           line-height: 30px;
                                                           font-size: 18px;
                                                    }
                                                    #blockheader{
                                                           font-family: "Georgia", Times, "Times New Roman", serif;
                                                           font-weight: bold;
                                                           font-size: 24px;
                                                           text-align: center;
                                                    }
                                                    .navbar-brand{
                                                           font-family: "Georgia", Times, "Times New Roman", serif;
                                                           font-weight: bold;
                                                           font-size: 24px;
                                                    }'))
  ),
  
  div(
    class = "noprint",
    id = "form",
    div(id = "blockheader", "Bingo erstellen"),
    selectInput("cardType", NULL,
                c("Bingo als PDF" = "pdf")
    ),
    numericInput("numberToMake", "Anzahl an Bingo Karten", 1, 1),
    selectInput("uploadType", "Eingabemethode",
                c("Manuelle Eingabe" = "box",
                  "'.txt' mit Kommas" = "file")),
    div(
      id = "wordsinput",
      conditionalPanel(
        condition = "input.uploadType == 'box'",
        
        div(class = "form-group shiny-input-container",
            tags$textarea(id = "wordsBox", rows = 7, class = "form-control",
                          paste(LETTERS, collapse = ","))
        ),
        helpText("Eingaben bitte durch Komma trennen!")
      ),
      conditionalPanel(
        condition = "input.uploadType == 'file'",
        fileInput("wordsFile", NULL),
        helpText("Eingaben bitte durch Komma trennen!")
      )
    ),
    actionLink("advancedBtn", "Mehr Optionen"), br(),
    shinyjs::hidden(
      div(
        id = "advanced",
        selectInput("size", "Dimensionen", selected = "5",
                    c("5x5" = "5")),
        numericInput("textSize", "Textgröße (in Pixel)", 14, 8),
        conditionalPanel(
          condition = "input.cardType == 'html'",
          numericInput("length", "Kartengröße (in Centimetern)", 20, 5)
        )
      )
    ),
    br(),
    conditionalPanel(
      condition = "input.cardType == 'pdf'",
      downloadButton('generatePdf', 'STEXbingo herunterladen!', class = "btn-primary btn-lg")
    ),
    conditionalPanel(
      condition = "input.cardType == 'html'",
      actionButton("generateHtml", "Generate cards!", class = "btn-primary btn-lg"),
      shinyjs::hidden(
        actionButton("print", "Print these cards", icon("print"),
                     onclick = "javascript:window.print()", class = "btn-lg")
      )
    ),
    br(),
    conditionalPanel(
      condition = "input.cardType == 'html'",
      shinyjs::hidden(
        div(
          id = "error",
          strong("Error: "),
          span(id = "errormsg")
        )
      )
    )
  ),
  conditionalPanel(
    condition = "input.cardType == 'html'",
    uiOutput("cards")
  )
  ),
  mainPanel(width = 7,
            br(),
            fluidRow(id = "blockheader", div("Willkommen zum STEXbingo!"), align = "center"),
            br(),
            fluidRow(id = "textblock",div("Ihr habt euch sicherlich schon gefragt was das IMPP für die H21ler im Petto hat. Ich mich auch! Und wie das bei Bingo so ist, gewinnt derjenige, der zuerst 5 in einer Reihe hat. Das funktioniert so: Du wählst 25 Themen/Erkrankungen/IMPP-Facts, wo du sicher bist, dass sie im STEX drankommen und generierst daraus eine Bingokarte pro Mitspieler. Das kannst du bis zum 4.10.2021 um 24 Uhr machen. Für das beste Bingofeeling empfehle ich die Bingokarte auszudrucken (damit du auch vor deinen Freunden damit angeben kannst!). Nach jedem Tag darfst du die Schlagworte wegstreichen, die an dem Tag geprüft worden sind. Kriegst du 5 in einer Reihe, hast du gewonnen! Gewinner schicken dann schnell ein Foto der Bingokarte an floriantruncus@gmail.com. Klingt einfach oder? Genauere Infos findest du nochmal unter 'About'. Folgende Regeln gelten:"), align="justify"),
            fluidRow(id = "bulletpoints", HTML("<ul><li>Keine zu allgemeinen Begriffe wählen (z.B. NSAR, Herzinfarkt, Unfallchirurgie)</li><li>Bonuspunkte kassieren für 'Limbische Enzephalitis', 'Glykogenose Drölf' und co</li><li>Amboss TOP 100 nur für Anfänger</li><li>Bingokarte bis spätestens 04.10.21 24 Uhr erstellen</li><li>Lustige Beiträge teilen</li><li>Freunden weitersagen</li></ul>")),
            fluidRow(id = "Pic", img(src='Bottom100.jpg', align = "center", width = "100%"), align = "center")
            
            )
  )
  )
),
tabPanel("About",
         fluidPage(
           br(),
           
           
           navlistPanel(widths = c(3, 9),
                        "Dashboard",
                        tabPanel("Creators",
                                 fluidRow(id = "bulletpoints",
                                   br(),
                                   br(),
                                   br(), 
                                   div(id = "authors",
                                       "Bingo package by", a("Jenny Bryan", href = "https://twitter.com/jennybryan"),
                                       "and", a("Dean Attali", href = "http://deanattali.com")),
                                   br(),      
                                   div(id = "modifier",
                                       "Modified by", a("Florian Rumpf", href = "https://www.instagram.com/upfl0/"), "for STEX H21"),
                                   br(), 
                                   div(id = "code",
                                       "Code", a("on GitHub", href = "https://github.com/upfl0/examensbingo")
                                   ),
                                   align = "center"   
                                 )
                        ),
                        tabPanel("Shiny",
                                 fluidRow(id = "bulletpoints",
                                   br(),
                                   br(),
                                   br(), 
                                   div(id = "shiny",
                                       "Mehr über", a("Shiny apps", href = "https://shiny.rstudio.com/"))
                                   , align = "center"
                                 )
                                 ),
                        tabPanel("Bugs",
                                 fluidRow(id = "bulletpoints",
                                          br(),
                                          br(),
                                          br(), 
                                          div("Bug gefunden? Email an", a("Flo", href = "mailto:floriantruncus@gmail.com"))
                                          , align = "center"
                                 )
                        ),
                        
                        "STEX 101",
                        tabPanel("Luck-o-meter",
                                 fluidRow(id = "bulletpoints",
                                          br(),
                                          br(),
                                          br(),
                                          div("Basierend auf deinen Eingaben, deiner Kreuzstatistik und deinem Internet-Fingerprint, habe ich einen Score entwickelt, der dein IMPP Luck abbildet. Die zugehörige Publikation findest du", a("hier.", href = "https://doi.org/10.1111/j.2044-8295.2012.02114.x"), "Anbei deine persönlich Auswertung:"), 
                                          plotOutput("luckometer"),
                                          align = "center"
                                 )
                        ),
                        tabPanel("Stexmemes",
                                 fluidRow(id = "bulletpoints",
                                   br(),
                                   br(),
                                   br(),
                                   uiOutput("link_memes"), 
                                   align = "center"
                                 )
                        ),
                        tabPanel("Quotes",
                                 fluidRow(id = "textblock",
                                          br(),
                                          br(),
                                          br(),
                                          div(id ="quotes", "Wait! That's illegal!"),
                                          div(id = "textblock", "- Master Chief"), 
                                          align = "center"
                                          
                                 )
                        ),
                        
                        tabPanel("Amboss Ergebnisse",
                                 fluidRow(id = "bulletpoints",
                                   br(),
                                   br(),
                                   br(),
                                   uiOutput("link_amboss"), 
                                   align = "center"
                                 )
                        )
           )
           
         )
)

)

server <- function(input, output, session) {
  
  output$link_memes <- renderUI({
    tagList(a("Lebensretter", href="https://www.instagram.com/stexmemes/"), "an den Examenstagen")
  })
  output$link_amboss <- renderUI({
    tagList(a("Ergebnisse", href="https://next.amboss.com/de/"), "eintragen.")
  })
  
  output$luckometer <- renderPlot({
    df <- data.frame("v1" = sample(c("Du", "Dein Banknachbar", "Deine Mama", "IMPP-Chefin", "Facharzty für IMPP-Fragen"), 35, replace = TRUE))
    df <- rbind(df, data.frame("v1"= c(rep("Du", 7))))
    df$v1 <- factor(df$v1, levels = c("Du", "Dein Banknachbar", "Deine Mama", "IMPP-Chefin", "Facharzty für IMPP-Fragen"))
    ggplot(df, aes(v1, fill = v1)) +
      geom_bar(col = "black", size = 0.2)+
      scale_fill_brewer(palette = "Spectral") +
      theme_classic() +
      theme(legend.position = "none") +
      scale_x_discrete(name = NULL) +
      scale_y_continuous(name = "IMPP Luck", breaks = c(3, 7, 15), limits = c(0, 16), labels = c("Lappen", "Normal", "Sheesh"))
  })
  
  values <- reactiveValues(cardsHTML = NULL)
  
  # show/hide the advanced options
  observeEvent(input$advancedBtn, {
    toggle("advanced", anim = TRUE)
  })
  
  # show an error message when an error occurs
  observeEvent(values$error, {
    html("errormsg", values$error)
    show("error", anim = TRUE)
  })
  
  # disable the Generate button when no words are entered
  observe({
    submitEnabled <- TRUE
    if ( (input$uploadType == "box" && !nzchar(input$wordsBox)) ||
         (input$uploadType == "file" && is.null(input$wordsFile))
    ) {
      submitEnabled <- FALSE
    }
    toggleState("generatePdf", condition = submitEnabled)
    toggleState("generateHtml", condition = submitEnabled)
  })
  
  # Generate PDF cards
  output$generatePdf <- downloadHandler(
    filename = function() {
      "bingo-cards.zip"
    },
    content = function(file) {
      tryCatch({
        # make sure there are enough phrases to fill at least one card
        size <- as.integer(input$size)
        validateSize(words(), size)
        
        # generate the cards
        cards <- bingo::bingo(n_cards = input$numberToMake, words = words(), n = size)
        filenames <- plot(cards, dir = tempdir(), fontsize = input$textSize)
        wd <- setwd(dirname(filenames[1]))
        zip(file, basename(filenames))
        setwd(wd)
      },
      error = function(err) {
        stop(err$message)
      })
    }
  )
  
  words <- reactive({
    if (input$uploadType == "box") {
      words <- getWordsText(input$wordsBox)
    } else if (input$uploadType == "file") {
      words <- getWordsFile(input$wordsFile$datapath)
    } else {
      words <- bingo::get_topic(input$uploadType)
    }
  })
  
  # Generate HTML cards
  observeEvent(input$generateHtml, {
    tryCatch({
      size <- as.integer(input$size)
      validateSize(words(), size)
      
      hide("error")
      show("print")
      
      # Joe Cheng won't like, but I have to use reactiveValues here
      values$cardsHTML <-
        tagList(
          generateCardCSS(input$length, input$textSize),
          lapply(seq(input$numberToMake), function(i) {
            generateCard(words(), size)
          })
        )
    },
    error = function(err) {
      values$error <- err$message
    })
  })
  
  # render the cards HTML when it changes
  output$cards <- renderUI({
    values$cardsHTML
  })
}

shinyApp(ui = ui, server = server)
