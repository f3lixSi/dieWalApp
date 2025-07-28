library(shiny)
library(ggplot2)
library(dplyr)
library(readr)

# Lade registrierte Wählende
waehler_df <- read_csv("waehler.csv", col_types = cols(
  id = col_character(),
  geburtsdatum = col_date()
))

# Parteienliste
parteien <- c("ScheißCDU", "SPD", "Grüne", "brauchniemandFDP", "Linke", "FuckAfD", "Pommes Partei Deutschland", "Holland ist die geilste Stadt der Welt")

# Initialisiere leeres Datenset für Stimmen
stimmen_df <- data.frame(id = character(), partei = character(), stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("Kommunalwahl NRW – Online-Stimmabgabe (Prototyp)"),
  
  tabsetPanel(id = "tabs",
              
              tabPanel("Login",
                       h4("Zugang zur Stimmabgabe"),
                       textInput("id_input", "Wähler-ID"),
                       dateInput("geb_input", "Geburtsdatum"),
                       actionButton("login_btn", "Login"),
                       textOutput("login_status")
              ),
              
              tabPanel("Stimmabgabe",
                       conditionalPanel(
                         condition = "output.authentifiziert === true",
                         h4("Ihre Stimme für die Kommunalwahl NRW"),
                         radioButtons("wahl", "Bitte wählen Sie eine Partei:", choices = parteien),
                         actionButton("abstimmen", "Stimme abgeben"),
                         textOutput("stimm_status")
                       ),
                       conditionalPanel(
                         condition = "output.authentifiziert === false",
                         h4("Nicht authentifiziert."),
                         p("Bitte melden Sie sich zuerst auf der Login-Seite an.")
                       )
              ),
              
              tabPanel("Ergebnis",
                       h4("Live-Ergebnis (Simuliert)"),
                       plotOutput("ergebnisPlot"),
                       downloadButton("download_stimmen", "Stimmen als CSV exportieren")
              )
  )
)

server <- function(input, output, session) {
  
  # Reaktive Variablen
  authentifiziert <- reactiveVal(FALSE)
  gewaehlt <- reactiveVal(NULL)
  stimmeAbgegeben <- reactiveVal(FALSE)
  user_id <- reactiveVal(NULL)
  
  # Authentifizierung
  observeEvent(input$login_btn, {
    req(input$id_input, input$geb_input)
    eintrag <- waehler_df %>%
      filter(id == input$id_input, geburtsdatum == input$geb_input)
    
    if (nrow(eintrag) == 1) {
      authentifiziert(TRUE)
      user_id(input$id_input)
      updateTabsetPanel(session, "tabs", selected = "Stimmabgabe")
      output$login_status <- renderText("Login erfolgreich.")
    } else {
      authentifiziert(FALSE)
      output$login_status <- renderText("Login fehlgeschlagen. Prüfen Sie ID und Geburtsdatum.")
    }
  })
  
  output$authentifiziert <- reactive({ authentifiziert() })
  outputOptions(output, "authentifiziert", suspendWhenHidden = FALSE)
  
  # Stimmenliste (in-memory)
  stimmen <- reactiveVal(stimmen_df)
  
  # Stimmabgabe
  observeEvent(input$abstimmen, {
    req(authentifiziert(), !stimmeAbgegeben(), input$wahl)
    
    # Prüfe ob schon gewählt
    if (user_id() %in% stimmen()$id) {
      output$stimm_status <- renderText("Sie haben bereits abgestimmt.")
    } else {
      # Speichere Stimme
      neue_stimme <- data.frame(id = user_id(), partei = input$wahl, stringsAsFactors = FALSE)
      stimmen(bind_rows(stimmen(), neue_stimme))
      stimmeAbgegeben(TRUE)
      gewaehlt(input$wahl)
      output$stimm_status <- renderText(paste("Vielen Dank. Sie haben", input$wahl, "gewählt."))
      updateTabsetPanel(session, "tabs", selected = "Ergebnis")
    }
  })
  
  # Ergebnisanzeige
  output$ergebnisPlot <- renderPlot({
    df <- stimmen()
    if (nrow(df) == 0) return(NULL)
    ggplot(df, aes(x = partei)) +
      geom_bar(fill = "steelblue") +
      labs(x = "Partei", y = "Anzahl Stimmen", title = "Live-Ergebnis der Kommunalwahl (Simuliert)") +
      theme_minimal()
  })
  
  # Download
  output$download_stimmen <- downloadHandler(
    filename = function() {
      paste0("stimmen_export_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(stimmen(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)