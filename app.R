library(shiny)
library(shinyjs)
library(stringi)


drawAOC <- function(d){
  idx <- sample(1:length(d$AOC.AOP), 1)
  return(d[idx,])
}

data <- read.csv("data/TableAOC.csv", encoding = "UTF-8")
dataRegion <- read.csv("data/TableRegion.csv", encoding = "UTF-8", sep = ";")
# availableMaps <- c("Alsace", "Beaujolais", "Bordeaux", "Bourgogne", "Champagne", 
#                    "Languedoc-Roussillon", "Loire", "Provence", "Rhône", "Sud-Ouest")


ui <- fluidPage(
  useShinyjs(),
  titlePanel(
    h1(icon("wine-bottle"),span("Localisez les AOP-AOC !", 
      style="color: MediumVioletRed;
            font-family: cursive;
            font-size: 40px;
            font-style: italic;
            text-align: center;
            font-weight: bold;"),
      icon("wine-glass-alt"), align="center"),
    windowTitle = "AOC-AOP Game"
    ),
  br(), 
  br(),
  
  sidebarLayout(
    sidebarPanel(
      #img(src="www/rstudio.png", height=200, width=220, alt="blavla"),
      actionButton("newguess", "Nouvelle Appellation"),
      br(),
      br(),
      p(strong("Score : \n")),
      p("Appellations trouvées :",textOutput("correctGuesses")),
      p("Nb moyen d'essais :", textOutput("triedGuesses")),
      
      br(),
      br(),
      helpText(strong("Source :\n"),
               "\nCartes issues de l'excellent", a("COAM", href="https://www.lecoam.eu/"), "par Yann Rousselin.",
               "Les appellations et présentations des régions viticoles sont extraites du site du ", 
               a("Guide des vins et des vignes de France.", href= "http://www.vin-vigne.com"))
      ),
    mainPanel(
      tabsetPanel(
        id = "hidden_tabs",
        # Hide the tab values.
        type = "hidden",
        tabPanelBody("panel1",
                     br(),
                     p("Où se situe l'appellation viticole suivante ?"),
                     textOutput("guess"),
                     tags$head(tags$style("#guess{color: purple;
                                                  font-size: 20px;
                                                  text-align: center;
                                                  font-weight: bold;}"
                                          )
                               ),
                      br(),
                     
                     radioButtons("region", "L'appellation se situe dans la région :",
                                   sort(unique(data$Region)),
                                   selected = "Alsace"
                      ),
                      actionButton("submit", "Valider", icon("glass-cheers")),
                     br(),
                      br(),
                      uiOutput("correction"),
                      br(),
                      actionButton("viewMap", "Voir la carte")
                      ),
        tabPanelBody("panel2", 
                     actionButton("back", "Retour au quizz"),
                     textOutput("titleRegion"),
                     tags$head(tags$style("#titleRegion{color: purple;
                                                        font-size: 20px;
                                                        text-align: center;
                                                        font-weight: bold;}"
                                          )
                               ),
                     textOutput("introRegion"),
                     tags$head(tags$style("#introRegion{text-align: center;
                                                        font-style: italic;}")
                               ),
                     br(),
                     br(),
                     textOutput("teaseAOC"),
                     tags$head(tags$style("#teaseAOC{text-align: center;
                                                        font-weight: bold;}"
                     )
                     ),
                     actionButton("fullMap", "Carte complète"),
                     br(),
                     
                     imageOutput("map")
                     )
        )
    ),
    position = "left"
    )
  )

server <- function(input, output, session){

  
  
  #Click New guess
  observeEvent(input$newguess,{
    devinette <- drawAOC(data)
    guessAOC <- devinette$AOC.AOP[1]
    guessRegion <- devinette$Region[1]
    updateTabsetPanel(session, "hidden_tabs", selected = "panel1")
    guess_reactive$guess <- guessAOC
    guess_reactive$answer <- guessRegion
    guess_reactive$picName <- stri_trans_general(guess_reactive$answer,"Latin-ASCII")
    guess_reactive$correction <-""
    guess_reactive$viewButton <- 0
  })
  
  #Click Submit answer
  observeEvent(input$submit, {
    if(input$region == guess_reactive$answer){
      guess_reactive$nbCorrectAnswers <- guess_reactive$nbCorrectAnswers + 1
      guess_reactive$nbTrials <- guess_reactive$nbTrials + 1
      guess_reactive$correction <- paste(icon("check"), "Bonne réponse !")
      guess_reactive$viewButton <- 1
      output$correctGuesses <- renderText({guess_reactive$nbCorrectAnswers})
      output$triedGuesses <- renderText({guess_reactive$nbTrials / guess_reactive$nbCorrectAnswers})
      }
    else {
      guess_reactive$nbTrials <- guess_reactive$nbTrials + 1
      guess_reactive$correction <- paste(icon("times-circle"), "Essaie encore...")
      }
  })
  
  #Initialisation
  devinette <- drawAOC(data)
  guessAOC <- devinette$AOC.AOP[1]
  guessRegion <- devinette$Region[1]
  
  
  guess_reactive <- reactiveValues(
    guess = guessAOC,
    correction = "",
    answer = guessRegion,
    nbCorrectAnswers = 0,
    nbTrials = 0,
    viewButton = 0,
    picName = stri_trans_general(guessRegion,"Latin-ASCII")
  )
  
  output$guess <- renderText({guess_reactive$guess})
  output$correction <- renderUI({HTML(guess_reactive$correction)})
  output$teaseAOC <- renderText({paste("Saurez-vous situer l'appellation",guess_reactive$guess,"?")})
  
 
  
   #Navigation quizz-carte
  observeEvent(input$viewMap, {
    updateTabsetPanel(session, "hidden_tabs", selected = "panel2")
  })
  observeEvent(input$back, {
    updateTabsetPanel(session, "hidden_tabs", selected = "panel1")
  })
  observe({
    toggleState(id = "viewMap", condition = guess_reactive$viewButton == 1)
  })
  
  
  #Onglet Carte
  output$titleRegion <- renderText(guess_reactive$answer)
  output$introRegion <- renderText(
    dataRegion[dataRegion$Region==guess_reactive$answer,]$Laius
    )
  
  observeEvent(input$fullMap,{
    guess_reactive$picName <- paste0(guess_reactive$answer, '_full')
      })
  
  
  output$map <- renderImage({
    width  <- session$clientData$output_map_width
    height <- session$clientData$output_map_height
    filename <- paste('./', guess_reactive$picName,'.png', sep='')
    
    list(src = filename,
         width = width,
         alt = paste("Image indisponible pour la région ", guess_reactive$answer))
    },
    deleteFile = FALSE)

}

shinyApp(ui, server)