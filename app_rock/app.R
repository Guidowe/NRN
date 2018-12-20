#https://diegokoz.shinyapps.io/wordcloud_marx/

library(tm)
library(wordcloud2)
# library(wordcloud)
library(memoise)
library(tidyverse)
library(magrittr)
library(shiny)

Letras <- readRDS("app_Rock_Nac/Letras_Rock_Limpio.RDS")
Colores <- c("#0042A6", "#354E98", "#4C5A8E", "#5E6586", "#6E717C", "#7E6D70", "#875A63", "#8D4656", "#90304A",
             "#910D3E", "#664F2B", "#715D42", "#7E6D59", "#8C8073", "#9E9892", "#909C99", "#6D8782", "#4E786F",
             "#2F6A60", "#005E52")
autores <-c(sort(unique(Letras$autor)))

# cachea resultados y no recalcula todo
get_Cant_Canciones <- memoise(function(aut){
  
    Letras2 <- Letras %>% 
      filter(autor==aut) 
    nrow(Letras2)
    }
)

  getTermMatrix <- memoise(function(aut) {
  
  if (!(aut %in% autores)) {
    stop("Autor desconocido")
  } 
  
  if (aut != "TODES") {
    Letras <- Letras %>% 
      filter(autor==aut) 
  } 
  

  myCorpus = Corpus(VectorSource(Letras$texto))
  myCorpus = tm_map(myCorpus, content_transformer(tolower))
  myCorpus = tm_map(myCorpus, removePunctuation)
  myCorpus = tm_map(myCorpus, removeNumbers)
  myCorpus = tm_map(myCorpus, removeWords,
                    c(stopwords(kind = "es"),stopwords(kind = "en")))
  
   myDTM = TermDocumentMatrix(myCorpus)
  
  
  m = as.matrix(myDTM)
  sort(rowSums(m), decreasing = TRUE)
})

### UI

ui <- fluidPage(
  # titulo
  titlePanel("Nube de palabras"),
  
  sidebarLayout(
    # inputs
    sidebarPanel(
      selectInput("aut", "Elejir autor",
                  choices = autores, 
                  selected=autores[5]),
      actionButton("update", "Actualizar"),
      hr(),
      sliderInput("freq",
                  "Frecuencia mínima:",
                  min = 1,  max = 50, value = 5),
      sliderInput("max",
                  "Máxima cantidad de palabras:",
                  min = 1,  max = 300,  value = 25),
      sliderInput("caracteres",
                  "Minimo de caracteres de palabra:",
                  min = 1,  max = 50, value = 3)
      
    ),
    
    # Show Word Cloud
    mainPanel(
      textOutput(outputId = "Cantidad_canciones"),  
      wordcloud2Output("plot",height ="600px")
    )
  )
)


###

server <- function(input, output, session) {

  
  observe(
    {
      input$aut
      # Update based on the month change event
})
  Cant_Canciones <- reactive({
    # actualiza
    input$update
    isolate({
      withProgress({
        setProgress(message = "procesando el texto...")
        get_Cant_Canciones(input$aut)
        
        
      })
    })
  })
  terms <- reactive({
    # actualiza
    input$update
    isolate({
      withProgress({
        setProgress(message = "procesando el texto...")
        getTermMatrix(input$aut)
       
        
      })
    })
  })
   # para que se repita el mismo wordcloud en la sesion
  wordcloud_rep <- repeatable(wordcloud2)
  output$plot <- renderWordcloud2({
    v <- terms()
    vv <- data_frame(word = names(v), freq = v)
    vv <- vv %>% 
      filter(freq>=input$freq, nchar(word)>= input$caracteres) %>% 
      top_n(.,n=input$max, wt = freq)

    wordcloud2(data = vv,size = 0.7, minSize = 0, gridSize = 0,
               fontFamily = 'Segoe UI', fontWeight = 'bold',
               backgroundColor = "white",color = Colores,
               minRotation = -pi/4, maxRotation = pi/4, shuffle = FALSE,
               rotateRatio = 0.4, shape = 'circle', ellipticity = 0.65,
               widgetsize = NULL, figPath = NULL, hoverFunction = NULL)    
        
  })
  output$Cantidad_canciones <- renderText({
     paste0("Catidad de Canciones del Artista: ",Cant_Canciones())}
  )    

}
##### RUN ##### 

shinyApp(ui, server)

