library(tm)
library(wordcloud2)
library(memoise)
library(tidyverse)
library(tidytext)
library(magrittr)
library(shiny)
library(topicmodels)
library(LDAvis)

Letras <- readRDS("app_rock/Letras_Rock_Limpio.RDS")
Colores <- c("#0042A6", "#354E98", "#4C5A8E", "#5E6586", "#6E717C", "#7E6D70", "#875A63", "#8D4656", "#90304A",
             "#910D3E", "#664F2B", "#715D42", "#7E6D59", "#8C8073", "#9E9892", "#909C99", "#6D8782", "#4E786F",
             "#2F6A60", "#005E52")
autores <-c(sort(unique(Letras$autor)))
Letras <- Letras %>% 
  group_by(autor) %>% 
  mutate(Canciones_incluidas = n())

####Corpus####
myCorpus = Corpus(VectorSource(Letras$texto))
myCorpus = tm_map(myCorpus, content_transformer(tolower))
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords,
                  c(stopwords(kind = "es"),stopwords(kind = "en")))

#ver que sea DocumentTerm y no TermDocument
myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))


inspect(myDTM)
palabras_frecuentes <- findMostFreqTerms(myDTM,n = 25,
                                         INDEX = rep(1,nDocs(myDTM)))[[1]]
palabras_frecuentes

ui = unique(myDTM$i)
dtm = myDTM[ui,]

dim(myDTM)
dim(dtm)

####Topic modelling####

# lda_fit <- LDA(dtm, k = 6,method = "Gibbs",
#                control = list(delta=0.6,seed = 1234))  

# saveRDS(lda_fit,'lda_fit.rds')
 lda_fit <- readRDS('app_rock/lda_fit.rds')
Terms <- terms(lda_fit, 5)
Terms

topicmodels_json_ldavis <- function(fitted, dtm){
  svd_tsne <- function(x) tsne(svd(x)$u)
  
  # Find required quantities
  phi <- as.matrix(posterior(fitted)$terms)
  theta <- as.matrix(posterior(fitted)$topics)
  vocab <- colnames(phi)
  term_freq <- slam::col_sums(dtm)
  
  # Convert to json
  json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                                 vocab = vocab,
                                 mds.method = svd_tsne,
                                 plot.opts = list(xlab="tsne", ylab=""),
                                 doc.length = as.vector(table(dtm$i)),
                                 term.frequency = term_freq)
  
  return(json_lda)
}
json_res <- topicmodels_json_ldavis(lda_fit, dtm)

serVis(json_res)
