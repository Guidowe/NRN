library(rvest)
library(xml2)
library(tidyverse)
library(glue)
#### funciones #####

get_text <- function(link){
  tryCatch(html_text(read_html(link)),error = function(err) {glue::glue("error_in_link: {link}")})
}

get_letra <- function(link){
  tryCatch(html_text(read_html(link) %>% 
                       rvest::html_nodes("div.letra.text-left")),error = function(err) {glue::glue("error_in_link: {link}")})
}

limpiar_textos <- function(x){
  
  x %>% 
    iconv(., from = 'UTF-8', to = 'ASCII//TRANSLIT') %>% 
    # rvest::repair_encoding(., from = Encoding(x)) %>%
    str_replace_all(pattern = "[^[:alnum:]]", replacement = " ") %>%
    str_replace_all(pattern = "(?i)#([0-9A-F]{2})\1{2}", replacement = " ") %>%
    str_replace_all(pattern = "\n", replacement = " ") %>%
    str_replace_all(pattern = "[\\^]", replacement = " ") %>%
    str_replace_all(pattern = "\"", replacement = " ") %>%
    str_replace_all(pattern = "<.*>", replacement = " ") %>%
    str_replace_all(pattern = "<[^>]*>", replacement = " ") %>%
    str_replace_all(pattern = "<.*?>", replacement = " ") %>%
    str_replace_all(pattern = "\\s+", replacement = " ") %>%
    str_replace_all(pattern = "\\{ .* \\}", replacement = " ") %>%
    str_trim(side = "both")
}

get_new_links <- function(link){
  tryCatch({
  
  base <- link
  nodes <- read_html(link) %>% 
    rvest::html_nodes("a")  
  
  data.frame(link= html_attr(nodes, "href"), titulo = html_text(nodes)) %>% 
    filter(!str_detect(link,"(\\.\\.)|NA|.doc|.zip|.pdf|wikipedia|http|mailto"),
           link!="") %>% 
    na.omit(.) %>%
    mutate(link = gsub("#.*","",link),
           link= xml2::url_absolute(link, base= base)) %>% 
    distinct(link,.keep_all = TRUE )

  },error = function(err) {glue::glue("error_in_link: {link}")})
}

#### scrapping de rock.com.ar #####
url_rock <- "https://rock.com.ar/enciclopedia"

webpage_rock <- read_html(url_rock)

refs_rock <- webpage_rock %>%
  html_nodes("a") 

df_links_rock <- data.frame(autor = html_text(refs_rock),
                       link = html_attr(refs_rock, "href")) %>% 
  na.omit() %>%
  filter(str_detect(link,"/artistas")) %>% 
  mutate(link=paste0("https://rock.com.ar",link,"/letras"))

df_links_rock2 <- df_links_rock %>% 
  mutate(nested_link = map(link, get_new_links))



####Paso que tarda###
df_links_rock2 <-  df_links_rock2 %>%
  filter(!str_detect(nested_link,"error_in_link")) %>% 
  unnest()
#saveRDS(df_links_rock2,"data/txt/Links_rock.RDS") #guardo parcial porque tarda
###Puedo arrancar desde el proximo paso###
#df_links_rock2 <- read_rds("data/txt/Links_rock.RDS")


#Elimino links hacia publicidades, u otras secciones, discografia, fotos, biografia 
df_links_rock3 <- df_links_rock2 %>% 
  filter(str_detect(link1,"/artistas/"),str_detect(link1,"/letras/"))
  

set.seed(4773)
casos <- unique(round(1000*abs(rnorm(150,1))))
Inicio <- Sys.time()
texto_simple_rock <- df_links_rock3 %>% 
  #df_links_rock3[casos,] %>%     # Linea para probar unos pocos casos 
  mutate(texto = map(link1,get_letra)) %>%
  unnest() 
Final <- Sys.time()
Final-Inicio
#Este paso tarda un segundo por canción aprox

texto_simple_rock2 <- texto_simple_rock %>% 
  select(autor, titulo, texto)

#saveRDS(texto_simple_rock2,"data/txt/Letras_rock.RDS") #guardo parcial porque tarda

texto_simple_rock2 <- read_rds("data/txt/Letras_rock.RDS")

#  filter(!str_detect(nested_nested_link,"error_in_link")) %>% 
#### limpieza final###


textos_limpio <- texto_simple_rock2 %>%
  ungroup() %>% 
  mutate(texto = map(texto,limpiar_textos),
         autor = map(autor,limpiar_textos),
         titulo = map(titulo,limpiar_textos)) %>% 
  unnest()


textos_limpio <- textos_limpio %>% 
  na.omit(.)

saveRDS(textos_limpio, "app_MIA/Letras_Rock_Limpio.RDS")
saveRDS(textos_limpio, "data/txt/Letras_Rock_Limpio.RDS")



