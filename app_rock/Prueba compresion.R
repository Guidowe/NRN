#https://basicstatistics.tistory.com/entry/R-source-for-lempelziv-complexity

# FUNCTION lempel.ziv(____.VEC, ____.VEC)

# s is a sequence vector

# alphabet is a vector of alphabet letters

# function counts unique sub-sequence and normalized it

# ref) Aboy et al.(2006),

# Interpretation of the Lempel-Ziv Complexity Measure

#        in the Context of Biomedical Signal Analysis

# , IEEE Trans Biomed Eng. 2006 Nov;53(11):2282-8.


lempel.ziv=function(s, alphabet) {
  
  
  
  n=sum(!is.na(s))
  
  s=s[!is.na(s)]
  
  if (sum(s %in% alphabet)!= n) { stop("Alphabet error!") }
  
  
  
  voc=s[1]; cmpl=1
  
  r=1; i=1; 
  
  while (r+i<=n) {
    
    Q="";
    
    repeat {
      
      Q=paste(Q,s[r+i], sep="")
      
      if (Q %in% voc) {
        
        cmpl[r+i]=cmpl[r+i-1]; i=i+1; }
      
      if(!(Q %in% voc) | !(r+i<=n)) { break }
      
    } # repeat
    
    if (r+i > n) break;
    
    
    
    voc=c(voc, Q); cmpl[r+i]=cmpl[r+i-1]+1;
    
    r=r+i; i=1; 
    
  }
  
  
  
  cmpl=cmpl/(1:n/log(1:n,length(alphabet)))
  
  return(cmpl)}



# FUNCTION lempel.ziv2(____.CHR, ____.CHR)

# Wrapper for lempel.ziv

# str is a vector of strings

# str.alphabet is a vector of alphabets of length 1 or length str 

lempel.ziv2=function(str, str.alphabet) {
  
  s2=strsplit(str,"")
  
  alphabet=strsplit(str.alphabet,"")
  
  
  
  if (length(alphabet) ==1) { inc.alphabet = 1 }
  
  else { 
    
    if (length(alphabet) != length(s2)) 
      
    { stop("Number of Strings and alphabets aren't the same.") }
    
    else { inc.alphabet=0 }}
  
  index.alphabet = 1 
  
  
  
  lzs=c()
  
  
  
  for (s in s2) {        
    
    lzs=c(lzs, lempel.ziv(s, alphabet[[1]])[length(s)])
    
    index.alphabet=index.alphabet+inc.alphabet
    
  }
  
  lzs
  
}
library(tidyverse)

string<- c("qwertyuiopasdfghjklzxcvbnm  qwertyuiopasdfghjklzxcvbn qwertyuiopasdfghjklzxcvbn",
           "mnbvcxzlkjhgfdsapoiuytrewq mn")
str.alphabet.sin.espacio<- paste0(as.vector(letters),collapse = "")
str.nums<- paste0(as.vector(0:9),collapse = "")
str.alphabet <- paste0(str.alphabet.sin.espacio,str.nums," ")

Letras_Rock_Limpio2 <- Letras_Rock_Limpio %>% 
  head(n = 100) %>% 
  mutate(texto = stringr::str_to_lower(texto),
         complex  = lempel.ziv2(texto,str.alphabet)) 

str = Letras_Rock_Limpio2$texto
str.alphabet = str.alphabet


