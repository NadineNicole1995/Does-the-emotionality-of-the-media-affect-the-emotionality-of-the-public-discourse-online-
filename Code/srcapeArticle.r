library(rvest)
library(tidyverse)
library(jsonlite)
library(xml2)
library(readr)
library(xlsx)
library(purrr)
library(stringr)

getSpiegel <- function(url) {
  
  #url <- "https://www.spiegel.de/politik/deutschland/cdu-chef-friedrich-merz-lehnt-buergergeld-trotz-aenderungen-weiter-ab-a-eb43b166-b564-42a0-b543-84ae5d60a574#ref=rss"
  print(url) 
  #//*[@id="Inhalt"]/article/div/section[2]/div[2]/div[1]/div[1]/div/p
  tsXpath <- "//*[@id='Inhalt']/*"
  
  #read the url
  htmlPage <- read_html(url)
  
  #get the text lines from the article
  textLines <- htmlPage %>% html_elements(xpath =tsXpath)
  
  #identify all nodes with text
  textNodes <- textLines %>% xml_find_all('.//p') 

  #identify elements of specific class, we don't need
  textNodesRemove1 <-  html_elements(htmlPage, ".font-sansUI")

  #now remove unwanted nodes
  textNodes <- textNodes[!(textNodes %in% textNodesRemove1)]
  
    
  #get the final text
  text <- textNodes %>% xml2::xml_text()
  
  #remove header/tail
  textFinal <- text[2:(length(text))]
  
  #collaps all nodes together in one single string
  final <- paste(textFinal,collapse= " ")
  return(final)
} #getSpiegel

getTagesspiegel <- function(url) {
  
  #htmlPage <- "https://www.tagesspiegel.de/politik/eine-einigung-die-keine-ist-so-reagieren-die-kommunen-auf-den-bund-lander-gipfel-9802388.html"
  #//*[@id="story-elements"]/p[1]
  
  tsXpath <- "//*[@id='story-elements']/*"
  #print(url)
  
  #read the url
  htmlPage <- read_html(url)
  
  #get the text lines from the article
  textLines <- htmlPage %>% html_elements(xpath =tsXpath)
  
  #identify all nodes with text
  textNodesAll <- textLines %>% xml_find_all('//p') 
  
  #identify nodes we don't want in our analysis
  textNodesRemove <- textLines %>% xml_find_all('.//p')
  
  #neuX <- xa[!(xa %in% xr)]
  #remove unwanted nodeset 
  textNodesFinal <- textNodesAll[!(textNodesAll %in% textNodesRemove)] 
  
  #get the final text
  text <- textNodesFinal %>% xml2::xml_text()
  
  #remove header/tail
  textFinal <- text[3:(length(text)-2)]
  
  #collaps all nodes together in one single string
  final <- paste(textFinal,collapse= " ")
  return(final)
} #getTagesspiegel

isBildPlus <- function(s) {
  return(grepl("Lesen Sie .*mit BILDplus",s))
}

getBild <- function(url) {

  #url <- "https://www.bild.de/geld/wirtschaft/wirtschaft/handwerker-kritik-an-heils-buergergeld-plaenen-hartz-hammer-81297570.bild.html"
  print(url) 
  
  #//*[@id="__layout"]/div/div/div[2]/main/article/div[4]/p[3]/text()
  tsXpath <- "//*[@id='__layout']/*"
  
  
  #read the url
  htmlPage <- read_html(url)
  
  #get the text lines from the article
  textLines <- htmlPage %>% html_elements(xpath =tsXpath)

  #identify all nodes with text
  textNodesAll <- textLines %>% xml_find_all('//p')
  
  #identify "BILDPlus article
  ibv <- isBildPlus(textNodesAll %>% xml2::xml_text())
  textNodesRemoveFirst <-  keep(textNodesAll, ibv)

  #identify elements of specific class, we don't need
  textNodesRemove1 <-  html_elements(htmlPage, ".red-breaking-news__text")
  textNodesRemove2 <-  html_elements(htmlPage, ".teaser__text")
  textNodesRemove3 <-  html_elements(htmlPage, ".mtl__heading")
  
  
  #now remove unwanted nodes
  textNodes <- textNodesAll[!(textNodesAll %in% textNodesRemoveFirst)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove1)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove2)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove3)]
  
  #get the text
  text <- textNodes %>% xml2::xml_text()
  
  #remove header/tail
  textFinal <- text[2:(length(text))]
  
  #collaps all nodes together in one single string
  final <- paste(textFinal,collapse= " ")
  return(final)
} #getBild

getWelt <- function(url) {
  
  #url <- "https://www.welt.de/politik/deutschland/article235803198/Geimpfte-und-Ungeimpfte-Falsche-Covid-Zahlen-Amtspraesident-in-Bayern-wird-versetzt.html"
  #url <- "https://www.welt.de/kultur/article246029696/Trauma-Wunden-die-nicht-heilen-wollen-Corona-im-Museum.html"
  #url <- "https://www.welt.de/politik/deutschland/article235799666/Impfpflicht-Demo-Verbot-aus-Sorge-vor-Querdenker-Unterwanderung.html"
  #url <- "https://www.welt.de/sport/article234670822/Coronavirus-Sportarten-im-Vergleich-Impfquote-im-Tennis-gering.html"
  #url <- "https://www.welt.de/sport/article235553746/FC-Bayern-Lungenprobleme-Kimmich-macht-dieses-Jahr-kein-Spiel-mehr.html"
  print(url) 
  
  #//*[@id="top"]/div[3]/main/article/div[1]/header/div[5]/div/div
  #tsXpath <- "//*[@id='top']/*"
  
  #read the url
  htmlPage <- read_html(url)
  
  #get the text lines from the article
  #textLines <- htmlPage %>% html_elements(xpath =tsXpath)
  
  #identify all nodes with text
  #textNodesAll <- textLines %>% xml_find_all('.//p')
  articleNodes <- html_elements(htmlPage, ".c-article-text")
  textNodesAll <- articleNodes %>% xml_find_all('.//p')

    
  #identify elements of specific class, we don't need
  textNodesRemove1 <-  html_elements(htmlPage, ".c-page-footer__text")
  textNodesRemove2 <-  html_elements(htmlPage, "em")
  

  #now remove unwanted nodes
  textNodes <- textNodesAll[!(textNodesAll %in% textNodesRemove1)]

  
  #convert all elements but not the last to textP1
  textP1 <- textNodes[1:(length(textNodes)-1)] %>% xml2::xml_text()
  
  #get last entry string textP2
  textP2 <- textNodes[length(textNodes)] %>% xml2::xml_text()

  #if we find any textNodesRemove we clear textP2
  found <- FALSE
  if (length(textNodesRemove2) > 0) {
    for (i in 1: length(textNodesRemove2)) {
      pattern <- textNodesRemove2[i] %>% xml2::xml_text()
      pattern <- gsub("\\(", "", pattern)
      pattern <- gsub("\\)", "", pattern)
      if (length(pattern) > 0) {
        if (length(j <- grep(pattern, textP2))) {
          found<-TRUE
        }
      }
    }
  }
  
  #collaps all nodes together in one single string
  textNeu = paste(textP1[1:length(textP1)], "")
  preFinal <- paste(textNeu,collapse= " ")
  
  #we may need to add the final string also
  if (found) {
    final <- preFinal
  } else {
    final <- paste(preFinal, textP2)
  }
 
    
  return(final)
  
} #getWelt

wwRString <- function(source, pattern) {
  newStr <- gsub(pattern, '', source)
  return(newStr)
} #wwRString

getWirtschaftsWoche <- function(url) {
  
  url <- "https://www.wiwo.de/politik/deutschland/medienbericht-kuenftiges-buergergeld-soll-fuer-alleinstehende-502-euro-betragen/28673476.html"
  print(url)
  #read the url
  htmlPage <- read_html(url)
  
  #identify all nodes with text
  textNodesAll <- htmlPage %>% xml_find_all('//p') 
  
  #identify elements, we don't need
  textNodesRemove1 <-  html_elements(htmlPage, ".c-leadtext")
  textNodesRemove2 <-  html_elements(htmlPage, ".modalwindow__ctext")
  textNodesRemove3 <-  html_elements(htmlPage, ".modalwindow__footer-caption")
  textNodesRemove4 <-  html_elements(htmlPage, ".modalwindow__cpt")
  textNodesRemove5 <-  html_elements(htmlPage, "em")
  
  #now remove unwanted elements
  textNodes <- textNodesAll[!(textNodesAll %in% textNodesRemove1)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove2)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove3)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove4)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove5)]
  
  
  #convert elements to text
  textP1 <- textNodes[1:(length(textNodes)-1)] %>% xml2::xml_text()
  
  #get string of last entry
  textp2 <- textNodes[length(textNodes)] %>% xml2::xml_text()
  #now remove all "em" text only if we found any em's
  if (length(textNodesRemove5) > 0) {
   for (i in 1: length(textNodesRemove5)) {textp2 <- wwRString(textp2, textNodesRemove5[i] %>% xml2::xml_text())}
  }
  
  #remove header
  textFinal = paste( textP1[2:(length(textP1))], textp2)
  
  #collaps all text together in one single string
  final <- paste(textFinal,collapse= " ")
  
  return(final)
} #getWirtschaftsWoche

getNZZ <- function(url) {
  
  #url <- "https://www.nzz.ch/wirtschaft/buerger-und-buergergeld-warum-das-buergergeld-nicht-buergerlich-ist-ld.1715404"
  #print(url) 
  
  #read the url
  htmlPage <- read_html(url)
  
  #identify all nodes with text
  textNodesAll <- htmlPage %>% xml_find_all('//p') 
  
  #identify elements of specific class, we don't need
  textNodesRem <-  html_elements(htmlPage, ".comments-item__text")
  
  #now remove unwanted nodes
  textNodes <- textNodesAll[!(textNodesAll %in% textNodesRem)]
  
  #get the final text
  text <- textNodes %>% xml2::xml_text()
  
  #collaps all nodes together in one single string
  final <- paste(text,collapse= " ")
  return(final)
  
} #getNZZ

getSZintern <- function (url, checkMP) {

    #//*[@id="top"]/div[3]/main/article/div[1]/header/div[5]/div/div
  #//*[@id="article-app-container"]/article/div[5]/p[1]
  tsXpath <- "//*[@id='article-app-container']/*"
  
  #url <- "https://www.sueddeutsche.de/politik/politik-und-pseudowissenschaft-rueckkehr-der-rassisten-1.3468300-3"
  #checkMP <- TRUE
  #read the url
  print(url)
  htmlPage <- read_html(url)
  
  #get the text lines from the article
  textLines <- htmlPage %>% html_elements(xpath =tsXpath)
  #do we have multiple pages?
  
  multiPage <- FALSE
  if (checkMP) {
    mp <- html_elements(htmlPage, "nav")
    if (length(mp) > 0) {
      #mpNodes <- html_elements(htmlPage,".css-h5fkc8")
      mpNodes <- html_elements(htmlPage,".css-1dex8ct")
      if (length(mpNodes)>0) { multiPage <- TRUE}
       mpText <- html_elements(htmlPage,".css-1i15bnd")
       sText <- mpText %>% xml2::xml_text()
       print(sText)
    }
  }
  
  #identify all nodes with text
  textNodesAll <- textLines %>% xml_find_all('.//p') 
  
  #identify elements of specific class, we don't need
  textNodesRemove1 <-  html_elements(htmlPage, ".sz-teaser__summary")
  textNodesRemove2 <-  html_elements(htmlPage, ".css-x3s29y")
  textNodesRemove3 <-  html_elements(htmlPage, ".css-1485smx")
  textNodesRemove4 <-  html_elements(htmlPage, ".css-dgxek7")
  textNodesRemove5 <-  html_elements(htmlPage, ".css-1oy28g2")
  textNodesRemove6 <-  html_elements(htmlPage, ".css-1485smx")
  textNodesRemove7 <-  html_elements(htmlPage, ".css-1nw5r4g")
  textNodesRemove8 <-  html_elements(htmlPage, ".sz-teaser__overline-title")
  
  
  #now remove unwanted nodes
  textNodes <- textNodesAll[!(textNodesAll %in% textNodesRemove1)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove2)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove3)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove4)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove5)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove6)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove7)]
  textNodes <- textNodes[!(textNodes %in% textNodesRemove8)]
  
  
  #get the final text
  text <- textNodes %>% xml2::xml_text()
  
  #remove header/tail
  textNeu <- text[2:(length(text))]
  
  #collaps all nodes together in one single string
  page <- paste(textNeu,collapse= " ")
  
  if (multiPage) {
    final <- c("True", page)
  } else {
    final <- c("False", page)
  }
  
  return(final)
  
} #getSZintern

getSZsubPage <- function(baseUrl, pNo){
  
  #setup new url to access page 3
  #newUrl <- paste(url, "-3", sep="")
  newUrl <- paste(baseUrl, pNo, sep="")
  #get page3 check for another page
  subResult <- getSZintern(newUrl,TRUE)
  
  return(subResult)
}

getSZ <- function(url) {
  
  #url <- "https://www.sueddeutsche.de/politik/politik-und-pseudowissenschaft-rueckkehr-der-rassisten-1.3468300"
  
  url <- str_trim(url)
  #initialize variable
  multiPage <- FALSE
  page <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18")
  
  #get the html-page and check for multipage
  result <- getSZintern(url,TRUE)
  
  #do we have an article with multiple pages, then read also page 2
  if (result[1] == "True"){
    #print(url)
    #save page 1 without "Seite 1/x"
    page[1] <- result[2] #substr(result[2],1,(nchar(result[2])-11))
    
    #now get Page 2 and append to page1
    result <- getSZsubPage(url, "-2")
    page[2] <- result[2]
    pNo <- 2
    
    #Page 3?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-3")
      page[3] <- result[2]
      pNo <- 3
    }
    #Page 4?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-4")
      page[4] <- result[2]
      pNo <- 4
    }
    #Page 5?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-5")
      pNo <- 5
      page[pNo] <- result[2]
    }
    #Page 6?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-6")
      pNo <- 6
      page[pNo] <- result[2]
    }
    #Page 7?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-7")
      pNo <- 7
      page[pNo] <- result[2]
    }
    #Page 8?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-8")
      pNo <- 8
      page[pNo] <- result[2]
    }
    #Page 9?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-9")
      pNo <- 9
      page[pNo] <- result[2]
    }
 
    #Page 10?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-10")
      pNo <- 10
      page[pNo] <- result[2]
    }

    #Page 11?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-11")
      pNo <- 11
      page[pNo] <- result[2]
    }

    #Page 12?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-12")
      pNo <- 12
      page[pNo] <- result[2]
    }
    
    #Page 13?
    if (result[1] == "True"){
      result <- getSZsubPage(url, "-13")
      pNo <- 13
      page[pNo] <- result[2]
    }
    
    newPage <- ""
    #now remove Seite x/y
    
    #if less than 10 pages we need only to remove 11 characters e.g. Seite 1 / 9
    if (pNo < 10) {
      for (i in 1:pNo){
        page[i] <- substr(page[i],1,(nchar(page[i])-11))
        #print(page[i])
        newPage <- paste(newPage, page[i], sep = "")
      }
    }
    else{
      #if more than 9 pages we need to remove 12 characters e.g. Seite 1 / 10
      for (i in 1:9){
        page[i] <- substr(page[i],1,(nchar(page[i])-12))
        #print(page[i])
        newPage <- paste(newPage, page[i], sep = "")
      }
      #for the last pages staring at page 10 remove 13 characters, e.g. Seite 10 / 12
      for (i in 10:pNo){
        page[i] <- substr(page[i],1,(nchar(page[i])-13))
        #print(page[i])
        newPage <- paste(newPage, page[i], sep = "")
      }
    }

    #set multiPage flag
    multiPage <- TRUE  
  } else {
    #save page1
    page1 <- result[2]
  }
  
  #now build final string
  if (multiPage) {
    #we need to concatenate page1 and page2
    #if (nchar(page1) > 32765) { page1 <- substr(page1, 1, 32764)}
    final <- newPage
  } else {
    #we just need to move page1 to final
    final <- page1
  }
  return(final)
  
} #getSZ


getArticle <- function(dataframe, extractor){
  article <- sapply(dataframe$url, extractor)
  cbind(dataframe, article)
}



test <- function(){
data <- read.csv("D:/Eugen/Documents/Nadine/Artikel/BuegergeldSZ.csv", sep = ";")
dataRichB <- getArticle(data,getSZ)
##write.xlsx(dataRichB,"D:/Eugen/Documents/Nadine/WebScraper/Samstag/BuergergeldZeitErgebnis.xls")

data <- read.xlsx("D:/Eugen/Documents/Nadine/Artikel/MPHomoEheSZ.xlsx",1)
dataRichB <- getArticle(data,getSZ)
write.xlsx(dataRichB,"D:/Eugen/Documents/Nadine/Artikel/SZ-Final-Artikel-Buergergeld.xlsx")

return("finihed")
}

artikel <- getSZ("https://www.sueddeutsche.de/politik/democracy-lab-hauptsache-wir-reden-miteinander-1.3556363")

k <- test()

data <- read.xlsx("D:/Eugen/Documents/Nadine/July22/WeltArtikel-Start.xlsx",1)
dataRichB <- getArticle(data,getWelt)
write.xlsx(dataRichB,"D:/Eugen/Documents/Nadine/July22/WeltArtikel-Final.xlsx")

