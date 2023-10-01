# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(rvest)
library(tidyverse)
library(jsonlite)
library(xml2)
library(readr)
library(rlang)


getHB <- function(url) {
  
  #url <- "https://www.handelsblatt.com/finanzen/maerkte/marktberichte/dax-aktuell-dax-macht-verluste-wett-und-schliesst-im-plus-luxus-aktien-verlieren-15-milliarden-euro-an-wert/25454810.html"
  #print(url) 
  
  #//*[@id="hcf-wrapper"]/div[7]/div/div[1]/article/div/div[1]/div/div[3]/div[2]/p[1]
  #//*[@id="hcf-wrapper"]/div[7]/div/div[3]/div/section/header/div/div/div/p/span
  #tsXpath <- "//*[@id='hcf-wrapper']/*"
  
  htmlPage <- read_html(url)
  
  #identify all nodes with text
  textNodesAll <- htmlPage %>% xml_find_all('/p') 
  
  #identify elements, we don't need
  textNodesRemove1 <-  html_elements(textNodesAll, "em")
  
  semiFinal <- ""
  for (j in 1 : length(textNodesAll)-1) {
    
    #get string of entry i
    textP <- textNodesAll[j] %>% xml2::xml_text()
    
    #now remove all "em" text only if we found any em's
    if (length(textNodesRemove1) > 0) {
      for (i in 1: length(textNodesRemove1)) {textP <- wwRString(textP, textNodesRemove1[i] %>% xml2::xml_text())}
    }
    semiFinal <- trimws(paste(semiFinal, textP, collapse= " "))
  }
  final <- trimws(substring(semiFinal,1, (nchar(semiFinal)-17)))
  #print(final)
  
  return(final)
} #getHB



getCommentFromPageHB <- function(htmlPage){
  
  #url <- "https://www.handelsblatt.com/politik/deutschland/corona-news-epidemiologe-40-millionen-infektionen-woechentlich-in-china-bund-kann-corona-impfstofflieferungen-reduzieren/v_detail_tab_comments/25471608.html?commentSort=debate&pageNumber=1"
  #print(url) 
  #htmlPage <- read_html(url)
  
  #identify elements, we don't need
  comments <-  html_elements(htmlPage, ".vhb-comment-content")
  commentsText <- comments %>% xml2::xml_text()
  return(commentsText)
}

#check if there are more pages with comments
nextPageHB <- function(page) {
  
  nextPage <- html_elements(page, ".vhb-paging-next")
  if ( length(nextPage) > 0 ) {
    np <- TRUE
  }
  else {
    np <- FALSE
  }
  return(np)
}


getCommentsHB <- function (startUrl, Id) {
  
  #startUrl <- "https://www.handelsblatt.com/politik/deutschland/corona-news-epidemiologe-40-millionen-infektionen-woechentlich-in-china-bund-kann-corona-impfstofflieferungen-reduzieren/v_detail_tab_comments/25471608.html"
  #startUrl <- "https://www.handelsblatt.com/politik/deutschland/corona-news-epidemiologe-40-millionen-infektionen-woechentlich-in-china-bund-kann-corona-impfstofflieferungen-reduzieren/25471608.html"
  #startUrl <- "https://www.handelsblatt.com/politik/konjunktur/nachrichten/konjunktur-faustdicke-ueberraschung-ifo-geschaeftsklimaindex-sinkt/25476326.html"
  #startUrl <- "https://www.handelsblatt.com/finanzen/maerkte/marktberichte/dax-aktuell-dax-schliesst-mehr-als-350-punkte-im-minus-anleger-weichen-auf-gold-und-anleihen-aus/25476050.html"
  commentUrl <- "v_detail_tab_comments"
  pageNumber <- as.integer(1)
  
  position <- tail(unlist(gregexpr('/', startUrl)), n=1)
  baseUrl <- paste(substring(startUrl, 1,position), commentUrl,  sep= "" )
  baseUrl <- paste(baseUrl, substring(startUrl, (position),nchar(startUrl)), sep= "" )
  baseUrl <- paste(baseUrl, "?commentSort=debate&pageNumber=", sep="")
  
  nextPage <- TRUE
  first <- TRUE
  
  #comments <- ""
  
  #main loop
  while (nextPage) {
    
    #build url
    url <- paste(baseUrl, trimws(as.character(pageNumber)), sep="")
    #print(url)
    pageNumber <- pageNumber + 1
    
    #read the page
    htmlPage <- read_html(url)
    
    #check if there are more comments
    nextPage <- nextPageHB(htmlPage)
    #print(nextPage)
    
    pc <- getCommentFromPageHB(htmlPage)
    if(!is_empty(pc)){
      if (first) {
        comments <- data.frame("Id" = c(Id), "Comment"= c(pc))
        first <- FALSE
      } else {
        pcdata <- data.frame("Id" = c(Id), "Comment"= c(pc))
        comments <- rbind(comments, pcdata)
      }
    } else{
      if (first) {
        comments <- data.frame("Id" = c(Id), "Comment"= c("Empty"))
        first <- FALSE
      } else {
        pcdata <- data.frame("Id" = c(Id), "Comment"= c("Empty"))
        comments <- rbind(comments, pcdata)
      }
    }
    
    #print(startUrl)
    #print(comments)
    #print(length(comments)) 
  }
  
  return(comments)
  
}

getComment <- function(dataframe, extractor){
  
  first <- TRUE
  for (i in 1:nrow(dataframe)) {
    print(dataframe$`url-href`[i])
    sComment <- extractor(dataframe$`url-href`[i], dataframe$Id[i]) 
    if (first) {
      comment <- sComment
      first <- FALSE
    } else {
      comment <- rbind(comment, sComment)
    }
  }
  return(comment)
}


#read in handelsblatt data
hb <- read_xlsx("Handelsblatt/Handelsblatt.xlsx")

#select hb with no comments
hb_no_comments <- filter(hb, hb$numberComments==0)

#select hb with comments
hb_with_comments <- filter(hb, hb$numberComments!=0)

#scrape comments
#we only scrape comments for which we have comments

#try to do it for some observations
test <- hb_with_comments[1:3,]

dataRich <- getComment(hb_with_comments,getCommentsHB)
empty <- filter(dataRich, dataRich$Comment=="Empty") #scrape them manually
write.csv(empty, "emptyHBComments.csv")
save(dataRich, file= "HBComments.Rdata")
write.csv(dataRich, "HBComments.csv")
