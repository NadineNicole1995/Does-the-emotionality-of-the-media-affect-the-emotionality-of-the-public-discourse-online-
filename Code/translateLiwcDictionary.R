# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(googleLanguageR)


#Translation of LIWC dictionary
#using googleLanguageR


#we need a authenfication for API
gl_auth("translationKey.json")

#load liwc
load("liwc.Rdata")
liwc_certainty <- liwc$Certain

#translate each word
translation_liwc <- NULL
for(i in liwc_certainty){
  #print(i)
  translation <- gl_translate(i, target = "de", format = "text")
  translation <- translation$translatedText #extract German word
  translation <- tolower(translation)
  translation_liwc <- append(translation_liwc, translation)
}

#remove star
translation_liwc_new <- NULL
for(i in translation_liwc){
  #print(i)
  remove <- gsub("[*]", "", i)
  #print(remove)
  translation_liwc_new <- append(translation_liwc_new, remove)
}

liwc_certainty_german <- unique(translation_liwc_new) #94

save(liwc_certainty_german, file= "liwc_certainty_german.Rdata")
