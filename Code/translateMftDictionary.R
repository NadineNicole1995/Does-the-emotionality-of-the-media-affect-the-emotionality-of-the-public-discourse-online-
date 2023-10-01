# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(googleLanguageR)


#Translation of MFT dictionary
#using googleLanguageR


#we need a authenfication for API
gl_auth("translationKey.json")


#example
#translation <- gl_translate("Mum", target = "de", format = "text")
#translation
# A tibble: 1 × 3
#translatedText detectedSourceLanguage text 
#<chr>          <chr>                  <chr>
#  1 Mama           en                     Mum  

#read in English MFT dictionary
mft <- read.csv("mft_dictionary.csv")

mft <- mft[-1]

#translate each word
translation_mft <- NULL
for(i in mft$word){
  translation <- gl_translate(i, target = "de", format = "text")
  translation <- translation$translatedText #extract German word
  translation <- tolower(translation)
  translation_mft <- append(translation_mft, translation)
}

mft$word_German <- translation_mft

#for each category/ foundation we only one to have unique words (no duplicates)
#there are some words who have different meanings in English, but the same translation in German

#care 
mft_care <- filter(mft, mft$foundation== "care") #470 words

#mft_care %>% add_count(word_German) %>% 
  #filter(n>1) %>% 
  #distinct()

#mft_care %>% group_by(word_German) %>% filter(n()>1) %>% ungroup() #display duplicates

#get number of duplicates
#a <- c(1,2,3,2,3,3)
#sum(duplicated(a)) 3
#duplicated(a) FALSE FALSE FALSE  TRUE  TRUE  TRUE
#unique(a) 1 2 3
#length(unique(a)) 3

sum(duplicated(mft_care$word_German)) #115 duplicates
length(unique(mft_care$word_German)) #355 unique words

mft_care <- unique(mft_care$word_German)

#save
save(mft_care, file= "mft_care_german.Rdata")


#fairness

mft_fairness <- filter(mft, mft$foundation== "fairness") #351 words

sum(duplicated(mft_fairness$word_German)) #89 duplicates
length(unique(mft_fairness$word_German)) # 262 unique words

mft_fairness <- unique(mft_fairness$word_German)

#save
save(mft_fairness, file= "mft_fairness_german.Rdata")


#authority

mft_authority <- filter(mft, mft$foundation== "authority") #431 words

sum(duplicated(mft_authority$word_German)) #76 duplicates
length(unique(mft_authority$word_German)) # 355 unique words

mft_authority <- unique(mft_authority$word_German)

#save
save(mft_authority, file= "mft_authority_german.Rdata")


#sanctity

mft_sanctity <- filter(mft, mft$foundation== "sanctity") #660 words

sum(duplicated(mft_sanctity$word_German)) #146 duplicates
length(unique(mft_sanctity$word_German)) #514 unique words

mft_sanctity <- unique(mft_sanctity$word_German)

#save
save(mft_sanctity, file= "mft_sanctity_german.Rdata")


#loyalty

mft_loyalty <- filter(mft, mft$foundation== "loyalty") #191 words

sum(duplicated(mft_loyalty$word_German)) #27 duplicates
length(unique(mft_loyalty$word_German)) # 164 unique words

mft_loyalty <- unique(mft_loyalty$word_German)

#save
save(mft_loyalty, file= "mft_loyalty_german.Rdata")
