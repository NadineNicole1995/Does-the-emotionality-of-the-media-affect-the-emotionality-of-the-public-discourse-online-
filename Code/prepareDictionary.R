# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(quanteda)
library(quanteda.sentiment)
library(quanteda.textstats)
library(googleLanguageR)


#we need a authenfication for API
gl_auth("translationKey.json")

#load self-trained embeddings
load("word_vectors_150.Rdata")

#load pre-trained embeddings
load("preTrained.Rdata")

#load dictionaries
#NRC
nrc <- read.delim("German-NRC-EmoLex.txt")

#anger
nrc_anger <- filter(nrc, nrc$anger ==1) #1245
nrc_anger <- nrc_anger$German.Word %>%
  tolower() %>%
  unique() #1069

#fear
nrc_fear <- filter(nrc, nrc$fear ==1) #1474
nrc_fear <- nrc_fear$German.Word %>%
  tolower() %>%
  unique() #1301

#trust
nrc_trust <- filter(nrc, nrc$trust ==1) #1230
nrc_trust <- nrc_trust$German.Word %>%
  tolower() %>%
  unique() #1144

#sadness
nrc_sadness <- filter(nrc, nrc$sadness ==1) #1187
nrc_sadness <- nrc_sadness$German.Word %>%
  tolower() %>%
  unique() #1054


#disgust
nrc_disgust <- filter(nrc, nrc$disgust ==1) #1056
nrc_disgust <- nrc_disgust$German.Word %>%  
  tolower() %>%
  unique() #913



#Rauhs dictionary

#negative
rauh_neg <- data_dictionary_Rauh$negative
rauh_neg_pos <- data_dictionary_Rauh$neg_positive
#replace "NOT aufbesser" with "not_aufbesser"
for(i in 1:length(rauh_neg_pos)){
  rauh_neg_pos[i] <- gsub("NOT ", "not_", rauh_neg_pos[i])
}
rauh_negative <- append(rauh_neg, rauh_neg_pos) #37080


#positive
rauh_pos <- data_dictionary_Rauh$positive
rauh_neg_neg <- data_dictionary_Rauh$neg_negative
#replace "NOT aufbesser" with "not_aufbesser"
for(i in 1:length(rauh_neg_neg)){
  rauh_neg_neg[i] <- gsub("NOT ", "not_", rauh_neg_neg[i])
}
rauh_positive <- append(rauh_pos, rauh_neg_neg) #37080

#MFT

load("mft_care_german.Rdata")
load("mft_fairness_german.Rdata")
load("mft_authority_german.Rdata")
load("mft_sanctity_german.Rdata")
load("mft_loyalty_german.Rdata")

#LIWC
load("liwc_certainty_german.Rdata")



###########################################################################################################

#sigmoid function
calculate_sigmoid <- function(x,a,c){
  1/(1+exp(-a*(x-c)))
}



#function to expand the dictionaries
expandDictionary <- function(wordList, embedding){
  #wordList <- nrc_anger
  #embedding <- word_vectors
  
  #1) extract words from embeddings which are in wordList
  words_emb <- embedding[rownames(embedding) %in% wordList,]
  
  #2) calculate mean embedding vector of the dictionary words
  words_mean <- colMeans(words_emb) #is a numeric vector
  
  #3) calculate the similarity between the mean vector and every other word in the embeddings
  cos_sim <- sim2 (x=embedding,
                   y= matrix(words_mean, nrow=1))
  
  #4) store results
  scores <- data.frame(score = cos_sim[,1], in_original_dictionary = dimnames(cos_sim)[[1]] %in% wordList)
  scores <- scores[order(scores$score, decreasing = TRUE),]
  
  #scores$sigmoid <- calculate_sigmoid(scores$score, 40, .35)
  
  return(scores)
}

exploreScores <- function(scores){
  original <- filter(scores, scores$in_original_dictionary)
  expanded <- filter(scores, !(scores$in_original_dictionary))
  top <- round(head(original, 30),3)
  topTranslated <- translateToEnglish(rownames(top))
  added <- round(head(expanded, 30),3)
  addedTranslated <- translateToEnglish(rownames(added))
  removed <- round(tail(original, 30),3)
  removed <- removed[order(removed$score, decreasing = FALSE),]
  removedTranslated <- translateToEnglish(rownames(removed))
  df <- data.frame("Top" = paste0(paste(rownames(top), "("), paste0(top$score, ")")),
                   "Top Translated" = topTranslated,
                   "Added" = paste0(paste(rownames(added), "("), paste0(added$score, ")")),
                   "Added Translated" = addedTranslated,
                   "Removed" = paste0(paste(rownames(removed), "("), paste0(removed$score, ")")),
                   "Removed Translated" = removedTranslated)
  return(df)
}

translateToEnglish <- function(x){
  #translate each word
  translation_x <- NULL
  for(i in x){
    translation <- gl_translate(i, target = "en", format = "text")
    translation <- translation$translatedText #extract English word
    translation_x <- append(translation_x, translation)
  }
  
  return(translation_x)
}


#NRC anger
nrc_anger_scores <- expandDictionary(nrc_anger, word_vectors)
extension_nrc_anger <- exploreScores(nrc_anger_scores)
#extension_nrc_anger <- formattable(extension_nrc_anger, caption ="NRC anger")
nrc_anger_scores_pre <- expandDictionary(nrc_anger, preTrained)
extension_nrc_anger_pre <- exploreScores(nrc_anger_scores_pre)
#extension_nrc_anger_pre <- formattable(extension_nrc_anger_pre, caption ="NRC anger (using preTrained embeddings)")
save(nrc_anger_scores, file = "expandedDictionaries/nrc_anger_scores.Rdata")
save(extension_nrc_anger, file = "descriptionExpandedDictionaries/extension_nrc_anger.Rdata")
save(nrc_anger_scores_pre, file = "expandedDictionariesPreTrained/nrc_anger_scores_pre.Rdata")
save(extension_nrc_anger_pre, file = "descriptionExpandedDictionaries/extension_nrc_anger_pre.Rdata")
write.csv(extension_nrc_anger, file = "descriptionExpandedDictionaries/extension_nrc_anger.csv" )
write.csv(extension_nrc_anger_pre, file = "descriptionExpandedDictionaries/extension_nrc_anger_pre.csv" )


#NRC fear
nrc_fear_scores <- expandDictionary(nrc_fear, word_vectors)
extension_nrc_fear <- exploreScores(nrc_fear_scores)
#extension_nrc_fear <- formattable(extension_nrc_fear, caption ="NRC fear")
nrc_fear_scores_pre <- expandDictionary(nrc_fear, preTrained)
extension_nrc_fear_pre <- exploreScores(nrc_fear_scores_pre)
#extension_nrc_fear_pre <- formattable(extension_nrc_fear_pre, caption ="NRC fear (using preTrained embeddings)")
save(nrc_fear_scores, file = "expandedDictionaries/nrc_fear_scores.Rdata")
save(extension_nrc_fear, file = "descriptionExpandedDictionaries/extension_nrc_fear.Rdata")
save(nrc_fear_scores_pre, file = "expandedDictionariesPreTrained/nrc_fear_scores_pre.Rdata")
save(extension_nrc_fear_pre, file = "descriptionExpandedDictionaries/extension_nrc_fear_pre.Rdata")
write.csv(extension_nrc_fear, file = "descriptionExpandedDictionaries/extension_nrc_fear.csv" )
write.csv(extension_nrc_fear_pre, file = "descriptionExpandedDictionaries/extension_nrc_fear_pre.csv" )


#NRC trust
nrc_trust_scores <- expandDictionary(nrc_trust, word_vectors)
extension_nrc_trust <- exploreScores(nrc_trust_scores)
#extension_nrc_trust <- formattable(extension_nrc_trust, caption ="NRC trust")
nrc_trust_scores_pre <- expandDictionary(nrc_trust, preTrained)
extension_nrc_trust_pre <- exploreScores(nrc_trust_scores_pre)
#extension_nrc_trust_pre <- formattable(extension_nrc_trust_pre, caption ="NRC trust (using preTrained embeddings)")
save(nrc_trust_scores, file = "expandedDictionaries/nrc_trust_scores.Rdata")
save(extension_nrc_trust, file = "descriptionExpandedDictionaries/extension_nrc_trust.Rdata")
save(nrc_trust_scores_pre, file = "expandedDictionariesPreTrained/nrc_trust_scores_pre.Rdata")
save(extension_nrc_trust_pre, file = "descriptionExpandedDictionaries/extension_nrc_trust_pre.Rdata")
write.csv(extension_nrc_trust, file = "descriptionExpandedDictionaries/extension_nrc_trust.csv" )
write.csv(extension_nrc_trust_pre, file = "descriptionExpandedDictionaries/extension_nrc_trust_pre.csv" )

#NRC sadness
nrc_sadness_scores <- expandDictionary(nrc_sadness, word_vectors)
extension_nrc_sadness <- exploreScores(nrc_sadness_scores)
#extension_nrc_sadness <- formattable(extension_nrc_sadness, caption ="NRC sadness")
nrc_sadness_scores_pre <- expandDictionary(nrc_sadness, preTrained)
extension_nrc_sadness_pre <- exploreScores(nrc_sadness_scores_pre)
#extension_nrc_sadness_pre <- formattable(extension_nrc_sadness_pre, caption ="NRC sadness (using preTrained embeddings)")
save(nrc_sadness_scores, file = "expandedDictionaries/nrc_sadness_scores.Rdata")
save(extension_nrc_sadness, file = "descriptionExpandedDictionaries/extension_nrc_sadness.Rdata")
save(nrc_sadness_scores_pre, file = "expandedDictionariesPreTrained/nrc_sadness_scores_pre.Rdata")
save(extension_nrc_sadness_pre, file = "descriptionExpandedDictionaries/extension_nrc_sadness_pre.Rdata")
write.csv(extension_nrc_sadness, file = "descriptionExpandedDictionaries/extension_nrc_sadness.csv" )
write.csv(extension_nrc_sadness_pre, file = "descriptionExpandedDictionaries/extension_nrc_sadness_pre.csv" )



#NRC disgust
nrc_disgust_scores <- expandDictionary(nrc_disgust, word_vectors)
extension_nrc_disgust <- exploreScores(nrc_disgust_scores)
#extension_nrc_disgust <- formattable(extension_nrc_disgust, caption ="NRC disgust")
nrc_disgust_scores_pre <- expandDictionary(nrc_disgust, preTrained)
extension_nrc_disgust_pre <- exploreScores(nrc_disgust_scores_pre)
#extension_nrc_disgust_pre <- formattable(extension_nrc_disgust_pre, caption ="NRC disgust (using preTrained embeddings)")
save(nrc_disgust_scores, file = "expandedDictionaries/nrc_disgust_scores.Rdata")
save(extension_nrc_disgust, file = "descriptionExpandedDictionaries/extension_nrc_disgust.Rdata")
save(nrc_disgust_scores_pre, file = "expandedDictionariesPreTrained/nrc_disgust_scores_pre.Rdata")
save(extension_nrc_disgust_pre, file = "descriptionExpandedDictionaries/extension_nrc_disgust_pre.Rdata")
write.csv(extension_nrc_disgust, file = "descriptionExpandedDictionaries/extension_nrc_disgust.csv" )
write.csv(extension_nrc_disgust_pre, file = "descriptionExpandedDictionaries/extension_nrc_disgust_pre.csv" )



#Rauh negative
rauh_negative_scores <- expandDictionary(rauh_negative, word_vectors)
extension_rauh_negative <- exploreScores(rauh_negative_scores)
#extension_rauh_negative <- formattable(extension_rauh_negative, caption ="Rauh negative")
save(rauh_negative_scores, file = "expandedDictionaries/rauh_negative_scores.Rdata")
save(extension_rauh_negative, file = "descriptionExpandedDictionaries/extension_rauh_negative.Rdata")

rauh_negative_scores_pre <- expandDictionary(rauh_negative, preTrained)
extension_rauh_negative_pre <- exploreScores(rauh_negative_scores_pre)
write.csv(extension_rauh_negative, file = "descriptionExpandedDictionaries/extension_rauh_negative.csv" )
write.csv(extension_rauh_negative_pre, file = "descriptionExpandedDictionaries/extension_rauh_negative_pre.csv" )


#Rauh positive
rauh_positive_scores <- expandDictionary(rauh_positive, word_vectors)
extension_rauh_positive <- exploreScores(rauh_positive_scores)
#extension_rauh_positive <- formattable(extension_rauh_positive, caption ="Rauh positive")
save(rauh_positive_scores, file = "expandedDictionaries/rauh_positive_scores.Rdata")
save(extension_rauh_positive, file = "descriptionExpandedDictionaries/extension_rauh_positive.Rdata")

rauh_positive_scores_pre <- expandDictionary(rauh_positive, preTrained)
extension_rauh_positive_pre <- exploreScores(rauh_positive_scores_pre)
write.csv(extension_rauh_positive, file = "descriptionExpandedDictionaries/extension_rauh_positive.csv" )
write.csv(extension_rauh_positive_pre, file = "descriptionExpandedDictionaries/extension_rauh_positive_pre.csv" )





#Mft care
mft_care_scores <- expandDictionary(mft_care, word_vectors)
extension_mft_care <- exploreScores(mft_care_scores)
#extension_mft_care <- formattable(extension_mft_care, caption ="Mft care")
save(mft_care_scores, file = "expandedDictionaries/mft_care_scores.Rdata")
save(extension_mft_care, file = "descriptionExpandedDictionaries/extension_mft_care.Rdata")

mft_care_scores_pre <- expandDictionary(mft_care, preTrained)
extension_mft_care_pre <- exploreScores(mft_care_scores_pre)
write.csv(extension_mft_care, file = "descriptionExpandedDictionaries/extension_mft_care.csv" )
write.csv(extension_mft_care_pre, file = "descriptionExpandedDictionaries/extension_mft_care_pre.csv" )




#Mft fairness
mft_fairness_scores <- expandDictionary(mft_fairness, word_vectors)
extension_mft_fairness <- exploreScores(mft_fairness_scores)
#extension_mft_fairness <- formattable(extension_mft_fairness, caption ="Mft fairness")
save(mft_fairness_scores, file = "expandedDictionaries/mft_fairness_scores.Rdata")
save(extension_mft_fairness, file = "descriptionExpandedDictionaries/extension_mft_fairness.Rdata")

mft_fairness_scores_pre <- expandDictionary(mft_fairness, preTrained)
extension_mft_fairness_pre <- exploreScores(mft_fairness_scores_pre)
write.csv(extension_mft_fairness, file = "descriptionExpandedDictionaries/extension_mft_fairness.csv" )
write.csv(extension_mft_fairness_pre, file = "descriptionExpandedDictionaries/extension_mft_fairness_pre.csv" )



#Mft authority
mft_authority_scores <- expandDictionary(mft_authority, word_vectors)
extension_mft_authority <- exploreScores(mft_authority_scores)
#extension_mft_authority <- formattable(extension_mft_authority, caption ="Mft authority")
save(mft_authority_scores, file = "expandedDictionaries/mft_authority_scores.Rdata")
save(extension_mft_authority, file = "descriptionExpandedDictionaries/extension_mft_authority.Rdata")

mft_authority_scores_pre <- expandDictionary(mft_authority, preTrained)
extension_mft_authority_pre <- exploreScores(mft_authority_scores_pre)
write.csv(extension_mft_authority, file = "descriptionExpandedDictionaries/extension_mft_authority.csv" )
write.csv(extension_mft_authority_pre, file = "descriptionExpandedDictionaries/extension_mft_authority_pre.csv" )




#Mft sanctity
mft_sanctity_scores <- expandDictionary(mft_sanctity, word_vectors)
extension_mft_sanctity <- exploreScores(mft_sanctity_scores)
#extension_mft_sanctity <- formattable(extension_mft_sanctity, caption ="Mft sanctity")
save(mft_sanctity_scores, file = "expandedDictionaries/mft_sanctity_scores.Rdata")
save(extension_mft_sanctity, file = "descriptionExpandedDictionaries/extension_mft_sanctity.Rdata")

mft_sanctity_scores_pre <- expandDictionary(mft_sanctity, preTrained)
extension_mft_sanctity_pre <- exploreScores(mft_sanctity_scores_pre)
write.csv(extension_mft_sanctity, file = "descriptionExpandedDictionaries/extension_mft_sanctity.csv" )
write.csv(extension_mft_sanctity_pre, file = "descriptionExpandedDictionaries/extension_mft_sanctity_pre.csv" )

#Mft loyalty
mft_loyalty_scores <- expandDictionary(mft_loyalty, word_vectors)
extension_mft_loyalty <- exploreScores(mft_loyalty_scores)
#extension_mft_loyalty <- formattable(extension_mft_loyalty, caption ="Mft loyalty")
save(mft_loyalty_scores, file = "expandedDictionaries/mft_loyalty_scores.Rdata")
save(extension_mft_loyalty, file = "descriptionExpandedDictionaries/extension_mft_loyalty.Rdata")

mft_loyalty_scores_pre <- expandDictionary(mft_loyalty, preTrained)
extension_mft_loyalty_pre <- exploreScores(mft_loyalty_scores_pre)
write.csv(extension_mft_loyalty, file = "descriptionExpandedDictionaries/extension_mft_loyalty.csv" )
write.csv(extension_mft_loyalty_pre, file = "descriptionExpandedDictionaries/extension_mft_loyalty_pre.csv" )





#LIWC certainty
liwc_certainty_scores <- expandDictionary(liwc_certainty_german, word_vectors)
extension_liwc_certainty <- exploreScores(liwc_certainty_scores)
extension_liwc_certainty <- formattable(extension_liwc_certainty, caption ="LIWC certainty")
save(liwc_certainty_scores, file = "expandedDictionaries/liwc_certainty_scores.Rdata")
save(extension_liwc_certainty, file = "descriptionExpandedDictionaries/extension_liwc_certainty.Rdata")

