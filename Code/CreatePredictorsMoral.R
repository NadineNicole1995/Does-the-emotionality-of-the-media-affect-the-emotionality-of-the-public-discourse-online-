# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())

library(ggplot2)
library(gender)
library(tidyverse)
library(stringi)
library(quanteda)
library(quanteda.textstats)
library(quanteda.sentiment)
library(readr)
library(readxl)

#load datafiles
load("Final/articles.Rdata")
#load("Final/HB_comment.Rdata")
#load("Final/Zeit_comment.Rdata")
#load("Final/Welt_comment.Rdata")
load("Final/comments.Rdata")

#predictors: date, lengthTitle, lengthArticle, typetokenratio article,  genderOfAuthor, ReadabilityArticle, topicArticle,
#SentimentsTitle, SentimentsArticle, 
#output: numberComments, sentimentComments

articlesRich <- articles

#Date

hb <- filter(articles, articles$outlet == "HB")
zeit <- filter(articles, articles$outlet == "Zeit")
welt <- filter(articles, articles$outlet == "Welt")


ggplot(hb, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


ggplot(zeit, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()

ggplot(welt, aes(x=date, y=numberComments)) + 
  geom_point()+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  geom_smooth()


#gender of author

#extract first name
#for simplicity we extract the first name of the first author when an article has multiple authors
names <- articles$author

names <- gsub("Dr.", "",names) #remove Dr.
names <- gsub("med.", "", names)#remove Dr.med
names <- gsub("Eine Rezension von ", "", names)
names <- gsub("Prof.   h. c. ", "", names)
names <- gsub("Prof. ", "", names)
names <- gsub("Professor. ", "", names)
names <- gsub("Protokoll: ", "", names)
names <- gsub("Quiz: ", "", names)
names <- gsub("Text: ", "", names)
names <- str_trim(names)

firstName <- sapply(strsplit(names, " "), `[`, 1)
save(firstName, file="firstName.Rdata")

#stri_isempty(c("Hi", ""))
#test <- stri_isempty(firstName)
#unique(test)

#test gender
#testing = c("unknown", 'Alice', 'Alice', 'Bob', 'Charles', 'Jürgen', 'Andreas', "Miasaus")
#gender_results <- gender(testing) # Returns a data frame

## A tibble: 6 × 6
#name    proportion_male proportion_female gender year_min year_max
#<chr>             <dbl>             <dbl> <chr>     <dbl>    <dbl>
# 1 Alice            0.0034            0.997  female     1932     2012
#2 Alice            0.0034            0.997  female     1932     2012
#3 Andreas          0.992             0.008  male       1932     2012
#4 Bob              0.997             0.0031 male       1932     2012
#5 Charles          0.995             0.0051 male       1932     2012
#6 unknown          0.473             0.527  female     1932     2012

#unknown gives gender female
#names like Jürgen, Miasaus not included

#gender_df <- gender_results %>% 
  #select(name, gender) %>%
  #distinct()

# A tibble: 5 × 2
#name    gender
#<chr>   <chr> 
#1 Alice   female
#2 Andreas male  
#3 Bob     male  
#4 Charles male  
#5 unknown female

#gender_df <- filter(gender_df, gender_df$name != "unknown") #remove unknown

# A tibble: 4 × 2
#name    gender
#<chr>   <chr> 
#1 Alice   female
#2 Andreas male  
#3 Bob     male  
#4 Charles male 

#test_df <- data.frame("name" = testing)

#name
#1 unknown
#2   Alice
#3   Alice
#4     Bob
#5 Charles
#6  Jürgen
#7 Andreas
#8 Miasaus

#test_df<- merge(test_df, gender_df, by = "name", all.x = TRUE)
#name gender
#1   Alice female
#2   Alice female
#3 Andreas   male
#4     Bob   male
#5 Charles   male
#6  Jürgen   <NA>
#7 Miasaus   <NA>
#8 unknown   <NA>

#genderFirstName <- gender(firstName) # Returns a data frame
save(genderFirstName, file= "genderFirstName.Rdata")
load("genderFirstName.Rdata")


gender_df <- genderFirstName %>% 
  select(name, gender) %>%
  rename(firstNameAuthor=name, genderAuthor = gender)%>% 
  distinct()


gender_df <- filter(gender_df, gender_df$firstNameAuthor != "unknown") #remove unknown

#unknownNames <- filter(articlesRich, is.na(articlesRich$genderAuthor))
#write.csv(unknownNames, "unknownNames.csv")
unknownNames <- read_xlsx("unknownNames.xlsx") #manually labeled gender

gender_df <- rbind(gender_df, unknownNames)

#add firstName to articles
articlesRich$firstNameAuthor <- firstName

articlesRich<- merge(articlesRich, gender_df, by = "firstNameAuthor", all.x = TRUE)

articlesRich <- articlesRich[, c(2,3,4,5,6,7,8,9,10,1,11)]

#set NAs to unknown
articlesRich$genderAuthor[is.na(articlesRich$genderAuthor)] <- "unknown"

#set them as factor
articlesRich$genderAuthor <- as.factor(articlesRich$genderAuthor)

###########################################################################


#TopicArticle
load("topicsArticle.Rdata")

articlesRich <- merge(articlesRich, topicsArticle, by= "Id", all.x=TRUE )

articlesRich$TopicArticle <- as.factor(articlesRich$TopicArticle)

ggplot(articles) + 
  geom_bar(aes(x=topics, fill = as.factor(outlet)),stat="count", position="dodge") + 
  #scale_fill_manual(values = c("blue", "pink")) + 
  ylab("Count article associted with topic") 

ggplot(hb) + 
  geom_point(aes(x=topics, y = numberComments))

ggplot(zeit) + 
  geom_point(aes(x=topics, y = numberComments))
ggplot(welt) + 
  geom_point(aes(x=topics, y = numberComments))



###############################################################################################
#length of title (ntoken)
corpus_title <- articlesRich %>%
  corpus(text_field = "title")

#get ntokens
ntokens <- ntoken(corpus_title)

articlesRich$numberTokensTitle <- ntokens

# extract summary statistics of corpus
#ntokens_df <- summary(corpus_title, n = ndoc(corpus_title))

#number types/ number tokens
#typeToTokenRatio <- ntokens_df$Types/ntokens_df$Tokens

########################################################################################
#length of article (ntoken)
corpus_articles <- articlesRich %>%
  corpus(text_field = "article")

#get ntokens
ntokens <- ntoken(corpus_articles)

articlesRich$numberTokensArticle <- ntokens

# extract summary statistics of corpus
ntokens_df <- summary(corpus_articles, n = ndoc(corpus_articles))

#number types/ number tokens
typeToTokenRatio <- ntokens_df$Types/ntokens_df$Tokens

articlesRich$typeToTokenRatioArticle <- typeToTokenRatio

###########################################################################################
#readabilty score

readability <- textstat_readability(corpus_articles, measure ="Flesch")

articlesRich$readabilityArticle <- readability$Flesch



###########################################################################################

#sentiment
#score = (sim * tdf)/sum(tdf)

#example
colors_df <- data.frame("id" = c(1,2,3,4,5), 
                     "text"= c("Blue is a beautiful color, but I do not like blue and prefer red",
                               "I hate red and orange dresses. Dresses have to be yellow!!!",
                               "The Ukraine colors are blue and yellow",
                               "Is white a color?",
                              "Rainbow has multiple colors such as  blue, yellow, red and orange"))

#named vector of weights of our dictionary words
color_dic <- c(0.5, 0.4, 0.1, 0.2, 0.1, 0.2, 0.4)
names(color_dic) <- c("blue", "red", "black", "yellow", "white", "orange", "green")

#create corpus
colors_corpus <- colors_df %>%
  corpus(text_field = "text")

#create dfm with tf-idf
colors_dfm <- colors_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower() %>%
  dfm() %>%
  dfm_tfidf()

#dfm with only the terms in our dictionary
colors_dfm_dic <- colors_dfm %>%
  dfm_select(pattern = names(color_dic)) #only words in dictionary

#weight the tdfs
colors_dfm_weighted <- dfm_weight(colors_dfm_dic, weight = color_dic)

#calculate score
numerator <- rowSums(colors_dfm_weighted)
denominator <-rowSums(colors_dfm) 
score <- numerator/denominator  
colors_df$score <- score


########################load dictionaries and create named vector###################################
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation/expandedDictionaries")
load("nrc_anger_scores.Rdata")
load("nrc_fear_scores.Rdata")
load("nrc_sadness_scores.Rdata")
load("nrc_trust_scores.Rdata")
load("nrc_disgust_scores.Rdata")
load("rauh_negative_scores.Rdata")
load("rauh_positive_scores.Rdata")
load("mft_care_scores.Rdata")
load("mft_authority_scores.Rdata")
load("mft_fairness_scores.Rdata")
load("mft_sanctity_scores.Rdata")
load("mft_loyalty_scores.Rdata")
load("liwc_certainty_scores.Rdata")

anger <- nrc_anger_scores[,1]
names(anger) <- row.names(nrc_anger_scores)

fear <- nrc_fear_scores[,1]
names(fear) <- row.names(nrc_fear_scores)

sadness <- nrc_sadness_scores[,1]
names(sadness) <- row.names(nrc_sadness_scores)

trust <- nrc_trust_scores[,1]
names(trust) <- row.names(nrc_trust_scores)

disgust <- nrc_disgust_scores[,1]
names(disgust) <- row.names(nrc_disgust_scores)

negative <- rauh_negative_scores[,1]
names(negative) <- row.names(rauh_negative_scores)

positive <- rauh_positive_scores[,1]
names(positive) <- row.names(rauh_positive_scores)

care <- mft_care_scores[,1]
names(care) <- row.names(mft_care_scores)

authority <- mft_authority_scores[,1]
names(authority) <- row.names(mft_authority_scores)

fairness <- mft_fairness_scores[,1]
names(fairness) <- row.names(mft_fairness_scores)

sanctity <- mft_sanctity_scores[,1]
names(sanctity) <- row.names(mft_sanctity_scores)


loyalty <- mft_loyalty_scores[,1]
names(loyalty) <- row.names(mft_loyalty_scores)

certainty <- liwc_certainty_scores[,1]
names(certainty) <- row.names(liwc_certainty_scores)


###################function to calculate sentiment scores##################

getScore <- function(dic, dfm){
  #dfm with only the terms in dictionary
  dfm_dic <- dfm %>%
    dfm_select(pattern = names(dic)) #only words in dictionary
  
  #weight the tdfs
  dfm_dic_weighted <- dfm_weight(dfm_dic, weight = dic)
  
  #calculate fear score
  numerator <- rowSums(dfm_dic_weighted)
  denominator <-rowSums(dfm_dic) 
  score <- numerator/denominator  
  return(score)
  
}

###################sentiment title##########################################
#create corpus
title_corpus <- articlesRich %>%
  corpus(text_field = "title")

#create dfm with tf-idf
title_toks <- title_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower()
  
toks1 <- tokens_replace(title_toks, pattern = c("nicht", "nichts", "kein",
                                            "keine", "keinen"),
                          replacement = rep("not", 5))
title_tokens <- tokens_compound(toks1, data_dictionary_Rauh)
  
title_dfm  <- title_tokens %>%
  dfm() %>%
  dfm_remove(stopwords("de")) %>%
  dfm_remove(c("dass")) %>%
  dfm_tfidf()

#calculate title sentiment scores and store it in articlesRich
#articlesRich$titleAngerScore <- getScore(anger, title_dfm)
#articlesRich$titleFearScore <- getScore(fear, title_dfm)
#articlesRich$titleSadnessScore <- getScore(sadness, title_dfm)
#articlesRich$titleTrustScore <- getScore(trust, title_dfm)
#articlesRich$titleDisgustScore <- getScore(disgust, title_dfm)
#articlesRich$titleNegativeScore <- getScore(negative, title_dfm)
#articlesRich$titlePositiveScore <- getScore(positive, title_dfm)
articlesRich$titleCareScore <- getScore(care, title_dfm)
articlesRich$titleAuthorityScore <- getScore(authority, title_dfm)
articlesRich$titleFairnessScore <- getScore(fairness, title_dfm)
articlesRich$titleSanctityScore <- getScore(sanctity, title_dfm)
articlesRich$titleLoyaltyScore <- getScore(loyalty, title_dfm)

#replace NA values
#articlesRich$titleAngerScore[is.na(articlesRich$titleAngerScore)] <-0
#articlesRich$titleFearScore[is.na(articlesRich$titleFearScore)] <-0
#articlesRich$titleSadnessScore[is.na(articlesRich$titleSadnessScore)] <-0
#articlesRich$titleTrustScore[is.na(articlesRich$titleTrustScore)] <-0
#articlesRich$titleDisgustScore[is.na(articlesRich$titleDisgustScore)] <-0
#articlesRich$titleNegativeScore[is.na(articlesRich$titleNegativeScore)] <-0
#articlesRich$titlePositiveScore[is.na(articlesRich$titlePositiveScore)] <-0
articlesRich$titleCareScore[is.na(articlesRich$titleCareScore)] <-0
articlesRich$titleAuthorityScore[is.na(articlesRich$titleAuthorityScore)] <-0
articlesRich$titleFairnessScore[is.na(articlesRich$titleFairnessScore)] <-0
articlesRich$titleSanctityScore[is.na(articlesRich$titleSanctityScore)] <-0
articlesRich$titleLoyaltyScore[is.na(articlesRich$titleLoyaltyScore)] <-0



####################################sentiment article####################################################
#create corpus
article_corpus <- articlesRich %>%
  corpus(text_field = "article")

#create dfm with tf-idf
article_toks <- article_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower()

toks1 <- tokens_replace(article_toks, pattern = c("nicht", "nichts", "kein",
                                                "keine", "keinen"),
                        replacement = rep("not", 5))
article_tokens <- tokens_compound(toks1, data_dictionary_Rauh)

article_dfm  <- article_tokens %>%
  dfm() %>%
  dfm_remove(stopwords("de")) %>%
  dfm_remove(c("dass")) %>%
  #dfm_trim(min_docfreq = .0002, max_docfreq = .90, docfreq_type = "prop") %>%
  dfm_tfidf()

#calculate article sentiment scores and store it in articlesRich
#articlesRich$articleAngerScore <- getScore(anger, article_dfm)
#articlesRich$articleFearScore <- getScore(fear, article_dfm)
#articlesRich$articleSadnessScore <- getScore(sadness, article_dfm)
#articlesRich$articleTrustScore <- getScore(trust, article_dfm)
#articlesRich$articleDisgustScore <- getScore(disgust, article_dfm)
#articlesRich$articleNegativeScore <- getScore(negative, article_dfm)
#articlesRich$articlePositiveScore <- getScore(positive, article_dfm)
articlesRich$articleCareScore <- getScore(care, article_dfm)
articlesRich$articleAuthorityScore <- getScore(authority, article_dfm)
articlesRich$articleFairnessScore <- getScore(fairness, article_dfm)
articlesRich$articleSanctityScore <- getScore(sanctity, article_dfm)
articlesRich$articleLoyaltyScore <- getScore(loyalty, article_dfm)



###############sentiment comments######################################################
#create corpus
comments_corpus <- comments %>%
  corpus(text_field = "comment")

#create dfm with tf-idf
comments_toks <- comments_corpus %>%
  tokens() %>%
  tokens(remove_punct = TRUE) %>%
  tokens_tolower()

toks1 <- tokens_replace(comments_toks, pattern = c("nicht", "nichts", "kein",
                                                  "keine", "keinen"),
                        replacement = rep("not", 5))
comments_tokens <- tokens_compound(toks1, data_dictionary_Rauh)

comments_dfm  <- comments_tokens %>%
  dfm() %>%
  dfm_remove(stopwords("de")) %>%
  dfm_remove(c("dass")) %>%
  dfm_tfidf()

#calculate comments sentiment scores and store it in comments
#comments$commentsAngerScore <- getScore(anger, comments_dfm)
#comments$commentsFearScore <- getScore(fear, comments_dfm)
#comments$commentsSadnessScore <- getScore(sadness, comments_dfm)
#comments$commentsTrustScore <- getScore(trust, comments_dfm)
#comments$commentsDisgustScore <- getScore(disgust, comments_dfm)
#comments$commentsNegativeScore <- getScore(negative, comments_dfm)
#comments$commentsPositiveScore <- getScore(positive, comments_dfm)
comments$commentsCareScore <- getScore(care, comments_dfm)
comments$commentsAuthorityScore <- getScore(authority, comments_dfm)
comments$commentsFairnessScore <- getScore(fairness, comments_dfm)
comments$commentsSanctityScore <- getScore(sanctity, comments_dfm)
comments$commentsLoyaltyScore <- getScore(loyalty, comments_dfm)



#replace NA values
#comments$commentsAngerScore[is.na(comments$commentsAngerScore)] <-0
#comments$commentsFearScore[is.na(comments$commentsFearScore)] <-0
#comments$commentsSadnessScore[is.na(comments$commentsSadnessScore)] <-0
#comments$commentsTrustScore[is.na(comments$commentsTrustScore)] <-0
#comments$commentsDisgustScore[is.na(comments$commentsDisgustScore)] <-0
#comments$commentsNegativeScore[is.na(comments$commentsNegativeScore)] <-0
#comments$commentsPositiveScore[is.na(comments$commentsPositiveScore)] <-0
comments$commentsCareScore[is.na(comments$commentsCareScore)] <-0
comments$commentsAuthorityScore[is.na(comments$commentsAuthorityScore)] <-0
comments$commentsFairnessScore[is.na(comments$commentsFairnessScore)] <-0
comments$commentsSanctityScore[is.na(comments$commentsSanctityScore)] <-0
comments$commentsLoyaltyScore[is.na(comments$commentsLoyaltyScore)] <-0


#mean scores

#calculate mean comments scores
comments_scores <- comments %>%
  group_by(Id) %>%
  summarise(#commentsAngerScore= mean(commentsAngerScore),
            #commentsFearScore= mean(commentsFearScore),
            #commentsSadnessScore= mean(commentsSadnessScore),
            #commentsTrustScore= mean(commentsTrustScore),
            #commentsDisgustScore= mean(commentsDisgustScore),
            #commentsNegativeScore= mean(commentsNegativeScore),
            #commentsPositiveScore= mean(commentsPositiveScore),
            commentsCareScore= mean(commentsCareScore),
            commentsAuthorityScore= mean(commentsAuthorityScore),
            commentsFairnessScore= mean(commentsFairnessScore),
            commentsSanctityScore= mean(commentsSanctityScore),
            commentsLoyaltyScore= mean(commentsLoyaltyScore),)

#correct numbercomments
articlesRich$numberComments[articlesRich$Id == "Z-3878"] <- 92
articlesRich$numberComments[articlesRich$Id == "Z-3880"] <- 109
articlesRich$numberComments[articlesRich$Id == "Z-3882"] <- 25
articlesRich$numberComments[articlesRich$Id == "Z-3884"] <- 2

#merge with articlesRich
articlesRich <- merge(articlesRich, comments_scores, by = "Id", all.x = TRUE)

articlesRichMoral <- articlesRich


save(articlesRichMoral, file = "articlesRichMoral.Rdata")

#save in smaller data files
articlesRichMoral1 <- articlesRichMoral[1:10000,]
save(articlesRichMoral1, file = "articlesRichMoral1.Rdata" )
articlesRichMoral2 <- articlesRichMoral[10001:17787,]
save(articlesRichMoral2, file = "articlesRichMoral2.Rdata" )


