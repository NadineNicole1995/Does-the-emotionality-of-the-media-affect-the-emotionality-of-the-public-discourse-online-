# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(stm)
library(tidyverse)
library(quanteda)
library(ggforce)
library(Rtsne)
library(rsvd)
library(geometry)
library(tm)
library(readr)
library(readxl)

#load articles datafile
load("Final/articles.Rdata")

#create corpus
articles_corpus <- articles %>%
  corpus(text_field = "article")

#create dfm
articles_dfm <- articles_corpus %>%
  tokens(remove_punct = TRUE) %>%
  dfm() %>%
  dfm_remove(stopwords("de")) %>%
  dfm_remove(c("dass")) %>% #only "daß" in stopwords
  dfm_wordstem() %>%
  dfm_trim(min_termfreq = 5)

##################search K############################################################
search_k <- searchK(documents  = articles_dfm,
                    K          = seq(5,70, by=5), N =2000)

save(search_k, file ="search_k.Rdata")
load("search_k.Rdata")
plot(search_k)

df  <- search_k$results 

df$semcoh <- unlist(df$semcoh)
df$exclus <- unlist(df$exclus)
df$K <- unlist(df$K)
df <- df %>%
  mutate(K = as.factor(K))

df %>% 
  ggplot(aes(semcoh, exclus, col = K)) +
  geom_point(size = 3) +
  geom_mark_ellipse(aes(filter = K %in% c(10, 15, 20,25)), 
                    col = "red", description = "Potentials best candidates for K") +
  labs(x = "Semantic Coherence", y = "Exclusivity", title = "Search K (5 to 70)") +
  theme(legend.position = "bottom")


#more in detail
search_k_detail <- searchK(documents  = articles_dfm,
                           K          = seq(10,25, by=1), N =2000)

save(search_k_detail, file ="search_k_detail.Rdata")
load("search_k_detail.Rdata")

plot(search_k_detail)

df  <- search_k_detail$results 

df$semcoh <- unlist(df$semcoh)
df$exclus <- unlist(df$exclus)
df$K <- unlist(df$K)
df <- df %>%
  mutate(K = as.factor(K))

df %>% 
  ggplot(aes(semcoh, exclus, col = K)) +
  geom_point(size = 3) +http://127.0.0.1:15791/graphics/plot_zoom_png?width=1189&height=900
  geom_mark_ellipse(aes(filter = K %in% c(15,16, 22)), 
                    col = "red", description = "Potentials best candidates for K") +
  labs(x = "Semantic Coherence", y = "Exclusivity", title = "Search K (10 to 25)") +
  theme(legend.position = "bottom")

#use K=16 for the best K and train model
modelCovid <- stm(documents = articles_dfm,
                  K          = 16, 
                  seed       = 1234,
                  init.type = "Spectral",
                  #max.em.its = 150,
                  verbose =TRUE)
save(modelCovid, file ="topicModel.Rdata")
load("TopicModeling/topicModel.Rdata")

labelTopics(modelCovid)
plot(modelCovid, labeltype = "frex")
cloud(modelCovid, topic=5)
findThoughts(model = modelCovid,
             text= as.character(articles_corpus),
             topic =2, 3)
findThoughts(model = modelCovid,
             text= as.character(articles_corpus),
             topic =12)
findThoughts(model = modelCovid,
             text= as.character(articles_corpus),
             topic =4)
#directly access topic proportions
head(modelCovid$theta)
thetas <- modelCovid$theta
which.max(thetas[1,])

topics <- apply(thetas, 1, which.max)
id <- articles_dfm$Id

topicsArticle <- data.frame("Id" =id, "TopicArticle" = topics)
save(topicsArticle, file = "topicsArticle.Rdata")

out_corr <- topicCorr(modelCovid)
plot(topicCorr(modelCovid))
