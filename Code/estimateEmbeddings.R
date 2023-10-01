# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library(readr)
library(quanteda)
library(text2vec)
library(quanteda.textstats)
library(quanteda.textplots)
library(umap)
library(tidyverse)
library(superheat)
#library(Rtsne)
#library(PsychWordVec)
library(data.table)
library(googleLanguageR)
library(cocor)
library(formattable)
library(reactablefmtr)

#load datafiles articles and comments and put both together (articles and comments)
load("Final/articles.Rdata")
load("Final/comments.Rdata")


textAll <- articles[,c(1,5)]
textAll <- rbind(textAll, welt[,c(1,7)])
colnames(textAll) <- c("Id", "text")
colnames(comments) <- c("Id", "text")
textAll <- rbind(textAll, comments)




#########################estimate word embeddings#########################################################

textAll_tokens<- textAll %>% 
  corpus(text_field = "text") %>% 
  tokens(remove_punct = TRUE) %>%
  tokens_tolower()


textAll_feats <- dfm(textAll_tokens, verbose = TRUE)

# Exclude words appearing in fewer than 0.02% or more than 90%; exclude stopwords

textAll_feats_small <- textAll_feats %>% 
  dfm_trim(min_docfreq = .0002, max_docfreq = .90, docfreq_type = "prop") %>%
  dfm_select(pattern = stopwords("de"), selection = "remove") %>%
  dfm_select(pattern = c("dass"), selection = "remove") %>%
  featnames()

textAll_tokens_small <- tokens_remove(textAll_tokens, 
                                     pattern = featnames(textAll_feats)[!featnames(textAll_feats) %in% textAll_feats_small], 
                                     padding = TRUE)


## Covert to feature-context matrix

textAll_fcm <- fcm(textAll_tokens_small, 
                  context = "window", 
                  count = "weighted", 
                  window = 6, 
                  weights = 1 / (1:6), 
                  tri = TRUE)


save(textAll_fcm, file = "textAll_fcm.Rdata")




## Fit GLOVE model

glove = GlobalVectors$new(rank = 150, x_max = 2500L, learning_rate = .145)
textAll_main = glove$fit_transform(textAll_fcm, n_iter = 500, convergence_tol = 0.005, n_threads = 8)

textAll_context = glove$components

word_vectors = textAll_main + t(textAll_context)

save(word_vectors, file = "word_vectors_150.Rdata")


############################################################################
#topfeatures 
topfeat <- names(topfeatures(textAll_fcm, n=50))

fcm_top <- fcm_select(textAll_fcm, pattern =topfeat) 


fcm_top%>%
  textplot_network(min_freq = 0.9)#, 
  #vertex_labelsize = rowSums(fcm_top)/min(rowSums(fcm_top)))






##############################################################################
#function to calculate similarities
#target_word: word for which we would like to calculate similarities
#n: number of nearest neighbouring words returned
#embedding: word embeddings
similarities <- function(target_word, n, embedding){
  
  # Extract embedding of target word
  target_vector <- embedding[which(rownames(embedding) %in% target_word),]  
  
  # Calculate cosine similarity between target word and other words
  target_sim <- sim2(embedding, matrix(target_vector, nrow = 1))
  
  # Report nearest neighbours of target word
  names(sort(target_sim[,1], decreasing = T))[1:n]
  
  #report also similarity score
  sort(target_sim[,1], decreasing = T)[1:n]
  
}


similarities("cdu", 10, word_vectors)
similarities("merkel", 10, word_vectors)
similarities("twitter", 10, word_vectors)
similarities("biontech", 10, word_vectors)
similarities("impfen", 10, word_vectors)
similarities("lockdown", 10, word_vectors)
similarities("lauterbach", 10, word_vectors)
similarities("ischgl", 10, word_vectors)
similarities("böse", 10, word_vectors)
similarities("angst", 10, word_vectors)
similarities("ungeimpft", 10, word_vectors)
similarities("hass", 10, word_vectors)

#function to calculate analogies
#a is to b as c is to....
#target <- king -men + women
#a: men
#b: women
#c: king
analogies <- function(a, b, c, n, embedding){
  
  # Extract vectors for each of the three words in analogy task
  a_vec <- embedding[which(rownames(embedding) == a),]
  b_vec <- embedding[which(rownames(embedding) == b),]
  c_vec <- embedding[which(rownames(embedding) == c),]
  
  # Generate analogy vector (vector(c) - vector(a) + vector(b))
  target <- c_vec - a_vec + b_vec
  
  # Calculate cosine similarity between anaology vector and all other vectors
  target_sim <- sim2(embedding, matrix(target, nrow = 1))
  
  # Report nearest neighbours of analogy vector
  sort(target_sim[,1], decreasing = T)[1:n]
  
}

analogies("deutschland", "frankreich", "berlin", 10, word_vectors)
analogies("deutschland", "usa", "merkel", 10, word_vectors)
analogies("merkel", "scholz", "cdu", 10, word_vectors)
analogies("studenten", "schüler", "universität", 10, word_vectors)
analogies("paris", "london", "frankreich", 10, word_vectors)

analogies("deutschland", "china", "europa", 10, word_vectors)
analogies("mann", "frau", "könig", 10, word_vectors)


#Visualising cosine similarity for the 40 most common words
# code from https://rlbarter.github.io/superheat-examples/word2vec/

#extract 40 most common features

forty_topfeat <- names(topfeatures(textAll_fcm, n=40))
head(forty_topfeat,40)

CosineFun <- function(x, y){
  # calculate the cosine similarity between two vectors: x and y
  c <- sum(x*y) / (sqrt(sum(x * x)) * sqrt(sum(y * y)))
  return(c)
}

CosineSim <- function(X) {
  # calculate the pairwise cosine similarity between columns of the matrix X.
  # initialize similarity matrix
  m <- matrix(NA, 
              nrow = ncol(X),
              ncol = ncol(X),
              dimnames = list(colnames(X), colnames(X)))
  cos <- as.data.frame(m)
  
  # calculate the pairwise cosine similarity
  for(i in 1:ncol(X)) {
    for(j in i:ncol(X)) {
      co_rate_1 <- X[which(X[, i] & X[, j]), i]
      co_rate_2 <- X[which(X[, i] & X[, j]), j]  
      cos[i, j] <- CosineFun(co_rate_1, co_rate_2)
      # fill in the opposite diagonal entry
      cos[j, i] <- cos[i, j]        
    }
  }
  return(cos)
}

# calculate the cosine similarity matrix  between the forty most common words
cosineSimilarity <- CosineSim(t(word_vectors[forty_topfeat, ]))

#Since the diagonal similarity values are all 1 
#(the similarity of a word with itself is 1), 
#and this can skew the color scale, we make a point of setting these values to NA.

diag(cosineSimilarity) <- NA

#plot superheat
superheat(cosineSimilarity, 
          
          # place dendrograms on columns and rows 
          row.dendrogram = T, 
          col.dendrogram = T,
          
          # make gridlines white for enhanced prettiness
          grid.hline.col = "white",
          grid.vline.col = "white",
          
          # rotate bottom label text
          bottom.label.text.angle = 90,
          
          #legend.breaks = c(-0.1, 0.1, 0.3, 0.5)
)



