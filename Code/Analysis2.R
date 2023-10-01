# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())


library("psych")
library("weights")
library("tidyverse")
library("lmtest")
library("rstatix")
library("openxlsx")
library("corrplot")
library("scales")
library("arm")
library("stringr")
library("jtools")
library("sandwich")
library("grid")
library("gridExtra")
library("dplyr")
library("qcc")
library(ggQC)
library(broom)
library(sjPlot)
library(ggstance)

######Load and prepare data for analysis ######

load("articlesRich.RData")

dat <- articlesRich


#load("articlesRichWithSigmoid.RData")
#dat.sigmoid <- articlesRich

rm(articlesRich)
dat$outlet <- factor(dat$outlet)
#dat.sigmoid$outlet <- factor(dat.sigmoid$outlet)

#Standardize Sentiments 
n <- colnames(dat[17:52])
dat.std <- dat %>% mutate_at(c(n, "date", "readabilityArticle"), ~(scale(.) %>% as.vector))


#rewrite topics
levels(dat$TopicArticle)

#Topic 1: Daily News about Covid-19 in Hamburg
#Topic 2: Report incidences
#Topic 3: Covid-19 in China and its origin
#Topic 4: How different countries combat the pandemic
#Topic 5: Development/ accreditation of vaccines
#Topic 6: Social Media, Fake News $ controversal debates about Covid-19
#Topic 7: Impacts of Covid-19
#Topic 8: Travel restrictions/ warnings
#Topic 9: Covid-19-warning-app & public sport events
#Topic 10: Political discussions about restrictions
#Topic 11: Information about the virus
#Topic 12: Impact of Covid-19 on stock markets
#Topic 13: Economic impacts/ job market situation
#Topic 14: Demonstrations/ stop and search
#Topic 15: Public aid
#Topic 16: Covid-19 in US

dat.std$Topic <- as.character(dat.std$TopicArticle)
dat.std$Topic[dat.std$Topic == "1"] <-"Daily News about Covid-19 in Hamburg"
dat.std$Topic[dat.std$Topic == "2"] <-"Report incidences"
dat.std$Topic[dat.std$Topic == "3"] <-"Covid-19 in China and its origin"
dat.std$Topic[dat.std$Topic == "4"] <-"How different countries combat the pandemic"
dat.std$Topic[dat.std$Topic == "5"] <-"Development/ accreditation of vaccines"
dat.std$Topic[dat.std$Topic == "6"] <-"Social Media, Fake News & controversal debates"
dat.std$Topic[dat.std$Topic == "7"] <-"Impacts of Covid-19"
dat.std$Topic[dat.std$Topic == "8"] <-"Travel restrictions/ warnings"
dat.std$Topic[dat.std$Topic == "9"] <-"Covid-19-warning-app & public sport events"
dat.std$Topic[dat.std$Topic == "10"] <-"Political discussions about restrictions"
dat.std$Topic[dat.std$Topic == "11"] <-"Information about the virus"
dat.std$Topic[dat.std$Topic == "12"] <-"Impact of Covid-19 on stock markets"
dat.std$Topic[dat.std$Topic == "13"] <-"Economic impacts/ job market situation"
dat.std$Topic[dat.std$Topic == "14"] <-"Demonstrations/ stop and search"
dat.std$Topic[dat.std$Topic == "15"] <-"Public aid"
dat.std$Topic[dat.std$Topic == "16"] <-"Covid-19 in US"

dat.std$Topic <- factor(dat.std$Topic, levels = c("Daily News about Covid-19 in Hamburg", 
                                                  "Report incidences",
                                                  "Covid-19 in China and its origin",
                                                  "How different countries combat the pandemic",
                                                  "Development/ accreditation of vaccines",
                                                  "Social Media, Fake News & controversal debates",
                                                  "Impacts of Covid-19",
                                                  "Travel restrictions/ warnings",
                                                  "Covid-19-warning-app & public sport events",
                                                  "Political discussions about restrictions",
                                                  "Information about the virus",
                                                  "Impact of Covid-19 on stock markets",
                                                  "Economic impacts/ job market situation",
                                                  "Demonstrations/ stop and search",
                                                  "Public aid",
                                                  "Covid-19 in US"))
levels(dat.std$Topic)
#rewrite the levels
levels(dat.std$Topic) <- str_wrap(levels(dat.std$Topic), width =10)


#the same for unstandardized 
dat$Topic <- as.character(dat$TopicArticle)
dat$Topic[dat$Topic == "1"] <-"Daily News about Covid-19 in Hamburg"
dat$Topic[dat$Topic == "2"] <-"Report incidences"
dat$Topic[dat$Topic == "3"] <-"Covid-19 in China and its origin"
dat$Topic[dat$Topic == "4"] <-"How different countries combat the pandemic"
dat$Topic[dat$Topic == "5"] <-"Development/ accreditation of vaccines"
dat$Topic[dat$Topic == "6"] <-"Social Media, Fake News & controversal debates"
dat$Topic[dat$Topic == "7"] <-"Impacts of Covid-19"
dat$Topic[dat$Topic == "8"] <-"Travel restrictions/ warnings"
dat$Topic[dat$Topic == "9"] <-"Covid-19-warning-app & public sport events"
dat$Topic[dat$Topic == "10"] <-"Political discussions about restrictions"
dat$Topic[dat$Topic == "11"] <-"Information about the virus"
dat$Topic[dat$Topic == "12"] <-"Impact of Covid-19 on stock markets"
dat$Topic[dat$Topic == "13"] <-"Economic impacts/ job market situation"
dat$Topic[dat$Topic == "14"] <-"Demonstrations/ stop and search"
dat$Topic[dat$Topic == "15"] <-"Public aid"
dat$Topic[dat$Topic == "16"] <-"Covid-19 in US"

dat$Topic <- factor(dat$Topic, levels = c("Daily News about Covid-19 in Hamburg", 
                                                  "Report incidences",
                                                  "Covid-19 in China and its origin",
                                                  "How different countries combat the pandemic",
                                                  "Development/ accreditation of vaccines",
                                                  "Social Media, Fake News & controversal debates",
                                                  "Impacts of Covid-19",
                                                  "Travel restrictions/ warnings",
                                                  "Covid-19-warning-app & public sport events",
                                                  "Political discussions about restrictions",
                                                  "Information about the virus",
                                                  "Impact of Covid-19 on stock markets",
                                                  "Economic impacts/ job market situation",
                                                  "Demonstrations/ stop and search",
                                                  "Public aid",
                                                  "Covid-19 in US"))
levels(dat$Topic)
#rewrite the levels
levels(dat$Topic) <- str_wrap(levels(dat$Topic), width =10)

#colors
c16 <- c("blue1", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold3", "dodgerblue1", "green1", "yellow", 
         "gray70", "maroon", "orchid1", "turquoise", "darkorange4", "deeppink2")
pie(rep(1, 16), col = c16)

c7 <- c("blue1", "#E31A1C", "turquoise", "gray70",  "green4", "yellow", "black")
pie(rep(1, 7), col = c7)

c7 <- c("darkorange4", "yellow2", "orchid1", "red2",  "green1", "#FF7F00", "dodgerblue1")
pie(rep(1, 7), col = c7)

#####################################################################################################
#SQ1: Explore descriptive statistics of the title, article and comment sentiments 
#(e.g. in general, which sentiment is most prevalent in the title?)
#####################################################################################################

#make table with obs mean quartile median quartile sd

#summary statistics
summary(dat[,c(17:23, 29:35, 41:47)])
describe(dat[,c(17:23, 29:35, 41:47)])

#sentiment title over time
sum <- dat %>% 
  group_by(date = floor_date(date, unit ="month")) %>%
  summarise(Anger = mean(titleAngerScore, na.rm =T),
            Fear = mean(titleFearScore, na.rm =T),
            Sadness = mean(titleSadnessScore, na.rm =T),
            Trust = mean(titleTrustScore, na.rm =T), 
            Disgust = mean(titleDisgustScore, na.rm =T),
            Negative = mean(titleNegativeScore, na.rm =T),
            Positive = mean(titlePositiveScore, na.rm =T))

sum_long <- pivot_longer(sum, -date, names_to = "Category", values_to= "mean")

ggplot(sum_long, aes(x=date, y= mean, color= Category ) )+
  geom_point()+
  geom_line()+
  labs(y="Mean Title Sentiment Score ", x ="Date")+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_color_manual(values = c7)


#sentiment aricle over time
sum <- dat %>% 
  group_by(date = floor_date(date, unit ="month")) %>%
  summarise(Anger = mean(articleAngerScore, na.rm =T),
            Fear = mean(articleFearScore, na.rm =T),
            Sadness = mean(articleSadnessScore, na.rm =T),
            Trust = mean(articleTrustScore, na.rm =T), 
            Disgust = mean(articleDisgustScore, na.rm =T),
            Negative = mean(articleNegativeScore, na.rm =T),
            Positive = mean(articlePositiveScore, na.rm =T))

sum_long <- pivot_longer(sum, -date, names_to = "Category", values_to= "mean")

ggplot(sum_long, aes(x=date, y= mean, color= Category ) )+
  geom_point()+
  geom_line()+
  labs(y="Mean Article Sentiment Score ", x ="Date")+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_color_manual(values = c7)

#sentiment comments over time
sum <- dat %>% 
  group_by(date = floor_date(date, unit ="month")) %>%
  summarise(Anger = mean(commentsAngerScore, na.rm =T),
            Fear = mean(commentsFearScore, na.rm =T),
            Sadness = mean(commentsSadnessScore, na.rm =T),
            Trust = mean(commentsTrustScore, na.rm =T), 
            Disgust = mean(commentsDisgustScore, na.rm =T),
            Negative = mean(commentsNegativeScore, na.rm =T),
            Positive = mean(commentsPositiveScore, na.rm =T))

sum_long <- pivot_longer(sum, -date, names_to = "Category", values_to= "mean")

ggplot(sum_long, aes(x=date, y= mean, color= Category ) )+
  geom_point()+
  geom_line()+
  labs(y="Mean Comments Sentiment Score ", x ="Date")+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  scale_color_manual(values = c7)



#title

#mean sentiment title scores general (not standardized)
x <- dat[,17:23] 
x_mean <- x %>%
  summarise(means_titleAnger = mean(titleAngerScore),
            means_titleFear = mean(titleFearScore),
            means_titleSadness = mean(titleSadnessScore),
            means_titleTrust = mean(titleTrustScore), 
            means_titleDisgust = mean(titleDisgustScore),
            means_titleNegative = mean(titleNegativeScore),
            means_titlePositive = mean(titlePositiveScore))
colnames(x_mean) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive") 
x_sd <- x %>%
  summarise(sd_titleAnger = sd(titleAngerScore),
            sd_titleFear = sd(titleFearScore),
            sd_titleSadness = sd(titleSadnessScore),
            sd_titleTrust = sd(titleTrustScore), 
            sd_titleDisgust = sd(titleDisgustScore),
            sd_titleNegative = sd(titleNegativeScore),
            sd_titlePositive = sd(titlePositiveScore))
colnames(x_sd) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")

#get it into longer format in order to plot it
xmean_long <- pivot_longer(x_mean, cols = everything(), names_to = "Sentiment", values_to= "mean")
xsd_long <- pivot_longer(x_sd, cols = everything(), names_to = "Sentiment", values_to= "sd")

x <- merge(xmean_long, xsd_long)

#options(scipen = 999)
x %>% 
  ggplot(aes(y = mean, x = Sentiment, color = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
  labs(y="Mean Title Sentiment Score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(legend.position="none") +
  #scale_color_manual(values=c7)
  geom_errorbar(aes(ymin=mean-(sd), 
                    ymax=mean+(sd)), width=.2, position=position_dodge(.9))
#?how to make description of this figure: The figure shows the mean title sentiment score for each sentiment and 
#the standard deviation from the mean 

#article

x <- dat[,29:35] 
x_mean <- x %>%
  summarise(means_articleAnger = mean(articleAngerScore),
            means_articleFear = mean(articleFearScore),
            means_articleSadness = mean(articleSadnessScore),
            means_articleTrust = mean(articleTrustScore), 
            means_articleDisgust = mean(articleDisgustScore),
            means_articleNegative = mean(articleNegativeScore),
            means_articlePositive = mean(articlePositiveScore))
colnames(x_mean) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive") 
x_sd <- x %>%
  summarise(sd_articleAnger = sd(articleAngerScore),
            sd_articleFear = sd(articleFearScore),
            sd_articleSadness = sd(articleSadnessScore),
            sd_articleTrust = sd(articleTrustScore), 
            sd_articleDisgust = sd(articleDisgustScore),
            sd_articleNegative = sd(articleNegativeScore),
            sd_articlePositive = sd(articlePositiveScore))
colnames(x_sd) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")


#get it into longer format in order to plot it
xmean_long <- pivot_longer(x_mean, cols = everything(), names_to = "Sentiment", values_to= "mean")
xsd_long <- pivot_longer(x_sd, cols = everything(), names_to = "Sentiment", values_to= "sd")

x <- merge(xmean_long, xsd_long)

#options(scipen = 999)
x %>% 
  ggplot(aes(y = mean, x = Sentiment, color = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
  labs(y="Mean Article Sentiment score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=mean-(sd), 
                    ymax=mean+(sd)), width=.2, position=position_dodge(.9))


#comments

x <- dat[,41:47] 
x_mean <- x %>%
  summarise(means_commentsAnger = mean(commentsAngerScore, na.rm =T),
            means_commentsFear = mean(commentsFearScore, na.rm =T),
            means_commentsSadness = mean(commentsSadnessScore, na.rm =T),
            means_commentsTrust = mean(commentsTrustScore, na.rm =T), 
            means_commentsDisgust = mean(commentsDisgustScore, na.rm =T),
            means_commentsNegative = mean(commentsNegativeScore, na.rm =T),
            means_commentsPositive = mean(commentsPositiveScore, na.rm =T))
colnames(x_mean) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive") 
x_sd <- x %>%
  summarise(sd_commentsAnger = sd(commentsAngerScore, na.rm =T),
            sd_commentsFear = sd(commentsFearScore, na.rm =T),
            sd_commentsSadness = sd(commentsSadnessScore, na.rm =T),
            sd_commentsTrust = sd(commentsTrustScore, na.rm =T), 
            sd_commentsDisgust = sd(commentsDisgustScore, na.rm =T),
            sd_commentsNegative = sd(commentsNegativeScore, na.rm =T),
            sd_commentsPositive = sd(commentsPositiveScore, na.rm =T))
colnames(x_sd) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")


#get it into longer format in order to plot it
xmean_long <- pivot_longer(x_mean, cols = everything(), names_to = "Sentiment", values_to= "mean")
xsd_long <- pivot_longer(x_sd, cols = everything(), names_to = "Sentiment", values_to= "sd")

x <- merge(xmean_long, xsd_long)

#options(scipen = 999)
x %>% 
  ggplot(aes(y = mean, x = Sentiment, color = Sentiment)) +
  geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
  labs(y="Mean Comments Sentiment score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=mean-(sd), 
                    ymax=mean+(sd)), width=.2, position=position_dodge(.9))

#####################################################################################################
#SQ2: 
#####################################################################################################

#functions to create table 
#put stars
#test <- data.frame(a = c(0.002, 2, 0.5),
#b = c(0.02, 1.2, 0.49),
#c = c(0.0002, 0, 0.1))


pstars <- function(dataframe){
  dataframe2 <- dataframe
  for(i in 1:nrow(dataframe)){
    for(y in 1:nrow(dataframe)){
      print(dataframe[i,y])
      print(i)
      print(y)
      if(dataframe[i,y] < 0.001){
        dataframe2[i,y] <- "***"
      }else if (dataframe[i,y] < 0.01){
        dataframe2[i,y] <- "**"
      }else if (dataframe[i,y] < 0.05){
        dataframe2[i,y] <- "*"
      }else{
        dataframe2[i,y] <- ""
      }
      print(dataframe2[i,y])
    }
  }
  print(dataframe2)
  #use only values above diagonal
  for(i in 1:nrow(dataframe2)){
    for(y in 1:nrow(dataframe2)){
      dataframe2[y,i] <- dataframe2[i,y]
    }
  }
  
  #delete values above and on diagonal
  for(row in 1:nrow(dataframe2)){
    for(col in 1:ncol(dataframe2)){
      # if column number is greater than row
      if(col >= row){
        dataframe2[row,col] <- ""
      }
    }
  }
  
  return(dataframe2)
}



putTogether <- function(r,p){
  for(i in 1:nrow(r)){
    for(y in 1:nrow(r)){
      r[i,y] <- paste0(r[i,y], p[i,y])
    }
  }
  return(r)
}

#delete element above diagonal
delete <- function(mat){
  for(row in 1:nrow(mat)){
    for(col in 1:ncol(mat)){
      # if column number is greater than row
      if(col > row){
        mat[row,col] <- "-"
      }
    }
  }
  return(mat)
}



#title
x <- dat[,17:23]


correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)

colnames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")

rownames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")
#round values
correlation.r <- rd(correlation.r, digits =2)

correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)

write.csv(correlation.r, file ="Outputs/SQ2/titleCorrelations.csv")


#article
x <- dat[,29:35]


correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)

colnames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")

rownames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")
#round values
correlation.r <- rd(correlation.r, digits =2)
correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)

write.csv(correlation.r, file ="Outputs/SQ2/articleCorrelations.csv")


#comments
x <- dat[,41:47]

correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)

colnames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")

rownames(correlation.r) <- c("Anger", "Fear","Sadness", "Trust", "Disgust", "Negative", "Positive")
#round values
correlation.r <- rd(correlation.r, digits =2)

correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)

write.csv(correlation.r, file ="Outputs/SQ2/commentsCorrelations.csv")




#####################################################################################################
#SQ3: 
#####################################################################################################

#title and article

x <- dat[,c(17:23, 29:35)]

correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)


#round values
correlation.r <- rd(correlation.r, digits =2)

correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)


#remove columns and rows 
correlation.r <- as.data.frame(correlation.r)
correlation.r <- correlation.r[, 1:7] 
correlation.r <- correlation.r[8:14, ]

colnames(correlation.r) <- c("titleAnger", "titleFear","titleSadness", "titleTrust", "titleDisgust", "titleNegative", "titlePositive")

rownames(correlation.r) <- c("articleAnger", "articleFear","articleSadness", "articleTrust", "articleDisgust", "articleNegative", "articlePositive")


write.csv(correlation.r, file ="Outputs/SQ3/title_articleCorrelations.csv")


#title and comment
x <- dat[,c(17:23, 41:47)]

correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)


#round values
correlation.r <- rd(correlation.r, digits =2)

correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)


#remove columns and rows 
correlation.r <- as.data.frame(correlation.r)
correlation.r <- correlation.r[, 1:7] 
correlation.r <- correlation.r[8:14, ]

colnames(correlation.r) <- c("titleAnger", "titleFear","titleSadness", "titleTrust", "titleDisgust", "titleNegative", "titlePositive")

rownames(correlation.r) <- c("commentsAnger", "commentsFear","commentsSadness", "commentsTrust", "commentsDisgust", "commentsNegative", "commentsPositive")


write.csv(correlation.r, file ="Outputs/SQ3/title_commentsCorrelations.csv")


#article and comment 

x <- dat[,c(29:35, 41:47)]

correlation <- corr.test(x, use = "pairwise.complete.obs")
correlation.r <- correlation$r
correlation.p <- correlation$p #which should I use (above or below diagonal?)


#round values
correlation.r <- rd(correlation.r, digits =2)

correlation.p <- pstars(correlation.p)

correlation.r <- putTogether(correlation.r, correlation.p)
correlation.r <- delete(correlation.r)


#remove columns and rows 
correlation.r <- as.data.frame(correlation.r)
correlation.r <- correlation.r[, 1:7] 
correlation.r <- correlation.r[8:14, ]

colnames(correlation.r) <- c("articleAnger", "articleFear","articleSadness", "articleTrust", "articleDisgust", "articleNegative", "articlePositive")

rownames(correlation.r) <- c("commentsAnger", "commentsFear","commentsSadness", "commentsTrust", "commentsDisgust", "commentsNegative", "commentsPositive")


write.csv(correlation.r, file ="Outputs/SQ3/article_commentsCorrelations.csv")



#####################################################################################################
#SQ4: Guided by previous research, some topics are more controversal than others. 
#Therefore we should investigate if we can see differences in the title/ article and comments 
#sentiments between our 16 topics about Covid-19 
#(which impact does the topic have on the sentiments of the title/ article/ comment?)
#####################################################################################################

#?How should I describe that we now used standardized values?

#Number of Articles per topic (put this as descriptive statistics in methodology part)

dat.std %>% 
  plot_frq(Topic)

#political discussions about restrictions (topic 10), impact of covid 19 on stock markets (topic 12) 
#and public aid (topic 15) acount for about one third of all articles
#wherease social media &fake news and controversal debates (topic 6) and daily news about covid 19 in hamburg
#acount for not at least 5 per cent of all articles


plotPareto <- function(dataframe, xLabel, yLabel){
  dat <- dataframe 
  #dat <- articles_perTopic
  names(dat) <- c("group", "count")
  
  #sort it 
  dat<- dat %>%
    arrange(desc(count))
  
  #cumulative
  dat<- dat %>%
    mutate(cumfreq = cumsum(count), cumperc = percent(cumfreq/sum(dat$count), accuracy =0.01))
  
  #order
  dat_alpha <- dat[order(dat$group),]
  cum_freq <- dat$cumfreq
  cum_perc <- dat$cumperc
  
  #plot
  ggplot(dat_alpha, aes(x= group, y= count))+
    stat_pareto(point.color ="red", point.size =3, line.color = "black",
                bars.fill = c("blue", "orange"))+
    #scale_y_continuous()+
    #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+ 
    geom_label(aes(label=cum_perc, y=cum_freq, vjust=1.3))+
    labs(x = xLabel, 
       y = yLabel) +
    theme(plot.caption = element_text(color="grey", size=5),#hjust=0 face ="italic"
          plot.subtitle = element_text(color ="darkgrey", size = 10),
          plot.title = element_text(face = "bold", size = 16),
          legend.position = "bottom")+
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) 
  
}

articles_perTopic <- dat.std %>% 
  group_by(Topic) %>%
  summarise(count= n())

plotPareto(articles_perTopic, "Topic", "Number Of Articles")



#pareto chart 
articles_perTopic <- dat.std %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- articles_perTopic$count
names(counts) <- articles_perTopic$Topic
pareto.chart(counts, #main = "Pareto Chart for Number of Articles per Topic outlet Handelsblatt", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))


#articles topics over time
sum <- dat %>% 
  group_by(month_year = floor_date(date, unit ="month"), TopicArticle) %>%
  summarise(count = n())

ggplot(sum, aes(x=month_year, y= count) )+
  #geom_point()+
  geom_line(aes(color=TopicArticle))+
  labs(y="Number Articles", x ="Date")+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  scale_color_manual(values=c16)

#comments topics over time
#remove outliers
dat.o <- dat[-c(7946,10452),]
sum <- dat.o %>% 
  group_by(month_year = floor_date(date, unit ="month"), TopicArticle) %>%
  summarise(mean = mean(numberComments))

ggplot(sum, aes(x=month_year, y= mean, color= TopicArticle ) )+
  #geom_point()+
  geom_line()+
  labs(y="Mean Number of Comments ", x ="Date")+
  scale_x_date(breaks= "1 month", date_labels =  "%b%Y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) +
  scale_color_manual(values=c16)


### Differences Sentiments between topics Title

#sentiment: string of sentiment
plotDescriptiveStatisticsTitle <- function(sentiment){
  dat.small <- dat.std[, c("Topic", paste0(paste0("title", sentiment),"Score"))]
  names(dat.small) <- c("TopicArticle", "TitleScore")
  x <- group_by(dat.small, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(TitleScore, na.rm = TRUE),
      sd = sd(TitleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, color = TopicArticle)) +
    geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
    labs(y= paste("Mean Title",paste(sentiment, "Score")), x="Topic")+
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  #return(x)
}

plotDescriptiveStatisticsTitle("Anger")
plotDescriptiveStatisticsTitle("Fear")
#fear is not used much on the most prevalent topics, although we have a high title fear score on general
plotDescriptiveStatisticsTitle("Sadness")
plotDescriptiveStatisticsTitle("Trust")
#trust is in general highest among the titles but the lowest on the most prevalent topic
plotDescriptiveStatisticsTitle("Disgust")
plotDescriptiveStatisticsTitle("Negative")
#lowest negative score in general is not driven by the most prevalent topics
plotDescriptiveStatisticsTitle("Positive")



#heatmap
scores_means_by_topic <- dat.std %>%
  group_by(Topic) %>%
  summarise(Anger = mean(titleAngerScore),
            Fear = mean(titleFearScore),
            Sadness = mean(titleSadnessScore),
            Trust = mean(titleTrustScore), 
            Disgust = mean(titleDisgustScore),
            Negative = mean(titleNegativeScore),
            Positive = mean(titlePositiveScore))                            

#get it into longer format in order to plot it
scores_means_by_topic_long <- pivot_longer(scores_means_by_topic, -Topic, 
                                           names_to = "Sentiment", values_to= "mean")
ggplot(data = scores_means_by_topic_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() +
  scale_fill_gradientn(colours = rainbow(7))
  #scale_fill_gradient(low="white", high="purple")


#table 
table <- dat.std %>%
            group_by(Topic) %>%
            summarise(
              count =n(),
              Anger = mean(titleAngerScore),
              Fear = mean(titleFearScore),
              Sadness = mean(titleSadnessScore),
              Trust = mean(titleTrustScore), 
              Disgust = mean(titleDisgustScore),
              Negative = mean(titleNegativeScore),
              Positive = mean(titlePositiveScore))     
table <- table %>% mutate(across(where(is.numeric), round, digits=3))

sd <- dat.std %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(titleAngerScore),
    Fear = sd(titleFearScore),
    Sadness = sd(titleSadnessScore),
    Trust = sd(titleTrustScore), 
    Disgust = sd(titleDisgustScore),
    Negative = sd(titleNegativeScore),
    Positive = sd(titlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
table <- table %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
    for(y in 1:16){
      table[y,i] <- paste0(table[y,i], " (")
      table[y,i] <- paste0(table[y,i], sd[y,i])
      table[y,i] <- paste0(table[y,i], ")")
    }
  }


write.csv(table, file ="Outputs/SQ4/tableTitle.csv")

### Article Descriptive statistics

#sentiment: string of sentiment
plotDescriptiveStatisticsArticle <- function(sentiment){
  dat.small <- dat.std[, c("Topic", paste0(paste0("article", sentiment),"Score"))]
  names(dat.small) <- c("TopicArticle", "ArticleScore")
  x <- group_by(dat.small, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(ArticleScore, na.rm = TRUE),
      sd = sd(ArticleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, color = TopicArticle)) +
    geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
    labs(y= paste("Mean Article",paste(sentiment, "Score")), x="Topic") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}

plotDescriptiveStatisticsArticle("Anger")
plotDescriptiveStatisticsArticle("Fear")
plotDescriptiveStatisticsArticle("Sadness")
plotDescriptiveStatisticsArticle("Trust")
plotDescriptiveStatisticsArticle("Disgust")
plotDescriptiveStatisticsArticle("Negative")
plotDescriptiveStatisticsArticle("Positive")



#heatmap
scores_means_by_article <- dat.std %>%
  group_by(Topic) %>%
  summarise(Anger = mean(articleAngerScore),
            Fear = mean(articleFearScore),
            Sadness = mean(articleSadnessScore),
            Trust = mean(articleTrustScore), 
            Disgust = mean(articleDisgustScore),
            Negative = mean(articleNegativeScore),
            Positive = mean(articlePositiveScore))
#get it into longer format in order to plot it
scores_means_by_article_long <- pivot_longer(scores_means_by_article, -Topic, 
                                             names_to = "Sentiment", values_to= "mean")
ggplot(data = scores_means_by_article_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() + 
  #scale_fill_gradient(low="green", high="red")
  scale_fill_gradientn(colours = rainbow(7))

#table 
table <- dat.std %>%
            group_by(Topic) %>%
            summarise(
              count =n(),
              Anger = mean(articleAngerScore),
              Fear = mean(articleFearScore),
              Sadness = mean(articleSadnessScore),
              Trust = mean(articleTrustScore), 
              Disgust = mean(articleDisgustScore),
              Negative = mean(articleNegativeScore),
              Positive = mean(articlePositiveScore))     
table <- table %>% mutate(across(where(is.numeric), round, digits=3))

sd <- dat.std %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(articleAngerScore),
    Fear = sd(articleFearScore),
    Sadness = sd(articleSadnessScore),
    Trust = sd(articleTrustScore), 
    Disgust = sd(articleDisgustScore),
    Negative = sd(articleNegativeScore),
    Positive = sd(articlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
table <- table %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
    for(y in 1:16){
      table[y,i] <- paste0(table[y,i], " (")
      table[y,i] <- paste0(table[y,i], sd[y,i])
      table[y,i] <- paste0(table[y,i], ")")
    }
  }


write.csv(table, file ="Outputs/SQ4/tableArticle.csv")



### Comment Descriptive statistics

#sentiment: string of sentiment
plotDescriptiveStatisticsComments <- function(sentiment){
  dat.small <- dat.std[, c("Topic", paste0(paste0("comments", sentiment),"Score"))]
  names(dat.small) <- c("TopicArticle", "CommentsScore")
  x <- group_by(dat.small, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(CommentsScore, na.rm = TRUE),
      sd = sd(CommentsScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, color = TopicArticle)) +
    geom_bar(stat = "identity", position = "dodge", width = .5, fill = "white") +
    labs(y= paste("Mean Comments",paste(sentiment, "Score")), x="Topic") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none") +
    geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}

plotDescriptiveStatisticsComments("Anger")
plotDescriptiveStatisticsComments("Fear")
plotDescriptiveStatisticsComments("Sadness")
plotDescriptiveStatisticsComments("Trust")
plotDescriptiveStatisticsComments("Disgust")
plotDescriptiveStatisticsComments("Negative")
plotDescriptiveStatisticsComments("Positive")


#heatmap
scores_means_by_comments <- dat.std %>%
  group_by(Topic) %>%
  summarise(Anger = mean(commentsAngerScore, na.rm =T),
            Fear = mean(commentsFearScore, na.rm =T),
            Sadness = mean(commentsSadnessScore, na.rm =T),
            Trust = mean(commentsTrustScore, na.rm =T), 
            Disgust = mean(commentsDisgustScore, na.rm =T),
            Negative = mean(commentsNegativeScore, na.rm =T),
            Positive = mean(commentsPositiveScore, na.rm =T))
#get it into longer format in order to plot it
scores_means_by_comments_long <- pivot_longer(scores_means_by_comments, -Topic, 
                                              names_to = "Sentiment", values_to= "mean")
ggplot(data = scores_means_by_comments_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() + 
  #scale_fill_gradient(low="green", high="red")
  scale_fill_gradientn(colours = rainbow(7))

#table 
#table 
table <- dat.std %>%
  group_by(Topic) %>%
  summarise(
    count =n() -sum(is.na(commentsAngerScore)),
    Anger = mean(commentsAngerScore, na.rm =T),
    Fear = mean(commentsFearScore, na.rm =T),
    Sadness = mean(commentsSadnessScore, na.rm =T),
    Trust = mean(commentsTrustScore, na.rm =T), 
    Disgust = mean(commentsDisgustScore, na.rm =T),
    Negative = mean(commentsNegativeScore, na.rm =T),
    Positive = mean(commentsPositiveScore, na.rm =T))     
table <- table %>% mutate(across(where(is.numeric), round, digits=3))

sd <- dat.std %>%
  group_by(Topic) %>%
  summarise(
    count = n() -sum(is.na(commentsAngerScore)),
    Anger = sd(commentsAngerScore, na.rm =T),
    Fear = sd(commentsFearScore, na.rm =T),
    Sadness = sd(commentsSadnessScore, na.rm =T),
    Trust = sd(commentsTrustScore, na.rm =T), 
    Disgust = sd(commentsDisgustScore, na.rm =T),
    Negative = sd(commentsNegativeScore, na.rm =T),
    Positive = sd(commentsPositiveScore, na.rm =T))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
table <- table %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    table[y,i] <- paste0(table[y,i], " (")
    table[y,i] <- paste0(table[y,i], sd[y,i])
    table[y,i] <- paste0(table[y,i], ")")
  }
}

#is it ok to show here only the count with no NA?
write.csv(table, file ="Outputs/SQ4/tableComments.csv")



#####################################################################################################
#SQ5: 
#####################################################################################################
dat.std %>% 
  plot_frq(outlet)


t <- xtabs( ~ outlet + TopicArticle, data = dat)
t
prop.table(t, 1)*100 
#1 means anteilig an outlets (2 wäre anteilig der topics) 
#,i.e. wie viel prozent der HB Artikel gehen zurück auf Topic 1

#Is there a significant difference in the topics per outlet?

#gibt es signifikante Unterschiede?
#chi quadrat test: überprüft die Verteilung für kategoriale Variablen
#d.h. ob sich die Topics zwischen den Outlets unterscheidet
chisq.test(dat$outlet,dat$TopicArticle) 
#aufgrund der Stichprobengröße hat man einen signifikanten Test
#d.h. die Verteilung der Topics ist zwischen den Outlets unterschiedlich

#quantifizieren mit Effektstärke
#da bei Stichprobengröße eh signifikanter Test
#man hat einen starken Effekt in den Unterschieden der Topics zwischen Outlets
sqrt(7974.4/(30*nrow(dat)))*sqrt(30) # Cohens Omega


#which topics are covered by which outlet?
dat.std %>% 
  ggplot(aes(x = Topic, fill = outlet)) +
  geom_histogram(position = "dodge", stat = "count") +
  labs(x="Topic", y="Frequency", fill="Outlet") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))

dat.std %>% 
  count(TopicArticle, outlet) %>% 
  group_by(outlet) %>% 
  mutate(pct = n / sum(n)) %>%   
  ggplot(aes(x = TopicArticle, y = pct, fill = outlet)) + 
  geom_col(position = "dodge", width = .7) +
  labs(x="Topic", y="Percentage of outlet", fill="Outlet") +
  scale_y_continuous(labels = percent_format()) +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))


#pareto chart for outlet HB
hb <- filter(dat.std, outlet =="HB")

hb_perTopic <- hb %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- hb_perTopic$count
names(counts) <- hb_perTopic$Topic
pareto.chart(counts, #main = "Pareto Chart for Number of Articles per Topic outlet Handelsblatt", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))

plotPareto(hb_perTopic, "Topic", "Number Of Articles")


#pareto chart for outlet Zeit
zeit <- filter(dat.std, outlet =="Zeit")

zeit_perTopic <- zeit %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- zeit_perTopic$count
names(counts) <- zeit_perTopic$Topic
pareto.chart(counts, #main = "Pareto Chart for Number of Articles per Topic outlet Zeit", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))

plotPareto(zeit_perTopic, "Topic", "Number Of Articles")


#pareto chart for outlet Welt
welt <- filter(dat.std, outlet =="Welt")

welt_perTopic <- welt %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- welt_perTopic$count
names(counts) <- welt_perTopic$Topic
pareto.chart(counts, #main = "Pareto Chart for Number of Articles per Topic outlet Zeit", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))

plotPareto(welt_perTopic, "Topic", "Number Of Articles")



### Differences sentiments between outlets

#Title


getTitleSentimentPerOutlet <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("title", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "TitleScore")
  x <- group_by(dat.small, outlet) %>%
    summarise(
      count = n(),
      mean = mean(TitleScore, na.rm = TRUE),
      sd = sd(TitleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = outlet, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Title",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none")+
    geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}

#show it with confidence intervalls?
getTitleSentimentPerOutlet("Anger")
getTitleSentimentPerOutlet("Fear")
getTitleSentimentPerOutlet("Sadness")
getTitleSentimentPerOutlet("Trust")
getTitleSentimentPerOutlet("Disgust")
getTitleSentimentPerOutlet("Negative")
getTitleSentimentPerOutlet("Positive")

#Sentiments per outlet (not grouped per topic)
x <- dat.std %>%
  group_by(outlet) %>%
  summarise(Anger = mean(titleAngerScore),
            Fear = mean(titleFearScore),
            Sadness = mean(titleSadnessScore),
            Trust = mean(titleTrustScore), 
            Disgust = mean(titleDisgustScore),
            Negative = mean(titleNegativeScore),
            Positive = mean(titlePositiveScore))
x_long <- pivot_longer(x, -outlet, names_to = "Sentiment", values_to= "mean")

as.data.frame(x_long) %>% 
  ggplot(aes(y = mean, x = Sentiment, fill = outlet)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  labs(y="Mean Title Sentiment Score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))


getTitleSentimentPerOutletTopic <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("title", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "TitleScore")
  x <- group_by(dat.small, outlet, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(TitleScore, na.rm = TRUE),
      sd = sd(TitleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Title",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) #+
    #geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}


getTitleSentimentPerOutletTopic("Anger")
getTitleSentimentPerOutletTopic("Fear")
getTitleSentimentPerOutletTopic("Sadness")
getTitleSentimentPerOutletTopic("Trust")
getTitleSentimentPerOutletTopic("Disgust")
getTitleSentimentPerOutletTopic("Negative")
getTitleSentimentPerOutletTopic("Positive")


#heatmap 
tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("HB",16)

tableHB$outlet <- outlet


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Zeit",16)

tableZeit$outlet <- outlet

tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Welt",16)

tableWelt$outlet <- outlet


table <- rbind(tableHB, tableZeit)
table <- rbind(table, tableWelt)

#heatmap
table_long <- pivot_longer(table, -c(Topic, outlet), 
                           names_to = "Sentiment", values_to= "mean")
ggplot(data = table_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() + 
  scale_fill_gradientn(colours = rainbow(7))+
  facet_wrap(~outlet, nrow=3)

#get it into a table 

tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))


sd <- hb %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(titleAngerScore),
    Fear = sd(titleFearScore),
    Sadness = sd(titleSadnessScore),
    Trust = sd(titleTrustScore), 
    Disgust = sd(titleDisgustScore),
    Negative = sd(titleNegativeScore),
    Positive = sd(titlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableHB <- tableHB %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableHB[y,i] <- paste0(tableHB[y,i], " (")
    tableHB[y,i] <- paste0(tableHB[y,i], sd[y,i])
    tableHB[y,i] <- paste0(tableHB[y,i], ")")
  }
}


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), round, digits=3))


sd <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(titleAngerScore),
    Fear = sd(titleFearScore),
    Sadness = sd(titleSadnessScore),
    Trust = sd(titleTrustScore), 
    Disgust = sd(titleDisgustScore),
    Negative = sd(titleNegativeScore),
    Positive = sd(titlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableZeit[y,i] <- paste0(tableZeit[y,i], " (")
    tableZeit[y,i] <- paste0(tableZeit[y,i], sd[y,i])
    tableZeit[y,i] <- paste0(tableZeit[y,i], ")")
  }
}



tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(titleAngerScore),
    Fear = mean(titleFearScore),
    Sadness = mean(titleSadnessScore),
    Trust = mean(titleTrustScore), 
    Disgust = mean(titleDisgustScore),
    Negative = mean(titleNegativeScore),
    Positive = mean(titlePositiveScore))     
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), round, digits=3))


sd <- welt %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(titleAngerScore),
    Fear = sd(titleFearScore),
    Sadness = sd(titleSadnessScore),
    Trust = sd(titleTrustScore), 
    Disgust = sd(titleDisgustScore),
    Negative = sd(titleNegativeScore),
    Positive = sd(titlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableWelt[y,i] <- paste0(tableWelt[y,i], " (")
    tableWelt[y,i] <- paste0(tableWelt[y,i], sd[y,i])
    tableWelt[y,i] <- paste0(tableWelt[y,i], ")")
  }
}


table <- rbind(tableHB, tableZeit)
table <- rbind(table, tableWelt)

write.csv(table, file ="Outputs/SQ5/SQ5_table.csv")


#Article

getArticleSentimentPerOutlet <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("article", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "ArticleScore")
  x <- group_by(dat.small, outlet) %>%
    summarise(
      count = n(),
      mean = mean(ArticleScore, na.rm = TRUE),
      sd = sd(ArticleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = outlet, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Article",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none")#+
  #geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}


getArticleSentimentPerOutlet("Anger")
getArticleSentimentPerOutlet("Fear")
getArticleSentimentPerOutlet("Sadness")
getArticleSentimentPerOutlet("Trust")
getArticleSentimentPerOutlet("Disgust")
getArticleSentimentPerOutlet("Negative")
getArticleSentimentPerOutlet("Positive")


#Sentiments per outlet (not grouped per topic)
x <- dat.std %>%
  group_by(outlet) %>%
  summarise(Anger = mean(articleAngerScore),
            Fear = mean(articleFearScore),
            Sadness = mean(articleSadnessScore),
            Trust = mean(articleTrustScore), 
            Disgust = mean(articleDisgustScore),
            Negative = mean(articleNegativeScore),
            Positive = mean(articlePositiveScore))
x_long <- pivot_longer(x, -outlet, names_to = "Sentiment", values_to= "mean")

as.data.frame(x_long) %>% 
  ggplot(aes(y = mean, x = Sentiment, fill = outlet)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  labs(y="Mean Article Sentiment Score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))



getArticleSentimentPerOutletTopic <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("article", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "ArticleScore")
  x <- group_by(dat.small, outlet, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(ArticleScore, na.rm = TRUE),
      sd = sd(ArticleScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Article",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) 
  #geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}


getArticleSentimentPerOutletTopic("Anger")
getArticleSentimentPerOutletTopic("Fear")
getArticleSentimentPerOutletTopic("Sadness")
getArticleSentimentPerOutletTopic("Trust")
getArticleSentimentPerOutletTopic("Disgust")
getArticleSentimentPerOutletTopic("Negative")
getArticleSentimentPerOutletTopic("Positive")

#heatmap 
tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("HB",16)

tableHB$outlet <- outlet


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Zeit",16)

tableZeit$outlet <- outlet

tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Welt",16)

tableWelt$outlet <- outlet


table <- rbind(tableHB, tableZeit)
table <- rbind(table, tableWelt)

#heatmap
table_long <- pivot_longer(table, -c(Topic, outlet), 
                           names_to = "Sentiment", values_to= "mean")
ggplot(data = table_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() + 
  scale_fill_gradientn(colours = rainbow(7))+
  facet_wrap(~outlet, nrow=3)

#get it into a table 

tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))


sd <- hb %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(articleAngerScore),
    Fear = sd(articleFearScore),
    Sadness = sd(articleSadnessScore),
    Trust = sd(articleTrustScore), 
    Disgust = sd(articleDisgustScore),
    Negative = sd(articleNegativeScore),
    Positive = sd(articlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableHB <- tableHB %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableHB[y,i] <- paste0(tableHB[y,i], " (")
    tableHB[y,i] <- paste0(tableHB[y,i], sd[y,i])
    tableHB[y,i] <- paste0(tableHB[y,i], ")")
  }
}


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), round, digits=3))


sd <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(articleAngerScore),
    Fear = sd(articleFearScore),
    Sadness = sd(articleSadnessScore),
    Trust = sd(articleTrustScore), 
    Disgust = sd(articleDisgustScore),
    Negative = sd(articleNegativeScore),
    Positive = sd(articlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableZeit[y,i] <- paste0(tableZeit[y,i], " (")
    tableZeit[y,i] <- paste0(tableZeit[y,i], sd[y,i])
    tableZeit[y,i] <- paste0(tableZeit[y,i], ")")
  }
}



tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(articleAngerScore),
    Fear = mean(articleFearScore),
    Sadness = mean(articleSadnessScore),
    Trust = mean(articleTrustScore), 
    Disgust = mean(articleDisgustScore),
    Negative = mean(articleNegativeScore),
    Positive = mean(articlePositiveScore))     
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), round, digits=3))


sd <- welt %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(articleAngerScore),
    Fear = sd(articleFearScore),
    Sadness = sd(articleSadnessScore),
    Trust = sd(articleTrustScore), 
    Disgust = sd(articleDisgustScore),
    Negative = sd(articleNegativeScore),
    Positive = sd(articlePositiveScore))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableWelt[y,i] <- paste0(tableWelt[y,i], " (")
    tableWelt[y,i] <- paste0(tableWelt[y,i], sd[y,i])
    tableWelt[y,i] <- paste0(tableWelt[y,i], ")")
  }
}


table <- rbind(tableHB, tableWelt)
table <- rbind(table, tableZeit)

write.csv(table, file ="Outputs/SQ5/SQ5_tableArticle.csv")




#Comments

getCommentsSentimentPerOutlet <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("comments", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "CommentsScore")
  x <- group_by(dat.small, outlet) %>%
    summarise(
      count = n(),
      mean = mean(CommentsScore, na.rm = TRUE),
      sd = sd(CommentsScore, na.rm = TRUE))
  as.data.frame(x)
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = outlet, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Comments",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) +
    theme(legend.position="none")#+
  #geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}


getCommentsSentimentPerOutlet("Anger")
getCommentsSentimentPerOutlet("Fear")
getCommentsSentimentPerOutlet("Sadness")
getCommentsSentimentPerOutlet("Trust")
getCommentsSentimentPerOutlet("Disgust")
getCommentsSentimentPerOutlet("Negative")
getCommentsSentimentPerOutlet("Positive")


#Sentiments per outlet (not grouped per topic)
x <- dat.std %>%
  group_by(outlet) %>%
  summarise(Anger = mean(commentsAngerScore, na.rm = TRUE),
            Fear = mean(commentsFearScore, na.rm = TRUE),
            Sadness = mean(commentsSadnessScore, na.rm = TRUE),
            Trust = mean(commentsTrustScore, na.rm = TRUE), 
            Disgust = mean(commentsDisgustScore, na.rm = TRUE),
            Negative = mean(commentsNegativeScore, na.rm = TRUE),
            Positive = mean(commentsPositiveScore, na.rm = TRUE))
x_long <- pivot_longer(x, -outlet, names_to = "Sentiment", values_to= "mean")

as.data.frame(x_long) %>% 
  ggplot(aes(y = mean, x = Sentiment, fill = outlet)) +
  geom_bar(stat = "identity", position = "dodge", width = .5) +
  labs(y="Mean Comments Sentiment Score", x="Sentiment") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))



getCommentsSentimentPerOutletTopic <- function(sentiment){
  dat.small <- dat.std[, c("outlet", "Topic", paste0(paste0("comments", sentiment),"Score"))]
  names(dat.small) <- c("outlet", "TopicArticle", "CommentsScore")
  x <- group_by(dat.small, outlet, TopicArticle) %>%
    summarise(
      count = n(),
      mean = mean(CommentsScore, na.rm = TRUE),
      sd = sd(CommentsScore, na.rm = TRUE))
  as.data.frame(x)
  x <- x[c(2:48),]
  
  as.data.frame(x) %>% 
    ggplot(aes(y = mean, x = TopicArticle, fill = outlet)) +
    geom_bar(stat = "identity", position = "dodge", width = .5) +
    labs(y=paste("Mean Comments",paste(sentiment, "Score")), x="Outlet") +
    theme(panel.background = element_blank(), 
          panel.grid.major.y = element_line(color = "grey"),
          panel.grid.minor.y = element_line(color = "grey")) 
  #geom_errorbar(aes(ymin=mean-1.96*(sd/sqrt(count)), ymax=mean+1.96*(sd/sqrt(count))), width=.2, position=position_dodge(.9))
  
}


getCommentsSentimentPerOutletTopic("Anger")
getCommentsSentimentPerOutletTopic("Fear")
getCommentsSentimentPerOutletTopic("Sadness")
getCommentsSentimentPerOutletTopic("Trust")
getCommentsSentimentPerOutletTopic("Disgust")
getCommentsSentimentPerOutletTopic("Negative")
getCommentsSentimentPerOutletTopic("Positive")


#heatmap 
tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("HB",16)

tableHB$outlet <- outlet


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Zeit",16)

tableZeit$outlet <- outlet

tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
#tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))

outlet <- rep("Welt",16)

tableWelt$outlet <- outlet


table <- rbind(tableHB, tableZeit)
table <- rbind(table, tableWelt)
table <- table[c(2:48),]

#heatmap
table_long <- pivot_longer(table, -c(Topic, outlet), 
                           names_to = "Sentiment", values_to= "mean")
ggplot(data = table_long,
       aes(x=Sentiment, y= Topic, fill = mean))+
  geom_tile()+ #heatmap
  coord_flip() + 
  scale_fill_gradientn(colours = rainbow(7))+
  facet_wrap(~outlet, nrow=3)

#get it into a table 

tableHB <- hb %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
tableHB <- tableHB %>% mutate(across(where(is.numeric), round, digits=3))
#?????????????????????????????????????????
#warum hier nur NA Werte für sd im Topic 1?????????????????????????????????????????
sd <- hb %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(commentsAngerScore, na.rm = TRUE),
    Fear = sd(commentsFearScore, na.rm = TRUE),
    Sadness = sd(commentsSadnessScore, na.rm = TRUE),
    Trust = sd(commentsTrustScore, na.rm = TRUE), 
    Disgust = sd(commentsDisgustScore, na.rm = TRUE),
    Negative = sd(commentsNegativeScore, na.rm = TRUE),
    Positive = sd(commentsPositiveScore, na.rm = TRUE))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableHB <- tableHB %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableHB[y,i] <- paste0(tableHB[y,i], " (")
    tableHB[y,i] <- paste0(tableHB[y,i], sd[y,i])
    tableHB[y,i] <- paste0(tableHB[y,i], ")")
  }
}


tableZeit <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), round, digits=3))


sd <- zeit %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(commentsAngerScore, na.rm = TRUE),
    Fear = sd(commentsFearScore, na.rm = TRUE),
    Sadness = sd(commentsSadnessScore, na.rm = TRUE),
    Trust = sd(commentsTrustScore, na.rm = TRUE), 
    Disgust = sd(commentsDisgustScore, na.rm = TRUE),
    Negative = sd(commentsNegativeScore, na.rm = TRUE),
    Positive = sd(commentsPositiveScore, na.rm = TRUE))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableZeit <- tableZeit %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableZeit[y,i] <- paste0(tableZeit[y,i], " (")
    tableZeit[y,i] <- paste0(tableZeit[y,i], sd[y,i])
    tableZeit[y,i] <- paste0(tableZeit[y,i], ")")
  }
}



tableWelt <- welt %>%
  group_by(Topic) %>%
  summarise(
    count =n(),
    Anger = mean(commentsAngerScore, na.rm = TRUE),
    Fear = mean(commentsFearScore, na.rm = TRUE),
    Sadness = mean(commentsSadnessScore, na.rm = TRUE),
    Trust = mean(commentsTrustScore, na.rm = TRUE), 
    Disgust = mean(commentsDisgustScore, na.rm = TRUE),
    Negative = mean(commentsNegativeScore, na.rm = TRUE),
    Positive = mean(commentsPositiveScore, na.rm = TRUE))     
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), round, digits=3))


sd <- welt %>%
  group_by(Topic) %>%
  summarise(
    count = n(),
    Anger = sd(commentsAngerScore, na.rm = TRUE),
    Fear = sd(commentsFearScore, na.rm = TRUE),
    Sadness = sd(commentsSadnessScore, na.rm = TRUE),
    Trust = sd(commentsTrustScore, na.rm = TRUE), 
    Disgust = sd(commentsDisgustScore, na.rm = TRUE),
    Negative = sd(commentsNegativeScore, na.rm = TRUE),
    Positive = sd(commentsPositiveScore, na.rm = TRUE))  
sd <- sd %>% mutate(across(where(is.numeric), round, digits=3))

sd <- sd %>% mutate(across(where(is.numeric), as.character ))
tableWelt <- tableWelt %>% mutate(across(where(is.numeric), as.character ))


for(i in 3:9){
  for(y in 1:16){
    tableWelt[y,i] <- paste0(tableWelt[y,i], " (")
    tableWelt[y,i] <- paste0(tableWelt[y,i], sd[y,i])
    tableWelt[y,i] <- paste0(tableWelt[y,i], ")")
  }
}


table <- rbind(tableHB, tableWelt)
table <- rbind(table, tableZeit)

write.csv(table, file ="Outputs/SQ5/SQ5_tableComments.csv")




#####################################################################################################
#SQ6: 
#####################################################################################################
#number comments


ggplot(dat.o, aes(x=numberComments)) + 
  geom_histogram(color="black") +
  labs(y="Frequency", x = "Number of Comments") +
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey"))


#number Comments per outlet
x <- group_by(dat.std, outlet) %>%
  summarise(
    countArticles = n(),
    countArticlesWithComments = n() -sum(is.na(commentsAngerScore)),
    min = min(numberComments),
    max = max(numberComments),
    mean = mean(numberComments, na.rm = TRUE),
    sd = sd(numberComments, na.rm = TRUE))

x$countArticlesWithComments <- paste(x$countArticlesWithComments, paste(paste("(", 
                                    round((x$countArticlesWithComments/x$countArticles)*100,2)), "%)"))
x <- as.data.frame(x)

write.csv(x, file = "Outputs/SQ6/table.csv")

#number comments vs. scraped Number of Comments
dat.com <- dat.std[dat.std$numberComments > 0,]

hb.o <- filter(dat.com, outlet =="HB")
ggplot(hb.o) +
  geom_point(aes(x= scrapedComments, y= numberComments))
  
welt.o <- filter(dat.com, outlet =="Welt")
ggplot(welt.o) +
  geom_point(aes(x= scrapedComments, y= numberComments))

zeit.o <- filter(dat.com, outlet =="Zeit")
ggplot(zeit.o) +
  geom_point(aes(x= scrapedComments, y= numberComments))


x <- group_by(dat.std, outlet) %>%
  summarise(
    meanNumberComments = mean(numberComments, na.rm = TRUE),
    meanScrapedComments = mean(scrapedComments, na.rm = TRUE))
as.data.frame(x)


#wo wird überhaupt kommentiert?
hb_numberComments <- hb %>% 
  filter(numberComments != 0) %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- hb_numberComments$count
names(counts) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
pareto.chart(counts, main = "Pareto Chart for Number of Comments outlet Handelsblatt", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))  

plotPareto(hb_numberComments, "Topic", "Number of articles with comments")


zeit_numberComments <- zeit %>% 
  filter(numberComments != 0) %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- zeit_numberComments$count
names(counts) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
pareto.chart(counts, main = "Pareto Chart for Number of Comments outlet Handelsblatt", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))  
plotPareto(zeit_numberComments, "Topic", "Number of articles with comments")


welt_numberComments <- zeit %>% 
  filter(numberComments != 0) %>% 
  group_by(Topic) %>%
  summarise(count= n())

counts <- welt_numberComments$count
names(counts) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
pareto.chart(counts, main = "Pareto Chart for Number of Comments outlet Handelsblatt", 
             col = rainbow(length(counts)), 
             cumperc = seq(0,100, by= 2.5))  
plotPareto(welt_numberComments, "Topic", "Number of articles with comments")

## Modelle

#mod <- lm(numberComments ~ TopicArticle, data = dat)

#hist(residuals(mod))
#head(sort(cooks.distance(mod), decreasing = TRUE))
#plot(mod,4)
#plot(mod,3)
#summary(mod)

#describe(dat$numberComments)

dat.o <- dat.std[-c(7946,10452),]


#mod.poisson <- glm(numberComments ~ TopicArticle + outlet + readabilityArticle + genderAuthor + date + 
                     #titleAngerScore + titleFearScore + titleSadnessScore +
                     #titleTrustScore + titleDisgustScore + titleNegativeScore + 
                     #titlePositiveScore +
                     #articleAngerScore + articleFearScore + articleSadnessScore +
                     #articleTrustScore + articleDisgustScore + articleNegativeScore + 
                     #articlePositiveScore, family = poisson, data = dat)
#head(sort(cooks.distance(mod.poisson), decreasing = TRUE))
#plot(mod.poisson,4)
#o <- which(cooks.distance(mod.poisson) > 1)
#summary(mod.poisson)

#plot_summs(mod.poisson, scale = TRUE, exp = TRUE)

#dat$phat <- predict(mod.poisson, type="response")
#ggplot(dat, aes(x = TopicArticle, y = phat, color = outlet)) +
  #geom_point(aes(y = numberComments), alpha = .7, position = position_jitter(h = .2)) +
  #geom_line() +
  #labs(x = "Topic", y = "Expected number of comments")


#mod.poisson.out <- glm(numberComments ~ TopicArticle + outlet, family = poisson, data = dat.o)
#head(sort(cooks.distance(mod.poisson.out), decreasing = TRUE))
#plot(mod.poisson.out,4)
#summary(mod.poisson.out)

#plot_summs(mod.poisson, mod.poisson.out, scale = TRUE, exp = TRUE)

#dat.o$phat <- predict(mod.poisson.out, type="response")
#ggplot(dat.o, aes(x = TopicArticle, y = phat, color = outlet)) +
  #geom_point(aes(y = numberComments), alpha = .7, position = position_jitter(h = .2)) +
  #geom_line() +
  #labs(x = "Topic", y = "Expected number of comments")

#with outliers
mod.quasipoisson <- glm(numberComments ~ TopicArticle + outlet + readabilityArticle + genderAuthor + date + 
                          titleAngerScore + titleFearScore + titleSadnessScore +
                          titleTrustScore + titleDisgustScore + titleNegativeScore + 
                          titlePositiveScore +
                          articleAngerScore + articleFearScore + articleSadnessScore +
                          articleTrustScore + articleDisgustScore + articleNegativeScore + 
                          articlePositiveScore, family = quasipoisson, data = dat.std)
summary(mod.quasipoisson)

head(sort(cooks.distance(mod.quasipoisson), decreasing = TRUE))
plot(mod.quasipoisson,4)


plot_summs(mod.quasipoisson, scale = TRUE, exp = TRUE)

#coef1 = coef(mod.poisson)
#coef2 = coef(mod.quasipoisson)
#se.coef1 = se.coef(mod.poisson)
#se.coef2 = se.coef(mod.quasipoisson)
#cbind(coef1, coef2, se.coef1, se.coef2, exponent = exp(coef1))
#cbind(coef1, se.coef1, exponent = exp(coef1))

dat.std$phat <- predict(mod.quasipoisson, type="response")
ggplot(dat.std, aes(x = Topic, y = phat, color = outlet)) +
  geom_point(aes(y = numberComments), alpha = .7, position = position_jitter(h = .2)) +
  geom_line() +
  labs(x = "Topic", y = "Expected number of comments")+
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) 


#without outliers
mod.quasipoisson.out <- glm(numberComments ~ TopicArticle + outlet + readabilityArticle + genderAuthor + date + 
                          titleAngerScore + titleFearScore + titleSadnessScore +
                          titleTrustScore + titleDisgustScore + titleNegativeScore + 
                          titlePositiveScore +
                          articleAngerScore + articleFearScore + articleSadnessScore +
                          articleTrustScore + articleDisgustScore + articleNegativeScore + 
                          articlePositiveScore, family = quasipoisson, data = dat.o)
summary(mod.quasipoisson.out)

head(sort(cooks.distance(mod.quasipoisson.out), decreasing = TRUE))
plot(mod.quasipoisson.out,4)


plot_summs(mod.quasipoisson.out, scale = TRUE, exp = TRUE)


dat.o$phat <- predict(mod.quasipoisson.out, type="response")
ggplot(dat.o, aes(x = Topic, y = phat, color = outlet)) +
  geom_point(aes(y = numberComments), alpha = .7, position = position_jitter(h = .2)) +
  geom_line() +
  labs(x = "Topic", y = "Expected number of comments")+
  theme(panel.background = element_blank(), 
        panel.grid.major.y = element_line(color = "grey"),
        panel.grid.minor.y = element_line(color = "grey")) 


#mean and variance
mean(dat.o$numberComments)
var(dat.o$numberComments)


#write to output file
output <- tidy(mod.quasipoisson.out)
b <- output$estimate
p <- output$p.value
output <- output[,c(1,2,3,4)]
names(output) <- c("Variables", "b", "SE", "t")

#b_exp <- as.character(round(exp(b), 3))

for(i in 1:length(b)){
  b[i] <- exp(b[i])
}

output$exp <- b
output <- output[, c(1,2,5,3,4)]
names(output) <- c("Variables", "b", "exp(b)", "SE", "t")

output <- output %>% mutate(across(where(is.numeric), round, digits=3))

pstar  <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,5] <- paste0(output[i,5], pstar[i])
}  

#output$`exp(b)` <- round(output$`exp(b)`,2)
  
write.csv(output, file = "Outputs/SQ6/output.csv")  

#####################################################################################################
#SQ7: 
#####################################################################################################

### Predict sentiment of comments

#????Vorrausetzungen: linearität, exogenität der prädiktoren, Homoskedastizität, 
#Unabhängigkeit der Residuen,Normalverteilung der Residuen, Multikollinearität
#extreme werte und einflussreiche Datenpunkte

#???wie gut ist das modell?: library(ggfortify) autoplot(mod)

#linearität
car::avPlots(anger.1)



dat.com <- dat.std[dat.std$numberComments > 0,]

#anger
anger.1 <- lm(commentsAngerScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsFearScore +
                commentsSadnessScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleFearScore +
                titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore, data = dat.com)

a <- car::vif(anger.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(anger.1))
head(sort(cooks.distance(anger.1), decreasing = TRUE))
plot(anger.1,4)
plot(anger.1,3)

summary(anger.1)
coeftest(anger.1, vcov = vcovHC(anger.1, type = 'HC3'))

anger.2 <- lm(commentsAngerScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsFearScore +
                commentsSadnessScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleFearScore +
                titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore + titleAngerScore + articleAngerScore, data = dat.com)
           
hist(residuals(anger.2))
head(sort(cooks.distance(anger.2), decreasing = TRUE))
plot(anger.2,4)
plot(anger.2,3)
a <- car::vif(anger.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(anger.2)

anova(anger.1,anger.2) # Modellvergleich: Geschachtelter F-Test

coeftest(anger.2, vcov = vcovHC(anger.2, type = 'HC3'))
waldtest(anger.1, anger.2, vcov = vcovHC(anger.2, type = 'HC3'))


output <- tidy(anger.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(anger.1, vcov = vcovHC(anger.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/anger1.csv")  


output <- tidy(anger.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(anger.2, vcov = vcovHC(anger.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/anger2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/anger.csv")  




#fear
fear.1 <- lm(commentsFearScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsSadnessScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleSadnessScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore, data = dat.com)

a <- car::vif(fear.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(fear.1))
head(sort(cooks.distance(fear.1), decreasing = TRUE))
plot(fear.1,4)
plot(fear.1,3)

summary(fear.1)
coeftest(fear.1, vcov = vcovHC(fear.1, type = 'HC3'))


fear.2 <- lm(commentsFearScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsSadnessScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleSadnessScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore + titleFearScore + articleFearScore, data = dat.com)

hist(residuals(fear.2))
head(sort(cooks.distance(fear.2), decreasing = TRUE))
plot(fear.2,4)
plot(fear.2,3)
a <- car::vif(fear.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(fear.2)

anova(fear.1,fear.2) # Modellvergleich: Geschachtelter F-Test

coeftest(fear.2, vcov = vcovHC(fear.2, type = 'HC3'))
waldtest(fear.1, fear.2, vcov = vcovHC(fear.2, type = 'HC3'))

output <- tidy(fear.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(fear.1, vcov = vcovHC(fear.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/fear1.csv")  


output <- tidy(fear.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(fear.2, vcov = vcovHC(fear.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/fear2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/fear.csv")  



#sadness
sadness.1 <- lm(commentsSadnessScore ~ TopicArticle + outlet + date + 
                  readabilityArticle + genderAuthor + commentsAngerScore +
                  commentsFearScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                  commentsPositiveScore + titleAngerScore +
                  titleFearScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                  articleAngerScore + articleFearScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                  articlePositiveScore, data = dat.com)

a <- car::vif(sadness.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(sadness.1))
head(sort(cooks.distance(sadness.1), decreasing = TRUE))
plot(sadness.1,4)
plot(sadness.1,3)

summary(sadness.1)
coeftest(sadness.1, vcov = vcovHC(sadness.1, type = 'HC3'))


sadness.2 <- lm(commentsSadnessScore ~ TopicArticle + outlet + date + 
                  readabilityArticle + genderAuthor + commentsAngerScore +
                  commentsFearScore + commentsTrustScore + commentsDisgustScore + commentsNegativeScore +
                  commentsPositiveScore + titleAngerScore +
                  titleFearScore + titleTrustScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                  articleAngerScore + articleFearScore + articleTrustScore + articleDisgustScore + articleNegativeScore +    
                  articlePositiveScore + titleSadnessScore + articleSadnessScore, data = dat.com)

hist(residuals(sadness.2))
head(sort(cooks.distance(sadness.2), decreasing = TRUE))
plot(sadness.2,4)
plot(sadness.2,3)
a <- car::vif(sadness.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(sadness.2)

anova(sadness.1,sadness.2) # Modellvergleich: Geschachtelter F-Test

coeftest(sadness.2, vcov = vcovHC(sadness.2, type = 'HC3'))
waldtest(sadness.1, sadness.2, vcov = vcovHC(sadness.2, type = 'HC3'))


output <- tidy(sadness.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(sadness.1, vcov = vcovHC(sadness.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/sadness1.csv")  


output <- tidy(sadness.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(sadness.2, vcov = vcovHC(sadness.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/sadness2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/sadness.csv")  



#trust
trust.1 <- lm(commentsTrustScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsFearScore + commentsSadnessScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleFearScore + titleSadnessScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleFearScore + articleSadnessScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore, data = dat.com)

a <- car::vif(trust.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(trust.1))
head(sort(cooks.distance(trust.1), decreasing = TRUE))
plot(trust.1,4)
plot(trust.1,3)

summary(trust.1)
coeftest(trust.1, vcov = vcovHC(trust.1, type = 'HC3'))


trust.2 <- lm(commentsTrustScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsFearScore + commentsSadnessScore + commentsDisgustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleFearScore + titleSadnessScore + titleDisgustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleFearScore + articleSadnessScore + articleDisgustScore + articleNegativeScore +    
                articlePositiveScore + titleTrustScore + articleTrustScore, data = dat.com)

hist(residuals(trust.2))
head(sort(cooks.distance(trust.2), decreasing = TRUE))
plot(trust.2,4)
plot(trust.2,3)
a <- car::vif(trust.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(trust.2)

anova(trust.1,trust.2) # Modellvergleich: Geschachtelter F-Test

coeftest(trust.2, vcov = vcovHC(trust.2, type = 'HC3'))
waldtest(trust.1, trust.2, vcov = vcovHC(trust.2, type = 'HC3'))



output <- tidy(trust.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(trust.1, vcov = vcovHC(trust.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/trust1.csv")  


output <- tidy(trust.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(trust.2, vcov = vcovHC(trust.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/trust2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/trust.csv")  




#disgust
disgust.1 <- lm(commentsDisgustScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleFearScore + titleSadnessScore + titleTrustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleNegativeScore +    
                articlePositiveScore, data = dat.com)

a <- car::vif(disgust.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(disgust.1))
head(sort(cooks.distance(disgust.1), decreasing = TRUE))
plot(disgust.1,4)
plot(disgust.1,3)

summary(disgust.1)
coeftest(disgust.1, vcov = vcovHC(disgust.1, type = 'HC3'))


disgust.2 <- lm(commentsDisgustScore ~ TopicArticle + outlet + date + 
                readabilityArticle + genderAuthor + commentsAngerScore +
                commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsNegativeScore +
                commentsPositiveScore + titleAngerScore +
                titleFearScore + titleSadnessScore + titleTrustScore + titleNegativeScore + titlePositiveScore + 
                articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleNegativeScore +    
                articlePositiveScore + titleDisgustScore + articleDisgustScore, data = dat.com)

hist(residuals(disgust.2))
head(sort(cooks.distance(disgust.2), decreasing = TRUE))
plot(disgust.2,4)
plot(disgust.2,3)
a <- car::vif(disgust.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(disgust.2)

anova(disgust.1,disgust.2) # Modellvergleich: Geschachtelter F-Test

coeftest(disgust.2, vcov = vcovHC(disgust.2, type = 'HC3'))
waldtest(disgust.1, disgust.2, vcov = vcovHC(disgust.2, type = 'HC3'))


output <- tidy(disgust.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(disgust.1, vcov = vcovHC(disgust.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/disgust1.csv")  


output <- tidy(disgust.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(disgust.2, vcov = vcovHC(disgust.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/disgust2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/disgust.csv") 




#negative
negative.1 <- lm(commentsNegativeScore ~ TopicArticle + outlet + date + 
                  readabilityArticle + genderAuthor + commentsAngerScore +
                  commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsDisgustScore +
                  commentsPositiveScore + titleAngerScore +
                  titleFearScore + titleSadnessScore + titleTrustScore + titleDisgustScore + titlePositiveScore + 
                  articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore +    
                  articlePositiveScore, data = dat.com)

a <- car::vif(negative.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(negative.1))
head(sort(cooks.distance(negative.1), decreasing = TRUE))
plot(negative.1,4)
plot(negative.1,3)

summary(negative.1)
coeftest(negative.1, vcov = vcovHC(negative.1, type = 'HC3'))


negative.2 <- lm(commentsNegativeScore ~ TopicArticle + outlet + date + 
                  readabilityArticle + genderAuthor + commentsAngerScore +
                  commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsDisgustScore +
                  commentsPositiveScore + titleAngerScore +
                  titleFearScore + titleSadnessScore + titleTrustScore + titleDisgustScore + titlePositiveScore + 
                  articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore +    
                  articlePositiveScore + titleNegativeScore + articleNegativeScore, data = dat.com)

hist(residuals(negative.2))
head(sort(cooks.distance(negative.2), decreasing = TRUE))
plot(negative.2,4)
plot(negative.2,3)
a <- car::vif(negative.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(negative.2)

anova(negative.1,negative.2) # Modellvergleich: Geschachtelter F-Test

coeftest(negative.2, vcov = vcovHC(negative.2, type = 'HC3'))
waldtest(negative.1, negative.2, vcov = vcovHC(negative.2, type = 'HC3'))



output <- tidy(negative.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(negative.1, vcov = vcovHC(negative.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/negative1.csv")  


output <- tidy(negative.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(negative.2, vcov = vcovHC(negative.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/negative2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/negative.csv") 


#positive
positive.1 <- lm(commentsPositiveScore ~ TopicArticle + outlet + date + 
                   readabilityArticle + genderAuthor + commentsAngerScore +
                   commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsDisgustScore +
                   commentsNegativeScore + titleAngerScore +
                   titleFearScore + titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + 
                   articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore +    
                   articleNegativeScore, data = dat.com)

a <- car::vif(positive.1)
a[,3]^2 # dürfen nicht höher als 10 sein


hist(residuals(positive.1))
head(sort(cooks.distance(positive.1), decreasing = TRUE))
plot(positive.1,4)
plot(positive.1,3)

summary(positive.1)
coeftest(positive.1, vcov = vcovHC(positive.1, type = 'HC3'))


positive.2 <- lm(commentsPositiveScore ~ TopicArticle + outlet + date + 
                   readabilityArticle + genderAuthor + commentsAngerScore +
                   commentsFearScore + commentsSadnessScore + commentsTrustScore + commentsDisgustScore +
                   commentsNegativeScore + titleAngerScore +
                   titleFearScore + titleSadnessScore + titleTrustScore + titleDisgustScore + titleNegativeScore + 
                   articleAngerScore + articleFearScore + articleSadnessScore + articleTrustScore + articleDisgustScore +    
                   articleNegativeScore + titlePositiveScore + articlePositiveScore, data = dat.com)

hist(residuals(positive.2))
head(sort(cooks.distance(positive.2), decreasing = TRUE))
plot(positive.2,4)
plot(positive.2,3)
a <- car::vif(positive.2)
a[,3]^2 # dürfen nicht höher als 10 sein
summary(positive.2)

anova(positive.1,positive.2) # Modellvergleich: Geschachtelter F-Test

coeftest(positive.2, vcov = vcovHC(positive.2, type = 'HC3'))
waldtest(positive.1, positive.2, vcov = vcovHC(positive.2, type = 'HC3'))


output <- tidy(positive.1)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(positive.1, vcov = vcovHC(positive.1, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output1 <- output

write.csv(output1, file = "Outputs/SQ7/positive1.csv")  


output <- tidy(positive.2)

output <- output[,c(1,2)]
names(output) <- c("Variables", "beta")

errors <- tidy(coeftest(positive.2, vcov = vcovHC(positive.2, type = 'HC3')))
output$t <- errors$statistic
output <- output %>% mutate(across(where(is.numeric), round, digits=3))

p  <- errors$p.value
pstar <- p
pstar <- as.character(pstar)

for(i in 1: length(p)){
  if(p[i] < 0.001){
    pstar[i] <- "***"
  }else if (p[i] < 0.01){
    pstar[i] <- "**"
  }else if (p[i] < 0.05){
    pstar[i] <- "*"
  }else{
    pstar[i] <- ""
  }
}

output$t <- as.character(output$t)  
for(i in 1:nrow(output)){
  output[i,3] <- paste0(output[i,3], pstar[i])
}  

output2 <- output

write.csv(output2, file = "Outputs/SQ7/positive2.csv")  

x <- data.frame("Variables" = rep(NA,2), "beta" = rep(NA,2), "t"= rep(NA,2))
output1 <- rbind(output1, x)

outputTogether <- cbind(output1, output2)
outputTogether[,1] <- outputTogether[,4]
outputTogether <- outputTogether[,c(1,2,3,5,6)]


write.csv(outputTogether, file = "Outputs/SQ7/positive.csv") 
