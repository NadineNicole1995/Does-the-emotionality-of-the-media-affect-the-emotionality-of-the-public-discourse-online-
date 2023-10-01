# Change working directory 
setwd("C:/Users/nadin/OneDrive/Dokumente/Dissertation")

# Clear the environment by removing any defined objects/functions
rm(list = ls())

library(readxl)
library(formattable)

#load datafiles
hb <- read_xlsx("Final/HandelsblattArtikel.xlsx")
zeit <- read_xlsx("Final/ZeitArtikel.xlsx")
welt <- read_xlsx("Final/WeltArtikel.xlsx")
hb_comment <- read_xlsx("Final/HandelsblattComment.xlsx")
zeit_comment <- read_xlsx("Final/ZeitComment.xlsx")
welt_comment <- read_xlsx("Final/WeltComment.xlsx")
colnames(welt_comment) <- c("Id", "comment")

#check for NA
#articles
na <- hb[!complete.cases(hb), ]#no NAs
na <- zeit[!complete.cases(zeit), ] #title is missing
zeit <- filter(zeit, !(zeit$Id %in% na$Id)) #remove them
#remove zeit comments for which there are NAs in article Zeit file
zeit_comment <- filter(zeit_comment, !(zeit_comment$Id %in% na$Id))

na <- welt[!complete.cases(welt), ]#no NAs

#comments
na <- hb_comment[!complete.cases(hb_comment), ] #HB-3477 comment is missing
hb_comment <- hb_comment[complete.cases(hb_comment), ] #remove NA
na <- zeit_comment[!complete.cases(zeit_comment), ] #comments missing
zeit_comment <- zeit_comment[complete.cases(zeit_comment), ] #remove NA
na <- welt_comment[!complete.cases(welt_comment), ] #comments missing
welt_comment <- welt_comment[complete.cases(welt_comment), ] #remove NA

#remove englisch articles
load("Final/englisch.Rdata")
hb <-filter(hb, !(hb$Id %in% englisch_data$Id))
zeit <-filter(zeit, !(zeit$Id %in% englisch_data$Id))
welt <-filter(welt, !(welt$Id %in% englisch_data$Id))

#date: only articles from 01.01.2020 to 30.06.2023
hb$date <- as.Date(hb$date, "%Y-%m-%d")
hb <- filter(hb, hb$date >= "2020-01-01")
hb <- filter(hb, hb$date <= "2023-06-30")

zeit$date <- as.Date(zeit$date, "%Y-%m-%d")
zeit <- filter(zeit, zeit$date >= "2020-01-01")
zeit <- filter(zeit, zeit$date <= "2023-06-30")

welt$date <- as.Date(welt$date, "%Y-%m-%d")
welt <- filter(welt, welt$date >= "2020-01-01")
welt <- filter(welt, welt$date <= "2023-06-30")


#identify comments where we not found article
hb_comments_no_article <- filter(hb_comment, !(hb_comment$Id %in% hb$Id)) #nothing missing
zeit_comments_no_article <- filter(zeit_comment, !(zeit_comment$Id %in% zeit$Id)) #373 missing
zeit_comment <- filter(zeit_comment, (zeit_comment$Id %in% zeit$Id))#remove them 
welt_comments_no_article <- filter(welt_comment, !(welt_comment$Id %in% welt$Id)) #1566 missing
welt_comment <- filter(welt_comment, (welt_comment$Id %in% welt$Id))#remove them 

#identify articles where we not found comments 
hb_with_Comments  <- filter(hb, hb$numberComments != 0)
zeit_with_Comments  <- filter(zeit, zeit$numberComments != 0)
welt_with_Comments  <- filter(welt, welt$numberComments != 0)

hb_with_Comments_false <- filter(hb_with_Comments, !(hb_with_Comments$Id %in% hb_comment$Id))#nothing missing
zeit_with_Comments_false <- filter(zeit_with_Comments, !(zeit_with_Comments$Id %in% zeit_comment$Id))#nothing missing
welt_with_Comments_false <- filter(welt_with_Comments, !(welt_with_Comments$Id %in% welt_comment$Id))#17 missing
welt <- filter(welt, !(welt$Id %in% welt_with_Comments_false$Id))#remove them



#add column scraped comments
#hb
hb_comment_grouped <- hb_comment %>%
  group_by(hb_comment$Id) %>%
  summarize(scrapedComments = n())
colnames(hb_comment_grouped) <- c("Id", "scrapedComments")

hb <- merge(hb, hb_comment_grouped, by = "Id", all.x = TRUE)
hb["scrapedComments"][is.na(hb["scrapedComments"])] <- 0 #replace NAs with 0

#zeit
zeit_comment_grouped <- zeit_comment %>%
  group_by(zeit_comment$Id) %>%
  summarize(scrapedComments = n())
colnames(zeit_comment_grouped) <- c("Id", "scrapedComments")

zeit <- merge(zeit, zeit_comment_grouped, by = "Id", all.x = TRUE)
zeit["scrapedComments"][is.na(zeit["scrapedComments"])] <- 0 #replace NAs with 0

#welt
welt_comment_grouped <- welt_comment %>%
  group_by(welt_comment$Id) %>%
  summarize(scrapedComments = n())
colnames(welt_comment_grouped) <- c("Id", "scrapedComments")

welt <- merge(welt, welt_comment_grouped, by = "Id", all.x = TRUE)
welt["scrapedComments"][is.na(welt["scrapedComments"])] <- 0 #replace NAs with 0


#add column outlet
hb$outlet <- rep("HB", nrow(hb))
zeit$outlet <- rep("Zeit", nrow(zeit))
welt$outlet <- rep("Welt", nrow(welt))

#reorder columns
#"Id","url", "title" date","numberComments", "author", "article","scrapedComments", "outlet" 
hb <- hb[, c(1,9,2,3,7,4,6,5,8)]
zeit <- zeit[, c(1,9,2,3,7,4,6,5,8)]
welt <- welt[, c(1,9,2,3,7,4,6,5,8)]

#put articles together
articles <- rbind(hb, zeit)
articles <- rbind(articles, welt)

#put comments together
comments <- rbind(hb_comment, zeit_comment)
comments <- rbind(comments, welt_comment)

#save them all
save(hb, file = "Final/HB.Rdata")
save(zeit, file = "Final/Zeit.Rdata")
save(welt, file = "Final/Welt.Rdata")
save(hb_comment, file = "Final/HB_comment.Rdata")
save(zeit_comment, file = "Final/Zeit_comment.Rdata")
save(welt_comment, file = "Final/Welt_comment.Rdata")

save(articles, file = "Final/articles.Rdata")
save(comments, file = "Final/comments.Rdata" )

#save in smaller data files
articles1 <- articles[1:10000,]
save(articles1, file = "Final/articles1.Rdata" )
articles2 <- articles[10001:17787,]
save(articles2, file = "Final/articles2.Rdata" )

comments1 <- comments[1:150000,]
save(comments1, file = "Final/comments1.Rdata" )
comments2 <- comments[150001:280000,]
save(comments2, file = "Final/comments2.Rdata" )
comments3 <- comments[280001:450000,]
save(comments3, file = "Final/comments3.Rdata" )
comments4 <- comments[450001:650000,]
save(comments4, file = "Final/comments4.Rdata" )
comments5 <- comments[650001:726710,]
save(comments5, file = "Final/comments5.Rdata" )



#create table number of articles/ comments
df <- data.frame("Zeit" = c(nrow(zeit),nrow(zeit_comment)),
                 "Welt" = c(nrow(welt),nrow(welt_comment)),
                 "Handelsblatt" = c(nrow(hb),nrow(hb_comment)),
                 "Total" = c(0,0))

rownames(df) <- c("Article", "scraped Comments")
df[1,4] <- df[1,1] + df[1,2] +df[1,3]
df[2,4] <- df[2,1] + df[2,2] +df[2,3]
formattable(df)


