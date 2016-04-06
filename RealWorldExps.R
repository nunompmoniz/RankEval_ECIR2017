#EXPERIMENTS WITH REAL DATA

library(ggplot2)
library(pracma)
library(dplyr)
library(scales)

setwd("") #USER MUST SET WORKING DIRECTORY

source("NDCGphi.R")

#LOAD DATA

ranktable.economy <- read.csv("RankTable_Economy.csv")
ranktable.economy["Day"] <- substr(ranktable.economy$Timestamp,1,10)
ranktable.economy <- ranktable.economy %>% filter(Day >= "2014-09-01")  %>% filter(Day < "2014-10-01")
ranktable.economy$Day <- NULL

ranktable.microsoft <- read.csv("RankTable_Microsoft.csv")
ranktable.microsoft["Day"] <- substr(ranktable.microsoft$Timestamp,1,10)
ranktable.microsoft <- ranktable.microsoft %>% filter(Day >= "2014-09-01")  %>% filter(Day < "2014-10-01")
ranktable.microsoft$Day <- NULL

ranktable.obama <- read.csv("RankTable_Obama.csv")
ranktable.obama["Day"] <- substr(ranktable.obama$Timestamp,1,10)
ranktable.obama <- ranktable.obama %>% filter(Day >= "2014-09-01")  %>% filter(Day < "2014-10-01")
ranktable.obama$Day <- NULL

ranktable.palestine <- read.csv("RankTable_Palestine.csv")
ranktable.palestine["Day"] <- substr(ranktable.palestine$Timestamp,1,10)
ranktable.palestine <- ranktable.palestine %>% filter(Day >= "2014-09-01")  %>% filter(Day < "2014-10-01")
ranktable.palestine$Day <- NULL

news <- read.csv("News.csv")
news$PublishDate <- as.POSIXct(news$PublishDate)

ranktables <- list(ranktable.economy,ranktable.microsoft,ranktable.obama,ranktable.palestine)
final_results <- list()

for(i in 1:length(ranktables)) { #FOR EACH TOPIC
  
  ranktable <- ranktables[[i]]
  
  results <- data.frame(NDCG_phi=numeric(0),NDCG=numeric(0))
  
  for(index.i in 1:nrow(ranktable)) { #FOR EACH AVALIABLE RANKING
    
    print(index.i)
    
    querytime <- as.POSIXct(ranktable[index.i,1])
    testrank <- as.numeric(ranktable[index.i,2:101])
    google.df <- data.frame(IDLink=testrank,Rank=seq(1,length(testrank),by=1))
    twitter.df <- data.frame(IDLink=testrank,NTweets=news[match(testrank,news$IDLink),]$TimesPublishedTwitter)
    twitter.df["PublishDate"] <- news[match(twitter.df$IDLink,news$IDLink),]$PublishDate
    twitter.df <- twitter.df[with(twitter.df,order(twitter.df$NTweets,decreasing=TRUE)),]
    twitter.df["TimeDiff"] <- NA
    for(time.i in 1:nrow(twitter.df)) {
      twitter.df[time.i,]$TimeDiff <- difftime(querytime,as.POSIXct(twitter.df[time.i,]$PublishDate),unit="mins")
    }
    twitter.df["TimeSlice"] <- -1
    
    timeslice <- 30 #30 MINUTES
    timeslice.i <- 96 #NUMBER OF TIMESLICES
    
    for(aux.i in 1:timeslice.i) {
      twitter.df.aux <- twitter.df[!is.na(twitter.df$TimeDiff) & twitter.df$TimeDiff<=(timeslice*aux.i) & twitter.df$TimeSlice<0,]
      if(nrow(twitter.df.aux)>0) {
        twitter.df[!is.na(twitter.df$TimeDiff) & twitter.df$TimeDiff<=(timeslice*aux.i) & twitter.df$TimeSlice<0,]$TimeSlice <- aux.i
      }
    }
    
    #DECAY FACTOR
    twitter.df["Weight"] <- 0
    twitter.df[twitter.df$TimeSlice>0,]$Weight <- 1-((twitter.df[twitter.df$TimeSlice>0,]$TimeSlice-1)/96)
    twitter.df$NTweets <- twitter.df$NTweets * twitter.df$Weight
    
    twitter.df <- twitter.df[with(twitter.df,order(twitter.df$NTweets,decreasing=TRUE)),]
    twitter.df["TwitterRank"] <- seq(1,length(testrank),by=1)
    
    finalrank <- twitter.df
    finalrank["GoogleRank"] <- google.df[match(finalrank$IDLink,google.df$IDLink),]$Rank
    
    y <- finalrank$NTweets
    
    y.phi <- GoD_phi(y) #CALCULATE RELEVANCE FOR EACH VALUE
    
    finalrank["y.phi"] <- y.phi
    finalrank["Relevance"] <- c(runif(10, 3, 3),runif(15, 2, 2),runif(25, 1, 1),runif(nrow(finalrank)-50, 0, 0)) #AD-HOC RELEVANCE (FOR STANDARD NDCG)
    
    data <- finalrank[,c("NTweets","TwitterRank","GoogleRank","y.phi","Relevance")]
    data.in <- data[with(data,order(data$GoogleRank)),]
    
    k <- 10 #CUT-OFF
    ndcg_phi <- sum((2^(data.in[1:k,]$y.phi)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$y.phi)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
    ndcg <- sum((2^(data.in[1:k,]$Relevance)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$Relevance)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
    
    row <- data.frame(NDCG_phi = ndcg_phi, NDCG = ndcg)
    results <- rbind(results,row)
    
  }
  
  final_results[[i]] <- results
  
}

economy <- final_results[[1]]
microsoft <- final_results[[2]]
obama <- final_results[[3]]
palestine <- final_results[[4]]

#######################

#PLOTTING FIGURE 3

economy.plot <- ggplot(economy,aes(x=NDCG,y=NDCG_phi)) + geom_point() + ylim(c(0,1)) + xlim(c(0,1)) + geom_abline(slope=1) + ylab(expression(paste("NDCG",phi,"@10"))) + xlab("") + ggtitle("Economy")
microsoft.plot <- ggplot(microsoft,aes(x=NDCG,y=NDCG_phi)) + geom_point() + ylim(c(0,1)) + xlim(c(0,1)) + geom_abline(slope=1) + ylab("") + xlab("") + ggtitle("Microsoft")
obama.plot <- ggplot(obama,aes(x=NDCG,y=NDCG_phi)) + geom_point() + ylim(c(0,1)) + xlim(c(0,1)) + geom_abline(slope=1) + ylab(expression(paste("NDCG",phi,"@10"))) + xlab("NDCG@10") + ggtitle("Obama")
palestine.plot <- ggplot(palestine,aes(x=NDCG,y=NDCG_phi)) + geom_point() + ylim(c(0,1)) + xlim(c(0,1)) + geom_abline(slope=1) + ylab("") + xlab("NDCG@10") + ggtitle("Palestine")

pdf("realworld.pdf",width=12,height=4)
grid.newpage()
pushViewport(viewport(layout=grid.layout(2,2)))
print(economy.plot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
print(microsoft.plot,vp=viewport(layout.pos.row=1,layout.pos.col=2))
print(obama.plot,vp=viewport(layout.pos.row=2,layout.pos.col=1))
print(palestine.plot,vp=viewport(layout.pos.row=2,layout.pos.col=2))
dev.off()

##############################################################################
