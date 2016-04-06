#EXPERIMENTS WITH SYNTHETIC DATA

library(ggplot2)
library(pracma)

setwd("") #USER MUST SET WORKING DIRECTORY

source("NDCGphi.R") #LOAD FUNCTION TO INTERPOLATE RELEVANCE FROM THE DISTRIBUTION OF VALUES

#WORST-BEST SCENARIO

final.tbl <- data.frame(NDCG_phi=numeric(0),NDCG=numeric(0))

for(i in 1:1000) {
  
  print(i)
  
  y <- sample(1:1000,100,replace=T) #GENERATE VALUES FOR BALANCED DISTRIBUTION
  y <- y[order(y,decreasing=TRUE)]
  
  y.phi <- GoD_phi(y) #CALCULATE RELEVANCE FOR EACH VALUE
  
  data <- data.frame(Rank=1:length(y),y=y,y.phi=y.phi)
  
  data["Relevance"] <- c(runif(10, 3, 3),runif(15, 2, 2),runif(25, 1, 1),runif(50, 0, 0)) #AD-HOC RELEVANCE (FOR STANDARD NDCG)
  
  k <- 10 #CUT-OFF
  data.in <- data
  data.in["NewRank"] <- c(10,9,8,7,6,5,4,3,2,1,seq(11,100,by=1)) #WORST-BEST SCENARIO
  data.in <- data.in[with(data.in,order(data.in$NewRank)),]
  
  ndcg_phi <- sum((2^(data.in[1:k,]$y.phi)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$y.phi)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  ndcg <- sum((2^(data.in[1:k,]$Relevance)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$Relevance)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  
  row <- data.frame(NDCG_phi=ndcg_phi,NDCG=ndcg)
  final.tbl <- rbind(final.tbl,row)
  
}

balanced <- final.tbl
balanced["Type"] <- "Balanced"

final.tbl <- data.frame(NDCG_phi=numeric(0),NDCG=numeric(0))

for(i in 1:1000) {
  
  print(i)
  
  y <- sample(1:500,80,replace=T) #GENERATE VALUES FOR IMBALANCED DISTRIBUTION
  y <- c(y,sample(500:1000,20,replace=T))
  
  while(length(boxplot.stats(y)$out)==0) {
    y <- sample(1:500,80,replace=T)
    y <- c(y,sample(500:1000,20,replace=T))
  }
  
  y <- y[order(y,decreasing=TRUE)]
  
  y.phi <- GoD_phi(y) #CALCULATE RELEVANCE FOR EACH VALUE
  
  data <- data.frame(Rank=1:length(y),y=y,y.phi=y.phi)
  
  data["Relevance"] <- c(runif(10, 3, 3),runif(15, 2, 2),runif(25, 1, 1),runif(50, 0, 0)) #AD-HOC RELEVANCE (FOR STANDARD NDCG)
  
  k <- 10 #CUT-OFF
  data.in <- data
  data.in["NewRank"] <- c(10,9,8,7,6,5,4,3,2,1,seq(11,100,by=1)) #WORST-BEST SCENARIO
  data.in <- data.in[with(data.in,order(data.in$NewRank)),]
  
  ndcg_phi <- sum((2^(data.in[1:k,]$y.phi)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$y.phi)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  ndcg <- sum((2^(data.in[1:k,]$Relevance)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$Relevance)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  
  row <- data.frame(NDCG_phi=ndcg_phi,NDCG=ndcg)
  final.tbl <- rbind(final.tbl,row)
  
}

imbalanced <- final.tbl
imbalanced["Type"] <- "Imbalanced"

data_worstbest <- rbind(imbalanced,balanced)
data_worstbest["Scenario"] <- "Worst-Best"

#######################

#BEST-WORST SCENARIO

final.tbl <- data.frame(NDCG_phi=numeric(0),NDCG=numeric(0))

for(i in 1:1000) {
  
  print(i)
  
  y <- sample(1:1000,100,replace=T) #GENERATE VALUES FOR BALANCED DISTRIBUTION
  y <- y[order(y,decreasing=TRUE)]
  
  y.phi <- GoD_phi(y) #CALCULATE RELEVANCE FOR EACH VALUE
  
  data <- data.frame(Rank=1:length(y),y=y,y.phi=y.phi)
  
  data["Relevance"] <- c(runif(10, 3, 3),runif(15, 2, 2),runif(25, 1, 1),runif(50, 0, 0)) #AD-HOC RELEVANCE (FOR STANDARD NDCG)
  
  k <- 10 #CUT-OFF
  data.in <- data
  
  data.in["NewRank"] <- c(1,2,3,4,5,6,7,8,9,11,10,seq(12,100,by=1)) #BEST-WORST SCENARIO
  data.in <- data.in[with(data.in,order(data.in$NewRank)),]
  
  ndcg_phi <- sum((2^(data.in[1:k,]$y.phi)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$y.phi)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  ndcg <- sum((2^(data.in[1:k,]$Relevance)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$Relevance)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  
  row <- data.frame(NDCG_phi=ndcg_phi,NDCG=ndcg)
  final.tbl <- rbind(final.tbl,row)
  
}

balanced <- final.tbl
balanced["Type"] <- "Balanced"

final.tbl <- data.frame(NDCG_phi=numeric(0),NDCG=numeric(0))

for(i in 1:1000) {
  
  print(i)
  
  y <- sample(1:500,80,replace=T) #GENERATE VALUES FOR IMBALANCED DISTRIBUTION
  y <- c(y,sample(500:1000,20,replace=T))
  
  while(length(boxplot.stats(y)$out)==0) {
    y <- sample(1:500,80,replace=T)
    y <- c(y,sample(500:1000,20,replace=T))
  }
  
  y <- y[order(y,decreasing=TRUE)]
  
  y.phi <- GoD_phi(y) #CALCULATE RELEVANCE FOR EACH VALUE
  
  data <- data.frame(Rank=1:length(y),y=y,y.phi=y.phi)
  
  data["Relevance"] <- c(runif(10, 3, 3),runif(15, 2, 2),runif(25, 1, 1),runif(50, 0, 0)) #AD-HOC RELEVANCE (FOR STANDARD NDCG)
  
  k <- 10 #CUT-OFF
  data.in <- data
  
  data.in["NewRank"] <- c(1,2,3,4,5,6,7,8,9,11,10,seq(12,100,by=1)) #BEST-WORST SCENARIO
  data.in <- data.in[with(data.in,order(data.in$NewRank)),]
  
  ndcg_phi <- sum((2^(data.in[1:k,]$y.phi)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$y.phi)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  ndcg <- sum((2^(data.in[1:k,]$Relevance)-1)/log((1+(1:nrow(data.in[1:k,]))),base=2))/sum((2^(data[1:k,]$Relevance)-1)/log((1+(1:nrow(data[1:k,]))),base=2))
  
  row <- data.frame(NDCG_phi=ndcg_phi,NDCG=ndcg)
  final.tbl <- rbind(final.tbl,row)
  
}

imbalanced <- final.tbl
imbalanced["Type"] <- "Imbalanced"

data_bestworst <- rbind(imbalanced,balanced)
data_bestworst["Scenario"] <- "Best-Worst"

#######################

#PLOTTING FIGURE 2

data <- rbind(data_worstbest,data_bestworst)
data$Scenario <- factor(data$Scenario)

horlines <- data.frame(Scenario=levels(data$Scenario),vl=c(data_bestworst[1,]$NDCG,data_worstbest[1,]$NDCG))

synthetic.plot <- ggplot(data,aes(Type,NDCG_phi)) + geom_boxplot() + geom_hline(aes(yintercept=vl),data=horlines) + facet_grid(.~Scenario) + ylab(expression(paste("NDCG",phi,"@10"))) +xlab("Type of Distribution")

worstbest <- ggplot(data_worstbest,aes(Type,NDCG_phi)) + geom_boxplot() + ylim(c(0.75,1)) + ylab(expression(paste("NDCG",phi,"@10"))) + geom_hline(yintercept=data_worstbest[1,]$NDCG) + ggtitle("Worst-Best Scenario")
bestworst <- ggplot(data_bestworst,aes(Type,NDCG_phi)) + geom_boxplot() + ylim(c(0.75,1)) + ylab(expression(paste("NDCG",phi,"@10"))) + geom_hline(yintercept=data_bestworst[1,]$NDCG) + ggtitle("Best-Worst Scenario")

pdf("synthetic.pdf",width=6,height=4)
grid.newpage()
print(synthetic.plot,vp=viewport(layout.pos.row=1,layout.pos.col=1))
dev.off()

##############################################################################
