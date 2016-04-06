#NDCG_phi implementation

GoD_phi <- function(y) {
  
  require(pracma)
  
  x1 <- c()
  y1 <- c()
  alpha <- (max(y[y>0],na.rm=TRUE)-boxplot.stats(y[y>0])$stats[5])/(max(y[y>0],na.rm=TRUE)-min(y[y>0],na.rm=TRUE))
  
  if(alpha==0) {
    
    x1 <- c(0,boxplot.stats(y[y>0])$stats[3],max(y[y>0],na.rm=TRUE))
    y1 <- c(0,0,1)
    
  } else {
    
    if(boxplot.stats(y)$stats[3]!=0) {
      x1 <- c(0,boxplot.stats(y[y>0])$stats[3],boxplot.stats(y[y>0])$stats[5],max(y[y>0],na.rm=TRUE))
      y1 <- c(0,0,(1-alpha),1)
    } else {
      alpha <- (max(y[y>0],na.rm=TRUE)-boxplot.stats(y[y>0])$stats[5])/(max(y[y>0],na.rm=TRUE)-min(y[y>0],na.rm=TRUE))
      x1 <- c(0,boxplot.stats(y[y>0])$stats[3],boxplot.stats(y[y>0])$stats[5],max(y[y>0],na.rm=TRUE))
      y1 <- c(0,0,(1-alpha),1)
    }
    
  }
  
  y.phi <- pchip(x1, y1, y)
  
  y.phi
  
}


