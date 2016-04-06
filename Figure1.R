#FIRST FIGURE

library(ggplot2)
library(grid)
library(gtable)

y <- sample(1:1000,100,replace=T)
y <- y[order(y,decreasing=TRUE)]

data <- data.frame(y=y)

p1 <- ggplot(data,aes(x=seq(1,100,by=1),y=y)) + geom_point() + geom_vline(xintercept=10) + geom_vline(xintercept=25) + geom_vline(xintercept=50) + xlab("Position in Ranking") + ylab("y") + scale_x_continuous(expand=c(0,0), breaks=c(1,25,50,75,100)) + scale_y_continuous(expand=c(0,0)) + expand_limits(y=-20) + expand_limits(y=1020) + expand_limits(x=-2) + expand_limits(x=102) + theme(plot.margin = unit(c(0.5, 0.2, 0.5, 0.3), "lines"))
p2 <- ggplot(data,aes(x=factor(1),y=y)) + geom_boxplot() + scale_y_continuous(expand = c(0, 0)) + expand_limits(y=c(-20,1020)) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), plot.margin = unit(c(0.5, 1, 1.6, 0), "lines")) + xlab("Position")

gt1 <- ggplot_gtable(ggplot_build(p1))
gt2 <- ggplot_gtable(ggplot_build(p2))

maxWidth <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])

gt <- gtable(widths = unit(c(8, 1.5), "null"), height = unit(c(20,1), "null"))
gt <- gtable_add_grob(gt, gt1, 1, 1)
gt <- gtable_add_grob(gt, gt2, 1, 2)
gt1$widths[2:3] <- as.list(maxWidth)
gt2$widths[2:3] <- as.list(maxWidth)

plot(gt)