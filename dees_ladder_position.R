year <- seq(from = 1989, to = 2013, by = 1)
position_rev <- c(17/18,16/18,13/17,12/16,16/16,16/16,14/16,7/16,7/16,5/16,14/16,6/16,11/16,3/16,14/16,4/16,16/16,14/16,9/16,7/15,10/15,11/15,5/15,4/14,4/14) #this starts in 2013 and goes backward to 1989
position <- rev(position_rev)
fff <- function (x){1-x}
relPos <- sapply(position, fff)

library("ggplot2")

data<-as.data.frame(cbind(year, relPos))

width <- 3236
height <- 2000
res <- 400
png(file = "deesChances.png", width = width, height = height, res = res)
ggplot(data, aes(x=year, y=relPos)) +
  geom_line(aes(colour = relPos, size = 3)) +
  scale_colour_gradient(low = "blue", high = "red")
dev.off()