

tohistogram <- data.frame(year = c(2015,2016),`avg deal size (incl upgrades and paid trial`=c(43465.91,42199.86),
           `closed won (incl upgrades and paid trials)` = c(44,37),
           `new clients (not upsells)` = c(41,32),
           `new trials (include bus dev)`=c(129,135),
           `closed lost` = c(158,210))




hist(tohistogram)

x <- 1:5
y1 <- rnorm(5)
y2 <- rnorm(5,20)
par(mar=c(5,4,4,5)+.1)
# plot(x,y1,type="l",col="red")
hist(y1)
par(new=TRUE)
# plot(x, y2,,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
hist(y2)
# axis(4)
mtext("y2",side=4,line=3)
# legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))



# set up some fake test data
time <- seq(0,72,12)
betagal.abs <- c(0.05,0.18,0.25,0.31,0.32,0.34,0.35)
cell.density <- c(0,1000,2000,3000,4000,5000,6000)

## add extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

## Plot first set of data and draw its axis
plot(time, betagal.abs, pch=16, axes=FALSE, ylim=c(0,1), xlab="", ylab="", 
     type="b",col="black", main="Mike's test data")
axis(2, ylim=c(0,1),col="black",las=1)  ## las=1 makes horizontal labels
mtext("Beta Gal Absorbance",side=2,line=2.5)
box()

## Allow a second plot on the same graph
par(new=TRUE)

## Plot the second plot and put axis scale on right
plot(time, cell.density, pch=15,  xlab="", ylab="", ylim=c(0,7000), 
     axes=FALSE, type="b", col="red")
## a little farther out (line=4) to make room for labels
mtext("Cell Density",side=4,col="red",line=4) 
axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## Draw the time axis
axis(1,pretty(range(time),10))
mtext("Time (Hours)",side=1,col="black",line=2.5)  

## Add Legend
legend("topleft",legend=c("Beta Gal","Cell Density"),
       text.col=c("black","red"),pch=c(16,15),col=c("black","red"))





ave(1:3)  # no grouping -> grand mean
ave(1:4)  # no grouping -> grand mean

attach(warpbreaks)
ave(breaks, wool)
ave(breaks, tension)
ave(breaks, tension, FUN = function(x) mean(x, trim = 0.1))
plot(breaks, main =
       "ave( Warpbreaks )  for   wool  x  tension  combinations")
lines(ave(breaks, wool, tension              ), type = "s", col = "blue")
lines(ave(breaks, wool, tension, FUN = median), type = "s", col = "green")
legend(40, 70, c("mean", "median"), lty = 1,
       col = c("blue","green"), bg = "gray90")
detach()
