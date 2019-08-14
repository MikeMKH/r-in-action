# 6
opar <- par(no.readonly=TRUE)

# 6.1
library(vcd)
counts <- table(Arthritis$Improved)
counts

barplot(counts, 
        main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")

barplot(counts, 
        main="Horizontal Bar Plot", 
        xlab="Frequency", ylab="Improvement", 
        horiz=TRUE)

# 6.2
library(vcd)
counts <- table(Arthritis$Improved, Arthritis$Treatment)
counts

barplot(counts, 
        main="Stacked Bar Plot",
        xlab="Treatment", ylab="Frequency", 
        col=c("red", "yellow","green"),            
        legend=rownames(counts)) 

barplot(counts, 
        main="Grouped Bar Plot", 
        xlab="Treatment", ylab="Frequency",
        col=c("red", "yellow", "green"),
        legend=rownames(counts), beside=TRUE)

# 6.3
states <- data.frame(state.region, state.x77)
means <- aggregate(states$Illiteracy, by=list(state.region), FUN=mean)
means

means <- means[order(means$x),]  
means

barplot(means$x, names.arg=means$Group.1) 
title("Mean Illiteracy Rate")  

# 6.4
par(las=2)
par(mar=c(5,8,4,2))
counts <- table(Arthritis$Improved)
counts

barplot(counts, 
        main="Treatment Outcome", horiz=TRUE, cex.names=0.8,
        names.arg=c("No Improvement", "Some Improvement", "Marked Improvement")
)
par(opar)

library(vcd)
attach(Arthritis)
counts <- table(Treatment,Improved)
counts
spine(counts, main="Spinogram Example")
detach(Arthritis)

# 6.5
par(mfrow=c(2,2))                             
slices <- c(10, 12,4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")
slices

pie(slices, labels = lbls, 
    main="Simple Pie Chart")

pct <- round(slices/sum(slices)*100)                      
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="")
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart with Percentages")

library(plotrix)                                               
pie3D(slices, labels=lbls,explode=0.1,
      main="3D Pie Chart ")

mytable <- table(state.region)
mytable
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart from a dataframe\n (with sample sizes)")

par(opar)

library(plotrix)
slices <- c(10, 12,4, 16, 8) 
lbls <- c("US", "UK", "Australia", "Germany", "France")   
fan.plot(slices, labels = lbls, main="Fan Plot")

# 6.6
summary(mtcars$mpg)
hist(mtcars$mpg)

hist(mtcars$mpg, 
     breaks=15, 
     col="red", 
     xlab="Miles Per Gallon", 
     main="Colored histogram with 12 bins")

hist(mtcars$mpg, 
     freq=FALSE, 
     breaks=20, 
     col="red", 
     xlab="Miles Per Gallon", 
     main="Histogram, rug plot, density curve")  
rug(jitter(mtcars$mpg)) 
lines(density(mtcars$mpg), col="blue", lwd=2)

x <- mtcars$mpg 
h<-hist(x, 
        breaks=12, 
        col="red", 
        xlab="Miles Per Gallon", 
        main="Histogram with normal curve and box") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)
box()

# 6.7
d <- density(mtcars$mpg)
plot(d)
d

d <- density(mtcars$mpg)                                  
plot(d, main="Kernel Density of Miles Per Gallon")       
polygon(d, col="red", border="blue")                     
rug(mtcars$mpg, col="brown") 

# 6.8
par(lwd=2)                                                       
library(sm)
attach(mtcars)

cyl.f <- factor(cyl, levels= c(4, 6, 8),                               
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 
summary(cyl.f)

sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")                
title(main="MPG Distribution by Car Cylinders")

colfill<-c(2:(2+length(levels(cyl.f)))) 
cat("Use mouse to place legend...","\n\n")
# legend(locator(1), levels(cyl.f), fill=colfill)
detach(mtcars)
par(lwd=1)

boxplot(mpg~cyl,data=mtcars,
        main="Car Milage Data", 
        xlab="Number of Cylinders", 
        ylab="Miles Per Gallon")

boxplot(mpg~cyl,data=mtcars, 
        notch=TRUE, 
        varwidth=TRUE,
        col="red",
        main="Car Mileage Data", 
        xlab="Number of Cylinders", 
        ylab="Miles Per Gallon")

# 6.9
mtcars$cyl.f <- factor(mtcars$cyl,
                       levels=c(4,6,8),
                       labels=c("4","6","8"))

mtcars$am.f <- factor(mtcars$am, 
                      levels=c(0,1), 
                      labels=c("auto","standard"))

boxplot(mpg ~ am.f *cyl.f, 
        data=mtcars, 
        varwidth=TRUE,
        col=c("gold", "darkgreen"),
        main="MPG Distribution by Auto Type", 
        xlab="Auto Type")

# 6.10
library(vioplot)
x1 <- mtcars$mpg[mtcars$cyl==4] 
x2 <- mtcars$mpg[mtcars$cyl==6]
x3 <- mtcars$mpg[mtcars$cyl==8]
vioplot(x1, x2, x3, 
        names=c("4 cyl", "6 cyl", "8 cyl"), 
        col="gold")
title("Violin Plots of Miles Per Gallon")

dotchart(mtcars$mpg,labels=row.names(mtcars),cex=.7,
         main="Gas Mileage for Car Models", 
         xlab="Miles Per Gallon")

# 6.11
x <- mtcars[order(mtcars$mpg),]                      
x$cyl <- factor(x$cyl)                                 
x$color[x$cyl==4] <- "red"                              
x$color[x$cyl==6] <- "blue"
x$color[x$cyl==8] <- "darkgreen"
summary(x)
dotchart(x$mpg,
         labels = row.names(x),                               
         cex=.7, 
         pch=19,                                              
         groups = x$cyl,                                       
         gcolor = "black",
         color = x$color,
         main = "Gas Mileage for Car Models\ngrouped by cylinder",
         xlab = "Miles Per Gallon")

par(opar)