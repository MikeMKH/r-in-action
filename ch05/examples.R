# 5.1
x <- c(1, 2, 3, 4, 5, 6, 7, 8)
mean(x)
sd(x)
n <- length(x)
meanx <- sum(x)/n
css <- sum((x - meanx)**2)            
sdx <- sqrt(css / (n-1))
meanx
sdx

runif(5)
runif(5)
set.seed(1234)                                                     
runif(5)
set.seed(1234)                                                      
runif(5)

# 5.3
library(MASS)
mean <- c(230.7, 146.7, 3.6)                                           
sigma <- matrix( c(15360.8, 6721.2, -47.1,                              
                   6721.2, 4700.9, -16.5,
                   -47.1,  -16.5,   0.3), nrow=3, ncol=3)
set.seed(1234)
mydata <- mvrnorm(500, mean, sigma)                                     
mydata <- as.data.frame(mydata)                                         
names(mydata) <- c("y", "x1", "x2")                                       
dim(mydata)                                                             
head(mydata, n=10)
rm(list=ls())

# 5.4
a <- 5
sqrt(a)
b <- c(1.243, 5.654, 2.99)
round(b)
c <- matrix(runif(12), nrow=3)
c
log(c)
mean(c)
sd(c)

# 5.5
set.seed(1234)
mydata <- matrix(rnorm(30), nrow=6)
mydata
apply(mydata, 1, mean)
apply(mydata, 2, mean)
apply(mydata, 2, mean, trim=.4)
apply(mydata, 1, sd)
apply(mydata, 2, sd)

# 5.6
options(digits=2)
Student <- c("John Davis", "Angela Williams", "Bullwinkle Moose",
             "David Jones", "Janice Markhammer", "Cheryl Cushing",
             "Reuven Ytzrhak", "Greg Knox", "Joel England",
             "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)

roster <- data.frame(Student, Math, Science, English,
                     stringsAsFactors=FALSE)
pairs(roster[,2:4])

score <- apply(scale(roster[,2:4]), 1, mean)
roster <- cbind(roster, score)
pairs(roster[,2:5])

q <- quantile(score, c(.8,.6,.4,.2))
q
roster$grade[score >= q[1]] <- "A"
roster$grade[score < q[1] & score >= q[2]] <- "B"
roster$grade[score < q[2] & score >= q[3]] <- "C"
roster$grade[score < q[3] & score >= q[4]] <- "D"
roster$grade[score < q[4]] <- "F"

name <- strsplit((roster$Student), " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)
roster <- cbind(Firstname,Lastname, roster[,-1])
roster <- roster[order(Lastname,Firstname),]
roster

# 5.9
cars <- mtcars[1:5, 1:4]      
cars
t(cars)

# 5.10
options(digits=3)
attach(mtcars)
aggdata <-aggregate(mtcars, by=list(cyl,gear), 
                    FUN=mean, na.rm=TRUE)
aggdata
aggdata <-aggregate(mtcars, by=list(cyl,gear), 
                    FUN=sd, na.rm=TRUE)
aggdata

rm(list=ls())
library(reshape2)
mydata <- read.table(header=TRUE, sep=" ", text="
ID Time X1 X2
1 1 5 6
1 2 3 5
2 1 6 1
2 2 2 4
")
mydata

md <- melt(mydata, id=c("ID", "Time"))

dcast(md, ID~variable, mean)
dcast(md, Time~variable, mean)
dcast(md, ID~Time, mean)

dcast(md, ID~variable, sd)
dcast(md, Time~variable, sd)
dcast(md, ID~Time, sd)

dcast(md, ID+Time~variable)
dcast(md, ID+variable~Time)
dcast(md, ID~variable+Time)