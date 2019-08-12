# 4
manager <- c(1,2,3,4,5)
date <- c("10/24/08","10/28/08","10/1/08","10/12/08","5/1/09")
gender <- c("M","F","F","M","F")
age <- c(32,45,25,39,99)
q1 <- c(5,3,3,3,2)
q2 <- c(4,5,5,3,2)
q3 <- c(5,2,5,4,1)
q4 <- c(5,5,5,NA,2)
q5 <- c(5,5,2,NA,1)
leadership <- data.frame(manager,date,gender,age,q1,q2,q3,q4,q5, 
                         stringsAsFactors=FALSE)
summary(leadership)

# 4.2
mydata<-data.frame(x1 = c(2, 2, 6, 4),
                   x2 = c(3, 4, 2, 8))

mydata$sumx <- mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2
mydata$avgx <- mydata$sumx / length(mydata)

attach(mydata)
mydata$sumx <- x1 + x2
mydata$meanx <- (x1 + x2)/2
mydata$avgx <- sumx / length(mydata)
detach(mydata)

mydata <- transform(mydata,
                    sumx = x1 + x2,
                    meanx = (x1 + x2)/2,
                    avgx = sumx / length(mydata))

# recording variables
leadership$agecat[leadership$age > 75] <- "Elder"
leadership$agecat[leadership$age >= 55 &
                    leadership$age <= 75] <- "Middle Aged"
leadership$agecat[leadership$age < 55] <- "Young"

leadership <- within(leadership,{
  agecat <- NA
  agecat[age > 75] <- "Elder"
  agecat[age >= 55 & age <= 75] <- "Middle Aged"
  agecat[age < 55] <- "Young" })

# renaming variables
names(leadership)
names(leadership)[2] <- "testDate"
names(leadership)

names(leadership)
names(leadership)[1] <- "manager"
names(leadership)[2] <- "date"
names(leadership)

library(plyr)
leadership <- rename(leadership,
                     c(manager="managerID", date="testDate"))

# exclude NA from calculations
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4]

z <- sum(x)
z

z <- sum(x, na.rm=TRUE)
z

# 4.4
is.na(leadership[, 6:10])

leadership[age == 99, "age"] <- NA
leadership

leadership
newdata <- na.omit(leadership)
newdata

# dates
mydates <- as.Date(c("2007-06-22", "2004-02-13"))

strDates <- c("12/02/1980", "12/31/1980")
dates <- as.Date(strDates, "%m/%d/%Y")

today <- Sys.Date()
format(today, format="%B %d %Y")
format(today, format="%A")

format(as.Date("1980-12-02"), format="%A")
format(as.Date("1980-12-31"), format="%A")

startdate <- as.Date("2004-02-13")
enddate   <- as.Date("2009-06-22")
enddate - startdate

today <- Sys.Date()
dob <- as.Date("1980-12-02")
difftime(today, dob, units="weeks")

# 4.5
a <- c(1,2,3)
a
is.numeric(a)
is.vector(a)
is.character(a)

a <- as.character(a)
a
is.numeric(a)
is.vector(a)
is.character(a)

# sorting
newdata <- leadership[order(leadership$age),]

attach(leadership)
newdata <- leadership[order(gender, age),]
detach(leadership)

attach(leadership)
newdata <- leadership[order(gender, -age),]
detach(leadership)

# selecting
newdata <- leadership[, c(6:10)]
newdata

myvars <- c("q1", "q2", "q3", "q4", "q5")
newdata <-leadership[myvars]
newdata

myvars <- paste("q", 1:5, sep="")
newdata <- leadership[myvars]
newdata

myvars <- names(leadership) %in% c("q3", "q4") 
leadership[!myvars]

# 4.6
newdata <- leadership[1:3,]
newdata

newdata <- leadership[leadership$gender=="M" &
                        leadership$age > 30,]
newdata

attach(leadership)
newdata <- leadership[gender=='M' & age > 30,]
detach(leadership)
newdata

newdata <- subset(leadership, age >= 35 | age < 24,
                  select=c(q1, q2, q3, q4))
newdata

newdata <- subset(leadership, gender=="M" & age > 25,
                  select=gender:q4)
newdata

# 4.7
library(sqldf)
newdf <- sqldf("select * from mtcars
                where carb=1 order by mpg",
               row.names=TRUE)
newdf

sqldf("select
         avg(mpg) as avg_mpg
        ,avg(disp) as avg_disp
        ,gear
        from mtcars
        where cyl in (4, 6)
        group by gear")