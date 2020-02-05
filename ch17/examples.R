# 17.1
loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"
ds  <- "breast-cancer-wisconsin/breast-cancer-wisconsin.data"
url <- paste(loc, ds, sep="")
breast <- read.table(url, sep=",", header=FALSE, na.strings="?")
names(breast) <- c("ID", "clumpThickness", "sizeUniformity",
                   "shapeUniformity", "maginalAdhesion",
                   "singleEpithelialCellSize", "bareNuclei",
                   "blandChromatin", "normalNucleoli", "mitosis", "class")

df <- breast[-1]
df$class <- factor(df$class, levels=c(2,4),
                   labels=c("benign", "malignant"))
summary(df)
pairs(df)

set.seed(1234)
train <- sample(nrow(df), 0.7*nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

# 17.2
fit.glm <- glm(class~., data=df.train, family=binomial())
summary(fit.glm)

p <- predict(fit.glm, df.validate, type="response")
glm.pred <- factor(p > 0.5, levels=c(FALSE, TRUE), labels=c("benign", "malignant"))
(glm.perf <- table(df.validate$class, glm.pred, dnn=c("Actual", "Predicted")))

fit.glm.reduced <- step(fit.glm)
summary(fit.glm.reduced)

p <- predict(fit.glm.reduced, df.validate, type="response")
glm.reduced.pred <- factor(p > 0.5, levels=c(FALSE, TRUE), labels=c("benign", "malignant"))
(glm.reduced.perf <- table(df.validate$class, glm.reduced.pred, dnn=c("Actual", "Predicted")))

glm.perf
glm.reduced.perf