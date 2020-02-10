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
# pairs(df)

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

# 17.3
library(rpart)
set.seed(1234)
dtree <- rpart(class~., data=df.train, method="class",
               parms=list(split="information"))
dtree$cptable
plotcp(dtree)
dtree.prune = prune(dtree, cp=0.024)

library(rpart.plot)
prp(dtree.prune, type=2, extra=104,
    fallen.leaves=TRUE, main="Decision Tree")

dtree.pred <- predict(dtree, df.validate, type="class")
(dtree.pref <- table(df.validate$class,
                     dtree.pred, dnn=c("Actual", "Prediction")))

dtree.prune.pred <- predict(dtree.prune, df.validate, type="class")
(dtree.prune.pref <- table(df.validate$class,
                           dtree.prune.pred, dnn=c("Actual", "Prediction")))

dtree.smaller = prune(dtree, cp=0.0125)
prp(dtree.smaller, type=2, extra=104,
    fallen.leaves=TRUE, main="Decision Tree")

library(party)
fit.ctree <- ctree(class~., data=df.train)
plot(fit.ctree, main="Conditional Inference Tree")

ctree.pred <- predict(fit.ctree, df.validate, type="response")
(ctree.perf <- table(df.validate$class, ctree.pred,
                    dnn=c("Actual", "Predicted")))

library(partykit)
plot(as.party(dtree.prune), main="Decision Tree")

# 17.4
library(randomForest)
set.seed(1234)
(fit.forest <- randomForest(class~., data=df.train,
                           importance=TRUE,
                           na.action=na.roughfix))
importance(fit.forest, type=2)

forest.pred <- predict(fit.forest, df.validate)
(forest.pref <- table(df.validate$class, forest.pred,
                      dnn=c("Actual", "Predicted")))

# 17.5
library(e1071)
set.seed(1234)
(fit.svm <- svm(class~., data=df.train))

svm.pred <- predict(fit.svm, na.omit(df.validate))
(svm.pref <- table(na.omit(df.validate)$class, svm.pred,
                   dnn=c("Actual", "Predicted")))

set.seed(1234)
(tuned <- tune.svm(class~., data=df.train,
                   gamma=10^(-8:1),
                   cost=10^(-10:10)))

(fit.tuned.svm <- svm(class~., data=df.train,
                      gamma=tuned$best.parameters$gamma,
                      cost=tuned$best.parameters$cost))

svm.tuned.pred <- predict(fit.tuned.svm, na.omit(df.validate))
(svm.tuned.pref <- table(na.omit(df.validate)$class, svm.tuned.pred,
                   dnn=c("Actual", "Predicted")))

svm.pref
svm.tuned.pref

# 17.6
performance <- function(table, n=2) {
  if(!all(dim(table) == c(2,2)))
    stop("Must be 2x2 table")
  tn = table[1,1]
  fp = table[1,2]
  fn = table[2,1]
  tp = table[2,2]
  sensitivity = tp/(tp+fn)
  specificity = tn/(tn+fp)
  ppp = tp/(tp+fp)
  npp = tn/(tn+fn)
  hitrate = (tp+tn)/(tp+tn+fp+fn)
  
  result <- "result"
  result$sensitivity <- sensitivity
  result$specificity <- specificity
  result$ppp <- ppp
  result$npp <- npp
  result$hitrate <- hitrate
  return(result)
}

(glm.performance <- performance(glm.perf))
(glm.reduced.performance <- performance(glm.reduced.perf))
(ctree.performance <- performance(ctree.perf))
(forest.performance <- performance(forest.pref))
(svm.performance <- performance(svm.pref))
(svm.tuned.performance <- performance(svm.tuned.pref))

glm.performance$hitrate
glm.reduced.performance$hitrate
ctree.performance$hitrate
forest.performance$hitrate
svm.performance$hitrate
svm.tuned.performance$hitrate