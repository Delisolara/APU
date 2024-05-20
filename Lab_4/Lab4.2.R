install.packages("randomForest")
install.packages("e1071")
install.packages("party")
install.packages("mlr")
install.packages("rFerns")

library(randomForest)
library(e1071)
library(party)
library(mlr)
library(rFerns)

df=read.csv("D:/Studia/APU/smartfony.csv")
df <- df [1:8]
df$nazwa = factor(df$nazwa)
df$ocena_klientow = factor(df$ocena_klientow)

summarizeColumns(df)

rdesc = makeResampleDesc("CV", iters = 10)

task = makeClassifTask(id = deparse(substitute(df)), df, "ocena_klientow",
                       weights = NULL, blocking = NULL, coordinates = NULL,
                       positive = NA_character_, fixup.data = "warn", check.data = TRUE)
lrns <- makeLearners(c("rpart", "C50", "ctree", "naiveBayes", "randomForest"), type = "classif")

bmr <- benchmark(learners = lrns, tasks = task, rdesc, models = TRUE, measures = list(acc, ber))
p = getBMRPredictions(bmr)
plotBMRSummary(bmr)

