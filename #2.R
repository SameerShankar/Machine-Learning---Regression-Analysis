#1
diabet = read.table(file = "C:/Users/Personal/Downloads/diabet.csv", sep = ",", 
                    header = T)
y = diabet$outcome
x1 = diabet$glucose
x2 = diabet$bmi
x3 = diabet$agegroup

#a
glm1 <- glm(outcome ~ glucose + bmi + agegroup, family = binomial, data = diabet)
summary(glm1)

#b
qt(0.975, 394)

#c
summary(glm1)
logLik(glm1)

#d
glm2 <- glm(outcome ~ glucose * bmi * agegroup, family = binomial, data = diabet)
summary(glm2)

#e
newA = data.frame(glucose = mean(x1), bmi = mean(x2), agegroup = "A")
newB = data.frame(glucose = mean(x1), bmi = mean(x2), agegroup = "B")
newC = data.frame(glucose = mean(x1), bmi = mean(x2), agegroup = "C")
newD = data.frame(glucose = mean(x1), bmi = mean(x2), agegroup = "D")

newpredA <- predict(glm1, newA, type = "response")
newpredB <- predict(glm1, newB, type = "response")
newpredC <- predict(glm1, newC, type = "response")
newpredD <- predict(glm1, newD, type = "response")

#f
smp_size <- floor(0.75*nrow(diabet))
set.seed(123)
train_ind <- sample(seq_len(nrow(diabet)), size = smp_size)
train <- diabet[train_ind,]
test <- diabet[-train_ind,]

install.packages("caret")
library(caret)
install.packages("ROCR")
library(ROCR)

# confusionMatrix for train
train_logit <- glm(formula = outcome ~ glucose + bmi + agegroup, 
                   family = "binomial", data = train)
predic.test = predict(train_logit, train, type = "response")

predic.test[predic.test <= 0.5] = 0
predic.test[predic.test > 0.5] = 1

confusionMatrix(factor(train$outcome), factor(predic.test))

# confusionMatrix for test
test_logit <- glm(formula = outcome ~ glucose + bmi + agegroup, 
                  family = "binomial", data = test)
predic.test = predict(test_logit, test, type = "response")

predic.test[predic.test <= 0.5] = 0
predic.test[predic.test > 0.5] = 1

confusionMatrix(factor(test$outcome), factor(predic.test))

#g
pred = prediction(test_logit$fitted.values,test$outcome)
roc = performance(pred, measure="tpr", x.measure="fpr")
plot(roc,colorize=TRUE, main = "ROC curve for test dataset")

#2
library(faraway)

#a
lm_full <- lm(formula = gamble ~ sex*income + status + verbal, data = teengamb)
summary(lm_full)

#b
lm_reduced <- lm(formula = gamble ~ income + income:sex, data = teengamb)
summary(lm_reduced)

#c
sample_size <- 30
set.seed(123)
train_ind_2 <- sample(seq_len(nrow(teengamb)) , size = sample_size)
train_2 <- teengamb[train_ind_2,]
test_2 <- teengamb[-train_ind_2,]
summary(train_2)

#e
train_lm_full <- lm(formula = gamble ~ sex*income + status + verbal, data = train_2)
predict_train_full = predict(train_lm_full, test_2, type = "response")
RMSE_full <- sqrt(mean((test2$gamble - predict_train_full)^2))

#f
train_lm_reduced <- lm(formula = gamble ~ income + income:sex, data = train_2)
predict_train_reduced = predict(train_lm_reduced, test_2, type = "response")
RMSE_reduced <- sqrt(mean((test2$gamble - predict_train_reduced)^2))

