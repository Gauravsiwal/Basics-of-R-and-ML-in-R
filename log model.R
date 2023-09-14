# Logistic Regression
View(mtcars)
summary(mtcars)

# Lets take vs column as a target variable 0 - V-shape engine
# 1 means straight engine.
library('caTools')
install.packages('ROCR')
library('ROCR')

# Correlation matrix
corr_mat = cor(mtcars)
View(corr_mat)
library('corrplot')
corrplot(corr_mat, method = c('color'),
         addCoef.col = 'black',
         tl.cex = 0.5,
         number.cex = 0.5)



# Splitting Dataset
split = sample.split(mtcars, SplitRatio = 0.8)

train_reg = subset(mtcars, split==TRUE)
test_reg = subset(mtcars, split==FALSE)

# Train the model
logistic_model = glm(vs~ cyl+disp+hp+wt, data=train_reg,
                     family = 'binomial')
summary(logistic_model)

# Predictions
y_pred = predict(logistic_model,test_reg, type = 'response')
View(y_pred)
y_pred = ifelse(y_pred>0.5,1,0)

predictions = data.frame(test_reg$vs,y_pred)
View(predictions)


# Confusion matrix
cm = table(test_reg$vs, y_pred)
accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
cat('Accuracy of the model is',accuracy)
cm

# ROC-AUC Curve
roc_pred = prediction(y_pred,test_reg$vs)
View(roc_pred)
perf = performance(roc_pred,measure = 'tpr',x.measure = 'fpr')
plot(perf)

# AUC
perf_auc = performance(roc_pred, measure = 'auc')
perf_auc = perf_auc@y.values[[1]]
cat('Area Under the curve is',perf_auc)

# ACCURACY, PRECISION, RECALL
accu = performance(roc_pred,measure='acc')
View(accu)
accu = accu@y.values[[1]][[2]]
cat('Accuracy is',accu)

recal = performance(roc_pred, measure = 'rec')
View(recal)
recal = recal@y.values[[1]][[2]]
cat('Recall is',recal)



















