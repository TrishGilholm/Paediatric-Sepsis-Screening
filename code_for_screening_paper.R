###############################################
# Analysis Script for Screening Paper         #
# Author: Patricia Gilholm                    #
# Date: 6th August 2021                       #                         
###############################################

#libraries required
library(tidyverse)#dplyr for data manipulation and ggplot2
library(gtsummary)#easy to read regression tables
library(caret)#folds for cross validation
library(pROC)#ROC curves
library(epiR)#model performance statistics

###################Results##################
############Assessment of Predictors########
########Individual Block Evaluations#######

####Block 1: Sepsis indicators####
#Fit model using only Sepsis indicators
glm.fit_block1 <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
                        paed_sepsis_indicator___3 + paed_sepsis_indicator___4 + paed_sepsis_indicator___5 + 
                        paed_sepsis_indicator___6 + paed_sepsis_indicator___7 + paed_sepsis_indicator___8 + 
                        paed_sepsis_indicator___9 ,
                        data = data,
                        family = "binomial")


summary(glm.fit_block1)
#create table of odds ratios and confidence intervals
tbl_regression(glm.fit_block1, exponentiate = TRUE)

#validate the model using 10-fold cross validation
#create folds
folds <- createFolds(data$suspected_sepsis, k = 10, list = TRUE, returnTrain = FALSE)

#run K-fold cross-validation
results_lr<-list()

for(i in 1:10){
  #remove fold to validate 
  fold<-folds[[i]]
  training_data <- data[fold, ]
      
 #select  validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
 #Fit model to training data and get predictions using validation set 
  glm.fit <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 +
                                    paed_sepsis_indicator___3 + paed_sepsis_indicator___4 + 
                                    paed_sepsis_indicator___5 + paed_sepsis_indicator___6 + 
                                    paed_sepsis_indicator___7 + paed_sepsis_indicator___8 +
                                    paed_sepsis_indicator___9,
                 data = training_data,
                 family = "binomial")
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr[[i]] <-glm.probs
}

#Produce the ROC CURVE of predictions

#Get the truth from the validated data sets
truth<-list()

for(i in 1:10){
  #remove subject to validate
  fold<-folds[[i]]
  validation_data <- data[-fold, which(names(random_forest_data) == "sepsis")]
  validation_data<-as.vector(as.numeric(validation_data))
  truth[[i]] <- validation_data
}

#ROC curve
truth<-unlist(truth)
results_lr<-unlist(results_lr)

results_roc<-cbind(truth, results_lr )
results_roc<-as.data.frame(results_roc)


roc_result_block1 <- roc(results_roc$truth,results_roc$results_lr,smoothed = TRUE,
                         # arguments for ci
                         ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                         # arguments for plot
                         plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                         print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

####Block 2: Sepsis Factors####
#Fit model using only Sepsis risk factors
glm.fit_block2 <- glm(suspected_sepsis~  pead_sepsis_factors___1 + pead_sepsis_factors___2 + 
                        pead_sepsis_factors___3 + pead_sepsis_factors___4 + pead_sepsis_factors___5 + 
                        pead_sepsis_factors___6 ,
                      data = data,
                      family = "binomial")

summary(glm.fit_block2)
#table of odds ratios and confidence intervals
tbl_regression(glm.fit_block2, exponentiate = TRUE)
#validate the model using 10-fold cross validation
#the same folds were used for all 10-fold cross validation procedures
#run K-fold cross validation
results_lr<-list()


for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  
  
  #select validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis ~  pead_sepsis_factors___1 + pead_sepsis_factors___2 + 
                   pead_sepsis_factors___3 + pead_sepsis_factors___4 + pead_sepsis_factors___5 + 
                   pead_sepsis_factors___6 ,
                 data = training_data,
                 family = "binomial")
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr[[i]] <-glm.probs
}

#Calculate ROC curve


truth<-unlist(truth)
results_lr<-unlist(results_lr)

results_roc<-cbind(truth, results_lr )
results_roc<-as.data.frame(results_roc)

roc_result_block2 <- roc(results_roc$truth,results_roc$results_lr,smoothed = TRUE,
                         # arguments for ci
                         ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                         # arguments for plot
                         plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                         print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

####Block 3: Severe illness features####
#fit model only using severe illness features
glm.fit_block3 <- glm(sepsis ~  severe_illness_feature___1 + severe_illness_feature___2 + 
                        severe_illness_feature___3 + severe_illness_feature___4 + 
                        severe_illness_feature___5 + severe_illness_feature___6 + 
                        severe_illness_feature___7 + severe_illness_feature___8,
                      data = data,
                      family = "binomial")

summary(glm.fit_block3)
#table of odds ratios and confidence intervals
tbl_regression(glm.fit_block3, exponentiate = TRUE)

#validate using 10-fold corss validation
#the same folds are used for all cross validation evaluations
#run K-fold cross validationn

results_lr<-list()


for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  #select  validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis ~  severe_illness_feature___1 + severe_illness_feature___2 + 
                   severe_illness_feature___3 + severe_illness_feature___4 + 
                   severe_illness_feature___5 + severe_illness_feature___6 + 
                   severe_illness_feature___7 + severe_illness_feature___8,
                 data = training_data,
                 family = binomial)
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr[[i]] <-glm.probs
}

#produce ROC curve of predictons
truth<-unlist(truth)
results_lr<-unlist(results_lr)

results_roc<-cbind(truth, results_lr )
results_roc<-as.data.frame(results_roc)

roc_result_block3 <- roc(results_roc$truth,results_roc$results_lr,smoothed = TRUE,
                         # arguments for ci
                         ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                         # arguments for plot
                         plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                         print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

####Block 4: Moderate illness features####
#Fit model only using the moderate illness features 
glm.fit_block4 <- glm(sepsis~
                        moderate_illness_feature___1 + moderate_illness_feature___2 + 
                        moderate_illness_feature___3 + moderate_illness_feature___4 + 
                        moderate_illness_feature___5 + moderate_illness_feature___6 + 
                        moderate_illness_feature___7 + moderate_illness_feature___8 + 
                        moderate_illness_feature___9,
                      data = data,
                      family = "binomial")

summary(glm.fit_block4)
#table of odds ratios and confidence intervals
tbl_regression(glm.fit_block4, exponentiate = TRUE)

#validate using 10 fold cross validation
#the same folds are used for all validation procedures
#Perform K-fold cross-validation
results_lr<-list()

for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
 #select  the validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis~ moderate_illness_feature___1 + moderate_illness_feature___2 + 
                   moderate_illness_feature___3 + moderate_illness_feature___4 + 
                   moderate_illness_feature___5 + moderate_illness_feature___6 + 
                   moderate_illness_feature___7 + moderate_illness_feature___8 + 
                   moderate_illness_feature___9,
                 data = training_data,
                 family = "binomial")
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr[[i]] <-glm.probs
}

#Produce ROC curve of predictions
truth<-unlist(truth)
results_lr<-unlist(results_lr)

results_roc<-cbind(truth, results_lr )
results_roc<-as.data.frame(results_roc)


roc_result_block4 <- roc(results_roc$truth,results_roc$results_lr,smoothed = TRUE,
                         # arguments for ci
                         ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                         # arguments for plot
                         plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                         print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

#Overlay ROC curves in one figure
All_ROC<-ggroc(list(`Block 1: Sepsis Indicators` = roc_result_block1, `Block 2: Sepsis Risk Factors` = roc_result_block2, 
                    `Block 3: Severe Illness Features` = roc_result_block3, `Block 4: Moderate Illness Features` = roc_result_block4),
               alpha = 0.5,linetype = 1, size = 2, legacy.axes = TRUE)+
  geom_abline(intercept = 0, slope = 1,
              color = "darkgrey", linetype = "dashed")+
  theme_bw()+
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
  theme(legend.position = c(0.8,0.25))+
  labs(x = "1 - Specificity", y = "Sensitivity", colour = "Model")+
  theme(axis.line = element_line(colour = "black"))+
  theme(plot.margin=unit(c(.2,.5,.2,.2),"cm"))


###Full model###
#calculate AUC of model using all 32 predictors - the same 10 folds are used.
results_full_model<-list()

#logistic regression SMOTE
for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  #select features in the validation set
  validation_data <- random_forest_data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 + 
                   paed_sepsis_indicator___3 + paed_sepsis_indicator___4 + paed_sepsis_indicator___5 + 
                   paed_sepsis_indicator___6 + paed_sepsis_indicator___7 + paed_sepsis_indicator___8 + 
                   paed_sepsis_indicator___9 + pead_sepsis_factors___1 + pead_sepsis_factors___2 + 
                   pead_sepsis_factors___3 + pead_sepsis_factors___4 + pead_sepsis_factors___5 + 
                   pead_sepsis_factors___6 + severe_illness_feature___1 + severe_illness_feature___2 + 
                   severe_illness_feature___3 + severe_illness_feature___4 + 
                   severe_illness_feature___5 + severe_illness_feature___6 + 
                   severe_illness_feature___7 + severe_illness_feature___8 + 
                   moderate_illness_feature___1 + moderate_illness_feature___2 + 
                   moderate_illness_feature___3 + moderate_illness_feature___4 + 
                   moderate_illness_feature___5 + moderate_illness_feature___6 + 
                   moderate_illness_feature___7 + moderate_illness_feature___8 + 
                   moderate_illness_feature___9,
                 data = training_data_formula,
                 family = binomial)
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_full_model[[i]] <-glm.probs
}

#calculate AUC
truth<-unlist(truth)
results_full_model<-unlist(results_full_model)

results_roc<-cbind(truth, results_full_model )
results_roc<-as.data.frame(results_roc)

library(pROC)
roc_result_full_model <- roc(results_roc$truth,results_roc$results_full_model,smoothed = TRUE,
                     # arguments for ci
                     ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                     # arguments for plot
                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                     print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

##################Derivation of final model############################

#Start with full model and then perform backwards selection
full_model <- glm(sepsis ~ paed_sepsis_indicator___1 + paed_sepsis_indicator___2 +
                                     paed_sepsis_indicator___3 + paed_sepsis_indicator___4 +
                                     paed_sepsis_indicator___5 + paed_sepsis_indicator___6 +
                                     paed_sepsis_indicator___7 + paed_sepsis_indicator___8 +
                                     paed_sepsis_indicator___9 + pead_sepsis_factors___1 +
                                     pead_sepsis_factors___2 +   pead_sepsis_factors___3 +
                                     pead_sepsis_factors___4 +   pead_sepsis_factors___5 +
                                     pead_sepsis_factors___6 +   severe_illness_feature___1 +
                                     severe_illness_feature___2 +severe_illness_feature___3 + 
                                     severe_illness_feature___4 +severe_illness_feature___5 + 
                                     severe_illness_feature___6 +severe_illness_feature___7 +
                                     severe_illness_feature___8 +moderate_illness_feature___1 + 
                                     moderate_illness_feature___2+moderate_illness_feature___3+
                                     moderate_illness_feature___4+moderate_illness_feature___5+
                                     moderate_illness_feature___6+ moderate_illness_feature___7+
                                     moderate_illness_feature___8+moderate_illness_feature___9,
                                     data = data,
                                     family = "binomial")

summary(full_model)
backwards <- step(full_model)
#show selected features from backward selection
formula(backwards)
#display results of final model
summary(backwards)
#table of odds ratios and confidence intervals 
tbl_regression(backwards, intercept = TRUE, estimate_fun = function(x) style_ratio(x, digits = 2))

#Evaluate model using 10-fold cross validation
#The same folds are used for all cross validation procedures
results_lr_2<-list()

for(i in 1:10){
  #remove fold to validate
  fold<-folds[[i]]
  training_data <- data[fold, ]
  #select  validation set
  validation_data <- data[-fold, -which(names(data) == "sepsis")]
  glm.fit <- glm(sepsis~  paed_sepsis_indicator___2 + paed_sepsis_indicator___4 +
                                    paed_sepsis_indicator___5 + pead_sepsis_factors___1 +
                                    pead_sepsis_factors___4 +  pead_sepsis_factors___6 +
                                    severe_illness_feature___1 + severe_illness_feature___2 +
                                    severe_illness_feature___3 + severe_illness_feature___4 +
                                    severe_illness_feature___5 +severe_illness_feature___6 +
                                    severe_illness_feature___7 + severe_illness_feature___8 +
                                    moderate_illness_feature___3 + moderate_illness_feature___7+
                                    moderate_illness_feature___9,
                                    data = data,
                                   family = "binomial")
  glm.probs <- predict(glm.fit, validation_data, type = "response")
  results_lr_2[[i]] <-glm.probs
}

#Produce ROC curve for predictions
results_lr_2<-unlist(results_lr_2)
truth<-unlist(truth)

results_roc<-cbind(truth, results_lr_2 )
results_roc<-as.data.frame(results_roc)

roc_result_backward <- roc(results_roc$truth,results_roc$results_lr_2,smoothed = TRUE,
                     # arguments for ci
                     ci=TRUE, ci.alpha=0.95, stratified=FALSE,
                     # arguments for plot
                     plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                     print.auc=TRUE, show.thres=TRUE,  print.thres="best", print.thres.best.method="youden")

####Model Performance####
#Calibration Plot#
#Code from "Evaluating a logistic regression based prediction tool in R" by Darren L. Dahly
#URL: https://darrendahly.github.io/post/homr/
g1<-mutate(results_roc, bin = ntile(results_lr_2, 10))

g1%<>%
  group_by(bin)%>%
  mutate(n=n(),
         bin_pred = mean(results_lr_2),
         bin_prob = mean(as.numeric(truth) -1),
         se = sqrt((bin_prob * (1-bin_prob))/ n),
         u1 = bin_prob +1.96 * se,
         l1 = bin_prob - 1.96 * se)%>%
  ungroup()


plot1<-ggplot(data = g1, aes(x = bin_pred, y = bin_prob, ymin = l1, ymax = u1)) +
  geom_pointrange(size = 0.5, color = "black") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  geom_abline() + # 45 degree line indicating perfect calibration
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed", 
  #color = "black", formula = y~-1 + x) + 
  # straight line fit through estimates
  geom_smooth(aes(x = results_lr_2, y = as.numeric(truth) - 1), 
              color = "red", se = FALSE, method = "loess") + 
  # loess fit through estimates
  xlab("") +
  ylab("Observed Probability") +
  theme_bw() 

plot2 <- ggplot(g1, aes(x = results_lr_2)) +
  geom_histogram(fill = "black", bins = 200) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  xlab("Predicted Probability") +
  ylab("") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 400)) +
  theme(panel.grid.minor = element_blank())

g <- arrangeGrob(plot1, plot2, respect = TRUE, heights = c(1, 0.25), ncol = 1)

grid.newpage()
grid.draw(g)

#Extract snesitivities and specificities from the ROC curve and calculate PLR and NLR
sensitivity<-roc_result_backward$sensitivities
specificity<-roc_result_backward$specificities
thresholds<-roc_result_backward$thresholds

roc_result_backward<-cbind(sensitivity, specificity, thresholds)
roc_result_backward<-as.data.frame(roc_result_backward)

roc_result_backward<-mutate(roc_result_backward, pos_LR = sensitivity / (1-specificity),
                     neg_LR = (1-sensitivity) / specificity)



