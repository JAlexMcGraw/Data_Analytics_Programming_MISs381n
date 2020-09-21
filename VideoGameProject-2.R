#working directory has the file in it. 
games <- read.csv('games.csv')

#drop nas 
games <- drop_na(games)

#make variables as numbers, but setting them as strings first so the function doesnt change the years/ratings. 
games$Year_of_Release <- as.numeric(as.character(games[,3]))
games$User_Score <- as.numeric(as.character(games[,13]))

#grabbing everything above 2010, use this to take out 0 values for sales??? 
newdata <- subset(games, Year_of_Release >= 2010)
newdata <- newdata[-c(733,2045),]
newdata
#these are jsut linear regression.
model1 <- lm(NA_Sales~Platform+Genre+Publisher+ Rating + Developer+ User_Score + Critic_Score, data=newdata)
summary(model1)

model2 <- lm(NA_Sales~Platform+Genre+Publisher + Developer+ User_Score + Critic_Score, data=newdata)
summary(model2)


model3 <- lm(NA_Sales~Platform+Genre+Publisher, data=newdata)
summary(model3)

model4 <- lm(NA_Sales~Platform+Genre+Publisher+Developer, data=newdata)
summary(model4)



#RegSubsets.
set.seed(1)
n = nrow(newdata)
tr = sample(1:n, #Sample indices do be used in training
            size = 2194, #Sample will have 5000 observation
            replace = FALSE) #Without replacement



null = lm(NA_Sales~1, data=newdata[tr,]) #only has an intercept
full = glm(NA_Sales~Platform+Genre+Publisher+ Rating + Developer+ User_Score + Critic_Score, data=newdata[tr,]) #Has all the selected variables

#Variable selection

regForward = step(null, #The most simple model
                  scope=formula(full),#Let us analyze all models until the full model
                  direction="forward", #Adding variables
                  k=log(length(tr))) #This is BIC
regBack = step(full, #Starting with the full model
               direction="backward", #And deleting variables
               k=log(length(tr))) #This is BIC
reghybrid = step(null, #The most simple model
                  scope=formula(full), #The most complicated model
                  direction="both", #Add or delete variables
                  k=log(length(tr))) #This is BIC

##
## This is the best regression model for the NA region.
##

best_model <- lm(NA_Sales ~ Critic_Score + User_Score + Platform + Rating, data = newdata)
summary(best_model)

##
## This is the best regression model for the NA region.
##


#Lasso
#Let us scale our model matrix
X <- model.matrix(model1, data=newdata)

library(glmnet)

#Another way of choosing variables: LASSO and RIDGE
Lasso.Fit = glmnet(X[tr,],newdata$NA_Sales[tr],alpha=1)


par(mfrow=c(1,2)) #Plot window: 1 row, 2 columns

#Evaluating LASSO and RIDGE
plot(Lasso.Fit)

CV.L = cv.glmnet(X[tr,], newdata$NA_Sales[tr],alpha=1) #For Lasso
LamL = CV.L$lambda.1se
CV.L$lambda.min

coef.L = predict(CV.L,type="coefficients",s=LamL)
coef.L 

lasso_model <- lm(NA_Sales ~ Critic_Score + Developer + Platform + Genre + Publisher,data = newdata)
summary(lasso_model)

#plots
plot(newdata$User_Score,newdata$NA_Sales)


#regression CV.
set.seed(1)
train = sample(1:n, #Sample indices do be used in training
            size = 659, #Sample will have 5000 observation
            replace = FALSE) #Without replacement
2194*.30

training.set <- newdata[-train,] # Bigger, to give a better chance to our model...
test.set <- newdata[train,] # Smaller, to test our model

#CV Best regsubset model
best_model_cv <- lm(NA_Sales ~ Critic_Score + User_Score + Platform + Rating, data = training.set)
OOS <- predict(best_model_cv, newdata = test.set)

#SofE
sum((OOS-test.set$NA_Sales)^2)

#MSE
mean((OOS-test.set$NA_Sales)^2)


#CV Lasso Model
lasso_model_cv <- lm(NA_Sales ~ Critic_Score + Developer + Platform + Genre + Publisher,data = training.set)
OOS_lasso <- predict(lasso_model_cv, newdata = test.set)

#SofE
sum((OOS_lasso-test.set$NA_Sales)^2)

#MSE
mean((OOS_lasso-test.set$NA_Sales)^2)



### do these plots again
###
###
###


##GGPLOT of NA sales and User_score
ggplot(data = newdata) + 
  geom_point(mapping = aes(x = User_Score, y = NA_Sales, color = Genre))




#stuff and things 
ggplot(newdata, aes(x= User_Score, y= NA_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2,alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(NA_Sales>5,as.character(Name),'')),hjust=0,vjust=0)


#Stuff and things part 2 
ggplot(newdata, aes(x= User_Score, y= NA_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2, alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(NA_Sales>5,as.character(Publisher),'')),hjust=0,vjust=0)


#linear regressions for the Europe. 
modelEU <- lm(EU_Sales~Platform+Genre+Publisher+ Rating + Developer+ User_Score + Critic_Score, data=newdata)
summary(modelEU)

#RegSubsets.
set.seed(1)
n = nrow(newdata)
tr2 = sample(1:n, #Sample indices do be used in training
            size = 2194, #Sample will have 5000 observation
            replace = FALSE) #Without replacement

null2 = lm(EU_Sales~1, data=newdata[tr,]) #only has an intercept
full2 = glm(EU_Sales~Platform+Genre+Publisher+ Rating + Developer+ User_Score + Critic_Score, data=newdata[tr2,]) #Has all the selected variables

#Variable selection

regForward = step(null2, #The most simple model
                  scope=formula(full2),#Let us analyze all models until the full model
                  direction="forward", #Adding variables
                  k=log(length(tr2))) #This is BIC
regBack = step(full2, #Starting with the full model
               direction="backward", #And deleting variables
               k=log(length(tr2))) #This is BIC
reghybrid = step(null2, #The most simple model
                 scope=formula(full2), #The most complicated model
                 direction="both", #Add or delete variables
                 k=log(length(tr2))) #This is BIC

##
## This is the best regression model for the Europe region.
##

best_model_EU <- lm(EU_Sales ~ Critic_Score + User_Score + Platform + Rating, data = newdata)
summary(best_model_EU)

##
## This is the best regression model for the Europe region.
##

#regression CV for EU.
set.seed(1)
train2 = sample(1:n, #Sample indices do be used in training
               size = 659, #Sample will have 5000 observation
               replace = FALSE) #Without replacement
2194*.30

training.set2 <- newdata[-train2,] # Bigger, to give a better chance to our model...
test.set2 <- newdata[train2,] # Smaller, to test our model

#CV Best regsubset model
best_model_cv_EU <- lm(EU_Sales ~ Critic_Score + User_Score + Platform + Rating, data = training.set2)
OOS2 <- predict(best_model_cv_EU, test.set2)

#SofE
sum((OOS2-test.set$EU_Sales)^2)

#MSE
mean((OOS2-test.set$EU_Sales)^2)


#Linear regression for Japan
modelJP <- lm(JP_Sales~Platform+Genre+Publisher+ Rating + Developer+ User_Score + Critic_Score, data=newdata)
summary(modelJP)

#RegSubsets.
set.seed(1)
n = nrow(newdata)
tr3 = sample(1:n, #Sample indices do be used in training
             size = 659, #Sample will have 5000 observation
             replace = FALSE) #Without replacement

null3 = lm(JP_Sales~1, data=newdata[tr3,]) #only has an intercept
full3 = glm(JP_Sales~Platform + Genre + Publisher + Rating + Developer + User_Score + Critic_Score, data=newdata[tr3,]) #Has all the selected variables

#Variable selection

regForward = step(null3, #The most simple model
                  scope=formula(full3),#Let us analyze all models until the full model
                  direction="forward", #Adding variables
                  k=log(length(tr3))) #This is BIC
regBack = step(full3, #Starting with the full model
               direction="backward", #And deleting variables
               k=log(length(tr3))) #This is BIC
reghybrid = step(null3, #The most simple model
                 scope=formula(full3), #The most complicated model
                 direction="both", #Add or delete variables
                 k=log(length(tr3))) #This is BIC


##
## This is the best regression model for the JP region.
##
best_model_JP <- lm(JP_Sales ~ Critic_Score + Platform, data = newdata)
summary(best_model_JP)
##
## This is the best regression model for the JP region.
##


#regression CV for EU.
set.seed(1)
train3 = sample(1:n, #Sample indices do be used in training
                size = 659, #Sample will have 5000 observation
                replace = FALSE) #Without replacement
2194*.30

training.set3 <- newdata[-train3,] # Bigger, to give a better chance to our model...
test.set3 <- newdata[train3,] # Smaller, to test our model

#CV Best regsubset model
best_model_JP <- lm(JP_Sales ~ Critic_Score + Platform, data = training.set3)
OOS3 <- predict(best_model_JP, newdata = test.set3)

#SofE
sum((OOS3-test.set$JP_Sales)^2)

#MSE
mean((OOS3-test.set$JP_Sales)^2)





#stuff and things 
ggplot(newdata, aes(x= User_Score, y= EU_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2,alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(EU_Sales>3.5,as.character(Name),'')),hjust=0,vjust=0)


#Stuff and things part 3
ggplot(newdata, aes(x= User_Score, y= EU_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2, alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(EU_Sales>3.5,as.character(Publisher),'')),hjust=0,vjust=0)





#stuff and things 
ggplot(newdata, aes(x= User_Score, y= JP_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2,alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(JP_Sales>2,as.character(Name),'')),hjust=0,vjust=0)


#Stuff and things part 4 
ggplot(newdata, aes(x= User_Score, y= JP_Sales, colour=Genre, label=Name)) + 
  
  geom_point(size = 2, alpha = 0.6) +
  
  theme_bw()+
  
  geom_text(aes(label=ifelse(JP_Sales>2,as.character(Publisher),'')),hjust=0,vjust=0)

