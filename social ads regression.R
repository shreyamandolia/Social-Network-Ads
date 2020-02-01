## logistic regression
file6 <- read.csv("C:/Users/Shreya pc/Downloads/Social_Network_Ads.csv")
View(file6)
file6= file6[,3:5]
View(file6)
# Encoding the target feature as factor
file6$Purchased = factor(file6$Purchased,levels= c(0,1))
## spliting the dataset into training set and test set
library(caTools)
set.seed(123) ###set.set= genric function ,we used it as we not biased about data
split <- sample.split(file6$Purchased, SplitRatio = 0.75)##here we split out set into 75:25 where trainning set is 75% and testset is 25%
training_new1 <- subset(file6,split==TRUE)                   
test_new1 <- subset(file6,split==FALSE)
##feature scaling  ### to levalises the values as they is abig difference  is big differece between them
training_new1[,1:2]=scale(training_new1[,1:2])

test_new1[,1:2]=scale(test_new1[,1:2])

print(training_new1)
## it looks same due to scaling
## fitting the logistic regression   ##beta1,beta2 are age and estimated salary according to medel
classifier = glm(formula = Purchased~.,### generalising the linear model(glm)
                 family = binomial,   ## link function logistic regression is a type of clustering algorithm
                 data = training_new1) ### we always perform data on 75%
classifier
## predicting the test set result
prob_pred <- predict(classifier,type = 'response',newdata = test_new1[-3])
y_pred <- ifelse(prob_pred>0.5,1,0)  ### if else is generic function
### making the confusion matrix
cm <- table(test_new1[,3],y_pred) ## to differenciate between the true and false prediction
### visulisingthe training set result
install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_new1
x1=seq(min(set[,1]) -1,max(set[,-1]) +1,by=0.01)
x2=seq(min(set[,2]) +1,max(set[,-2]) +1,by=0.01)
grid_set =expand.grid(x1,x2) 
colnames(grid_set) = c('Age','EstimatedSalary')
prob_set=predict(classifier,type='response',newdata = grid_set)
y_grid=ifelse(prob_set>0.5,1,0)
plot(set[,-3],
     main='Logistic Regression (Training_new1)',
     xlab = 'Age',ylab = 'EstimatedSalary',
     xlim = range(x1),ylim = range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch ='.',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch =21,bg =ifelse(set[,3]==1,'green4','red3'))
##visualising the Test set results
library(ElemStatLearn)
set = test_new1
x1=seq(min(set[,1]) -1,max(set[,-1]) +1,by=0.01)
x2=seq(min(set[,2]) +1,max(set[,-2]) +1,by=0.01)
grid_set =expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
prob_set=predict(classifier,type='response',newdata = grid_set)
y_grid=ifelse(prob_set>0.5,1,0)
plot(set[,-3],
     main='Logistic Regression (Test_new1)',
     xlab = 'Age',ylab = 'EstimatedSalary',
     xlim = range(x1),ylim = range(x2))
contour(x1,x2,matrix(as.numeric(y_grid),length(x1),length(x2)),add = TRUE)
points(grid_set,pch =',',col=ifelse(y_grid==1,'springgreen3','tomato'))
points(set,pch =21,bg =ifelse(set[,3]==1,'green4','red3'))

