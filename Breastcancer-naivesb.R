#Breast Canser Dataset Analysis
library(readr,dplyr,ggplot2,corrplot,gridExtra)
install.packages("pacman")
pacman::p_load(readr,dplyr,ggplot2,corrplot,gridExtra,pROC,MASS,caTools,caret,caretEnsemble,tm)
data<-read.csv("wisc_bc_data.csv")
str(data)
data$diagnosis<-as.factor(data$diagnosis)
summary(data)
data[,33]<-NULL
summary(data)
prop.table(table(data$diagnosis))
#checking correlation between some variables
corr_mat<-cor(data[,3:ncol(data)])
data[,3:ncol(data)]
?ncol
corrplot(corr_mat,order='hclust',tl.cex=1,addrect = 8)
#modelling
set.seed(1234)
data_index<-createDataPartition(data$diagnosis,p=0.7,list=FALSE)
train_data<-data[data_index,-1]
test_data<-data[-data_index,-1]
#Data Preprossing
pca_res<-prcomp(data[,3:ncol(data)],center=TRUE,scale. = TRUE)
plot(pca_res)
summary(pca_res)
#the first two componensts explains the 0.6324 of the variance.
#we need 10 principal components to explain more than 0.95 of the variance and 17 to explai more than 99
pca_df<-as.data.frame(pca_res$x)
ggplot(pca_df,aes(x=PC1,y=PC2,col=data$diagnosis))+geom_point(alpha=0.5)
#THE Dataset is Separated
g_pc1<-ggplot(pca_df,aes(x=PC1,fill=data$diagnosis))+geom_density(alpha=0.25)
g_pc2<-ggplot(pca_df,aes(x=PC2,fill=data$diagnosis))+geom_density(alpha=0.25)
grid.arrange(g_pc1,g_pc2)
grid.arrange(g_pc1,g_pc2,ncol=2)
#using caret PreProcess TO APPLY PCA with a 0.99 threshold
fitControl<-trainControl(method = "cv",number=5,preProcOptions = list(thresh=0.99),#threshold for pca preprocess
                         classProbs = TRUE,
                         summaryFunction = twoClassSummary)
model_nb<-train(diagnosis~.,
                train_data,
                method="nb",
                metric="ROC",
                preProcess=c('center','scale'),
                trace=FALSE,
                trControl=fitControl)
1
predict_nb<-predict(model_nb,test_data)
cm_nb<-confusionMatrix(predict_nb,test_data$diagnosis,positive = "M")
cm_nb
?corrplot
?createDataPartition
summary(data[data_index,-1])
