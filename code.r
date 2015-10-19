library(caret)
library(ggplot2)

#loading data
training_raw<-read.csv('pml-training.csv')
testing_raw<-read.csv('pml-testing.csv')

#preprocessing
NA_rate<-function(v){
        if (class(v)=="factor")
                {
                sum((v==''))/length(v)
            }
            else sum(is.na(v))/length(v)
}
c_NA_ratio<-0.1
selectCol<-(sapply(training_raw,NA_rate))<c_NA_ratio
preprocess_act<-function(dataframe){
        dataframe_out<-dataframe[,selectCol]
        dataframe_out<-dataframe_out[,-c(1:7)] #truncate non-motional variables
        
        for (i in 1:(ncol(dataframe_out)-1)) #leave the classe be factor
                {
                #print(class(dataframe_out[,i]))
                if (class(dataframe_out[,i])=="factor")
                        dataframe_out[,i]<-as.numeric(as.character(dataframe_out[,i]))
                #print(class(dataframe_out[,i]))
        }
        
        prep_obj1<-preProcess(dataframe_out[,-ncol(dataframe_out)],method="knnImpute") # missing imputation
        imputed<-predict(prep_obj1,dataframe_out[,-ncol(dataframe_out)])
        for (i in 1:(ncol(dataframe_out)-1)){
                dataframe_out[,i]<-(imputed[,i]-mean(imputed[,i]))/sd(imputed[,i]) #standardize
        }
        data.frame(dataframe_out)
}

#PCA Exploratory analysis
training_0<-preprocess_act(training_raw)
preProc<-preProcess(training_0[,-ncol(training_0)],method='pca',thresh = 0.9)
training_pc<-predict(preProc,training_0[,-ncol(training_0)])
summary(preProc)
plot(training_pc[,1],training_pc[,2],col=as.numeric(training_0$classe))


#Experiment design
set.seed(123)
inTrain<-createDataPartition(y=training_0$classe,p=0.7,list=FALSE)
training<-training_0[inTrain,]
validating<-training_0[-inTrain,]
#Machine learning
modelfit<-train(classe~.,methods="gbm",preProcess="pca",thresh=0.9,data=training)
prediction_valid<-predict(modelfit,validating)
table(prediction_valid,validating$classe)
testing<-preprocess_act(testing_raw)
prediction_valid<-predict(modelfit,validating)

#submission
answers2<-predict(modelfit,testing)
pml_write_files = function(x){
n = length(x)
for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
}
}
pml_write_files(answers)
