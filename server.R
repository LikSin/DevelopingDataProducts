library(shiny)
library(datasets)
library(caret)
data(Titanic)
library(randomForest)
library(e1071)


##Pre-processing data
data=data.frame(Titanic)
#check data
str(data)
#We realised that the survival for each combination of parameters is summed into a frequency column.
#For use in prediction model, we do not want it summed. Instead, it should be such that each row represent a person
#Reconstructing
flatdata=NULL
for(i in 1:4)
{
    flatdata=cbind(flatdata,rep(as.character(data[,i]),data$Freq))
}
flatdata=data.frame(flatdata)
names(flatdata)=names(data[1:4])

##Splitting into training set and test set
set.seed(150)
inTrain = createDataPartition(flatdata$Survived, p = 0.70,list=FALSE)
trainingset = flatdata[inTrain,]
testset = flatdata[-inTrain,]

##Generating prediction model for survive outcome
#Using random forest model
modFit=train(Survived ~ ., data=trainingset, model="rf")

##Apply our prediction model to the test data set
predval <- predict(modFit, newdata=testset)
confusionMatrix(predval,testset$Survived) #Model has accuracy of 0.7909

survival=function(class,sex,age){
    inputdata=NULL
    inputdata$Class=class
    inputdata$Sex=sex
    inputdata$Age=age
    inputdata=data.frame(inputdata)
    predval=predict(modFit, newdata=inputdata)
    as.character(predval)
    }

shinyServer(
    function(input, output) {
        output$oselClass=renderPrint({input$selClass})
        output$oselSex=renderPrint({input$selSex})
        output$oselAge=renderPrint({input$selAge})
        output$prediction=renderPrint({survival(input$selClass,input$selSex,input$selAge)})
        output$matrix=renderPrint({confusionMatrix(predval,testset$Survived)})
        output$table1=renderTable({table(predval,testset$Survived)})
    }
)