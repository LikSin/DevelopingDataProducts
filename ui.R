library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("See whether you survived the Titanic!"),
    sidebarPanel(
        h2("Inputs"),
        h3("Choose your class"),
        radioButtons("selClass",
                     "Class types:",
                     c("1st Class passenger"="1st", "2nd Class passenger"="2nd", "3rd Class passenger"="3rd", "Crew"="Crew")
                     ),
        h3("Choose your gender"),
        radioButtons("selSex",
                     "Sex:",
                     c("Male"="Male","Female"="Female")
                     ),
        h3("Choose your age category"),
        radioButtons("selAge",
                     "Age:",
                     c("Child"="Child", "Adult"="Adult")
                     )
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Input parameters",
                     h3("You have entered the following parameters"),
                     h4("Class:"),
                     verbatimTextOutput("oselClass"),
                     h4("Gender:"),
                     verbatimTextOutput("oselSex"),
                     h4("Age:"),
                     verbatimTextOutput("oselAge"),
                     h3("Results! Did you survive the Titanic?"),
                     verbatimTextOutput("prediction")
                     ), 
            tabPanel("Server calculations and outputs",
                     p("Results of cross validation"),
                     tableOutput("table1"),
                     p("Confusion matrix information for cross validation"),
                     tableOutput("matrix")
                     
                     ), 
            tabPanel("Documentation and Codes",
                     h2("Overview"),
                     p("This application takes in 3 input parameters from the user on the side panel through radio buttons.
                       It then outputs the parameters selected by the user.
                       It also outputs whether the user survived the Titanic based on the parameters selected.
                       This survivability output is based on a prediction model built upon the Titanic dataset in R datasets.
                       The prediction model achieved an accuracy of 79% based on cross validation check"),
                     
                     h2("Instructions on app use"),
                     p("Select your class types (1st, 2nd, 3rd class passenger or crew), Sex (Male or Female), and age category (Child, Adult) on the side bar panel.
                       "),
                     p("On the main panel, Input parameters tab, the options you have selected will be indicated. Please wait for the parameters to be updated as the server might take awhile to load.
                       "),
                     p("Based on the input parameters, the survivability will be shown at Results! Did you survive the Titanic?
                       "),
                     
                     h2("Details"),
                     
                     h3("Dataset"),
                     p("We use the Titanic dataset available in R datasets to build our prediction model on survivability of passengers.
                       The data set details the survival of passengers on ocean liner Titanic, sorted according to their economic status(class), sex, age and survival.
                       There are 2201 observations on these 4 variables in total. We set 70% of the observations for training data set and the other 30% for test set"),
                     
                     h3("Library required"),
                     p("The following R libraries are required"),
                     code("library(shiny)
                            library(datasets)
                          library(caret)
                          data(Titanic)"),
                     
                     h3("Preprocessing"),
                     p("The data exist as a 4-dimensional array. We need to change it to a form where we can train our prediction model.
                       "),
                     p("Pre-processing data"),
                     code("data=data.frame(Titanic)"),
                     p("Check data"),
                     code("str(data)"),
                     p("We realised that the survival for each combination of parameters is summed into a frequency column.
                          For use in prediction model, we do not want it summed. Instead, it should be such that each row represent a person.
                          Reconstructing"),
                     code("flatdata=NULL"),
                     code("for(i in 1:4)"),
                     code("     {"),
                     code("     flatdata=cbind(flatdata,rep(as.character(data[,i]),data$Freq))"),
                     code("     }"),
                     code("flatdata=data.frame(flatdata)"),
                     code("names(flatdata)=names(data[1:4])"),
                     
                     h3("Splitting training and test set"),
                     p("Set Seed for reproducibility"),
                     code("set.seed(150)"),
                     p("Splitting data into training and test set"),
                     code("inTrain = createDataPartition(flatdata$Survived, p = 0.70,list=FALSE)
trainingset = flatdata[inTrain,]
                          testset = flatdata[-inTrain,]"),
                     
                     h3("Generating prediction model for survive outcome"),
                     p("We use random forest model as prediction model"),
                     code('modFit=train(Survived ~ ., data=trainingset, model="rf")'),
                     
                     h3("Apply our prediction model to the test data set"),
                     code("predval <- predict(modFit, newdata=testset)"),
                     p("Check accuracy"),
                     code("confusionMatrix(predval,testset$Survived) #Model has accuracy of 0.7909"),
                     
                     h3("Create function to process user inputs"),
                     p("We create a function to process user inputs in the reactive function of shiny server"),
                     p("The prediction model was applied to the user input parameters and the output generated"),
                     code("survival=function(class,sex,age){
    inputdata=NULL
                          inputdata$Class=class
                          inputdata$Sex=sex
                          inputdata$Age=Age
                          inputdata=data.frame(inputdata)
                          predval=predict(modFit, newdata=inputdata)
                          as.character(predval)
                          }"),
                     
                     h3("Reactive function"),
                     p("The reactive function outputs user input parameters and the predicted survivability"),
                     code("shinyServer(
    function(input, output) {
                          output$oselClass=renderPrint({input$selClass})
                          output$oselSex=renderPrint({input$selSex})
                          output$oselAge=renderPrint({input$selAge})
                          output$prediction <- renderPrint({survival(input$selClass,input$selSex,input$selAge)})
                          }
                     )")
                     )
            
            
        )
        
    )
))

