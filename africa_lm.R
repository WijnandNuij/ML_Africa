
run <- function(dir = '/home/wijnand/R_workspace_africa')
{
      dir = '/home/wijnand/R_workspace_africa'
      set.seed(1)
      trainingdata <- read.csv(paste0(dir, '/resources/train.csv'), header = T)

      predictColumns <- c("Ca", "P", "pH", "SOC", "Sand")
      #trainColumns <- colnames(trainPart[, !colnames(trainPart) %in% c("PIDN", "Ca", "P", "pH", "SOC", "Sand")])
      #predictions <- as.data.frame(testPart$PIDN)
      
      # make Depth column numeric
      trainingdata$Depth <-  with ( trainingdata, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )
      
      library(caret)
      number <- createDataPartition(y = trainingdata$Ca, p = .8, list = F)
      Train.raw <- trainingdata[number,]
      Test.raw <- trainingdata[-number,]
      
      #delete highly correlated (>0.95) features.
      Xtrain <- Train.raw[,2:3595]
      Ytrain <- Train.raw[,3596:3600]
      Xtest <- Test.raw[,2:3595]
      Ytest <- Test.raw[,3596:3600]
      IDtest <- Test.raw[,1]

      tooHigh <- findCorrelation(cor(rbind(Xtrain,Xtest)), .95)
      
      Xtrainfiltered <- Xtrain[, -tooHigh]
      Xtestfiltered  <-  Xtest[, -tooHigh]
      
      Xtrainfiltered <- cbind(Xtrainfiltered, Ytrain)
      Xtestfiltered  <-  cbind(Xtestfiltered, Ytest)
      
      RMSE <- NULL
      predictions <- as.data.frame(Xtestfiltered$PIDN)
      ctrl = trainControl(method = "cv", 
                          number = 10)
      
      library(Metrics)
      for(predictor in predictColumns)
      {
            trainedModel <- train(x = Xtrainfiltered[, !colnames(Xtrainfiltered) %in% c("PIDN", "Ca", "P", "pH", "SOC", "Sand")],
                                  y = as.vector(t(Xtrainfiltered[predictor])),
                                  trControl = ctrl,
                                  method = "lm")
            
            prediction <- predict(trainedModel, newdata = Xtestfiltered)
            predictionFrame <- as.data.frame(prediction)
            
            predictions[predictor] <- predictionFrame
            RMSE <- c(RMSE, rmse(predictionFrame, Xtestfiltered[predictor]))              
            print(rmse(predictionFrame, Xtestfiltered[predictor]))
      }
      write.csv(predictions, paste0(dir, '/resources/result_lm.csv'), quote=F, row.names=F)
      
      print("===summary===")
      print(predictColumns)
      print(round(RMSE, digits = 3))
      print(paste0("MCRMSE (mean columnwise root mean squared error): ", sum(RMSE) / 5))
}