
run <- function(dir = '/home/wijnand/R_workspace_africa')
{
      set.seed(1)
      trainingdata <- read.csv(paste0(dir, '/resources/train.csv'), header = T)
      
      #training <- findCorrelation(cor(training), .95)
      
      ## 
      MIR_measurements <- trainingdata[, 2:2655]
      MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
      X_train <- cbind(trainingdata[, 3580:3595], MIR_DER[,-1])
      MIR_measurements <- trainingdata[, 2671:3579]
      MIR_DER <- MIR_measurements- cbind(NA, MIR_measurements)[, -(dim(MIR_measurements)[2]+1)]
      X_train <- cbind(X_train, MIR_DER[, -1])
      ##
      
      print(colnames(X_train))
      
      library(caret)
      number <- createDataPartition(y = X_train$BSAN, p = .8, list = F)
      trainPart <- X_train[number,]
      testPart <- X_train[-number,]
      
      predictColumns <- c("Ca", "P", "pH", "SOC", "Sand")
      trainColumns <- colnames(trainPart[, !colnames(trainPart) %in% c("PIDN", "Ca", "P", "pH", "SOC", "Sand")])
      
      predictions <- as.data.frame(testPart$PIDN)
      RMSE <- NULL
      
      library(Metrics)
      library(bartMachine)
      
      for(predictor in predictColumns)
      {
            trainedModel <- bartMachine(x = trainPart[, !colnames(trainPart) %in% c("PIDN", "Ca", "P", "pH", "SOC", "Sand")],
                                        y = predictor)
            
            prediction <- h2o.predict(trainedModel, newdata = testPart_h2o)
            predictionFrame <- as.data.frame(prediction)
            
            predictions[predictor] <- predictionFrame
            RMSE <- c(RMSE, rmse(predictionFrame, testPart[predictor]))              
            print(rmse(predictionFrame, testPart[predictor]))
      }
      write.csv(predictions, paste0(dir, '/resources/result_bart.csv'), quote=F, row.names=F)
      
      print("===summary===")
      print(predictColumns)
      print(round(RMSE, digits = 3))
      print(paste0("MCRMSE (mean columnwise root mean squared error): ", sum(RMSE) / 5))
}