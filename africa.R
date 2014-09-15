









loadData <- function(dir = '/home/wijnand/R_workspace_africa')
{
      library(caret)
      
      Train.raw <- read.csv(paste0(dir, '/resources/train.csv'), header = T)
      Test.raw <- read.csv(paste0(dir, '/resources/test.csv'), header = T)
      
      Train.raw$Depth <-  with ( Train.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) )
      Test.raw$Depth <-  with ( Test.raw, ifelse ( ( Depth == 'Subsoil' ), 0 , 1 ) ) 
      
      Xtrain <- Train.raw[,2:3595]
      Ytrain <- Train.raw[,3596:3600]
      Xtest <- Test.raw[,2:3595]
      IDtest <- Test.raw[,1]
            
      #delete highly correlated (>0.95) features.
      tooHigh <- findCorrelation(cor(rbind(Xtrain,Xtest)), .95)
      
      Xtrainfiltered <- Xtrain[, -tooHigh]
      Xtestfiltered  <-  Xtest[, -tooHigh]
      
      set.seed(1234)
      # 10 fold cv
      indx <- createFolds(Ytrain[,1], returnTrain = TRUE)
      ctrl <- trainControl(method = "cv", index = indx)
      
      #predict Ca
      lmTuneCa <- train(x = Xtrainfiltered, y = Ytrain$Ca,
                        method = "lm",
                        trControl = ctrl)
      print(lmTuneCa)
      #RMSE = 0.409

      testResults <- data.frame(PIDN = IDtest,
                                Ca = predict(lmTuneCa, Xtestfiltered))
      
      #predict P
      lmTuneP <- train(x = Xtrainfiltered, y = Ytrain$P,
                       method = "lm",
                       trControl = ctrl)
      print(lmTuneP)
      #RMSE = 0.925

      testResults$P <- predict(lmTuneP,Xtestfiltered)
      
      #predict pH
      lmTunepH <- train(x = Xtrainfiltered, y = Ytrain$pH,
                        method = "lm",
                        trControl = ctrl)
      print(lmTunepH)
      #RMSE = 0.508

      testResults$pH <- predict(lmTunepH,Xtestfiltered)
      
      #predict SOC
      lmTuneSOC <- train(x = Xtrainfiltered, y = Ytrain$SOC,
                         method = "lm",
                         trControl = ctrl)
      print(lmTuneSOC)
      #RMSE = 0.511

      testResults$SOC <- predict(lmTuneSOC,Xtestfiltered)
      
      #predict Sand
      lmTuneSand <- train(x = Xtrainfiltered, y = Ytrain$Sand,
                          method = "lm",
                          trControl = ctrl)
      print(lmTuneSand)
      
      #RMSE = 0.495
      testResults$Sand <- predict(lmTuneSand,Xtestfiltered)
      
      write.csv(testResults,file = "analytics/kaggle_challenges/africa_soil/slr.csv",row.names = FALSE)
}