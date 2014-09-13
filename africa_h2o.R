# predict: Ca, P, pH, SOC, Sand

run <- function(dir = '/home/wijnand/R_workspace_africa')
{
        ## delete h2o tmp
        system("killall -9 java")
        system("rm -rf /tmp/h2o-wijnand/*")
        ## start h2o
        system("nohup java -jar -Xmx2g /home/wijnand/h2o-2.6.0.11/h2o.jar > /home/wijnand/h2o-2.6.0.11/run.log &")
        print('starting h2o...')
        Sys.sleep(2)
        
        set.seed(1)
        training <- read.csv(paste0(dir, '/resources/train.csv'), header = T)
        
        library(h2o)
        localH2O <- h2o.init()
        print(localH2O)
        
        library(caret)
        number <- createDataPartition(y = training$Ca, p = .8, list = F)
        trainPart <- training[number,]
        testPart <- training[-number,]
        
        trainPart_h2o <- as.h2o(localH2O, trainPart, header = T)
        testPart_h2o <- as.h2o(localH2O, testPart, header = T)
        
        trainColumns <- colnames(trainPart[, !colnames(trainPart) %in% c("PIDN", "Ca", "P", "pH", "SOC", "Sand")])
        
        #trainedModel <- h2o.glm(x = trainColumns,
        #                        y = "Ca",
        #                        data = trainPart_h2o,
        #                        family = "gaussian")
        
        trainedModel <- h2o.gbm(x = trainColumns,
                                y = "Ca",
                                data = trainPart_h2o,
                                distribution = "gaussian",
                                n.trees = 400)
        
        prediction <- h2o.predict(trainedModel, newdata = testPart_h2o)
        predictionFrame <- as.data.frame(prediction)
        
        library(Metrics)
        print(rmse(predictionFrame, testPart$Ca))
        
        h2o.shutdown(localH2O, prompt = F)
}

