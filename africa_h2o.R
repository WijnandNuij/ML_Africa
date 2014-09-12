# predict:
# Ca
# P
# pH
# SOC
# Sand

run <- function(dir = '/home/wijnand/R_workspace_africa')
{
        dir <- '/home/wijnand/R_workspace_africa'
        training <- read.csv(paste0(dir, '/resources/train.csv'), header = T)
        
        # identifier
        PIDN <- training$PIDN
        training$PIDN <- NULL
        
        library(h2o)
        localH2O <- h2o.init()
        print(localH2O)
        
        dat_h2o <- as.h2o(localH2O, training, header = T)
        
        trainingColumns <- colnames(training[, !colnames(training) %in% c("Ca", "P", "pH", "SOC", "Sand")])
        
        trainedModel <- h2o.glm(x = trainingColumns,
                                y = "Ca",
                                data = dat_h2o,
                                family = "gaussian")
        
        h2o.shutdown(localH2O)
}