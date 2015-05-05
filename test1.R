if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))-1
}else{
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))
}

RecordGameInfo <- read.table(sprintf("~/Documents/Sport_analysis/basketball/Pred_Data/RecordGameInfo_%s.txt", SeasonCurrent), 
                             header = TRUE, sep = "\t")

### Predict the next game win precentage
require(psych)
###　harmonic mean
harmonicwp.fun <- function(variable.1, variable.2, variable.3, variable.4){
 har.wp <- apply(RecordGameInfo[, c(variable.1, variable.2, variable.3, variable.4)], 1 ,function(x) harmonic.mean(data.frame(x)))  
 return(har.wp)
}

<- harmonicwp.fun("AdaMeanAway", "GameRestDayWPAway", "CurrentSeasonWPAway")

RecordWinpAndPred <- data.frame(Schedule[t,], GameDate = GameDateTime, AwayTeam = AwayTeam, HomeTeam = HomeTeam,
                                AdaMeanAway = RecordWinpAndPredpAway$ada_mean,
                                AdaMeanHome = RecordWinpAndPredpHome$ada_mean,
                                SvmMeanAway = RecordWinpAndPredpAway$svm_mean,
                                SvmMeanHome = RecordWinpAndPredpHome$svm_mean
                                )  
RecordGameInfo <- rbind(RecordGameInfo, RecordWinpAndPred)



### Lasso regression
require(glmnet)
IndependentVariableHome <- as.matrix(FinalBettingandPredData[1:1000, c("AdaMean", "GameRestDayWP", 
                                                                       "CurrentSeasonWPHome", "BettingWPAway_BET365")])
ResponseVariableHome <- as.matrix(FinalBettingandPredData[1:1000, c("W.L")])

predictdata <- as.matrix(FinalBettingandPredData[1001:1057, c("AdaMean", "GameRestDayWP", 
                                                              "CurrentSeasonWPHome", "BettingWPAway_BET365")])
### alpha=1 is the lasso penalty, and alpha=0 the ridge penalty
ML.lasso <- cv.glmnet(IndependentVariableHome, ResponseVariableHome, family = c("binomial"), alpha = 0.5, nfold = 10)
coef(ML.lasso, s = "lambda.min")
coef(ML.lasso, s = "lambda.1se")
pred.ridge <- predict(ML.lasso, newx = predictdata, s = "lambda.1se", type = "class")
table(FinalBettingandPredData[1001:1057, c("W.L")], pred.ridge)


IndependentVariableHome <- as.matrix(AllDataHome$TeamDataHome[1:100, c("diffEFG", "diffTOV", "diffRB", "diffFTR")])
ResponseVariableHome <- as.matrix(AllDataHome$TeamDataHome[1:100, c("W.L")])
predictdata <- as.matrix(AllDataHome$TeamDataHome[100:114, c("diffEFG", "diffTOV", "diffRB", "diffFTR")])

ML.lasso2 <- glmnet(IndependentVariableHome, ResponseVariableHome, family = c("binomial"), alpha = 0.5)
test <- cv.glmnet(IndependentVariableHome, ResponseVariableHome, family = c("binomial"), alpha = 1)
coef(test, s = "lambda.min")
pred.test <- predict(test, newx = predictdata, s = "lambda.min", type = "class")
table(AllDataHome$TeamDataHome[100:114, c("W.L")], pred.test)

test2 <- ml.adaboost(model.formula, AllDataHome$TeamDataHome[1:100, ], 50, 0.1)
pred.test2 <- predict(test2$real, AllDataHome$TeamDataHome[100:114, c("diffEFG", "diffTOV", "diffRB", "diffFTR")], 41, type = c("both"))
table(AllDataHome$TeamDataHome[100:114, c("W.L")], pred.test2$class)
