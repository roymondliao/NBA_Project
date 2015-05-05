### Prediction the game record and model building
## load the package
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Packages.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Advanced_Statistic.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Normalize.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Predict_Data.R")
Sys.setlocale("LC_TIME", "C") # convert OS LC_NUMERIC = C for analysis data form 

### Season Schedule
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))-1
}else{
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))
}
ScheduleFile <- sprintf("~/Documents/Sport_analysis//basketball//Season_Data//Season_%s.txt", SeasonCurrent)
Schedule <- read.table(ScheduleFile, header=TRUE,  sep="\t", as.is=TRUE)
## function for convert date
convert.date.type<- function(DateVector) {
  datetime <- str_extract(DateVector, "[[:alpha:]]+.[0-9]+[[:punct:]].[0-9]+$")
  datetime <- str_replace(datetime , ",", "")
  datetime <- unlist(strsplit(datetime, " "))
  datetime <- str_replace_all(paste(datetime[3], datetime[1], datetime[2]), " ", "")
  datetime <- as.character(as.Date(datetime, "%Y %b %d"))
  return(datetime)
}
## call convert.date.type function
Schedule$CovertDate <- unlist(lapply(Schedule$Date, convert.date.type))
predgameinfoidx <- grep(Sys.Date()-1, Schedule$CovertDate)
Schedule <- Schedule[predgameinfoidx, ]
### For game prediction object programs
#loaction.date <- Sys.Date()-1 
#today.schedule.idx <- suppressWarnings(which(Schedule$CovertDate == loaction.date))
#Today.Game.Schedule <- Schedule[today.schedule.idx, ]

### NBATeamName
NBATeamName <- read.xlsx2("~/Documents/Sport_analysis/basketball//Team_Data//NBA_Team_Name.xlsx", 
                          sheetIndex = as.character(SeasonCurrent), header = TRUE)
NBATeamName <- data.table(NBATeamName)
RecordGameInfo <- data.table() # record all pred and real data

### calculate predict value of  W and L
for(t in 1:nrow(Schedule)){
  HomeTeam <- as.character(NBATeamName[Team_Name==Schedule$Home_Team[t], Team_Short_Name])
  AwayTeam <- as.character(NBATeamName[Team_Name==Schedule$Away_Team[t], Team_Short_Name])
  MergeSeason <- sort(seq(SeasonCurrent-3, SeasonCurrent), decreasing = TRUE)
  
  ## function for loading team data
  load.team.data <- function(Team.Name, MergeSeason) {
    i <- 1
    TeamData <- data.frame() 
    while(i<(length(MergeSeason)+1)){
      if(Team.Name=="BRK" &&  MergeSeason[i]<2013) {
        Team.Name <- "NJN"
      }else if(Team.Name=="NOP" &&  MergeSeason[i]<2014) {
        Team.Name <- "NOH"
      }else if(Team.Name=="CHO" &&  MergeSeason[i]<2015) {
        Team.Name <- "CHA"
      }
      MergeTeamdata <- read.table(sprintf("~/Documents//Sport_analysis/basketball//Team_Data//%s//%s_%s.txt", 
                                          MergeSeason[i], Team.Name, MergeSeason[i]), header = TRUE, sep = "\t", as.is = TRUE, dec = ".")
      TeamData <- rbind(TeamData, MergeTeamdata)
      i <- i+1
    } 
    return(TeamData)
  }
  ## call load.team.data function
  GameDateTime <- unlist(Schedule$CovertDate[t])
  HomeData <- load.team.data(HomeTeam, MergeSeason)
  HomeData <- HomeData[order(HomeData$Date), ]
  HomeTeamGamePlayed <- sum(ifelse(HomeData[which(HomeData$Season == SeasonCurrent & HomeData$H.A == "H"), "Date"] <= GameDateTime, 1, 0)) 
  
  AwayData <- load.team.data(AwayTeam, MergeSeason)
  AwayData <- AwayData[order(AwayData$Date), ]
  AwayTeamGamePlayed <- sum(ifelse(AwayData[which(AwayData$Season == SeasonCurrent & AwayData$H.A == "A"), "Date"] <= GameDateTime, 1, 0)) 
  
  if(HomeTeamGamePlayed>5 & AwayTeamGamePlayed >5){
    ### Team Data convert & separate data set
    ## function for convert to different data set 
    transfor.data <- function(TeamData ,SeasonCurrent) {
      TeamData <- Advanced_Statistic(TeamData)
      TeamData <- Normalize(TeamData)
      TeamDataHome <- data.frame(subset(TeamData, H.A == "H"))  ## Home team data since three season ago 
      rownames(TeamDataHome) <- c(1:nrow(TeamDataHome))
      TeamDataAway=data.frame(subset(TeamData, H.A == "A"))
      rownames(TeamDataAway) <- c(1:nrow(TeamDataAway))
      TeamDataSeason <- data.frame(subset(TeamData, Season == SeasonCurrent))  ## Home team data of current Season
      rownames(TeamDataSeason) <- c(1:nrow(TeamDataSeason))
      TeamDataSeasonHome <- data.frame(subset(TeamDataSeason, H.A == "H"))
      TeamDataSeasonAway <- data.frame(subset(TeamDataSeason, H.A == "A"))
      result <- list(TeamData = TeamData, TeamDataHome = TeamDataHome, TeamDataAway = TeamDataAway, 
                     TeamDataSeason = TeamDataSeason, TeamDataSeasonHome = TeamDataSeasonHome, TeamDataSeasonAway = TeamDataSeasonAway)
      return(result)
    }
    ## call transfor.data function
    AllDataHome <- transfor.data(HomeData, SeasonCurrent)
    AllDataAway <- transfor.data(AwayData, SeasonCurrent)
    
    ### Model building use machine learning method
    ## 1.adaboost method 
    if (!require(ada)) install.packages("ada")
    TotalGameHome <- nrow(AllDataHome$TeamDataHome) ## Game number of HomeData.H
    SeasonGameHome <- nrow(AllDataHome$TeamDataSeasonHome) ## Game number of HomeData_Season.H
    PredictGameHome <- SeasonGameHome-5 ## Game number for predict model
    PredictNextGameHome <- data.frame()
    
    TotalGameAway <- nrow(AllDataAway$TeamDataAway) ## Game number of HomeData.A
    SeasonGameAway <- nrow(AllDataAway$TeamDataSeasonAway) ## Game number of HomeData_Season.A
    PredictGameAway <- SeasonGameAway-5 ## Game number for predict model
    PredictNextGameAway <- data.frame()
    
    ### Prediction data of mean strategy
    ## function for preparing prediction data
    pred.data <- function(TeamData , CurrentSeasonGame) {
      # PredictData(data, frist.game.no, final.game.no)
      PredMeanData <- PredictData(TeamData, 1, CurrentSeasonGame) # mean strategy
      if(CurrentSeasonGame-4 >0){
        PredFiveData <- PredictData(TeamData, CurrentSeasonGame-4, CurrentSeasonGame) # previous five game strategy
      }else{
        PredFiveData <- PredictData(TeamData, 1, CurrentSeasonGame) # previous five game strategy
      }
      PredThreeData <- PredictData(TeamData, CurrentSeasonGame-2, CurrentSeasonGame)
      return(list(MeanData = PredMeanData, FiveData = PredFiveData, ThreeData = PredThreeData))
    }
    PredDataHome <- pred.data(AllDataHome$TeamDataSeasonHome, SeasonGameHome)
    PredDataAway <- pred.data(AllDataAway$TeamDataSeasonAway, SeasonGameAway)
    
    ### Model building
    iternumber <- 50 # defult=50
    model.formula <- as.formula("W.L ~ diffEFG + diffTOV + diffRB + diffFTR")
    
    ## 1. buliding adaboost model for real, discrete, gentle three different type method
    ml.adaboost <- function(model.formula, train.data, iternumber, nu) {
      ML.ada.real <- ada(model.formula, data = train.data, loss = c("logistic"), iter = iternumber, nu = nu, type = "real")
      ML.ada.discrete <- ada(model.formula, data = train.data, loss = c("logistic"), iter = iternumber, nu = nu, type = "discrete")
      ML.ada.gentle <- ada(model.formula, data = train.data, loss = c("logistic"), iter = iternumber, nu = nu, type = "gentle")
      return(list(real = ML.ada.real, discrete = ML.ada.discrete, gentle = ML.ada.gentle))
    }
    ## call ml.adaboost function
    MLAdaboostModelHome <- ml.adaboost(model.formula, AllDataHome$TeamDataHome, 50, 0.1)
    MLAdaboostModelAway <- ml.adaboost(model.formula, AllDataAway$TeamDataAway, 50, 0.1)
    
    ## predict game win or loss--Home team  
    PredictGameMeanHome <- predict(MLAdaboostModelHome$real, PredDataHome$MeanData, 41, type = c("both"))
    PredictGameFiveHome <- predict(MLAdaboostModelHome$real, PredDataHome$FiveData, 41, type = c("both"))
    PredictGameThreeHome <- predict(MLAdaboostModelHome$real, PredDataHome$ThreeData, 41, type = c("both"))
    
    ## record model's predict percentage
    PredictNextGameHome["Probability", "AdaMeanHome"] <- round(table(PredictGameMeanHome$class)["W"]/sum(table(PredictGameMeanHome$class)), 4)
    PredictNextGameHome["Probability", "AdaFiveHome"] <- round(table(PredictGameFiveHome$class)["W"]/sum(table(PredictGameFiveHome$class)), 4)
    PredictNextGameHome["Probability", "AdaThreeHome"] <- round(table(PredictGameThreeHome$class)["W"]/sum(table(PredictGameThreeHome$class)), 4)
    
    ## predict game win or loss--Away team 
    PredictGameMeanAway <- predict(MLAdaboostModelAway$real, PredDataAway$MeanData, 41, type = c("both"))
    PredictGameFiveAway <- predict(MLAdaboostModelAway$real, PredDataAway$FiveData, 41, type = c("both"))
    PredictGameThreeAway <- predict(MLAdaboostModelAway$real, PredDataAway$ThreeData, 41, type = c("both"))
    
    ## record model's predict percentage
    PredictNextGameAway["Probability", "AdaMeanAway"] <- round(table(PredictGameMeanAway$class)["W"]/sum(table(PredictGameMeanAway$class)), 4)
    PredictNextGameAway["Probability", "AdaFiveAway"] <- round(table(PredictGameFiveAway$class)["W"]/sum(table(PredictGameFiveAway$class)), 4)
    PredictNextGameAway["Probability", "AdaThreeAway"] <- round(table(PredictGameThreeAway$class)["W"]/sum(table(PredictGameThreeAway$class)), 4)
    
    ### 2. svm method
    require(kernlab)
    ml.svm.model <- function(train.data, pred.data, c.parameter = 2, cross.parameter = 10){
      ML.ksvm <- ksvm(W.L ~ diffEFG + diffTOV + diffRB + diffFTR, data = train.data, kernel = "rbfdot", 
                      C = c.parameter, cross = cross.parameter, prob.model = TRUE)
      pred.ksvm <- predict(ML.ksvm, pred.data, type = "probabilities")
      return(pred.ksvm)
    }
    
    ### call ml.svm.model function and svm pred--Home team
    PredictNextGameHome["Probability", "SvmMeanHome"] <- round(ml.svm.model(AllDataHome$TeamDataHome, PredDataHome$MeanData)[, "W"], 4)
    PredictNextGameHome["Probability", "SvmFiveHome"]<- round(ml.svm.model(AllDataHome$TeamDataHome, PredDataHome$FiveData)[, "W"], 4)
    PredictNextGameHome["Probability", "SvmThreeHome"]<- round(ml.svm.model(AllDataHome$TeamDataHome, PredDataHome$ThreeData)[, "W"], 4)
    
    ### call ml.svm.model function and svm pred--Away team
    PredictNextGameAway["Probability", "SvmMeanAway"] <- round(ml.svm.model(AllDataAway$TeamDataAway, PredDataAway$MeanData)[, "W"], 4)
    PredictNextGameAway["Probability", "SvmFiveAway"]<- round(ml.svm.model(AllDataAway$TeamDataAway, PredDataAway$FiveData)[, "W"], 4)
    PredictNextGameAway["Probability", "SvmThreeAway"]<- round(ml.svm.model(AllDataAway$TeamDataAway, PredDataAway$ThreeData)[, "W"], 4)
    
    ### 3. randomForest 
    #library(randomForest)
    #ML.RF <- randomForest(as.factor(W.L) ~ diffEFG + diffTOV + diffRB + diffFTR, 
    #                      data = AllDataHome$TeamDataHome[, c("W.L", "diffEFG", "diffTOV", "diffRB", "diffFTR")],
    #                      mtry = 2, importance = TRUE, proximity=TRUE)  
    #pred.RF <- predict(ML.RF, test.data) 
    #pred.RF.error.rate<- 1-round(sum(diag(table(pred.RF, test.data$W.L)))/sum(table(pred.RF, test.data$W.L)), 4)
    
    ### Split different day for rest
    CurrentSeasonGameHome <- nrow(AllDataHome$TeamDataSeason)
    CurrentSeasonGameAway <- nrow(AllDataAway$TeamDataSeason)
    
    ##  function for calculate the rest of different day
    game.date.diff <- function(GameData, GameNumber) {
      DateDiff<- data.frame()
      for(d in 2:(GameNumber+1)) {
        first.game <- as.Date(GameData$Date[d-1])
        second.game <- as.Date(GameData$Date[d])
        DateDiff[d-1, "Date"] <- GameData$Date[d-1]
        DateDiff[1, "DayDiff"] <- -1
        DateDiff[d, "DayDiff"] <- as.numeric(-(first.game - second.game))
        DateDiff[d-1, "W.L"] <- GameData$W.L[d-1]
        DateDiff[d-1, "H.A"] <- GameData$H.A[d-1]
        DateDiff[d-1, "Opp"] <- GameData$Opp[d-1]
      }
      return(DateDiff)
    }
    # call game.date.diff function
    DiffGameRestHome <- na.omit(game.date.diff(AllDataHome$TeamDataSeason, CurrentSeasonGameHome))
    DiffGameRestTableHome <- table(DiffGameRestHome[, 2:3])
    DiffGameRestAway <- na.omit(game.date.diff(AllDataAway$TeamDataSeason, CurrentSeasonGameAway))
    DiffGameRestTableAway <- table(DiffGameRestAway[, 2:3])
    
    ### Win percent different day for rest
    ## function for calculate the win percentage about rest day
    # GameDateTime <- Sys.Date() # the correct game time in American
    # previous.day.H <- AllDataHome$TeamDataSeason$Date[nrow(AllDataHome$TeamDataSeason)] 
    # game.date.idx.H<- which(AllDataHome$TeamDataSeason$Date == previous.day.H)
    
    ## previous game date by Hoem team
    GameRestDayHome <- as.numeric(as.Date(GameDateTime) - as.Date(AllDataHome$TeamDataSeason$Date[nrow(AllDataHome$TeamDataSeason)])) 
    ## previous game date by Away team
    GameRestDayAway <- as.numeric(as.Date(GameDateTime) - as.Date(AllDataAway$TeamDataSeason$Date[nrow(AllDataAway$TeamDataSeason)])) 
    
    ## function for calculate the rest of different day win percentage
    diff.day.wp <- function(GameRestData, DiffDayNumber) {
      if(sum(GameRestData[1:nrow(GameRestData), "DayDiff"] == DiffDayNumber) == 0){ 
        WP <- 0.1
      }else if(sum(GameRestData[1:nrow(GameRestData), "W.L"] == "W") == 0){
        WP <- 0.1
      }else{ 
        WP <- table(GameRestData[1:nrow(GameRestData), c("DayDiff", "W.L")])[as.character(DiffDayNumber), "W"] / 
          sum(table(GameRestData[1:nrow(GameRestData), c("DayDiff", "W.L")])[as.character(DiffDayNumber), ]) 
        if(WP == 0){
          WP = 0.1
        }
      }
      return(round(WP, 4))
    }
    # call diff.day.wp function
    PredictNextGameHome["Probability", "GameRestDayWPHome"] <- diff.day.wp(DiffGameRestHome, GameRestDayHome)
    PredictNextGameAway["Probability", "GameRestDayWPAway"] <- diff.day.wp(DiffGameRestAway, GameRestDayAway)
    
    ### Current season win percentage
    if( length(which(AllDataAway$TeamDataSeasonHome$W.L == "W")) !=0  ) {
      PredictNextGameHome["Probability", "CurrentSeasonWPHome"] <- round( table(AllDataHome$TeamDataSeasonHome$W.L)["W"] / 
                                                                            sum(table(AllDataHome$TeamDataSeasonHome$W.L)), 4 )
    }else{
      PredictNextGameHome["Probability", "CurrentSeasonWPHome"] <- 0.1
    }
    if( length(which(AllDataAway$TeamDataSeasonAway$W.L == "W")) !=0 ) {
      PredictNextGameAway["Probability", "CurrentSeasonWPAway"] <- round( table(AllDataAway$TeamDataSeasonAway$W.L)["W"] / 
                                                                            sum(table(AllDataAway$TeamDataSeasonAway$W.L)), 4 )    
    }else{
      PredictNextGameAway["Probability", "CurrentSeasonWPAway"] <- 0.1
    }
    
    ### Game win percentage with opponent ago
    ## function for calculate the game win precentage with opponent ago 
    wp.with.opponent <- function(TeamData, OpponentTeamName) {
      if(OpponentTeamName == "BRK") {
        opp.data.first <- data.frame(subset(TeamData, Opp == OpponentTeamName))
        opp.data.second <- data.frame(subset(TeamData, Opp == "NJN"))
        opp.data <- rbind(opp.data.first, opp.data.second)
        if(is.na(table(opp.data$W.L)["W"])) {
          wp <- 0.01
        }else{
          wp <- round(table(opp.data$W.L)["W"] / sum(table(opp.data$W.L)), 4)
        }
      }else if(OpponentTeamName == "NOP") {
        opp.data.first <- data.frame(subset(TeamData, Opp == OpponentTeamName))
        opp.data.second <- data.frame(subset(TeamData, Opp == "NOH"))
        opp.data <- rbind(opp.data.first, opp.data.second)
        if(is.na(table(opp.data$W.L)["W"])) {
          wp <- 0.01
        }else{
          wp <- round(table(opp.data$W.L)["W"] / sum(table(opp.data$W.L)), 4)
        }
      }else if(OpponentTeamName == "CHO") {
        opp.data.first <- data.frame(subset(TeamData, Opp == OpponentTeamName))
        opp.data.second <- data.frame(subset(TeamData, Opp == "CHA"))
        opp.data <- rbind(opp.data.first, opp.data.second)
        if(is.na(table(opp.data$W.L)["W"])) {
          wp <- 0.01
        }else{
          wp <- round(table(opp.data$W.L)["W"] / sum(table(opp.data$W.L)), 4)
        }
      }else{
        opp.data <- data.frame(subset(TeamData, Opp == OpponentTeamName))
        if(is.na(table(opp.data$W.L)["W"])) {
          wp <- 0.01
        }else{
          wp <- round(table(opp.data$W.L)["W"] / sum(table(opp.data$W.L)), 4)
        }
      }
      return(wp)
    }
    # call wp.with.opponent function
    PredictNextGameHome["Probability", "OpponentWPHome"] <- round(wp.with.opponent(AllDataHome$TeamDataHome, AwayTeam), 4)
    PredictNextGameAway["Probability", "OpponentWPAway"] <- round(wp.with.opponent(AllDataAway$TeamDataAway, HomeTeam), 4)
    
    RecordWinPAndPred <- data.frame(Schedule[t,], GameDate = GameDateTime, AwayTeam = AwayTeam, HomeTeam = HomeTeam,
                                    PredictNextGameAway, PredictNextGameHome)
    RecordGameInfo <- rbind(RecordGameInfo, RecordWinPAndPred)
    ### Predict the next game win precentage
    #require(psych)
    #RecordWinpAndPredpHome <- data.frame()
    #RecordWinpAndPredpAway <- data.frame()
    ###　harmonic mean
    #RecordWinpAndPredpHome[1, "ada_mean"] <- round(harmonic.mean(t(PredictNextGameHome[, c("AdaMean", "GameRestDayWP", "CurrentSeasonWPHome")]), 
    #                                               na.rm=TRUE), 4)
    #RecordWinpAndPredpAway[1, "ada_mean"] <- round(harmonic.mean(t(PredictNextGameAway[, c("AdaMean", "GameRestDayWP", "CurrentSeasonWPAway")]), 
    #                                               na.rm=TRUE), 4)
    
    #RecordWinpAndPredpHome[1, "svm_mean"] <- round(harmonic.mean(t(PredictNextGameHome[, c("SvmMean", "GameRestDayWP", "CurrentSeasonWPHome")]), 
    #                                               na.rm=TRUE), 4)
    #RecordWinpAndPredpAway[1, "svm_mean"] <- round(harmonic.mean(t(PredictNextGameAway[, c("SvmMean", "GameRestDayWP", "CurrentSeasonWPAway")]), 
    #                                               na.rm=TRUE), 4)
    
    #RecordWinpAndPred <- data.frame(Schedule[t,], GameDate = GameDateTime, AwayTeam = AwayTeam, HomeTeam = HomeTeam,
    #                                AdaMeanAway = RecordWinpAndPredpAway$ada_mean,
    #                                AdaMeanHome = RecordWinpAndPredpHome$ada_mean,
    #                                SvmMeanAway = RecordWinpAndPredpAway$svm_mean,
    #                                SvmMeanHome = RecordWinpAndPredpHome$svm_mean
    #                                )  
    #RecordGameInfo <- rbind(RecordGameInfo, RecordWinpAndPred)
    print(c(t))
  }else{
    print(t)
    next()
  }
}

### Predict the next game win precentage
require(psych)
###　harmonic mean
harmonicwp.fun <- function(data, variable.1, variable.2, variable.3){
  har.wp <- apply(data[, c(variable.1, variable.2, variable.3)], 1 ,function(x) harmonic.mean(data.frame(x)))  
  return(har.wp)
}
RecordGameInfo <- as.data.frame(RecordGameInfo)
RecordGameInfo$har.adameanAway <- harmonicwp.fun(RecordGameInfo, "AdaMeanAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.adameanHome <- harmonicwp.fun(RecordGameInfo, "AdaMeanHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
RecordGameInfo$har.adafiveAway <- harmonicwp.fun(RecordGameInfo, "AdaFiveAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.adafiveHome <- harmonicwp.fun(RecordGameInfo, "AdaFiveHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
RecordGameInfo$har.adathreeAway <- harmonicwp.fun(RecordGameInfo, "AdaThreeAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.adathreeHome <- harmonicwp.fun(RecordGameInfo, "AdaThreeHome", "GameRestDayWPHome", "CurrentSeasonWPHome")

RecordGameInfo$har.svmmeanAway <- harmonicwp.fun(RecordGameInfo, "SvmMeanAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.svmmeanHome <- harmonicwp.fun(RecordGameInfo, "SvmMeanHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
RecordGameInfo$har.svmfiveAway <- harmonicwp.fun(RecordGameInfo, "SvmFiveAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.svmfiveHome <- harmonicwp.fun(RecordGameInfo, "SvmFiveHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
RecordGameInfo$har.svmthreeAway <- harmonicwp.fun(RecordGameInfo, "SvmThreeAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
RecordGameInfo$har.svmthreeHome <- harmonicwp.fun(RecordGameInfo, "SvmThreeHome", "GameRestDayWPHome", "CurrentSeasonWPHome")

### calculate expect value - mean
RecordGameInfo$ADAstandardmean <- with(RecordGameInfo, round(har.adameanHome-har.adameanAway, 4))
RecordGameInfo$SVMstandardmean <- with(RecordGameInfo, round(har.svmmeanHome-har.svmmeanAway, 4))

### calculate expect value - five
RecordGameInfo$ADAstandardfive <- with(RecordGameInfo, round(har.adafiveHome-har.adafiveAway, 4))
RecordGameInfo$SVMstandardfive <- with(RecordGameInfo, round(har.svmfiveHome-har.svmfiveAway, 4))

### calculate expect value - five
RecordGameInfo$ADAstandardthree <- with(RecordGameInfo, round(har.adathreeHome-har.adathreeAway, 4))
RecordGameInfo$SVMstandardthree <- with(RecordGameInfo, round(har.svmthreeHome-har.svmthreeAway, 4))

RecordGameInfo <- RecordGameInfo[, c("GameDate", "AwayTeam", "HomeTeam",
                                     "AdaMeanAway", "AdaFiveAway", "AdaThreeAway", "SvmMeanAway", "SvmFiveAway", "SvmThreeAway", 
                                     "GameRestDayWPAway", "CurrentSeasonWPAway", "OpponentWPAway", 
                                     "AdaMeanHome", "AdaFiveHome", "AdaThreeHome", "SvmMeanHome", 'SvmFiveHome', "SvmThreeHome",
                                     "GameRestDayWPHome", "CurrentSeasonWPHome", "OpponentWPHome", 
                                     "ADAstandardmean", "SVMstandardmean", 
                                     "ADAstandardfive", "SVMstandardfive", 
                                     "ADAstandardthree", "SVMstandardthree")]

### show the final prediction
NextGamebEetting <- RecordGameInfo[, c("GameDate", "AwayTeam", "HomeTeam", "ADAstandardmean", "SVMstandardmean", 
                                       "ADAstandardfive", "SVMstandardfive",
                                       "ADAstandardthree", "SVMstandardthree")]

Sys.setlocale("LC_TIME", "")
