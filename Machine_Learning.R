##--------------------------------------Machine Learning and model testing--------------------------------------##
##----------------------load the package----------------------##
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Packages.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Advanced_Statistic.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Normalize.R")
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Predict_Data.R")

##----------------------Current Season----------------------##
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  Season_Current <- as.numeric(format(Sys.time(), "%Y"))-1
}else{
  Season_Current <- as.numeric(format(Sys.time(), "%Y"))
}

##----------------------NBA_Team_Name----------------------##
NBA_Team_Name <- read.xlsx("~/Documents/Sport_analysis/basketball//Team_Data//NBA_Team_Name.xlsx",                           
                           sheetIndex = as.character(Season_Current), header = TRUE) # load Team Name data
NBA_Team_Name <- data.table(NBA_Team_Name) # convert to data.table
calculate.df <- data.frame()  # record all of ML model's result
for(t in 1:nrow(NBA_Team_Name)) {
  Team.Name.pick <- as.character(NBA_Team_Name$Team_Short_Name[t])  # All of Team Name 
  merge.season <- sort(seq(Season_Current-3, Season_Current), decreasing = TRUE)  # passed three season and incloud current season
  # function for load team data
  load.team.data <- function(Team.Name, merge.season) {
    i <- 1
    Team.data <- data.frame() 
    while(i<5){
      if(Team.Name=="BRK" &&  merge.season[i]<2013) {
        Team.Name <- "NJN"   # merge NJN data to BRK data
      }else if(Team.Name=="NOP" &&  merge.season[i]<2014) {
        Team.Name <- "NOH"   # merge NOH data to NOP data
      }else if(Team.Name=="CHO" &&  merge.season[i]<2015) {
        Team.Name <- "CHA"   # merge CHA data to CHO data
      }
      Merge.Team.data <- read.table(sprintf("~/Documents/Sport_analysis/basketball//Team_Data//%d//%s_%d.txt", 
                                         merge.season[i], Team.Name, merge.season[i]), header = TRUE, sep = "\t", as.is = TRUE, dec = ".")
      Team.data <- rbind(Team.data, Merge.Team.data)  # load the team data
      i <- i+1
    } 
    return(Team.data)
  }
  # calculate
  Team.Data.Set <- load.team.data(Team.Name.pick, merge.season) # load the team data
  Team.Data.Set <- Team.Data.Set[order(Team.Data.Set$Date), ] # sort data's date
  
  ##----------------------Team Data Transfer & deal---------------------##
  # function for transfor to different data set 
  transfor.data <- function(team.data ,Season_Current) {
    team.data <- Advanced_Statistic(team.data)
    team.data <- Normalize(team.data)
    team.data.H <- data.frame(subset(team.data, H.A == "H"))  ## Home team data since three season ago 
    rownames(team.data.H) <- c(1:nrow(team.data.H))
    team.data.A=data.frame(subset(team.data, H.A == "A"))
    rownames(team.data.A) <- c(1:nrow(team.data.A))
    team.data.season <- data.frame(subset(team.data, Season == Season_Current))  ## Home team data of current Season
    rownames(team.data.season) <- c(1:nrow(team.data.season))
    team.data.season.H <- data.frame(subset(team.data.season, H.A == "H"))
    team.data.season.A <- data.frame(subset(team.data.season, H.A == "A"))
    result <- list(team.data = team.data, team.data.H = team.data.H, team.data.A = team.data.A, 
                   team.data.season = team.data.season, team.data.season.H = team.data.season.H, team.data.season.A = team.data.season.A)
    return(result)
  }
  Team.Data.Set <- transfor.data(Team.Data.Set, Season_Current)  # separate different type data
  
  # function 
  Game.Data.location<- function(team.data, location, Season_Current) {
    game.data <- data.frame(subset(team.data, H.A == as.character(location)))
    length.game <- nrow(game.data)
    length.season <- length(which(game.data$Season == Season_Current))
    return(list(Game.Data = game.data, length.game = length.game, length.season = length.season))
  }
  # function for prepare prediction data
  pred.data <- function(team.data, current.season.game, current.season.game.fnail) {
    # PredictData(data, frist.game.no, final.game.no)
    pred.Mean.Data <- PredictData(team.data, current.season.game, current.season.game.fnail) # mean strategy
    Pred.Five.Data <- PredictData(team.data, current.season.game.fnail-4, current.season.game.fnail) # previous five game strategy
    return(list(mean.data = pred.Mean.Data, five.data = Pred.Five.Data))
  }
  location = c("H", "A")
  for(l in location) {
    # Team data set
    Game.Data <- Game.Data.location(Team.Data.Set$team.data, as.character(l), Season_Current)
   for(i in 5:Game.DataSet$length.season) {
    # prediction data set
    Pred.data<- pred.data(Game.Data$Game.Data, Game.Data$length.game-Game.Data$length.season+1, (Game.Data$length.game-Game.Data$length.season)+i)
    ##----------------------Machine Learning method---------------------##
    model.formula <- as.formula("W.L ~ diffEFG + diffTOV + diffRB + diffFTR")
    train.data <- Game.Data$Game.Data[1:(Game.Data$length.game-Game.Data$length.season+5), ]
    train.idx <- sample(1:(Game.Data$length.game-Game.Data$length.season+5), 1000, replace = TRUE)
    train.data2 <- Game.Data$Game.Data[train.idx, ]
    test.data <- Game.Data$Game.Data[(Game.Data$length.game-Game.Data$length.season+6):(Game.Data$length.game-Game.Data$length.season+41), ]
    # adaboost
    iternumber <- 100 # defult=50
    ML.adaboost <- ada(model.formula, data = train.data, 
                       test.x = test.data[, c("diffEFG", "diffTOV", "diffRB", "diffFTR")],
                       test.y = test.data[, "W.L"],
                       loss = c("logistic"), iter = iternumber, nu = 0.1, type = "real",
                       control = rpart.control()) # nu:slow down learning for allowing to dramatically reduce overfitting.
    #plot(ML.adaboost, kappa = FALSE , test = TRUE)
    #varplot(ML.adaboost)
    pred.adaboost <- predict(ML.adaboost, test.data, type = c("vector"))
    ada.error.rate <- 1-round(sum(diag(table(pred.adaboost, test.data$W.L)))/sum(table(pred.adaboost, test.data$W.L)), 4)
    
    # bayes belive network
    # ANN - Artificial Neural Network - "response variable must be a binary classification"
    require(neuralnet)
    train.data2$W.L.Convert <- with(train.data2, ifelse(W.L == "W", 1, 0)) 
    ML.nn <- neuralnet("W.L.Convert ~ diffEFG + diffTOV + diffRB + diffFTR", data = train.data2, algorithm = "backprop",hidden = 6, 
                              err.fct = "sse", act.fct = "logistic", learningrate=0.5, linear.output=FALSE)
    pred.nn <- compute(ML.nn, train.data[, c("diffEFG", "diffTOV", "diffRB", "diffFTR")])
    cor(train.data$W.L.Convert, pred.nn$net.result)
    
    
    
    # svm - "dependent variable must be factor"
    require(e1071)
    train.data$W.L <- as.factor(train.data$W.L)
    ML.svm<- svm(model.formula, data = train.data, probability = TRUE, kernel = "radial" , cost = 2, cross = 10, gamma = 0.25)
    pred.svm <- predict(ML.svm, test.data, type = "class")
    svm.error.rate <- 1-round(sum(diag(table(pred.svm, test.data$W.L)))/sum(table(pred.svm, test.data$W.L)), 4)
    #tune(svm, model.formula, data = train.data, ranges = list(gamma = 2^(-2:2), cost = 2^(0:10)), 
    #                       tunecontrol = tune.control(sampling = "bootstrap"))

    require(kernlab)
    ML.ksvm <- ksvm(W.L ~ diffEFG + diffTOV + diffRB + diffFTR, data = train.data, kernel = "rbfdot", C = 2, cross = 10)
    pred.ksvm <- predict(ML.ksvm, test.data, type = "response")
    ksvm.error.rate <- 1-round(sum(diag(table(pred.ksvm, test.data$W.L)))/sum(table(pred.ksvm, test.data$W.L)), 4)
    
    # decision tree : CART (Class and Regression Tree)
    require(rpart)
    #ML.rpart.fun <- function(model.formula, team.data){
    #  ML.rpart.cart <- rpart(model.formula, data = team.data) 
    #  plot(ML.rpart, uniform = TRUE, compress = TRUE, lty = 3, branch = 0.7)
    #  text(ML.rpart, all = TRUE, digits = 6, use.n = TRUE, splits = TRUE, cex = 0.9, xpd = TRUE)
    #  return(ML.rpart.cart)
    #}
    ML.rpart <- rpart(model.formula, train.data, method = "class", control = rpart.control(minsplit = 10, xval = 10))
    pred.rpart <- data.frame(predict(ML.rpart, test.data))
    pred.rpart$label <- with(pred.rpart, ifelse(W>0.5, "W", "L"))
    pred.rpart.error.rate <- 1-round(sum(diag(table(pred.rpart$label, test.data$W.L)))/sum(table(pred.rpart$label, test.data$W.L)), 4)
    #tune.rpart(model.formula, train.data, minsplit = c(2:10), cp = c(0.1:0.5))
    
    # decision tree : C4.5
    require(RWeka)
    ML.C45 <- J48(model.formula, data = train.data, control = Weka_control(C = 0.25, M = 5))
    pred.C45 <- predict(ML.C4.5, test.data)
    pred.C45.error.rate <- 1-round(sum(diag(table(pred.C45, test.data$W.L)))/sum(table(pred.C45, test.data$W.L)), 4)
    
    #decision tree : C5.0
    require(C50)
    ML.C50 <- C5.0(train.data[, c("diffEFG", "diffTOV", "diffRB", "diffFTR")], train.data[, "W.L"], trials = 10)
    summary(ML.C50)
    pred.C50 <- predict(ML.C50, test.data) 
    pred.C50.error.rate<- 1-round(sum(diag(table(pred.C50, test.data$W.L)))/sum(table(pred.C50, test.data$W.L)), 4)

    # randomForest 
    library(randomForest)
    ML.RF <- randomForest(model.formula, data = train.data, mtry = 2, ntree = 500, importance = TRUE, proximity=TRUE)
    pred.RF <- predict(ML.RF, test.data) 
    pred.RF.error.rate<- 1-round(sum(diag(table(pred.RF, test.data$W.L)))/sum(table(pred.RF, test.data$W.L)), 4)
    }
  }
}




