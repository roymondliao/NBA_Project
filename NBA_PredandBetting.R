library(rJava)
library(xlsx)
library(data.table)
library(plyr)
library(dplyr)

SeasonCurrent = 2013
RecordGameInfo <- read.table(sprintf("~/Documents/Sport_analysis/basketball//Pred_Data//RecordGameInfo_%s.txt", SeasonCurrent), 
                              header = TRUE, sep = "\t")
BettingData <- read.table(sprintf("~/Documents/Sport_analysis/basketball//Betting_Data//Batting_Data_pointspreads_%s.txt", SeasonCurrent), 
                          header = TRUE, sep = "\t")
NBATeamName <- read.xlsx2("~/Documents/Sport_analysis/basketball//Team_Data//NBA_Team_Name.xlsx", 
                          sheetIndex = as.character(SeasonCurrent), header = TRUE)

### delete the noise information
searchword <- c("Conference", "West", "All Star", "Western", "Team")
idx <- unlist(apply(as.data.frame(searchword), 1, function(x) grep(x, BettingData$Away_Team)))
if(length(idx) != 0){
  BettingData <- BettingData[-idx, ]  
}

### convert the money line to point line
BettingData$OddAway_BET365 <- round(with(BettingData, ifelse(bet365_Away>0, (bet365_Away/100)+1, (100/-bet365_Away)+1)), 2)
BettingData$OddHome_BET365 <- round(with(BettingData, ifelse(bet365_Home>0, (bet365_Home/100)+1, (100/-bet365_Home)+1)), 2)
BettingData$spreadsOddAway_BET365 <- round(with(BettingData, ifelse(spreadline_bet365_Away>0, (spreadline_bet365_Away/100)+1, (100/-spreadline_bet365_Away)+1)), 2)
BettingData$spreadsOddHome_BET365 <- round(with(BettingData, ifelse(spreadline_bet365_Home>0, (spreadline_bet365_Home/100)+1, (100/-spreadline_bet365_Home)+1)), 2)
### calculate the win percent 
#BettingData$BettingWPAway_BET365 <- round(with(BettingData, 1/BettingAway_BET365), 4)
#BettingData$BettingWPHome_BET365 <- round(with(BettingData, 1/BettingHome_BET365), 4)
#BettingData$BettingWPAway_5Dimes <- round(with(BettingData, ifelse(X5Dimes_Away>0, 100/(100+X5Dimes_Away), abs(X5Dimes_Away)/(abs(X5Dimes_Away)+100))), 4)
#BettingData$BettingWPHome_5Dimes <- round(with(BettingData, ifelse(X5Dimes_Home>0, 100/(100+X5Dimes_Home), abs(X5Dimes_Home)/(abs(X5Dimes_Home)+100))), 4)
#BettingData$BettingWPTotal_BET365 <- with(BettingData, BettingWPAway+BettingWPHome)

### merge RecordGameInfo and BettingData
library(psych)
# NBATeamName must be data.frame format
matchAwayTeam <- lookup(BettingData[, "Away_Team"], NBATeamName[, c("Team_Short_Name", "Team_Name_Betting")], c1 = 2)[, 1]
matchHomeTeam <- lookup(BettingData[, "Home_Team"], NBATeamName[, c("Team_Short_Name", "Team_Name_Betting")], c1 = 2)[, 1]
BettingData$shortAwayTeam <- matchAwayTeam
BettingData$shortHomeTeam <- matchHomeTeam
colnames(BettingData) <- c("GameDate", "Away_Team",  "Home_Team", "bet365_Away", "bet365_Home", "X5Dimes_Away", "X5Dimes_Home",
                           "pointspreads_bet365_Away", "pointspreads_bet365_Home", "spreadline_bet365_Away", "spreadline_bet365_Home",
                           "OddAway_BET365", "OddHome_BET365", "spreadsOddAway_BET365", "spreadsOddHome_BET365", "AwayTeam", "HomeTeam")
#"BettingWPAway_BET365", "BettingWPHome_BET365", "BettingWPAway_5Dimes", "BettingWPHome_5Dimes",

### merge the betting data and pred data 
BettingData <- mutate(BettingData, GameDate = as.Date(as.character(GameDate)))
RecordGameInfo$W.L <- with(RecordGameInfo, ifelse(RecordGameInfo$Home_Score-RecordGameInfo$Away_Score >0, "HW", "AW"))
RecordGameInfo <- mutate(RecordGameInfo, GameDate = as.Date(GameDate))
FinalBettingandPredData <- merge(RecordGameInfo, BettingData, by = c("GameDate", "AwayTeam", "HomeTeam"))
FinalBettingandPredData <- FinalBettingandPredData[, c("GameDate", "AwayTeam", "HomeTeam", "Away_Score", "Home_Score", "W.L", 
                                   "AdaMeanAway", "AdaFiveAway", "AdaThreeAway", "SvmMeanAway", "SvmFiveAway", "SvmThreeAway", 
                                   "GameRestDayWPAway", "CurrentSeasonWPAway", "OpponentWPAway", 
                                   "AdaMeanHome", "AdaFiveHome", "AdaThreeHome", "SvmMeanHome", "SvmFiveHome", "SvmThreeHome",
                                   "GameRestDayWPHome", "CurrentSeasonWPHome", "OpponentWPHome", 
                                   "bet365_Away", "bet365_Home", "OddAway_BET365", "OddHome_BET365",
                                   "pointspreads_bet365_Away", "pointspreads_bet365_Home", "spreadsOddAway_BET365", "spreadsOddHome_BET365")]

### Predict the next game win precentage
require(psych)
###　harmonic mean
harmonicwp.fun <- function(data, variable.1, variable.2, variable.3){
  har.wp <- apply(data[, c(variable.1, variable.2, variable.3)], 1 ,function(x) harmonic.mean(data.frame(x)))  
  return(har.wp)
}
### strategy - mean
FinalBettingandPredData$har.adameanAway <- harmonicwp.fun(FinalBettingandPredData, "AdaMeanAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.adameanHome <- harmonicwp.fun(FinalBettingandPredData, "AdaMeanHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
FinalBettingandPredData$har.svmmeanAway <- harmonicwp.fun(FinalBettingandPredData, "SvmMeanAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.svmmeanHome <- harmonicwp.fun(FinalBettingandPredData, "SvmMeanHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
### strategy - five
FinalBettingandPredData$har.adafiveAway <- harmonicwp.fun(FinalBettingandPredData, "AdaFiveAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.adafiveHome <- harmonicwp.fun(FinalBettingandPredData, "AdaFiveHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
FinalBettingandPredData$har.svmfiveAway <- harmonicwp.fun(FinalBettingandPredData, "SvmFiveAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.svmfiveHome <- harmonicwp.fun(FinalBettingandPredData, "SvmFiveHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
### strategy - three
FinalBettingandPredData$har.adathreeAway <- harmonicwp.fun(FinalBettingandPredData, "AdaThreeAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.adathreeHome <- harmonicwp.fun(FinalBettingandPredData, "AdaThreeHome", "GameRestDayWPHome", "CurrentSeasonWPHome")
FinalBettingandPredData$har.svmthreeAway <- harmonicwp.fun(FinalBettingandPredData, "SvmThreeAway", "GameRestDayWPAway", "CurrentSeasonWPAway")
FinalBettingandPredData$har.svmthreeHome <- harmonicwp.fun(FinalBettingandPredData, "SvmThreeHome", "GameRestDayWPHome", "CurrentSeasonWPHome")

### calculate expect value - mean
FinalBettingandPredData$W.L_adamean <- with(FinalBettingandPredData, ifelse(har.adameanHome-har.adameanAway >0, "HW", "AW"))
FinalBettingandPredData$W.L_svmmean <- with(FinalBettingandPredData, ifelse(har.svmmeanHome-har.svmmeanAway >0, "HW", "AW"))
FinalBettingandPredData$diffadamean <- with(FinalBettingandPredData, abs(har.adameanHome-har.adameanAway))
FinalBettingandPredData$diffsvmmean <- with(FinalBettingandPredData, abs(har.svmmeanHome-har.svmmeanAway))
FinalBettingandPredData$EV_ADAmean <- with(FinalBettingandPredData, ifelse(W.L == W.L_adamean & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_adamean & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$EV_SVMmean <- with(FinalBettingandPredData, ifelse(W.L == W.L_svmmean & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_svmmean & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$reEV_ADAmean <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_adamean == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_adamean == "AW", OddHome_BET365-1, -1)))
FinalBettingandPredData$reEV_SVMmean <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_svmmean == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_svmmean == "AW", OddHome_BET365-1, -1)))

### calculate expect value - five
FinalBettingandPredData$W.L_adafive <- with(FinalBettingandPredData, ifelse(har.adafiveHome-har.adafiveAway >0, "HW", "AW"))
FinalBettingandPredData$W.L_svmfive <- with(FinalBettingandPredData, ifelse(har.svmfiveHome-har.svmfiveAway >0, "HW", "AW"))
FinalBettingandPredData$diffadafive <- with(FinalBettingandPredData, abs(har.adafiveHome-har.adafiveAway))
FinalBettingandPredData$diffsvmfive <- with(FinalBettingandPredData, abs(har.svmfiveHome-har.svmfiveAway))
FinalBettingandPredData$EV_ADAfive <- with(FinalBettingandPredData, ifelse(W.L == W.L_adafive & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_adafive & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$EV_SVMfive <- with(FinalBettingandPredData, ifelse(W.L == W.L_svmfive & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_svmfive & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$reEV_ADAfive <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_adafive == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_adafive == "AW", OddHome_BET365-1, -1)))
FinalBettingandPredData$reEV_SVMfive <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_svmfive == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_svmfive == "AW", OddHome_BET365-1, -1)))

### calculate expect value - three
FinalBettingandPredData$W.L_adathree <- with(FinalBettingandPredData, ifelse(har.adathreeHome-har.adathreeAway >0, "HW", "AW"))
FinalBettingandPredData$W.L_svmthree <- with(FinalBettingandPredData, ifelse(har.svmthreeHome-har.svmthreeAway >0, "HW", "AW"))
FinalBettingandPredData$diffadathree <- with(FinalBettingandPredData, abs(har.adathreeHome-har.adathreeAway))
FinalBettingandPredData$diffsvmthree <- with(FinalBettingandPredData, abs(har.svmthreeHome-har.svmthreeAway))
FinalBettingandPredData$EV_ADAthree <- with(FinalBettingandPredData, ifelse(W.L == W.L_adathree & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_adathree & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$EV_SVMthree <- with(FinalBettingandPredData, ifelse(W.L == W.L_svmthree & W.L == "HW", OddHome_BET365-1, 
                                                                           ifelse(W.L == W.L_svmthree & W.L == "AW", OddAway_BET365-1, -1)))
FinalBettingandPredData$reEV_ADAthree <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_adathree == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_adathree == "AW", OddHome_BET365-1, -1)))
FinalBettingandPredData$reEV_SVMthree <- with(FinalBettingandPredData, ifelse(W.L == "AW" & W.L_svmthree == "HW", OddAway_BET365-1, 
                                                                             ifelse(W.L == "HW" & W.L_svmthree == "AW", OddHome_BET365-1, -1)))
### output the outcome
All_star_game_date <- which(FinalBettingandPredData$GameDate[2:length(FinalBettingandPredData$GameDate)]-
                              FinalBettingandPredData$GameDate[1:length(FinalBettingandPredData$GameDate)-1] > 4)

write.xlsx2(FinalBettingandPredData[(1:All_star_game_date), ],
            sprintf("~/Documents/Sport_analysis/basketball//Betting_Data//final_%s.xlsx", SeasonCurrent), 
            row.names = FALSE, sheetName = "Pre")
write.xlsx2(FinalBettingandPredData[(All_star_game_date+1:length(FinalBettingandPredData$GameDate)), ] ,
            sprintf("~/Documents/Sport_analysis/basketball//Betting_Data//final_%s.xlsx", SeasonCurrent), 
            row.names = FALSE, append = TRUE, sheetName = "Post")
