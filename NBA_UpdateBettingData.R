require(RCurl)
library(stringr)
library(XML)
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))-1
}else{
  SeasonCurrent <- as.numeric(format(Sys.time(), "%Y"))
}
### game date 
#datetime <- seq.Date(from = as.Date("2014-10-28"), to = as.Date("2015-03-01"), by = "day")
datetime <- Sys.Date()-1
### money line and  point spreads line
BettingDataTotal <- data.frame()
#datetime <- Sys.Date()-1
for(d in 1:length(datetime)){
  BettingData <- data.frame()
  url <- paste("http://www.sportsbookreview.com/betting-odds/nba-basketball/money-line/?date=", format(datetime[d], "%Y%m%d"), sep = "")
  html <- getURLContent(url)  
  xml <- htmlParse(html, asText = TRUE, encoding = "UTF-8")
  ## team name
  xmlnodeteam <- getNodeSet(xml, path = "//div//span[@class = 'team-name']")
  teamname <- sapply(xmlnodeteam, xmlValue)
  if(length(teamname) != 0){
    gamenumber <- length(teamname)/2
    BettingData[1:gamenumber, "GameDate"] <- as.character(datetime[d])
    for(t in 0:(gamenumber-1)){
      BettingData[t+1,"Away_Team"] <- teamname[t*2+1]
      BettingData[t+1,"Home_Team"] <- teamname[t*2+2]
    }
    ## money line
    xmlnodeline <- getNodeSet(xml, path = "//div//div[@class = 'eventLine-book-value ']")
    moneyline <- sapply(xmlnodeline, xmlValue)
    moneyline <- moneyline[-grep(":", moneyline)]
    moneylineHome <- moneyline[(1:length(moneyline))%%2 == 0]
    moneylineAway <- moneyline[(1:length(moneyline))%%2 == 1]
    for(m in 0:(gamenumber-1)){
      BettingData[m+1, "bet365_Away"] <- moneylineAway[(m*10+7)]
      BettingData[m+1, "bet365_Home"] <- moneylineHome[(m*10+7)]
      BettingData[m+1, "PINNACLE_Away"] <- moneylineAway[(m*10+2)]
      BettingData[m+1, "PINNACLE_Home"] <- moneylineHome[(m*10+2)]  
    }
    
    ### point spreads url
    pointspreads.url <- paste("http://www.sportsbookreview.com/betting-odds/nba-basketball/?date=", format(datetime[d], "%Y%m%d"), sep = "")
    pointspreads.html <- getURLContent(pointspreads.url)  
    pointspreads.xml <- htmlParse(pointspreads.html, asText = TRUE, encoding = "UTF-8")
    ## total and point spreads
    xmlnodeline.bet365 <- getNodeSet(pointspreads.xml, path = "//div[@rel = '43']//div[@class = 'eventLine-book-value ']//b") ### bet365
    #xmlnodeline.PINNACLE <- getNodeSet(xml, path = "//div[@rel = '238']//div[@class = 'eventLine-book-value ']//b") ### 5Dimes
    pointspreads.bet365 <- sapply(xmlnodeline.bet365, xmlValue)
    #pointspreads.PINNACLE <- sapply(xmlnodeline.5Dimes, xmlValue)
    
    pointspreads.fun <- function(pointspreads){
      bettingdata <- data.frame()
      moneylineHome <- pointspreads[(1:length(pointspreads))%%2 == 0]
      moneylineAway <- pointspreads[(1:length(pointspreads))%%2 == 1]
      for(m in 1:gamenumber){
        bettingdata[m, "pointspreads_bet365_Away"] <- str_extract(moneylineAway[m], "^.\\w.")
        bettingdata[m, "pointspreads_bet365_Home"] <- str_extract(moneylineHome[m], "^.\\w.")
        bettingdata[m, "spreadline_bet365_Away"] <- str_extract(moneylineAway[m], ".\\d+$")
        bettingdata[m, "spreadline_bet365_Home"] <- str_extract(moneylineHome[m], ".\\d+$")
      }
      return(bettingdata)
    }
    pointspreadsdata <- pointspreads.fun(pointspreads.bet365)
    BettingData <- cbind(BettingData, pointspreadsdata)
    BettingDataTotal <- rbind(BettingDataTotal, BettingData)
    if(d %% 10 == 0){
      Sys.sleep(60)
    }
    print(d)
  }
}
BettingDataTotal$pointspreads_bet365_Away <- str_replace(BettingDataTotal$pointspreads_bet365_Away, iconv("½", "UTF-8", "UTF-8"), ".5")
BettingDataTotal$pointspreads_bet365_Home <- str_replace(BettingDataTotal$pointspreads_bet365_Home, iconv("½", "UTF-8", "UTF-8"), ".5")
BettingDataTotal$pointspreads_bet365_Away <- str_replace(BettingDataTotal$pointspreads_bet365_Away, "\\s", "")
BettingDataTotal$pointspreads_bet365_Home <- str_replace(BettingDataTotal$pointspreads_bet365_Home, "\\s", "")
BettingDataTotal$pointspreads_bet365_Away <- str_replace(BettingDataTotal$pointspreads_bet365_Away, "PK-", "0")
BettingDataTotal$pointspreads_bet365_Home <- str_replace(BettingDataTotal$pointspreads_bet365_Home, "PK-", "0")
idx <- grep("Conference", BettingDataTotal$AwayTeam)
if(length(idx) != 0){
  BettingDataTotal <- BettingDataTotal[-idx, ]
}

write.table(BettingDataTotal ,sprintf("~/Documents/Sport_analysis/basketball/Betting_Data/Batting_Data_pointspreads_%s.txt", SeasonCurrent), 
            row.names = FALSE, append = TRUE, sep = "\t", col.names = FALSE)

