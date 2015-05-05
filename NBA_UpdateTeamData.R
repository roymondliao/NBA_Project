##--------------------------------------Download the team gamelog--------------------------------------##
##----------------------Environment Setting----------------------##
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis/Setting_Packages.R")
options(stringAsFactor = TRUE)
##----------------------Season Setting----------------------##
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  Season <- seq(as.numeric(format(Sys.time(), "%Y"))-1, as.numeric(format(Sys.time(), "%Y"))-1)
}else{
  Season <- seq(as.numeric(format(Sys.time(), "%Y")), as.numeric(format(Sys.time(), "%Y")))
}
#Season <- seq(as.numeric(format(Sys.time(), "%Y"))-6, as.numeric(format(Sys.time(), "%Y"))-5)

### Season Schedule 
ScheduleFile <- sprintf("~/Documents/Sport_analysis/basketball/Season_Data/Season_%s.txt", Season)
TodayGameSchedule <- read.table(ScheduleFile, header=TRUE,  sep="\t", as.is=TRUE)
## function for convert date
convert.date.type<- function(DateVector) {
  datetime <- str_extract(DateVector, "[[:alpha:]]+.[0-9]+[[:punct:]].[0-9]+$")
  datetime <- str_replace(datetime , ",", "")
  datetime <- unlist(strsplit(datetime, " "))
  datetime <- str_replace_all(paste(datetime[3], datetime[1], datetime[2]), " ", "")
  datetime <- as.character(as.Date(datetime, "%Y %b %d"))
  return(datetime)
}
Sys.setlocale("LC_TIME", "C") # convert OS LC_NUMERIC = C for analysis data form 
## call convert.date.type function
TodayGameSchedule$CovertDate <- unlist(lapply(TodayGameSchedule$Date, convert.date.type))
predgameinfoidx <- grep(Sys.Date(), TodayGameSchedule$CovertDate)
TodayGameSchedule <- TodayGameSchedule[predgameinfoidx, ]

### get the team's short name
NBA_Team_Name <- read.xlsx("~/Documents/Sport_analysis/basketball/Team_Data/NBA_Team_Name.xlsx", sheetName = as.character(Season), 
                           header = TRUE)
### merge RecordGameInfo and BettingData
library(psych)
# NBATeamName must be data.frame format
matchAwayTeam <- lookup(TodayGameSchedule[, "Away_Team"], NBA_Team_Name[, c("Team_Short_Name", "Team_Name")], c1 = 2)[, 1]
matchHomeTeam <- lookup(TodayGameSchedule[, "Home_Team"], NBA_Team_Name[, c("Team_Short_Name", "Team_Name")], c1 = 2)[, 1]
TodayGameSchedule$shortAwayTeam <- matchAwayTeam
TodayGameSchedule$shortHomeTeam <- matchHomeTeam
colnames(TodayGameSchedule) <- c("Date", "Away_Team",  "Away_Score", "Home_Team", "Home_Score", "AwayTeam", "HomeTeam")
updateTeamName <- c(as.character(matchAwayTeam), as.character(matchHomeTeam))
### update the team data
system.time(
##----------------------NBA_Team_Name----------------------##
for(t in 1:length(updateTeamName)){
  Team_Name <- updateTeamName[t]
  url <- sprintf("http://www.basketball-reference.com/teams/%s/%s/gamelog/", Team_Name, Season)
      if(url.exists(url)){
        Game_Info <- readHTMLTable(url)
        Game_Log <- data.frame(Game_Info$tgl_basic)
        Game_Log <- Game_Log[ , -c(1, 25)]
        colnames(Game_Log) <- c("G", "Date", "H.A", "Opp", "W.L", "PTS", "Opp.PTS", "FG", "FGA", "FGP", "3P", "3PA", 
                                "3PP", "FT", "FTA", "FTP", "ORB", "TRB", "AST", "STL", "BLK", "TOV", "PF",     
                                "Opp.FG", "Opp.FGA", "Opp.FGP", "Opp.3P", "Opp.3PA", "Opp.3PP", "Opp.FT", "Opp.FTA", "Opp.FTP", "Opp.ORB", 
                                "Opp.TRB", "Opp.AST", "Opp.STL", "Opp.BLK", "Opp.TOV", "Opp.PF" )
        Game_Log <- mutate(Game_Log, H.A=str_replace_all(H.A, "", "H"))  
        Game_Log <- mutate(Game_Log, H.A=str_replace_all(H.A, "H@H", "A"))
        Game_Log <- mutate(Game_Log, W.L=substr(W.L, 1, 1))
        Game_Log <- Game_Log[!Game_Log$G=="G", ]
        Game_Log <- Game_Log[!Game_Log$G=="Team", ]
        Game_Log$Season <- with(Game_Log, Season)
      }
      ##----------------------JSON FORM----------------------##
      #file=sprintf("C://Users//user//Documents//My Dropbox//NBA_Code//NBA_Data//Team_Data//%s_%d.json", Team_Short_Name[t], Season[s])
      #Game_Log_Json=toJSON(Game_Log)
      #writeLines(Game_Log_Json, file)
      ##----------------------data.frame FORM----------------------##
      file <- sprintf("~/Documents/Sport_analysis/basketball/Team_Data/%s/%s_%s.txt", Season,
                      Team_Name, Season)
      write.table(Game_Log, file, col.names = TRUE, row.names = FALSE, sep = "\t")
  print(Team_Name)
}
)
 
Sys.setlocale("LC_TIME", "") # convert OS LC_NUMERIC = C for analysis data form 