##--------------------------------------Download the team gamelog--------------------------------------##
##----------------------Environment Setting----------------------##
source("~/Documents/Sport_analysis/basketball//NBA_Prediction_Analysis//Setting_Packages.R")
options(stringAsFactor = TRUE)
##----------------------Season Setting----------------------##
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  Season <- seq(as.numeric(format(Sys.time(), "%Y"))-1, as.numeric(format(Sys.time(), "%Y"))-1)
}else{
  Season <- seq(as.numeric(format(Sys.time(), "%Y")), as.numeric(format(Sys.time(), "%Y")))
}
#Season <- seq(as.numeric(format(Sys.time(), "%Y"))-6, as.numeric(format(Sys.time(), "%Y"))-5)
system.time(
  for(s in 1:length(Season)){ 
    ##----------------------NBA_Team_Name----------------------##
    season <- as.character(Season[s])
    NBA_Team_Name <- read.xlsx("~/Documents/Sport_analysis/basketball//Team_Data//NBA_Team_Name.xlsx", sheetName = season, 
                               header = TRUE)
    Team_Short_Name <- as.character(NBA_Team_Name$Team_Short_Name)
    for(t in 1:length(Team_Short_Name)){
      Team_Name <- Team_Short_Name[t]
      url <- sprintf("http://www.basketball-reference.com/teams/%s/%s/gamelog/", Team_Name, season)
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
        Game_Log$Season <- with(Game_Log, season)
      }
    ##----------------------JSON FORM----------------------##
    #file=sprintf("C://Users//user//Documents//My Dropbox//NBA_Code//NBA_Data//Team_Data//%s_%d.json", Team_Short_Name[t], Season[s])
    #Game_Log_Json=toJSON(Game_Log)
    #writeLines(Game_Log_Json, file)
    ##----------------------data.frame FORM----------------------##
    file <- sprintf("~/Documents/Sport_analysis/basketball/Team_Data/%s/%s_%s.txt", season,
                    Team_Name, season)
    write.table(Game_Log, file, col.names = TRUE, row.names = FALSE, sep = "\t")
    }
  }
)
