##----------------------Environment Setting----------------------##
source("~/Documents/Sport_analysis/basketball/NBA_Prediction_Analysis//Setting_Packages.R")

##----------------------NBA Season Schedule & Results----------------------##
#Season=seq(as.numeric(format(Sys.time(), "%Y")), as.numeric(format(Sys.time()+1, "%Y")))
#for(s in 1:length(Season)){ 
#  url=sprintf("http://www.basketball-reference.com/leagues/NBA_%d_games.html", Season[s])
#  if(url.exists(url)){
#    html=getURL(url)
#    xml=htmlParse(html, asText=TRUE)
#    Game_Schedule_left=getNodeSet(xml, path="//tr//td[@align='left']")
#    Game_Schedule_right=getNodeSet(xml, path="//tr//td[@align='right']")    
#    Game_left_Info=sapply(Game_Schedule_left, xmlValue)
#    Game_right_Info=sapply(Game_Schedule_right, xmlValue)
#    }
#  ##--------Schedule--------##
#    Game_Date=Game_left_Info[(1:length(Game_left_Info))%%4==1]
#    Away=Game_left_Info[(1:length(Game_left_Info))%%4==2]
#    Home=Game_left_Info[(1:length(Game_left_Info))%%4==3]
#  ##--------Results--------##
#    Away_score=Game_right_Info[(1:length(Game_right_Info))%%2==1]
#    Home_score=Game_right_Info[(1:length(Game_right_Info))%%2==0]
#    Season_Schedule=data.frame(Date=Game_Date, Away_Team=Away, Away_Score=Away_score, Home_Team=Home, Home_score=Home_score)
#  ##--------Save Schedule Data--------## 
#    file=sprintf("C://Users//user//Documents//My Dropbox//NBA_Code//NBA_Data//Season_Data//Season_%d.txt", Season[s])
#    write.table(Season_Schedule, file, col.names=TRUE, row.names=FALSE, sep="\t")
#}

##----------------------Season Setting----------------------##
if(format(Sys.time(), "%Y %m")<paste(as.numeric(format(Sys.time(), "%Y")), "11") &
     format(Sys.time(), "%Y %m")>paste(as.numeric(format(Sys.time(), "%Y")), "05") ){
  Season <- as.numeric(format(Sys.time(), "%Y"))-1 # for current
  #Season <- seq(as.numeric(format(Sys.time(), "%Y"))-1, as.numeric(format(Sys.time(), "%Y"))) # for pass to current
}else{
  Season <- as.numeric(format(Sys.time(), "%Y")) # for current
  #Season <- seq(as.numeric(format(Sys.time(), "%Y"))-2, as.numeric(format(Sys.time(), "%Y"))+1) # for pass to current
}
##----------------------NBA Season Schedule & Results----------------------##
system.time(
  for(s in 1:length(Season)){ 
    url=sprintf("http://www.basketball-reference.com/leagues/NBA_%d_games.html", Season[s])
    if(url.exists(url)){
      Game_Info <- readHTMLTable(url)
      Game_Schedule <- data.frame(Game_Info$games)
      Game_Schedule <- Game_Schedule[ , -c(2,7,8)]
      colnames(Game_Schedule) <- c("Date", "Away_Team", "Away_Score", "Home_Team", "Home_Score")
    }
    file=sprintf("~/Documents/Sport_analysis/basketball//Season_Data//Season_%d.txt", Season[s])
    write.table(Game_Schedule, file, col.names=TRUE, row.names=FALSE, sep="\t")
  }
)