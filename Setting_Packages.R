#version_platform <- R.version$platform
#if(substr(version_platform, 1, 6) == "x86_64"){
#  Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
#}else{
#  Sys.setenv(JAVA_HOME='C:\\Program Files (x86)\\Java\\jre7') # for 32-bit version
#}
if (!require(rJava)) install.packages("rJava")
if (!require(RCurl)) install.packages("RCurl")
if (!require(XML)) install.packages("XML")
if (!require(data.table)) install.packages("data.table")
if (!require(mailR)) install.packages("mailR")
if (!require(xlsx)) install.packages("xlsx")
if (!require(rjson)) install.packages("rjson")
if (!require(stringr)) install.packages("stringr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plyr)) install.packages("plyr")