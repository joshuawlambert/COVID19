library(tidyr)
library(geosphere)
library(shiny)
library(leaflet)
library(maps)
library(htmltools)
library(ggplot2)
library(sp)
library(shinybusy)

download.file(url = "https://static01.nyt.com/newsgraphics/2020/01/21/china-coronavirus/9483cba35612e9540bab09a69331e54b4bc19041/build/js/chunks/model-lite.js",
                destfile = "/srv/shiny-server/COVID19/data/data.js")


jscode <- readLines(con = "/srv/shiny-server/COVID19/data/data.js", warn = FALSE)
grep(jscode,pattern = "var")
z0<-jscode[which(jscode=="var us_counties = [" ):(which(jscode=="var ts_days = [" )-1)]
z1<-gsub(pattern = "\t",replacement = "",x = z0)
z2<-gsub(pattern='\"',replacement = "",x = z1)
z3<-gsub(pattern=",",replacement = "",x = z2)
z4<-gsub(pattern=":",replacement = ":",x = z3)
z5<-z4[-which(z4=="}")]
z6<-z5[-which(z5=="{")]
final<-data.frame(matrix(unlist(strsplit(z6[-c(1,17507:17508)],split = ":")),ncol = 2,byrow = TRUE),stringsAsFactors = FALSE)


cnt=0
for(i in 1:dim(final)[1]){
  if(final$X1[i]=="county"){cnt=cnt+1}
  final$cnt[i]<-cnt
}

final2<-spread(final,key = X1 , value = X2)

hosp<-read.csv("/srv/shiny-server/COVID19/data/Hospitals.csv")
hosp_icu<-read.csv("/srv/shiny-server/COVID19/data/ICU_beds.csv")
colnames(hosp_icu)[1]<-"COUNTYFIPS"
hospagg<-aggregate(hosp$BEDS~hosp$COUNTYFIPS,FUN = sum)
colnames(hospagg)<-c("COUNTYFIPS","TotalBEDS")
#hosp<-hosp[hosp$TYPE %in% c("CRITICAL ACCESS","GENERAL ACUTE CARE","LONG TERM CARE","MILITARY"),]
hosp2<-merge(hospagg,hosp_icu,by="COUNTYFIPS")
hosp<-hosp2

hosp$COUNTYFIPS<-as.numeric(as.character(hosp$COUNTYFIPS))
colnames(final2)[8]<-"COUNTYFIPS"
final2$COUNTYFIPS<-as.numeric(as.character(final2$COUNTYFIPS))

final3<-merge(hosp,final2, by="COUNTYFIPS",all=TRUE)
dim(final3)

hosp.cord<-data.frame(as.numeric(as.character(final3$lon)), as.numeric(as.character(final3$lat)))
cases.cord<-data.frame(as.numeric(as.character(final3$lon)), as.numeric(as.character(final3$lat)))


final3$within15.icubeds<-NA
final3$within15.beds<-NA
final3$within30.icubeds<-NA
final3$within30.beds<-NA
final3$within45.icubeds<-NA
final3$within45.beds<-NA
final3$within60.icubeds<-NA
final3$within60.beds<-NA
final3$within120.icubeds<-NA
final3$within120.beds<-NA

for(i in 1:dim(final3)[1]){
  #less than 15 miles
  if(length(which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<15))!=0){
    lst<-final3[which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<15),]
    final3$within15.icubeds[i]<-sum(lst$all_icu,na.rm = TRUE)
    final3$within15.beds[i]<-sum(lst$TotalBEDS,na.rm = TRUE)
  }
  
  #less than 30 miles
  if(length(which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<30))!=0){
    lst<-final3[which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<30),]
    final3$within30.icubeds[i]<-sum(lst$all_icu,na.rm = TRUE)
    final3$within30.beds[i]<-sum(lst$TotalBEDS,na.rm = TRUE)
  }
  
  #less than 45 miles
  if(length(which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<45))!=0){
    lst<-final3[which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<45),]
    final3$within45.icubeds[i]<-sum(lst$all_icu,na.rm = TRUE)
    final3$within45.beds[i]<-sum(lst$TotalBEDS,na.rm = TRUE)
  }
  
  #less than 60 miles
  if(length(which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<60))!=0){
    lst<-final3[which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<60),]
    final3$within60.icubeds[i]<-sum(lst$all_icu,na.rm = TRUE)
    final3$within60.beds[i]<-sum(lst$TotalBEDS,na.rm = TRUE)
  }
  
  #less than 120 miles
  if(length(which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<120))!=0){
    lst<-final3[which(distGeo (cases.cord[i,],hosp.cord)*0.00062137<120),]
    final3$within120.icubeds[i]<-sum(lst$all_icu,na.rm = TRUE)
    final3$within120.beds[i]<-sum(lst$TotalBEDS,na.rm = TRUE)
  }
}

final3$bedspercases.15<-final3$within15.beds/(as.numeric(final3$confirmed))
final3$bedspercases.30<-final3$within30.beds/(as.numeric(final3$confirmed))
final3$bedspercases.45<-final3$within45.beds/(as.numeric(final3$confirmed))
final3$bedspercases.60<-final3$within60.beds/(as.numeric(final3$confirmed))
final3$bedspercases.120<-final3$within120.beds/(as.numeric(final3$confirmed))

final3$icubedspercases.15<-final3$within15.icubeds/(as.numeric(final3$confirmed))
final3$icubedspercases.30<-final3$within30.icubeds/(as.numeric(final3$confirmed))
final3$icubedspercases.45<-final3$within45.icubeds/(as.numeric(final3$confirmed))
final3$icubedspercases.60<-final3$within60.icubeds/(as.numeric(final3$confirmed))
final3$icubedspercases.120<-final3$within120.icubeds/(as.numeric(final3$confirmed))

#for maps
final3$names<-tolower(paste(trimws(final3$state.x),final3$county,sep = ","))
final3$names<-gsub(final3$names,pattern = ", ",replacement = ",")

p <- "/srv/shiny-server/COVID19/data/data.js"
lastUpdated<-file.info(p)$ctime

mapCounty = map("county", fill = TRUE, plot = FALSE)
mapState = map("state",fill=FALSE, plot=FALSE)

mat<-data.frame(names=mapCounty$names)
mat.merge<-merge(mat,data.frame(final3),all.x=TRUE,by="names")

#order correctly
rownames(mat.merge)<-mat.merge$names
mat.merge<-mat.merge[mapCounty$names,]

mapCounty$cases<-as.numeric(mat.merge$confirmed)
mapCounty$deaths<-as.numeric(mat.merge$deaths)
mapCounty$beds<-mat.merge$TotalBEDS
mapCounty$icubeds<-mat.merge$all_icu
mapCounty$bedspercases.30<-mat.merge$bedspercases.30
mapCounty$bedspercases.60<-mat.merge$bedspercases.60
mapCounty$icubedspercases.30<-mat.merge$icubedspercases.30
mapCounty$icubedspercases.60<-mat.merge$icubedspercases.60
mapCounty$within30.beds<-mat.merge$within30.beds
mapCounty$within60.beds<-mat.merge$within60.beds
mapCounty$within30.icubeds<-mat.merge$within30.icubeds
mapCounty$within60.icubeds<-mat.merge$within60.icubeds


rm(list = ls()[!ls() %in% c("mapCounty", "mapState", "lastUpdated" )])

save.image("/srv/shiny-server/COVID19/data/data.RData")

