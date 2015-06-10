library(XML)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
urlResults <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;size=200;spanmin1=03+Apr+2011;spanval1=span;team=6;template=results;type=aggregate;view=results"
OverallResults <- readHTMLTable(urlResults, stringsAsFactors=F)
Results <- OverallResults[[3]]
rm(OverallResults)
Results <- Results[,c(1:4,6:8)]
names(Results)[7] <- "StartDate"
str_replace_all(Results$StartDate,pattern=" ",repl="")
Results$StartDate <- as.Date(Results$StartDate,"%d %B %Y")
attach(Results)
Results<-Results %>% mutate(Winner=ifelse(Result!="won",Result,Winner)) %>% select(-2)
Home<- c("Hyderabad (Deccan)", "Delhi","Mohali","Mumbai","Kolkata","Rajkot","Ranchi","Dharamshala","Pune","Cuttack","Bangalore","Visakhapatnam","Kanpur")
Results <- Results %>% mutate (HomeMatch=ifelse(Ground %in% Home, "Y","N"))
Results<- Results %>% mutate(Result=ifelse(Winner=="India","won",ifelse(Winner %in% NoResults, Winner,"not won")))
Results$Year<-as.numeric(format(Results$StartDate,"%y"))
NoResults <- c("n/r","tied","aban","canc")
Results2<- Results %>% mutate(Result=ifelse(Winner=="India","won",ifelse(Winner %in% NoResults, "No Result","not won")))
Results1<- Results %>% group_by(HomeMatch,Result) %>% summarise(count=n())
Wonbatting1st <- Results[grep("runs", Margin),] %>% filter (Result=="won")
ResultsbyYear <- Results %>% group_by(Year,HomeMatch,Result) %>% summarise(count=n())
