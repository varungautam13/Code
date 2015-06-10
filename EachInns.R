urlEachInn1 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;size=200;spanmin2=03+Apr+2011;spanval2=span;team=6;template=results;type=batting;view=innings"
urlEachInn2 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;page=2;size=200;spanmin2=03+Apr+2011;spanval2=span;team=6;template=results;type=batting;view=innings"
urlEachInn3 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;page=3;size=200;spanmin2=03+Apr+2011;spanval2=span;team=6;template=results;type=batting;view=innings"
urlEachInn4 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;page=4;size=200;spanmin2=03+Apr+2011;spanval2=span;team=6;template=results;type=batting;view=innings"
urlEachInn5 <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?captain_involve=7593;class=2;filter=advanced;orderby=start;page=5;size=200;spanmin2=03+Apr+2011;spanval2=span;team=6;template=results;type=batting;view=innings"
UrlEachInns <- c(urlEachInn1,urlEachInn2,urlEachInn3,urlEachInn4,urlEachInn5)
EachInns<-lapply(UrlEachInns,readHTMLTable, stringsAsFactor=F)

InnScore <- rbind(EachInns[[1]][[3]],EachInns[[2]][[3]],EachInns[[3]][[3]],EachInns[[4]][[3]],EachInns[[5]][[3]])
rm(EachInns)
InnScore<-InnScore[-c(9,13)]
names(InnScore)[11] <- "StartDate"
InnScore$StartDate <- str_replace_all(InnScore$StartDate,pattern=" ",repl="")
InnScore$StartDate <- as.Date(InnScore$StartDate,"%d %B %Y")
InnScore<-transform(InnScore,Inns=factor(Inns))
InnScore <- InnScore %>% mutate (HomeMatch=ifelse(Ground %in% Home, "Y","N"))
InnScoreMerged<-inner_join(InnScore,Results2,by = c("StartDate" = "StartDate"))
InnScoreMerged<-InnScoreMerged[,-c(15,16,17,18)]
InnScoreMerged<-InnScoreMerged %>% filter (Runs != "DNB", Runs != "TDNB")
InnScoreMerged$Runs <- as.character(InnScoreMerged$Runs)
InnScoreMerged$NO <-grepl("\\*",InnScoreMerged$Runs)
InnScoreMerged$Runs<-gsub("\\*","",InnScoreMerged$Runs)
InnScoreMerged$Runs<-as.numeric(InnScoreMerged$Runs)
InnScoreMerged<-InnScoreMerged[,c(1,2,17,3:16)]
InnScoreMerged<-InnScoreMerged %>% mutate(Opposition=str_sub(Opposition,3,5))
InnKohli <- InnScoreMerged %>% filter(Player=="V Kohli")
InnSachin
