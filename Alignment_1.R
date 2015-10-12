library(readxl)
library(dplyr)
alignment <- read.csv("Alignment db v04.csv",skip=1)
alignment <- mutate(alignment, MDParameter = ifelse(mkt_decile=="EPIL", 11, as.numeric(mkt_decile))*10 + ifelse(top_mol_nbrx_seg=="H",3,ifelse(top_mol_nbrx_seg=="M",2,1))
a <- group_by(alignment, Group.ID)  %>% arrange(MDParameter) %>% summarise(group_parameter = first(MDParameter)) 
b <-inner_join(alignment, a, by="Group.ID")
b <- mutate(b, Team = ifelse(group_parameter > 62,"FSS","CSS" )) 
Calls <- read.csv("Calls db v04.csv",skip=1)
Calls <- Calls %>% select(3:6)
d <-  left_join(b,Calls, by= c("MDParameter"="Parameter","specialty_fin"="Speciality")) 
d <- d %>% mutate (Calls=ifelse(is.na(Calls),0,Calls))
d <- d %>% select(-Segment)
e <- group_by(d, Group.ID) %>% 
        arrange(MDParameter) %>% 
        summarise(ims_id = first(ims_id)) %>% 
        mutate( css_calls=12)
d <- inner_join(d,e, by = "ims_id" ) %>% 
        select(-12) %>% 
        mutate(css_calls=ifelse(is.na(css_calls),0, css_calls))
d <- d %>% mutate (css_calls= ifelse(MDParameter<53,0,css_calls))
count <- d %>% filter(css_calls > 0) %>% group_by(Group_ZIP,specialty_fin) %>%
        summarise(call_sum=sum(Calls),css_sum=sum(css_calls),spec_fin =n()) 
