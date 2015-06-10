library(data.table)

alignment <- fread("Alignment.csv", skip=1, colClasses = rep("text",7))
alignment <- alignment[,MDParameter := ifelse(mkt_decile=="EPIL", 11, as.numeric(mkt_decile))*10 + ifelse(top_mol_nbrx_seg=="H",3,ifelse(top_mol_nbrx_seg=="M",2,1))]

setnames(alignment, "Group ID", "Group_ID")
alignment <- alignment[,group_parameter := first(MDParameter), by=Group_ID]
alignment <- alignment[,Team := ifelse(group_parameter > 62,"FSS","CSS" )]

Calls <- fread("Calls.csv",skip=1)  
Calls <- Calls[,4:6, with=F]
setnames(Calls,c("Parameter","Speciality"),c("MDParameter","specialty_fin"))
setkey(alignment,MDParameter,specialty_fin)
setkey(Calls,MDParameter,specialty_fin)
d<- Calls[alignment]

d <- d[,Calls := ifelse(is.na(Calls),0,Calls)]
d <- d[,css_calls := ifelse(last(ims_id),12,0), by=Group_ID,ims_id]
