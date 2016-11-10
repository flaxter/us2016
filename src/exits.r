exit.data = function(n.features) {
  exit = fread("../data/exit-polls.csv")
  nrow(exit)
  exit[, qid := NULL]
  exit = unique(exit)
  exit = exit[!is.na(exit$`Donald Trump`) & !is.na(exit$`Hillary Clinton`)]
  nrow(exit)
  labels = data.frame(state=c(state.name,"National"),st=c(state.abb,"National"), stringsAsFactors = F)
  exit = merge(exit,labels,by="state")
  exit[, state := NULL]
  setnames(exit,c("st","Donald Trump","Hillary Clinton","answer"),c("state","votes_R","votes_D","subgroup"))
  exit$factor = ""
  exit$subgroup[exit$subgroup == "Male"] = "SEX == 1"
  exit$subgroup[exit$subgroup == "Female"] = "SEX == 2"
  
  
  exit$subgroup[exit$subgroup == "18-29"] = "18 <= AGEP <= 29"
  exit$subgroup[exit$subgroup == "30-44"] = "30 <= AGEP <= 44"
  exit$subgroup[exit$subgroup == "45-64"] = "45 <= AGEP <= 64"
  exit$subgroup[exit$subgroup == "65 or over"] = "AGEP >= 65"
  exit$subgroup[exit$subgroup == "High school or less"] = "SCHL <= 17"
  exit$subgroup[exit$subgroup == "Some college/assoc. degree"] = "SCHL > 17 & SCHL < 21"
  exit$subgroup[exit$subgroup == "College graduate" & exit$question == "Which best describes your education?"] = "SCHL == 21"
  exit$subgroup[exit$subgroup == "Postgraduate study"] = "SCHL >= 22"
  exit$factor[grep("AGE",exit$subgroup)] = "AGE"
  exit$factor[grep("SEX",exit$subgroup)] = "SEX"
  exit$factor[grep("SCHL",exit$subgroup)] = "SCHL"
  exit$votes_D = exit$votes_D * exit$total_n
  exit$votes_R = exit$votes_R * exit$total_n
  
  states = read.hd5("~/us/data/embeddings-main_states.h5")
  states2 = read.hd5("~/us/data/educ1_states.h5")
  states3 = read.hd5("~/pummeler/regions/ethnicity_states.h5")
  library(plyr)
  states3$subgroup = revalue(states3$subgroup, c('ETHNICITY == "hispanic"'="Hispanic/Latino",
                             'ETHNICITY == "black"'="Black",
                             'ETHNICITY == "white"'="White",'ETHNICITY == "asian"'="Asian"))
  states = rbind(states,states2,states3)
  states$subgroup = as.character(states$subgroup)
  setnames(states,"weights","citizens")
  states$state = as.character(states$region)
  
  # subgroup
  merged.subgroup = merge(states,exit[,c("state","subgroup","votes_D","votes_R","total_n","factor"),with=F],by=c("state","subgroup"))
  unique(merged.subgroup$subgroup)
  merged.subgroup=data.table(merged.subgroup)
  merged.subgroup[, sample := sum(total_n), by=c("state","factor")]
  head(merged.subgroup[,c("state","subgroup","votes_D","votes_R","total_n","sample","citizens"),with=F])
  
  election2016 = data.table(read.csv("../data/results-2016-election.csv"))
  election2016.state = data.table(election2016)[, list(votes_tot = sum(votes_D + votes_R)),by=list(state=substr(region,1,2))]
  merged.subgroup=merge(merged.subgroup,election2016.state,by="state")
 # merged.subgroup$citizens = merged.subgroup$sample / merged.subgroup$votes_tot * merged.subgroup$citizens
  merged.subgroup$votes_D = merged.subgroup$votes_tot / merged.subgroup$sample * merged.subgroup$votes_D
  merged.subgroup$votes_R = merged.subgroup$votes_tot / merged.subgroup$sample * merged.subgroup$votes_R
  
    merged.subgroup = merged.subgroup[complete.cases(merged.subgroup),]
  keep = complete.cases(merged.subgroup)
  print(sprintf("Dropping %d cases",sum(!keep)))
  merged.subgroup = merged.subgroup[keep,]
  
  Y.subgroup = cbind(merged.subgroup$votes_D,merged.subgroup$votes_R, merged.subgroup$citizens-merged.subgroup$votes_D-merged.subgroup$votes_R)
  return(list(X=merged.subgroup[,(1:n.features)+4,with=F],Y=Y.subgroup,citizens=merged.subgroup$citizens,orig=merged.subgroup))
}
