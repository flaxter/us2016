library(data.table)
library(h5)
source("read.hd5.r")

read.data = function(omit.cats = TRUE) {
  regions.all = read.hd5("../data/embeddings-main.h5", omit.cats)
  regions = regions.all[regions.all$subgroup == "PWGTP>0",]
  regions.sex = regions.all[regions.all$subgroup == "SEX == 1" | regions.all$subgroup == "SEX == 2",]#read.hd5("../data/sex.h5",linear=!rff,rff=rff)
  
  # 2012 data
  election2012 = read.csv("../data/2012-by-region-puma10s.csv",as.is=T)
  election2012 = data.table(election2012)
  citizens = read.csv("../data/weight_counts.csv")
  election2012 = merge(election2012,citizens,by="region")
  #election2012[, n_votes := votes_D + votes_G + votes_L + votes_R + votes_oth]
  setnames(election2012,"total_wt","citizens")
  election2012[, votes_O := votes_L + votes_oth + votes_G]
  election2012[, votes_L := NULL]
  election2012[, votes_oth := NULL]
  election2012[, votes_G := NULL]
  
  # 2016 data
  election2016 = data.table(read.csv("../data/results-2016-election.csv"))
  #election2016 = merge(election2016,citizens,by="region")
  #setnames(election2016,"total_wt","citizens")
  
  # geographic coordinates
  centroids = read.csv("../data/centroids_cartesian_10.csv")
  regions = merge(regions,centroids,by="region")
  
  regions = merge(regions,as.data.frame(election2016),by="region")
  setnames(regions,"weights","citizens")
  #regions = merge(regions,centroids,by="region")
  #regions = regions[regions$region != "DC_00_01",]
  nrow(regions)
  regions = regions[regions$votes_R > 0 & regions$votes_D > 0, ]
  
  #n.features = ncol(regions)-10
  
  X = regions[,which(names(regions) == "AGEP"):(which(names(regions) == "x")-1)] #(1:n.features)+3]
  n.features = ncol(X)
  return(list(X=X,regions=regions,regions.all=regions.all,regions.sex=regions.sex,n.features=n.features,election2012=election2012,election2016=election2016)) 
}
