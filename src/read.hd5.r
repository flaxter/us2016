library(h5)

#list.datasets(embeddings)
read.hd5 = function(fname, omit.cats = TRUE, linear=T, rff=F) {
  embeddings = h5file(fname,"r")
  d.lin = dim(embeddings["emb_lin"])
  d.rff = dim(embeddings["emb_extra"])
  regions.linear = regions.rff = NULL
  subgroups = strsplit(embeddings["subset_queries"][1],", ")[[1]]
  region_weights = "region_weights"
  region_names = "region_names"
  if(grepl("states",fname)) {
    region_weights = "state_weights"
    region_names = "state_names"
  }
  for(i in 1:d.lin[3]) {
    s = embeddings["subset_queries"]
    weights = embeddings[region_weights][,i]
    regions.linear = rbind(regions.linear,cbind(data.frame(subgroup=subgroups[i], weights=weights, 
                                                           region=embeddings[region_names][,]),
                                                embeddings["emb_lin"][1:d.lin[1],1:d.lin[2],i]))
    regions.rff =  rbind(regions.rff,data.frame(embeddings["emb_extra"][1:d.rff[1],1:d.rff[2],i]))
    
  }
  names(regions.linear)[4:ncol(regions.linear)] = embeddings["feature_names"][1:d.lin[2]]
  names(regions.rff) = embeddings["extra_names"][1:d.rff[2]]
  
  if(omit.cats) {
    keep = as.logical(as.character(embeddings["keep_multilevels"][1:d.lin[2]]))
    regions.linear = regions.linear[,c(T,T,T,keep)]
    keep = as.logical(as.character(embeddings["extra_keep_multilevels"][1:d.rff[2]]))
    regions.rff = regions.rff[,keep]
  }
  regions = cbind(regions.linear,regions.rff)
  print(sprintf("%d cases have NAs, omitting", sum(!complete.cases(regions))))
  regions = regions[complete.cases(regions),]
  return(regions)
}
