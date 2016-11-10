library(glmnet)
library(doMC)
library(xtable)
#registerDoMC(4)
source("read.data.r")
data = read.data(omit.cats=F)
regions=data$regions; X = data$X
Y = cbind(regions$votes_D,regions$votes_R, regions$citizens-regions$votes_D-regions$votes_R)

test.logit = function(x,alpha) {
  fit = cv.glmnet(as.matrix(x),Y, family="multinomial",weights=regions$citizens,alpha=alpha,parallel=T,type.multinomial="grouped")
  fit2 = glmnet(as.matrix(x),Y,family="multinomial",weights=regions$citizens,alpha=alpha,lambda=fit$lambda.min,type.multinomial="grouped")
  return(list(fit=fit,fit2=fit2))
}

groupings = read.csv("../data/embeddings-main-feature-locs.csv") 
result = NULL
alpha = .5
for(feat in unique(groupings$feat)) {
  if(length(groupings$index[groupings$feat == feat]) > 1) {
    # if(grepl("rff",feat)) {
    #   alpha = .5 
    # } else {
    #   alpha = 1
    # }
    
    fit = tryCatch({
      fits = test.logit(X[,groupings$index[groupings$feat == feat]+1],alpha) # uses 0 indexing
      fit2 = fits$fit2
      fit = fits$fit
      result = rbind(result,data.frame(feature=feat, deviance=min(fit$cvm), frac.deviance=fit2$dev.ratio,alpha=alpha))
      features=data.frame(name=(rownames(coef(fit)[[1]])),democrat=round(as.numeric(coef(fit)[[1]]),1),
                          republican=round(as.numeric(coef(fit)[[2]]),1),
                          other=round(as.numeric(coef(fit)[[3]]),1))
      write.csv(features,file=sprintf("../results/%s.csv",feat),row.names=F)
      print(xtable(features),file=sprintf("../results/%s.tex",feat))
      print(result)
    },error = function(e) { print(sprintf("error with %s",feat)) })
  } else {
    print(sprintf("skipping %s",feat))   
  }
}
write.csv(result[order(result$deviance),],"../results/features-ranked-by-deviance.csv",row.names=F)

result[order(result$deviance),]
# 
# fit=test.logit(X[,groupings$index[groupings$feat == "ETHNICITY_HASDEGREE"]+1]) # uses 0 indexing
# pdf("../figures/ETHNICITY_HASDEGREE")
# features=data.frame(name=rownames(coef(fit)[[1]]),democrat=round(as.numeric(coef(fit)[[1]]),2),
#                     republican=round(as.numeric(coef(fit)[[2]]),2),
#                     other=round(as.numeric(coef(fit)[[3]]),2))
# #features = features[order(features$val),]
# features
# plot(fit$glmnet.fit, "lambda",label = TRUE)
# dev.off()
