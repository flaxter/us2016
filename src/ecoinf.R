library(glmnet)
library(doMC)
registerDoMC(32)
source("read.data.r")
data = read.data()
regions=data$regions; X = data$X; regions.sex = data$regions.sex; n.features = data$n.features
states = read.hd5("../data/embeddings-main_states.h5")

 # todo: try changing the cbind to be proportions instead of counts
 # todo: crossvalidate over alpha
library(doMC)

# todo: what about votes_oth?
Y = cbind(regions$votes_D,regions$votes_R, regions$citizens-regions$votes_D-regions$votes_R)
#Y = Y / rowSums(Y)

source("exits.r")
exits = exit.data(n.features)
X = rbind(X,exits$X)
Y = rbind(Y,exits$Y)

citizens = c(regions$citizens,exits$citizens)

results = NULL
for(alpha in seq(0,1,.05)) { 
  set.seed(1) # use the same folds (hopefully) for each value of alpha
  fit=cv.glmnet(as.matrix(X),Y,
              family="multinomial",weights=citizens,alpha=alpha,parallel=T,type.multinomial="grouped")
  results = rbind(results,data.frame(alpha=alpha,cvm=min(fit$cvm)))
  print(results)
}
results[which.min(results$cvm),]
fit = cv.glmnet(as.matrix(X),Y, family="multinomial",weights=citizens,alpha=0.05,parallel=T,type.multinomial="grouped")
features=data.frame(name=rownames(coef(fit)[[1]]),val=as.numeric(coef(fit)[[1]]))
features = features[features$val != 0,]
features = features[order(-features$val),]
nrow(features)
#features = features[2:length(features)]
#save(features,file="../data/lasso-linear-features.rdata")

pdf("~/us/figures/scatter.pdf",width=5,height=5)
plot(Y[,1] /citizens,predict(fit,as.matrix(X),s=c("lambda.min"),type="response")[,1,1],
     xlab="true",ylab="predicted",col=c(rep(1,nrow(X) - nrow(exits$X)),
                   #                     as.numeric(as.factor(exits$orig$subgroup))+1))
                                        rep(4,nrow(exits$X))))

abline(a=0,b=1)
dev.off()
# pdf("../figures/glmnet-fit.pdf")
# plot(fit$glmnet.fit, label = TRUE)
# plot(fit)
# dev.off()

library(xtable)
make.predictions = function(label,regions.ss) {
  regions.ss = as.data.frame(regions.ss)
  ystar = predict(fit,as.matrix(regions.ss[,(1:n.features)+3]),s=c("lambda.1se"),type="response", weights=regions.ss$weights)[,,1]
  
  # support among men/women
  f = data.table(cbind(regions.ss[,c("region","subgroup","weights")],data.frame(democrat=ystar[,1],republican=ystar[,2],remaining=ystar[,3])) )

  f$d_votes = f$weights * f$democrat
  f$r_votes = f$weights * f$republican
  total_votes = sum(f$d_votes + f$r_votes)
  summary = f[,   list(Clinton=sum(d_votes) / sum(d_votes + r_votes),
                               Trump=sum(r_votes) / sum(d_votes + r_votes),
                               Fraction.of.electorate=sum(d_votes+r_votes)/total_votes,
                       Participation.Rate=sum(d_votes+r_votes)/sum(d_votes+r_votes+weights*remaining),
                       Other=sum(weights * remaining)/sum(d_votes+r_votes+weights*remaining)),
                          by=subgroup]
  write.csv(summary,sprintf("~/us/results/%s-national.csv",label),row.names=F)
  print(xtable(summary),file=sprintf("~/us/results/%s-national.tex",label))
  write.csv(f[,  list(Clinton=sum(d_votes) / sum(d_votes + r_votes),
                                         Trump=sum(r_votes) / sum(d_votes + r_votes),
                                         Fraction.of.electorate=sum(d_votes+r_votes)/total_votes,
                                          Participation.Rate=sum(d_votes+r_votes)/sum(d_votes+r_votes+weights*remaining),
                                         Other=sum(weights * remaining)/sum(d_votes+r_votes+weights*remaining)),
                                    by=list(subgroup,region)], sprintf("~/us/results/%s-regional.csv",label),row.names=F)
  return(summary) # note that summary$electorate adds to 1, but if you want to report for subcategories in here, you need
  # to renormalize by the sum of those, e.g.:
  # sum(summary[grep("MAR",subgroup)]$electorate)
}

# predicting on subset of data
make.predictions("Education",read.hd5("~/pummeler/regions/educ1.h5"))
make.predictions("Age",data$regions.all[grep("AGE",data$regions.all$subgroup),])
make.predictions("Sex",data$regions.all[grep("SEX",data$regions.all$subgroup),])
make.predictions("Age-Sex",data.table(read.hd5("~/pummeler/regions/age-sex1.h5"))[grep("AGE",subgroup),])
make.predictions("Income-Sex",data.table(read.hd5("~/pummeler/regions/age-sex1.h5"))[grep("INCP",subgroup),])
make.predictions("Citizenship",read.hd5("~/pummeler/regions/citizenship1.h5"))
make.predictions("Ethnicity",read.hd5("~/pummeler/regions/ethnicity.h5"))
make.predictions("Ethnicity-Sex",read.hd5("~/pummeler/regions/ethnicity-sex.h5"))
make.predictions("Misc",read.hd5("~/pummeler/regions/misc1.h5"))
make.predictions("Socio",read.hd5("~/pummeler/regions/socio1.h5"))

make.predictions("Income",data$regions.all[grep("PINCP",data$regions.all$subgroup),])


#regions.ss = data$regions.all[grep("AGE",data$regions.all$subgroup),]
#regions.ss = data$regions.all[grep("PINCP",data$regions.all$subgroup),]
Xstar = regions.ss[,(1:n.features)+3]#/sqrt(n.features/2)

#fit = glmnet(as.matrix(X),Y, family="multinomial",weights=citizens,alpha=0.4,lambda=.0609,type.multinomial="grouped")
#range(as.numeric(fit$beta[[1]]))

#plot(fit$glmnet.fit, label = TRUE)

