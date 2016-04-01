library(rpart)
library(MASS)
maindir <- "E:\\software analytics\\Project1"
subdir <- list.files(maindir)

lm.cval = function(form, dat, ratio = 0.8, iter = 30) {
  set.seed(7243)
  n = nrow(dat)
  mae = rep(0, iter)
  cor = rep(0, iter)
  for (i in 1:iter) {
    trainset = sample.int(n, n*ratio)
    model = lm(form, data = dat[trainset,])
    predicted = predict(model, dat[-trainset,])
    actual = dat[-trainset,]$bug
    mae[i] = mean(abs(predicted - actual))
    cor[i] = cor.test(predicted, actual, method = 'spearman')$estimate
  }
  list( mae,cor)
} 

glm.cval = function(form, dat, ratio = 0.8, iter = 30) {
  set.seed(7243)
  n = nrow(dat)
  mae = rep(0, iter)
  cor = rep(0, iter)
  for (i in 1:iter) {
    trainset = sample.int(n, n*ratio)
    model = glm.nb(form, data = dat[trainset,])
    predicted = predict(model, dat[-trainset,])
    actual = dat[-trainset,]$bug
    mae[i] = mean(abs(predicted - actual))
    cor[i] = cor.test(predicted, actual, method = 'spearman')$estimate
  }
  list(mae,cor)
}

rpart.cval = function(form, dat, ratio = 0.8, iter = 30) {
  set.seed(7243)
  n = nrow(dat)
  mae = rep(0, iter)
  cor = rep(0, iter)
  for (i in 1:iter) {
    trainset = sample.int(n, n*ratio)
    model = rpart(form, data = dat[trainset,])
    predicted = predict(model, dat[-trainset,])
    actual = dat[-trainset,]$bug
    mae[i] = mean(abs(predicted - actual))
    cor[i] = cor.test(predicted, actual, method = 'spearman')$estimate
  }
  list(mae,cor)
}

pca_compute = function(dat) {
  
  k = ncol(dat) - 1
  bug = dat[,k+1]
  new.dat=dat[,1:k]
  pca = princomp(new.dat)
  p = select.pca(pca, 0.90)
  new.dat = as.data.frame(pca$scores[,1:p])
  new.dat$bug = bug
  l = g = r = result= list()
  l = lm.cval(bug ~ ., new.dat)
  g = glm.cval(bug ~ ., new.dat)
  r = rpart.cval(bug ~ ., new.dat)
  result[[1]]=l
  result[[2]]=g
  result[[3]]=r
  result
}

select.pca = function(pca, amount) {
  pca.var = pca$sdev^2
  sum.var = sum(pca.var)
  cum.sum = 0
  for (p in 1:length(pca.var)) {
    cum.sum = cum.sum + pca.var[p]
    if (cum.sum >= amount * sum.var)
      break
  }
  p
}

for(i in 1:5)
{
  if(subdir[i]=="equinox"||subdir[i]=="jdt"||subdir[i]=="lucene"||subdir[i]=="mylyn"||subdir[i]=="pde") 
  {
    setwd(file.path(maindir,paste(subdir[i])))
    files = dir(getwd(),pattern=".csv",recursive=TRUE)
    print(files)
    s = read.csv("single-version-ck-oo.csv",sep=";",header = TRUE)
    cleaned_data = s[,-which(names(s) %in% c("bugs","classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))]
    c = read.csv("change-metrics.csv",sep=";",header = TRUE)
    cleaned_data = cbind(cleaned_data,c[,-which(names(c) %in% c("bugs","classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))])
    b = read.csv("bug-metrics.csv",sep=";",header = TRUE)
    cleaned_data =cbind(cleaned_data, b[,-which(names(b) %in% c("classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))]) 
    single = list()
    
    single = pca_compute(cleaned_data)
    
    
    boxplot(unlist(single[[1]][1]),unlist(single[[2]][1]),unlist(single[[3]][1]),
            ylab="MAE",xlab="Regression Models with Combined Metrics",main=subdir[i],
            col=c("violetred","limegreen","darkorange2"),
            names=c('lm','glm','rpart'))
    
    boxplot(unlist(single[[1]][2]),unlist(single[[2]][2]),unlist(single[[3]][2]),
            ylab="RCOR",xlab="Regression Models with Combined Metrics",main=subdir[i],
            col=c("violetred","limegreen","darkorange2"),
            names=c('lm','glm','rpart'))    
    
  }
}

