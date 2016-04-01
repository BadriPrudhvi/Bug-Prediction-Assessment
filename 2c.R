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
    actual = dat[-trainset,]$bugs
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
    actual = dat[-trainset,]$bugs
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
    actual = dat[-trainset,]$bugs
    mae[i] = mean(abs(predicted - actual))
    cor[i] = cor.test(predicted, actual, method = 'spearman')$estimate
  }
  list(mae,cor)
}

for(i in 1:5)
{
  if(subdir[i]=="equinox"||subdir[i]=="jdt"||subdir[i]=="lucene"||subdir[i]=="mylyn"||subdir[i]=="pde") 
  {
    setwd(file.path(maindir,paste(subdir[i])))
    files = dir(getwd(),pattern=".csv",recursive=TRUE)
    print(files)
    s = read.csv("single-version-ck-oo.csv",sep=";",header = TRUE)
    cleaned_s = s[,-which(names(s) %in% c("classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))]
    c = read.csv("change-metrics.csv",sep=";",header = TRUE)
    cleaned_c = c[,-which(names(c) %in% c("classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))]
    b = read.csv("bug-metrics.csv",sep=";",header = TRUE)
    cleaned_b = b[,-which(names(b) %in% c("classname","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))] 
    l = g =r = list()
    l[[1]]=lm.cval(bugs~numberOfLinesOfCode,cleaned_s)
    g[[1]]=glm.cval(bugs~numberOfLinesOfCode,cleaned_s)
    r[[1]]=rpart.cval(bugs~numberOfLinesOfCode,cleaned_s)
    l[[2]]=lm.cval(bugs~linesAddedUntil.,cleaned_c)
    g[[2]]=glm.cval(bugs~linesAddedUntil.,cleaned_c)
    r[[2]]=rpart.cval(bugs~linesAddedUntil.,cleaned_c)
    l[[3]]=lm.cval(bugs~numberOfNonTrivialBugsFoundUntil.,cleaned_b)
    g[[3]]=glm.cval(bugs~numberOfNonTrivialBugsFoundUntil.,cleaned_b)
    r[[3]]=rpart.cval(bugs~numberOfNonTrivialBugsFoundUntil.,cleaned_b)
    
    boxplot(unlist(l[[1]][1]),unlist(g[[1]][1]),unlist(r[[1]][1]),unlist(l[[2]][1]),unlist(g[[2]][1]),unlist(r[[2]][1]),
            unlist(l[[3]][1]),unlist(g[[3]][1]),unlist(r[[3]][1]),ylab="MAE",xlab="Regression Models",main=subdir[i],
            col=c("deeppink","limegreen","coral4","firebrick2","blue","cyan","gold","sandybrown","yellowgreen"),
            names=c('lmsingle','glmsingle','rpartsingle','lmchange','glmchange','rpartchange','lmbug','glmbug','rpartbug'))    
    
    
    boxplot(unlist(l[[1]][2]),unlist(g[[1]][2]),unlist(r[[1]][2]),unlist(l[[2]][2]),unlist(g[[2]][2]),unlist(r[[2]][2]),
            unlist(l[[3]][2]),unlist(g[[3]][2]),unlist(r[[3]][2]),ylab="RCOR",xlab="Regression Models",main=subdir[i],
            col=c("deeppink","limegreen","coral4","firebrick2","blue","cyan","gold","sandybrown","yellowgreen"),
            names=c('lmsingle','glmsingle','rpartsingle','lmchange','glmchange','rpartchange','lmbug','glmbug','rpartbug'))
    
    }
}



