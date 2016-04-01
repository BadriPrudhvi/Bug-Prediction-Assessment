maindir <- "E:\\software analytics\\Project1"
subdir <- list.files(maindir)
single_metric=matrix(0,ncol=17,nrow=10)
change_metric=matrix(0,ncol=15,nrow=10)
bug_metric=matrix(0,ncol=5,nrow=10)

Avg_single_metric=matrix(0,ncol=17,nrow=2)
Avg_change_metric=matrix(0,ncol=15,nrow=2)
Avg_bug_metric=matrix(0,ncol=5,nrow=2)

compute.cor = function(dat, column, method) {
  res = rep(0, column)
  for (i in 1:column) 
    res[i] = cor.test(dat[,i], dat$bugs, method = method)$estimate
  res
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
  single_metric[i,]=compute.cor(cleaned_s, 17, "spearman")
  change_metric[i,]=compute.cor(cleaned_c,15,"spearman")
  bug_metric[i,]=compute.cor(cleaned_b,5,"spearman")
  single_metric[5+i,]=compute.cor(cleaned_s, 17, "pearson")
  change_metric[5+i,]=compute.cor(cleaned_c,15,"pearson")
  bug_metric[5+i,]=compute.cor(cleaned_b,5,"pearson")
}
}
for(i in 1:17)
{
  Avg_single_metric[1,i]=mean(single_metric[1:5,i])
  Avg_single_metric[2,i]=mean(single_metric[6:10,i])
}  
for(i in 1:15)
{
  Avg_change_metric[1,i]=mean(change_metric[1:5,i])
  Avg_change_metric[2,i]=mean(change_metric[6:10,i])
}
for(i in 1:5)
{
  Avg_bug_metric[1,i]=mean(bug_metric[1:5,i])
  Avg_bug_metric[2,i]=mean(bug_metric[6:10,i])
}

print (order((Avg_single_metric[1,]),decreasing=T))
print (order((Avg_change_metric[1,]),decreasing=T))
print (order((Avg_bug_metric[1,]),decreasing=T))
print (order((Avg_single_metric[2,]),decreasing=T))
print (order((Avg_change_metric[2,]),decreasing=T))
print (order((Avg_bug_metric[2,]),decreasing=T))

t.test(single_metric[1:5,9],single_metric[1:5,1],paired=T)
t.test(change_metric[1:5,5],change_metric[1:5,1],paired=T)
t.test(bug_metric[1:5,2],bug_metric[1:5,1],paired=T)
t.test(single_metric[6:10,16],single_metric[6:10,4],paired=T)
t.test(change_metric[6:10,1],change_metric[6:10,5],paired=T)
t.test(bug_metric[6:10,1],bug_metric[6:10,2],paired=T)

