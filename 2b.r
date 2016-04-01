setwd("E:\\software analytics\\Project1\\jdt")
files = dir(getwd(),pattern=".csv",recursive=TRUE)
s = read.csv("single-version-ck-oo.csv",sep=";",header = TRUE)
cleaned_s = s[,-which(names(s) %in% c("classname","bugs","nonTrivialBugs","majorBugs","criticalBugs","highPriorityBugs","X"))]
c = read.csv("change-metrics.csv",sep=";",header = TRUE)
cleaned_c = c[,-which(names(c) %in% c("classname","nonTrivialBugs","bugs","majorBugs","criticalBugs","highPriorityBugs","X"))]
b = read.csv("bug-metrics.csv",sep=";",header = TRUE)
cleaned_b = b[,-which(names(b) %in% c("classname","nonTrivialBugs","bugs","majorBugs","criticalBugs","highPriorityBugs","X"))]

computePairwiseCorrelation = function(dat) {
	n = ncol(dat)
	res = rep(0, n*(n-1)/2)
	k = 0
	pair = list()
	cnames = names(dat)
	for (i in 1:(n-1))
		for (j in (i+1):n) {
			res[k] = cor.test(dat[,i], dat[,j], method = 'spearman')$estimate
			pair[k] = paste(cnames[i], cnames[j], sep = '-')
			k = k + 1
		}
	names(res) = pair
	res
}

computePairwiseCorrelation2 = function(dat1, dat2) {
	n = ncol(dat1)
	m = ncol(dat2)
	res = rep(0, n*m/2)
	k = 0
	pair = list()
	cnames1 = names(dat1)
	cnames2 = names(dat2)
	for (i in 1:n)
		for (j in 1:m) {
			res[k] = cor.test(dat1[,i], dat2[,j], method = 'spearman')$estimate
			pair[k] = paste(cnames1[i], cnames2[j], sep = '-')
			k = k + 1
		}
	names(res) = pair
	res
}
  
pc.1 = computePairwiseCorrelation(cleaned_b)
pc.2 = computePairwiseCorrelation(cleaned_s)
pc.3 = computePairwiseCorrelation(cleaned_c)
pc2.1 = computePairwiseCorrelation2(cleaned_b,cleaned_s)
pc2.2 = computePairwiseCorrelation2(cleaned_b,cleaned_c)
pc2.3 = computePairwiseCorrelation2(cleaned_s,cleaned_c)
boxplot(pc.1,pc.2,pc.3,pc2.1,pc2.2,pc2.3,ylab="Correlation",xlab="Metrics",main="Jdt",
        col=c("deeppink","limegreen","coral4","firebrick2","blue","cyan"),
        names=c('bug','single','change','bugVsingle','bugVchange', 'singleVchange'))
