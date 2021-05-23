doc_csv <-read.csv("INSURANCE.csv",header=TRUE)
head(doc_csv)
str(doc_csv)
table(doc_csv$sex)
table(doc_csv$smoker)

res <- sapply(doc_csv,class)
res <- which(res=="numeric")

par(mfrow=c(1,2))
for(i in 1:2){
	boxplot(doc_csv[,res[i]],main=names(doc_csv)[res[i]],col="gray")
}

> clus <- doc_csv[,c("age","sex","bmi","children","smoker","region","charges")]
> clus

mean.n <- as.vector(sapply( doc_csv[,res ],mean,na.rm=TRUE ) )
std.n <- as.vector(sapply(doc_csv[,res ],sd, na.rm=TRUE))
median.n <- as.vector(sapply(doc_csv[,res],median, na.rm=TRUE))

plot(doc_csv[c("sex","smoker")],xlab="Fidelidad",ylab="Experiencia")

barplot(prop.table(table(doc_csv$sex,doc_csv$smoker)), col=c("darkblue","red"))