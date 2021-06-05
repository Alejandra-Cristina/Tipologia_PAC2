install.packages ("knitr", type = "win.binary")
library(knitr)

doc_csv <-read.csv("INSURANCE.csv",header=TRUE)
head(doc_csv)
str(doc_csv)

res <- sapply(doc_csv,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))


table(doc_csv$sex)
table(doc_csv$bmi )
table(doc_csv$children)
table(doc_csv$region)
table(doc_csv$charges)
table(doc_csv$smoker)

colSums(is.na(doc_csv))
colSums(doc_csv =="")



res <- sapply(doc_csv,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))
res <- which(res=="integer" | res == "numeric")

par(mfrow=c(1,4))
for(i in 1:4){
	boxplot(doc_csv[,res[i]],main=names(doc_csv)[res[i]],col="gray")
}

clus <- doc_csv[,c("age","sex","bmi","children","smoker","region","charges")]
clus

#/* ***************************************************** */

mean.n <- as.vector(sapply( doc_csv[,res ],mean,na.rm=TRUE ) )
std.n <- as.vector(sapply(doc_csv[,res ],sd, na.rm=TRUE))
median.n <- as.vector(sapply(doc_csv[,res],median, na.rm=TRUE))
mean.trim.0.05 <- as.vector(sapply(doc_csv[,res],mean, na.rm=TRUE, trim=0.05))
IQR.n <- as.vector(sapply(doc_csv[,res],IQR, na.rm=TRUE)) 
mad.n <- as.vector(sapply(doc_csv[,res],mad, na.rm=TRUE))

kable(data.frame(variables= names(doc_csv)[res],
                 Media = mean.n,
                 Mediana = median.n,
                 Media.recort.0.05= mean.trim.0.05                
                 ),
      digits=2, caption="Estimaciones de Tendencia Central")
kable(data.frame(variables= names(doc_csv)[res],
                 Desv.Standard = std.n,
                 IQR = IQR.n,
                 MAD = mad.n
                 ),
      digits=2, caption="Estimaciones de Dispersion")
#/* ***************************************************** */

plot(doc_csv[c("sex","smoker")],xlab="Fidelidad",ylab="Experiencia")

barplot(prop.table(table(doc_csv$sex,doc_csv$smoker)), col=c("darkblue","red"))

#/* ***************************************************** */
qqnorm( doc_csv$charges )
qqline( doc_csv$charges )
shapiro.test (doc_csv$charges)
t.test( doc_csv$charges,
        mu = 10, 
        alternative = "two.sided" )
		
HombresIni <- doc_csv$charges[doc_csv$sex == "male"]
MujeresIni <- doc_csv$charges[doc_csv$sex == "female"]
qqnorm( HombresIni )
qqline( HombresIni )
shapiro.test ( HombresIni ) # contraste de normalidad
qqnorm( MujeresIni )
qqline( MujeresIni )
shapiro.test ( MujeresIni ) # contraste de normalidad
var.test( HombresIni, MujeresIni )

#/* ***************************************************** */

smokerYes <- doc_csv$charges[doc_csv$smoker == "yes"]
smokerNo <- doc_csv$charges[doc_csv$smoker == "no"]
qqnorm( smokerYes )
qqline( smokerYes )
shapiro.test ( smokerYes ) # contraste de normalidad
qqnorm( smokerNo )
qqline( smokerNo )
shapiro.test ( smokerNo ) # contraste de normalidad
var.test( smokerYes, smokerNo )


#/***********************************************************/
#regresion lineal

pairs(doc_csv)

regresion <- lm(doc_csv$charges, doc_csv$smoker)