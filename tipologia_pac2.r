install.packages ("knitr", type = "win.binary")
library(knitr)
library(cluster)

doc_csv <-read.csv("INSURANCE.csv",header=TRUE)
head(doc_csv)
str(doc_csv)
#* ***************************************************** *#
res <- sapply(doc_csv,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))

table(doc_csv$sex)
table(doc_csv$smoker)

res <- sapply(doc_csv,class)
kable(data.frame(variables=names(res),clase=as.vector(res)))
res <- which(res=="numeric")

par(mfrow=c(1,2))
for(i in 1:2){
	boxplot(doc_csv[,res[i]],main=names(doc_csv)[res[i]],col="gray")
}

clus <- doc_csv[,c("age","sex","bmi","children","smoker","region","charges")]
clus

clus2[,c("age")] <- (clus$age-mean(clus$age))/sd(clus$age)
clus2[,c("children")] <- (clus$children-mean(clus$children))/sd(clus$children)
str(clus2)
plot(clus2[c("smoker","charges")], xlab="Fidelidad",ylab="Experiencia")
xk <- clus2[,c("age","bmi","children","charges")]
plot(xk[c("age","charges")], xlab="Fidelidad",ylab="Experiencia")
title(main="nube de puntos normalizados",col.main="blue",font.main=1)
clus2_k3 <-kmeans(xk,center=3)
clus2_k3$centers
clus2_k3$totss
clus2_k3$withinss
clus2_k3$tot.withinss
#suma de cuadrados entre grupos
kmeans(xk,2)$betweenss
kmeans(xk,3)$betweenss
#suma de cuadrados dentro de grupos
kmeans(xk,3)$tot.withinss
#suma de cuadrados total
kmeans(xk,3)$totss
kmeans(xk,2)$totss
bss <- kmeans(xk,centers=1)$betweens
for(i in 2:10) bss[i] <- kmeans(xk,center=i)$betweens
plot(1:10,bss,type="l",xlab="Número de grupos", ylab="Sumas de cuadrados entre grupos")
tot_w <- kmeans(xk,centers=1)$tot.withinss
for(i in 2:10) tot_w[i] <- kmeans(xk,center=i)$tot.withinss
plot(1:10,tot_w,type="l",xlab="Número de grupos", ylab="Sumas de cuadrados entre grupos")
x <- xk
fit <- kmeans(x,2)
clusplot(x,fit$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
fit <- kmeans(x,2)

#* ***************************************************** *#
# SUPUESTO DE NORMALIDAD (charges)
qqnorm( doc_csv$charges )
qqline( doc_csv$charges )

#CONTRASTE NORMALIDAD (charges)
shapiro.test (doc_csv$charges)
t.test( doc_csv$charges,
        mu = 10, 
        alternative = "two.sided" )
		
HombresIni <- doc_csv$charges[doc_csv$sex == "male"]
MujeresIni <- doc_csv$charges[doc_csv$sex == "female"]

#SUPUESTO DE NORMALIDAD (sex y charges)
qqnorm( HombresIni )
qqline( HombresIni )
shapiro.test ( HombresIni ) # contraste de normalidad
qqnorm( MujeresIni )
qqline( MujeresIni )
shapiro.test ( MujeresIni ) # contraste de normalidad
var.test( HombresIni, MujeresIni )

#SUPUESTO DE NORMALIDAD (smoker y charges)
smokerYes <- doc_csv$charges[doc_csv$smoker == "yes"]
smokerNo <- doc_csv$charges[doc_csv$smoker == "no"]
qqnorm( smokerYes )
qqline( smokerYes )
shapiro.test ( smokerYes ) # contraste de normalidad
qqnorm( smokerNo )
qqline( smokerNo )
shapiro.test ( smokerNo ) # contraste de normalidad
var.test( smokerYes, smokerNo )

#********************************************************#
#Modelo de regresión lineal múltiple (regresores cuantitativos)

Model.1.1<- lm(charges~bmi+children+age, data=clus2 )
summary(Model.1.1)

is_number <- sapply(clus2,is.numeric)
a <-cor(clus2[,is_number])
a

#Modelo de regresión lineal múltiple (regresores cuantitativos y cualitativos)
clus2$SexR=relevel(clus2$sex, ref = 'female')
clus2$SmokerR=relevel(clus2$smoker, ref = 'yes')
Model.1.2<- lm(charges~bmi+children+age+SmokerR+SexR, data=clus2 )
summary(Model.1.2)

/***********************************************************/
freqy <- table(doc_csv$smoker)
barplot(freqy, xlab="Smoker", ylab="Charges")
barplot(prop.table(table(doc_csv$smoker,doc_csv$sex)), col=c("darkblue","red"))