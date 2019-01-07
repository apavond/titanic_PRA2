
Dataset <- 
  read.table("F:/Google Drive/Master cuatrimeste 1 de 1819/Tipologia y Ciclo de Vida de los datos/PRA2/all/train.csv",
   header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
summary(Dataset)

sapply( Dataset, class)

Dataset <- within(Dataset, {
  SurvivedF <- as.factor(Survived)
})
Dataset <- within(Dataset, {
  PclassF <- as.factor(Pclass)
})
Dataset<-Dataset[, -(1:4)]
Dataset<-Dataset[, -(5:5)]
Dataset<-Dataset[, -(6:6)]
summary(Dataset)
sapply( Dataset, class)

sapply(Dataset, function(x) sum(is.na(x)))

stringEmptyEmbarked <- function (x){
	result<-0
	if (x==''|| x==Dataset[62,'Embarked']){
	 result<-result+1
	}
	return(result)
}
sum(sapply( Dataset[,'Embarked'], stringEmptyEmbarked))

summary(Dataset)
Dataset <- Dataset[Dataset$Embarked != Dataset[62,'Embarked'],]
Dataset$Embarked <- Dataset$Embarked[, drop=TRUE]
Dataset[62,'Embarked']

boxplot.stats(Dataset$Age)$out
boxplot.stats(Dataset$SibSp)$out
boxplot.stats(Dataset$Parch)$out
boxplot.stats(Dataset$Fare)$out

Boxplot( ~ Age, data=Dataset, id=list(method="y"))
Boxplot( ~ SibSp, data=Dataset, id=list(method="y"))
Boxplot( ~ Parch, data=Dataset, id=list(method="y"))
Boxplot( ~ Fare, data=Dataset, id=list(method="y"))

summary(Dataset)

write.csv(Dataset, "Titanic_clean.csv")

sobreviven <- Dataset[Dataset$SurvivedF == "0",]
mueren <- Dataset[Dataset$SurvivedF == "1",]

primeraClase <- Dataset[Dataset$PclassF == "1",]
sedundaClase <- Dataset[Dataset$PclassF == "2",]
terceraClase <- Dataset[Dataset$PclassF == "3",]

library(nortest)
alpha = 0.05
col.names = colnames(Dataset)
for (i in 1:ncol(Dataset)) {
	if (is.integer(Dataset[,i]) | is.numeric(Dataset[,i])) {
		p_val = ad.test(Dataset[,i])$p.value
		if (p_val < alpha) {
			cat(col.names[i])
			if (i < ncol(Dataset) - 1) cat(", ")
		}
	}
}

fligner.test(Fare ~ Age, data = Dataset)




modeloRLSuvirved1 <- glm(SurvivedF ~ Sex , family=binomial, data=Dataset)
summary(modeloRLSuvirved1)
modeloRLSuvirved2 <- glm(SurvivedF ~ Sex + PclassF + SibSp + Parch, family=binomial, data=Dataset)
summary(modeloRLSuvirved2)
modeloRLSuvirved3 <- glm(SurvivedF ~ Sex + PclassF + SibSp + Parch + Fare, family=binomial, data=Dataset)
summary(modeloRLSuvirved3)
modeloRLSuvirved4 <- glm(SurvivedF ~ Sex + PclassF + SibSp + Parch + Age, family=binomial, data=Dataset)
summary(modeloRLSuvirved4)
modeloRLSuvirved5 <- glm(SurvivedF ~ Sex + PclassF + SibSp + Parch + Fare + Age, family=binomial, data=Dataset)
summary(modeloRLSuvirved5)

pred1 <- data.frame(SuvirvedF=0, Sex='male', PclassF="3", SibSp=0, Parch=0, Age=34.5)
predict(modeloRLSuvirved4, pred1, type= "response")
pred2 <- data.frame(SuvirvedF='1', Sex='female', PclassF='3' , SibSp=1, Parch=0, Age=47)
predict(modeloRLSuvirved4, pred2, type= "response")
pred3 <- data.frame(SuvirvedF='0', Sex='male', PclassF='2' , SibSp=0, Parch=0, Age=62)
predict(modeloRLSuvirved4, pred3, type= "response")

predMi <- data.frame(Sex='male', PclassF='2' , SibSp=0, Parch=1, Age=26)
predict(modeloRLSuvirved4, predMi, type= "response")

prob_Survived <- predict(modeloRLSuvirved4, Dataset, type="response")
head(prob_Survived, 10)
head(Dataset$SurvivedF, 10)

head(table(prob_Survived, Dataset$SurvivedF))
pred_Survived <- ifelse(prob_Survived > 0.70,1,0)
head(pred_Survived, 10)
table (Dataset$SurvivedF, pred_Survived)

summary(Dataset[!is.na(Dataset[,'Age']),])


library(pROC)
rocSuvirved <- with(Dataset,roc(Dataset$SurvivedF,prob_Survived))
plot(rocSuvirved, col="red", print.auc=TRUE)



