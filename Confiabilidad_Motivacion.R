install.packages("MPsychoR")
install.packages("psych")
library("MPsychoR")
library("psych")
data("Rmotivation")
ind <- grep("hyb", colnames(Rmotivation))
HybMotivation <- na.omit(Rmotivation[,ind]) ## selección de ítems
k <- ncol(HybMotivation) ## número de ítems

vcmat <- cov(HybMotivation) #matriz de varianza-covarianza (VC) de los ítems

sigma2_Xi <- tr(vcmat) #De esta matriz extraemos las varianzas individuales de los ítems σX i2  de la diagonal principal y tomamos la traza
sigma2_X <- sum(vcmat) #La varianza total está formada por la suma de las varianzas y la suma de las covarianzas a ambos lados de la diagonal

cronalpha <- k/(k-1)*(1-sigma2_Xi/sigma2_X) #calculo del alfa
cronalpha
round(cronalpha, 2)

sqrt(sigma2_X)*sqrt(1-cronalpha) #error estandar de medicion

alpha.hyb <- psych::alpha(HybMotivation)
round(alpha.hyb$total[1], 2) ## Alfa de Cronbach

glb(HybMotivation) #greatest lower bound

omega(HybMotivation) #ωt y el ωh de McDonald 

guttman(HybMotivation) #coeficientes λ de Guttman


## TEORIA DE LA GENERAZIBILIDAD##
install.packages("reshape2")
library("reshape2")
Hyb1 <- data.frame(HybMotivation,
                   person = 1:nrow(HybMotivation)) 
Hyblong <- melt(Hyb1, id.vars = c("person"),
                variable.name = "item")
Hyblong$person <- as.factor(Hyblong$person)
summary(aov(value ~ person + item, data = Hyblong))

round((0.85-0.15)/0.85, 2)


icchyb <- ICC(HybMotivation) #coeficiente de correlación intraclase CCI
icchyb

#ANOVA de efectos aleatorios a partir de los efectos fijos ->

sqrt((0.85-0.15)/19)

sqrt((31.88-0.15)/777)

install.packages("lme4")
library(lme4)
VarCorr(lmer(value ~ (1|person) + (1|item), data = Hyblong))

install.packages("gtheory")
library(gtheory)
gfit <- gstudy(data = Hyblong, formula = value ~ (1|person)+ (1|item))
dfit <- dstudy(gfit, colname.objects = "person",
               colname.scores = "value", data = Hyblong)
coeficienteGenerazibilidad<-round(dfit$generalizability, 3) #el coeficiente de generalizabilidad es un coeficiente de fiabilidad 

#Multiples facetas #Estudio G

data("Lakes")
phydat <- subset(Lakes, subtest == "physical")
phydat$item <- droplevels(phydat$item)
head(phydat)

formula <- score ~ (1|personID) + (1|raterID) + (1|item) +
  (1|personID:raterID) + (1|personID:item) + (1|raterID:item)
gfit <- gstudy(formula = formula, data = phydat)
gfit

#Estudio d
dfit<-dstudy(gfit,colname.objects = "personID",
             colname.scores = "score",data=phydat)
dfit$components

dfit$var.error.abs #varianza de error absoluta
dfit$sem.abs #error estándar absoluto de medición
dfit$var.error.rel #error estándar relativo de medición
