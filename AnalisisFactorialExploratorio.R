#Distribución normarl bivariante

  #Dicotomización de ítems
install.packages("MPsychor")
library(MPsychoR)
data("YouthDep")
item1 <- YouthDep[, 1]
levels(item1) <- c("0", "1", "1")
item2 <- YouthDep[, 14]
levels(item2) <- c("0", "1", "1")
table(item1, item2)


#Correlacion tetracorica adecuada para items dicotomos 

install.packages("psych")
library(psych)
tetcor <- tetrachoric(cbind(item1, item2))
tetcor

#Correlación policorica para items politomicos

item1 <- YouthDep[, 1]
item2 <- YouthDep[, 14]
polcor <- polychoric(cbind(item1, item2))
polcor

#Correcion de continuidad

DepItems <- YouthDep[,1:26]
Depnum <- data.matrix(DepItems) - 1 ## convertir a numérico
Rdep <- polychoric(Depnum)
Rdep

#matriz definida positiva o no

data("Rmotivation")
vind <- grep("ext|int", colnames(Rmotivation)) 
Rmotivation1 <- Rmotivation[, vind]
Rmot1 <- tetrachoric(Rmotivation1, smooth = FALSE)
tail(round(eigen(Rmot1$rho)$values, 3))

#Suavizado de la matriz
Rmot <- tetrachoric(Rmotivation1)
tail(round(eigen(Rmot$rho)$values, 3))


#AFE

motFA <- fa(Rmot$rho, nfactors = 2, rotate = "none", fm = "ml")
print(motFA$loadings, cutoff = 0.2)

#Extraccion
round(motFA$communality, 2)

#Rotación ortogonal

motFA2 <- fa(Rmot$rho, nfactors = 2, rotate = "varimax", fm = "ml")
motFA2

factor.plot(motFA2)

#Rotación oblicua

Rmot2 <- tetrachoric(Rmotivation[,1:36])
motFA3 <- fa(Rmot2$rho, nfactors = 3, rotate = "oblimin", fm = "ml")
motFA3$loadings

  #matriz de correlación de los factores
round(motFA3$Phi, 3)

#Puntuacion de los factores
motFA2 <- fa(Rmotivation1, nfactors = 2, rotate = "varimax", cor = "tet", fm = "ml", scores = "regression",
             missing = TRUE, impute = "median")
dim(motFA2$scores)


#Determinación del número de factores

  #Scree plot - descomposicion de valores propios - criterio ad hoc
Rdep <- polychoric(Depnum)$rho 
evals <- eigen(Rdep)$values
scree(Rdep, factors = FALSE)

    #proporción de varianza explicada
    (evals/sum(evals)*100)[1:2]


  #Analisis paralelo

set.seed(123)
resPA <- fa.parallel(Depnum, fa = "pc", cor = "poly",fm = "ml")

  #analisis de estructura muy simple (VSS)
resvss <- vss(Rdep, fm = "ml", n.obs = nrow(Depnum), plot = FALSE)
resvss

#Ajuste del modelo
fadep <- fa(Depnum, 1, cor = "poly", fm = "ml")
summary(fadep)

#Numero de factores
resnf <- nfactors(Depnum, n = 8, fm = "ml", cor = "poly")
resnf
