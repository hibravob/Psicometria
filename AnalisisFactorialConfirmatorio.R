#ANALISIS FACTORIAL CONFIRMATORIO

#Conjunto de datos usados: Motivación de R (Mair et al., 2015) 
#items dicotomos

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("MPsychoR","lavaan","semPlot")
ipak(packages)

data("Rmotivation")
vind <- grep("ext|int", colnames(Rmotivation)) ## items ext/int
Rmot <- na.omit(Rmotivation[, vind])
mot_model <- ' 
  extrinsic = ~ ext1 + ext2 + ext3 + ext4 + ext5 + ext6 + 
  ext7 + ext8 + ext9 + ext10 + ext11 + ext12 
  intrínseco = ~ int1 + int2 + int3 + int4 + int5'

fitMot <- lavaan::cfa(mot_model, data = Rmot, ordered = names(Rmot))

#Diagrama de trayectorias del AFC

semPaths(fitMot, what = "est", edge.label.cex = 0.7, edge.color = 1, 
         esize = 1, sizeMan = 4.5, asize = 2.5, intercepts = FALSE, 
         rotation = 4, thresholdColor = "red", mar = c(1, 5, 1.5, 5), 
         fade = FALSE, nCharNodes = 4)
#Las variables manifiestas se presentan como cuadrados con sus varianzas de error asociadas,
#y las variables latentes como círculos con sus varianzas y covarianza asociadas.
#Los valores de las flechas son las cargas. 
#Las líneas rojas en los cuadrados denotan los umbrales de las correlaciones tetracóricas.

#varianza de error de los indicadores
inspect(fitMot, what = "est")$theta

#Cargas (Λ)
inspect(fitMot, what = "est")$lambda

#Cargas (Λ) estandarizadas
inspect(fitMot, what = "std")$lambda

#matriz de covarianza de la variable latente Ψ
inspect(fitMot, what = "est")$psi 
#matriz de correlacion de la variable latente Ψ
inspect(fitMot, what = "std")$psi

#pruebas de significación sobre parametros no estandarizados
parameterEstimates(fitMot, standardized = TRUE)

#Resumen del modelo
summary(fitMot, standardized = TRUE, fit.measures = TRUE)

#Obtenemos un CFI de 0,913, un RMSEA de 0,063 con un IC del 90% correspondiente de [0,058, 0,069], 
#y un SRMR de 0,119. El estadístico χ2 es de 492,422 (df =118, p = 0).

#¿Como mejorar el ajuste?

#La carga del item ext5 no es significativa, p value=0,301
parameterEstimates(fitMot)[5,]

#Podemos eliminar este item y volver a ajustar el modelo

mot_model2 <- '
extrínseco =~ ext1 + ext2 + ext3 + ext4 + ext6 + ext7 + ext8 + ext9 + ext10 + ext11 + ext12 
intrínseco =~ int1 + int2 + int3 + int4 + int5'
fitMot2 <- lavaan::cfa(mot_model2, data = Rmot,
                       ordered = names(Rmot)[-5])
summary(fitMot2, standardized = TRUE, fit.measures = TRUE)

#Obtenemos un CFI de 0,944, un RMSEA de 0,054 con un IC del 90% correspondiente de [0,047, 0,06], 
#y un SRMR de 0,109. El ajuste del modelo ha mejorado. 

#valores estimados de las variables latentes (puntuaciones factoriales)
lavPredict(fitMot2)

