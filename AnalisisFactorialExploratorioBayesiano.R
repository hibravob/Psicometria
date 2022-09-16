#Analisis factorial exploratorio bayesiano

#Base de datos utilizada Treiblmaier, H. (2006). Datenqualität und individualisierte Kommunikation [Calidad de los datos y comunicación individualizada]. Wiesbaden: DUV Gabler Edition Wissenschaft.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("MPsychoR","corrplot","BayesFM")
ipak(packages)


data("Privacy")
Privstd <- scale(Privacy)
corrplot(cor(Privstd))

#establecer el límite superior para el número de factores 
#a partir del número mínimo de variables manifiestas por factor 

Nid <- 2 ## número mínimo de variables por factor 
pmax <- trunc(ncol(Privstd)/Nid) ## número máximo de factores 
pmax

#Establecer matriz de correlacción
set.seed(123)
Rsim <- simul.R.prior(pmax, nu0 = pmax + c(1, 2, 5, 7, 10))
plot(Rsim)

#En el plot  buscamos una distribución basada en ν0 que limite 
#la prioridad suficientemente lejos de las regiones extremas
#es decir, las regiones en las que nos encontramos con problemas de identificabilidad

#Establecer numero de factores
Ksim <- simul.nfac.prior(nvar = ncol(Privstd), Nid = Nid, 
                         Kmax = pmax, kappa = c(.1, .2, .5, 1))
plot(Ksim)

#El plot muestra las probabilidades a priori de los distintos números de factores 
#para diferentes valores de κ.Elegimos un valor de κ = 0,2 para dar más peso a la solución de dos factores.

#probabilidad de que una variable manifiesta no cargue en ningún factor τ0
#Ajuste del modelo

set.seed(222)
        
fitbefa <- befa(Privstd, Nid = 2, Kmax = pmax, nu0 = 10, kappa = 0.2, kappa0 = 0.1, xi0 = 0.1, burnin = 5000, iter = 50000)
fitbefa <- post.column.switch(fitbefa) ## reordenación de columnas 
fitbefa <- post.sign.switch(fitbefa) ## cambio de signo
sumbefa <- summary(fitbefa)
sumbefa   
