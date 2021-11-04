source("src/OutliersLibrerias.R")


#
# 2 DATASET Y SELECCION DE VARIABLES
#


# creamos los conjuntos de datos

datos <- mtcars

columnas.num <- sapply(c(1:ncol(datos)), function(x) is.numeric(datos[, x]))
columnas.num

datos.num <- datos[, columnas.num]
datos.num <- datos.num[, -c(2, 8:11)]
head(datos.num)

datos.num <- na.omit(datos.num)


#
# 3 DETECCION DE OUTLIERS EN UNA DIMENSION
#

#
# 3.1 OUTLIERS IQR
#

# mostramos el histograma de todas las columnas
par(mfrow = c(2,3))
sapply(c(1:ncol(datos.num)), function(x) hist(datos.num[,x], main = "", xlab = names(datos.num)[x]) )
par(mfrow = c(1,1))



# variables a usar en el apartado

indice.columna = 1
columna <- datos.num[, indice.columna]
nombre.columna <- names(datos.num)[indice.columna]


#
# 3.1.1 Obtencion de outliers IQR
#


# Calculo del cuartil primero, tercero e irq

cuartiles <- quantile(columna, probs = c(0.25, 0.5, 0.75))
cuartil.primero <- cuartiles[1]
cuartil.tercero <- cuartiles[3]

iqr <- IQR(columna)

cuartil.primero
cuartil.tercero
iqr

# calculo de extremos que delimitan los outliers

extremo.superior.outlier.IQR <- cuartil.tercero + iqr * 1.5
extremo.inferior.outlier.IQR <- cuartil.primero - iqr * 1.5
extremo.superior.outlier.IQR.extremo <- cuartil.tercero + iqr * 3
extremo.inferior.outlier.IQR.extremo <- cuartil.primero - iqr * 3

extremo.superior.outlier.IQR
extremo.inferior.outlier.IQR
extremo.superior.outlier.IQR.extremo
extremo.inferior.outlier.IQR.extremo


# Contruimos vectores logicos que nos dicen si cada registro es outlier o no
son.outliers.IQR <- columna < extremo.inferior.outlier.IQR | columna > extremo.superior.outlier.IQR
son.outliers.IQR.extremos <- columna < extremo.inferior.outlier.IQR.extremo | columna > extremo.superior.outlier.IQR.extremo

head(son.outliers.IQR)
sum(son.outliers.IQR)

head(son.outliers.IQR.extremos)
sum(son.outliers.IQR.extremos)


#
# 3.1.2 Indices y valores de los outliers IQR
#

claves.outliers.IQR <- which(son.outliers.IQR)
df.outliers.IQR <- datos.num[claves.outliers.IQR,]
nombres.outliers.IQR <- row.names(df.outliers.IQR)
valores.outliers.IQR <- columna[claves.outliers.IQR]

claves.outliers.IQR.extremos <- which(son.outliers.IQR.extremos)
df.outliers.IQR.extremos <- datos.num[claves.outliers.IQR.extremos,]
nombres.outliers.IQR.extremos <- row.names(df.outliers.IQR.extremos)
valores.outliers.IQR.extremos <- columna[claves.outliers.IQR.extremos]

claves.outliers.IQR
df.outliers.IQR
nombres.outliers.IQR
valores.outliers.IQR

claves.outliers.IQR.extremos
df.outliers.IQR.extremos
nombres.outliers.IQR.extremos
valores.outliers.IQR.extremos


#
# 3.1.3 Cómputo de los outliers de IQR con funciones
#

source("src/OutliersFunciones_byCubero.R")

son.outliers.IQR     = son_outliers_IQR (datos.num, indice.columna)
head(son.outliers.IQR)

claves.outliers.IQR  = claves_outliers_IQR (datos.num, indice.columna)
claves.outliers.IQR

son.outliers.IQR.extremos    = son_outliers_IQR (datos.num, indice.columna, 3)
head(son.outliers.IQR.extremos)

claves.outliers.IQR.extremos = claves_outliers_IQR (datos.num, indice.columna, 3)
claves.outliers.IQR.extremos


#
# 3.1.4 Desviacion de los outliers con respecto a la media de la columna
#

datos.num.norm = scale(datos.num)
head(datos.num.norm)
columna.norm   = datos.num.norm[, indice.columna]

valores.outliers.IQR.norm <- columna.norm[claves.outliers.IQR]
valores.outliers.IQR.norm

datos.num.norm.outliers.IQR <- datos.num.norm[claves.outliers.IQR,]
datos.num.norm.outliers.IQR

#
# 3.1.5 Gráfico
#

par(mfrow = c(1,1))

plot_2_colores(columna.norm, claves.outliers.IQR, titulo = "mpg")


plot_2_colores(columna.norm, claves.outliers.IQR.extremos, titulo = "mpg")


#
# 3.1.6 Diagramas de cajas
#

diag_caja_outliers_IQR(datos.num.norm, indice.columna)

diag_caja(datos.num.norm, indice.columna, claves.a.mostrar = claves.outliers.IQR)


diag_caja_juntos(datos.num, titulo = "Outliers en alguna columna", claves.a.mostrar = claves.outliers.IQR)

#
# 3.2 Tests de hipotesis (OPCIONAL)
#

#
# 3.2.2 Comprobación de la hipótesis de Normalidad
#

ajusteNormal = fitdist(columna , "norm")
denscomp (ajusteNormal,  xlab = nombre.columna)

#
# 3.2.3 Test de Grubbs
#

test.de.Grubbs = grubbs.test(columna, two.sided = TRUE)
test.de.Grubbs$p.value

valor.posible.outlier = outlier(columna)
valor.posible.outlier


es.posible.outlier = outlier(columna, logical = TRUE)
clave.posible.outlier = which( es.posible.outlier == TRUE)
clave.posible.outlier

#
# 3.2.4 Test de Normalidad
#

datos.artificiales = c(45,56,54,34,32,45,67,45,67,65,140)


test.de.Grubbs = grubbs.test(datos.artificiales, two.sided = TRUE)
test.de.Grubbs$p.value

valor.posible.outlier = outlier(datos.artificiales)
valor.posible.outlier


es.posible.outlier = outlier(datos.artificiales, logical = TRUE)
clave.posible.outlier = which( es.posible.outlier == TRUE)
clave.posible.outlier

datos.artificiales.sin.outlier = datos.artificiales[-clave.posible.outlier]
datos.artificiales.sin.outlier

shapiro.test(datos.artificiales.sin.outlier)
goodness_fit = gofstat(ajusteNormal)
goodness_fit$adtest

#######################################################################
# Aplica el test de Grubbs sobre la columna ind.col de datos y devuelve una lista con:

# nombre.columna: Nombre de la columna datos[, ind.col]
# clave.mas.alejado.media: Clave del valor O que está más alejado de la media
# valor.mas.alejado.media: Valor de O en datos[, ind.col]
# nombre.mas.alejado.media: Nombre de O en datos
# es.outlier: TRUE/FALSE dependiendo del resultado del test de Grubbs sobre O
# p.value:  p-value calculado por el test de Grubbs
# es.distrib.norm: Resultado de aplicar el test de Normalidad 
#    de Shapiro-Wilks sobre datos[, ind.col]
#    El test de normalidad se aplica sin tener en cuenta el 
#    valor más alejado de la media (el posible outlier O)
#    TRUE si el test no ha podido rechazar
#       -> Sólo podemos concluir que los datos no contradicen una Normal
#    FALSE si el test rechaza 
#       -> Los datos no siguen una Normal

# Requiere el paquete outliers

test_Grubbs <- function(data.frame, indice.columna, alpha = 0.05) {
	resultado <- list()
	resultado$nombre.columna <- names(data.frame)[indice.columna]
	resultado$clave.mas.alejado.media <- which( outlier(data.frame[,indice.columna], logical = TRUE) == TRUE ) 
	resultado$valor.mas.alejado.media <- data.frame[resultado$clave.mas.alejado.media, indice.columna] 
	resultado$nombre.mas.alejado.media <- nombres_filas(data.frame, resultado$clave.mas.alejado.media)
	
	test.de.Grubbs <- grubbs.test(data.frame[, indice.columna], two.sided = TRUE)
	
	resultado$p.value <- test.de.Grubbs$p.value
	resultado$es.outlier <- test.de.Grubbs$p.value <= alpha
	
	
	datos.sin.outlier <- data.frame[-resultado$clave.mas.alejado.media, indice.columna]
	datos.sin.outlier
	
	resultado$p.value.test.normalidad <- shapiro.test(datos.sin.outlier)$p.value
	resultado$es.distrib.norm <- resultado$p.value.test.normalidad > alpha
	
	resultado
}

df.datos.artificiales <- as.data.frame(datos.artificiales)
test.Grubbs.datos.artificiales <- test_Grubbs(df.datos.artificiales, 1)

test.Grubbs.datos.artificiales

test.Grubbs.datos.num = test_Grubbs(datos.num, indice.columna)

test.Grubbs.datos.num

#
# 3.3 Trabajando con varias columnas
# 

#
# 3.3.1 Outliers IQR
#

claves.outliers.IQR.en.alguna.columna <- claves_outliers_IQR_en_alguna_columna(datos.num, 1.5)
claves.outliers.IQR.en.alguna.columna

claves.outliers.IQR.en.mas.de.una.columna <- unique(
	claves.outliers.IQR.en.alguna.columna[
		duplicated(claves.outliers.IQR.en.alguna.columna)
	]
)

claves.outliers.IQR.en.alguna.columna <- unique(claves.outliers.IQR.en.alguna.columna)

claves.outliers.IQR.en.mas.de.una.columna
claves.outliers.IQR.en.alguna.columna
nombres_filas(datos.num, claves.outliers.IQR.en.mas.de.una.columna)
nombres_filas(datos.num, claves.outliers.IQR.en.alguna.columna)

datos.num.norm[claves.outliers.IQR.en.alguna.columna,]

diag_caja_juntos(datos.num, titulo = "Outliers en alguna columna", claves.a.mostrar = claves.outliers.IQR.en.alguna.columna)


#
# 3.3.2 Tests de Hipótesis (OPCIONAL)
#

sapply(c(1:ncol(datos.num)), function(x) {test_Grubbs(datos.num, x)})

#
# 4 Outliers Multivariantes
#


#
# 4.1 Métodos estadísticos basados en la distancia de Mahalanobis (OPCIONAL)
#

son.col.normales <- sapply(c(1:ncol(datos.num)), function(x) {
	test_Grubbs(datos.num, x)$es.distrib.norm
})

son.col.normales

datos.num.distrib.norm <- datos.num[,son.col.normales]
head(datos.num.distrib.norm)

test.MVN = mvn(datos.num.distrib.norm, mvnTest = "energy")
test.MVN$multivariateNormality["MVN"]
test.MVN$multivariateNormality["p value"]

#
# 4.1.2 Tests de hipótesis para detectar outliers
#

corr.plot(datos.num[,1], datos.num[,2])

set.seed(2)

cerioli.test.individual <- cerioli2010.fsrmcd.test(datos.num.distrib.norm, signif.alpha = 0.05)

claves.test.individual <- which(cerioli.test.individual$outliers == TRUE)
claves.test.individual

nombres.test.individual <- nombres_filas(datos.num.distrib.norm, claves.test.individual)
nombres.test.individual

set.seed(2)

cerioli.test.interseccion <- cerioli2010.fsrmcd.test(datos.num.distrib.norm, signif.alpha = 1 - (1 - 0.05)^(1/nrow(datos.num.distrib.norm)))

claves.test.interseccion <- which(cerioli.test.interseccion$outliers == TRUE)
claves.test.interseccion

nombres.test.interseccion <- nombres_filas(datos.num.distrib.norm, claves.test.interseccion)
nombres.test.interseccion

distancias.cerioli.test.interseccion.ordenado <- sort(cerioli.test.interseccion$mahdist.rw, decreasing = FALSE)
clave.mayor.dist.Mah <- order(cerioli.test.interseccion$mahdist.rw, decreasing = FALSE)[1]
clave.mayor.dist.Mah
plot(distancias.cerioli.test.interseccion.ordenado)

#
# 4.2 Visualización de datos con un Biplot
#

biplot.outliers.IQR <- biplot_2_colores(datos.num,
										claves.outliers.IQR.en.alguna.columna,
										titulo.grupo.a.mostrar = "Outliers IQR",
										titulo = "Biplot Outliers IQR")
biplot.outliers.IQR

#
# 4.3 Métodos basados en distancias: LOF
#

num.vecinos.lof = 5
lof.scores <- LOF(datos.num.norm, k = num.vecinos.lof)

lof.scores.ordenados <- sort(lof.scores, decreasing = T) 

plot(lof.scores.ordenados)

num.outliers <- 3
claves.outliers.lof <- sapply(c(1:num.outliers), function(x) which(lof.scores == lof.scores.ordenados[x]))
claves.outliers.lof

nombres.outliers.lof <- nombres_filas(datos.num, claves.outliers.lof)
nombres.outliers.lof

datos.num.norm[claves.outliers.lof,]


clave.max.outlier.lof <- claves.outliers.lof[1]

colores <- rep("black", times = nrow(datos.num.norm))
colores[clave.max.outlier.lof] <- "red"
pairs(datos.num.norm, pch = 19, cex = 0.5, col = colores, lower.panel = NULL)


biplot.max.outlier.lof <- biplot_2_colores(datos.num.norm, clave.max.outlier.lof, titulo = "Mayor outlier LOF")
biplot.max.outlier.lof



#
# 4.4 Métodos basados en Clustering
#

num.outliers <- 5
num.clusters <- 3
set.seed(2)
modelo.kmeans <- kmeans(datos.num.norm, num.clusters)

asignaciones.clustering.kmeans <- modelo.kmeans$cluster
centroides.normalizados <- modelo.kmeans$centers


head(asignaciones.clustering.kmeans)
centroides.normalizados


centroides.desnormalizados <- desnormaliza(datos.num, centroides.normalizados)
centroides.desnormalizados



top.outliers.kmeans <- top_clustering_outliers(datos.num.norm,
											   asignaciones.clustering.kmeans,
											   centroides.normalizados,
											   num.outliers)

claves.outliers.kmeans <- top.outliers.kmeans$claves
nombres.outliers.kmeans <- nombres_filas(datos.num, claves.outliers.kmeans)
distancias.outliers.centroides <- top.outliers.kmeans$distancias

claves.outliers.kmeans

nombres.outliers.kmeans

distancias.outliers.centroides


biplot_outliers_clustering(datos.num,
						   titulo = "Outliers k-means",
						   asignaciones.clustering = asignaciones.clustering.kmeans,
						   claves.outliers = claves.outliers.kmeans)


diag_caja_juntos(datos.num, "Outliers k-means", claves.outliers.kmeans)


#
# 4.4.2 Clustering usando medoides (OPCIONAL)
#



#
# 4.5 Análisis de los outliers multivariantes puros
#

claves.outliers.lof.no.IQR <- setdiff(claves.outliers.lof, claves.outliers.IQR.en.alguna.columna)
nombres.outliers.lof.no.IQR <- nombres_filas(datos.num, claves.outliers.lof.no.IQR)

claves.outliers.IQR.en.alguna.columna
claves.outliers.lof

claves.outliers.lof.no.IQR
nombres.outliers.lof.no.IQR

num.outliers <- 11
claves.outliers.lof <- sapply(c(1:num.outliers), function(x) which(lof.scores == lof.scores.ordenados[x]))
claves.outliers.lof

nombres.outliers.lof <- nombres_filas(datos.num, claves.outliers.lof)
nombres.outliers.lof

claves.outliers.lof.no.IQR <- setdiff(claves.outliers.lof, claves.outliers.IQR.en.alguna.columna)
nombres.outliers.lof.no.IQR <- nombres_filas(datos.num, claves.outliers.lof.no.IQR)


claves.outliers.IQR.en.alguna.columna
claves.outliers.lof

claves.outliers.lof.no.IQR
nombres.outliers.lof.no.IQR


biplot.outliers.puros <- biplot_2_colores(datos.num.norm, 
										 claves.outliers.lof.no.IQR, 
										 titulo = "Outliers LOF (excluidos los que son IQR)")

biplot.outliers.puros


datos.num.norm[claves.outliers.lof.no.IQR, ]
