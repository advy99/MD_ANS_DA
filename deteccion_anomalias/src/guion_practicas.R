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
# lo haré si tengo tiempo y si finalmente hago el trabajo final
# sobre detección de anomalias
#


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



#
# 4 Outliers Multivariantes
#


#
# 4.1 Métodos estadísticos basados en la distancia de Mahalanobis (OPCIONAL)
#



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
