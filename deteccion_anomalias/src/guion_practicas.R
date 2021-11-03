
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

