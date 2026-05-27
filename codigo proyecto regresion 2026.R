

#librerias y paquetes a instalar
library(rio) 
library(car)

#base de datos
efi<- rio::import(file.choose())
names(efi)<-c("relcomp","surf","wall","roof","height","orient","glazing","gladist","Yheating","Ycooling")

head(efi)

attach(efi)
summary(efi) #análisis descriptivo



#Boxplots y búsqueda de outliers
par(mfrow = c(1,2) )
boxplot(Yheating,main= "Yheating", horizontal = T)
stripchart(Yheating, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(Ycooling,main= "Ycooling", horizontal = T)
stripchart(Ycooling, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

par(mar = c(2, 1, 1, 2))
par(mfrow = c(4,2) )
boxplot(relcomp,main= "relative compactness", horizontal = T)
stripchart(relcomp, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(surf, main= "surface area", horizontal = T)
stripchart(surf, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(wall,main= "wall area", horizontal = T)
stripchart(wall, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(roof, main= "roof area", horizontal = T)
stripchart(roof, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(height, main= "overall height", horizontal = T)
stripchart(height, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(orient, main= "orientation", horizontal = T)
stripchart(orient, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(glazing,main= "glazing", horizontal = T)
stripchart(glazing, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

boxplot(gladist,main= "glazing distribution", horizontal = T)
stripchart(gladist, method = "jitter", pch = 20, add = TRUE, col = "turquoise")

par(mfrow = c(1,1) )

#No hay outliers en ninguna variable









#matriz de correlación
efi_numeric <- efi[ , sapply(efi , is.numeric)]
cor.base <- cor(efi_numeric)    
corrplot::corrplot(cor.base,method = "number")
#tenemos problemas de correlacion en 4 variables 


#calculo vif surf y relcomp 
mh<-lm(Yheating~surf+relcomp)
summary(mh)
vif(mh)
#Vif=62, eliminar una variable: relcomp o surf (se autoexplican)


#cálculo vif height y roof 
mh<-lm(Yheating~height+roof)
summary(mh)
vif(mh)
#Vif=18, problema de colinealidad 


#cálculo vif relcomp y roof
mh<-lm(Yheating~relcomp+roof)
summary(mh)
vif(mh)
#Vif=4.07, no es significante


#cálculo vif height y surf
mh<-lm(Yheating~height+surf)
summary(mh)
vif(mh)
#Vif=3.79, menor a 10, se ignora


m<-lm(Yheating~height+surf+wall)
summary(m)

m<-lm(Yheating~relcomp+roof+surf+height+glazing+gladist+wall)
summary(m)
