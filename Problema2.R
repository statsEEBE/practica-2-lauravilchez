#Codigo para problema 2

mis_dades <-iris
mis_dades

#Calcular la recta de regressió

x <- mis_dades$Petal.Length
x
y <- mis_dades$Sepal.Length
y

plot(x,y)

#promedio

x_bar <- mean(x)
x_bar

y_bar <- mean(y)
y_bar

#m

m <- sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
m

#b

b <- y_bar-m*x_bar
b

#prediccio en un punt donat

pred <- m*1.5+b
pred

#prediccio sobre les observacions

y_pred <- m*x+b
y_pred

plot(x,y)
lines(x,y_pred)

#Coef de determinacion R^2

R <- sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
R

#Coef de corelacio
Cc <- sqrt(R)
Cc

### Usando funciones de R ###

#Calcula la recta de regresion
lm(y~x)

mod <- lm(y~x)
mod
summary(mod)

#coeficiente de correlacion

cor.test(x,y)
