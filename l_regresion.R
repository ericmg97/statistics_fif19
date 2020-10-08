library(lmtest)

# Cargar los datos de los jugadores
players <- read.csv('./players_19.csv')

#Trabajar unicamente con jugadores del Inter de Milan
inter_players <- subset(players,players$club == "Inter")

#Eleccion de las variables a analizar
vars_select = c("overall","age","potential")

dataset = inter_players[vars_select]
attach(dataset)

# Calcular la matriz de correlacion
cor(dataset)

# Averiguar si overall tiene alguna relacion con la edad y el potencial
regression_model <- lm(overall ~ potential + age, data=dataset)

#Mostrar los resutados de la regresion
summary(regression_model)


#Analizar el cumplimiento de los supuestos del modelo en los residuos

#1: La media y la suma de los errores es 0
mean(regression_model$residuals)
sum(regression_model$residuals)

#2: Los errores tienen distribucion normal
hist(regression_model$residuals)
qqnorm(regression_model$residuals); qqline(regression_model$residuals, col=2)

#3: Los errores son independientes
dwtest(regression_model)

#4: Homocedasticidad
plot(regression_model$residuals, ylab = 'Studentized residuals',
     xlab = 'Predictions',
     main = 'Multi Fit Studentized residuals'); qqline(0, col=3)

detach(dataset)

