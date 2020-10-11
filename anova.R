library(lmtest)

# Cargar los datos de los jugadores
players <- read.csv('./players_19.csv')

#Trabajar con los jugadores de campo Argentinos con edades de 20, 25 y 30 años
players_data <- subset(players,(players$age == 20 | players$age == 25 | players$age == 30) & !is.na(players$physic) & players$nationality == "Argentina")

#Analizar como se comporta el fisico de los jugadores con respecto a la edad
vars_select = c("age","physic")

dataset = players_data[vars_select]

#Ordenar los jugadores por edades
age_physic = dataset[order(dataset$age),]


df = data.frame(age_physic$age,age_physic$physic)

boxplot(age_physic$physic ~ age_physic$age, data=df)

#Analisis de Varianza
physic_anova <- aov(age_physic$physic ~ age_physic$age, data = df)
summary(physic_anova)

anova_residuals <- physic_anova$residuals

#Verificar que se cumplen los supuestos del modelo (Graficamente)
hist(anova_residuals)
qqnorm(anova_residuals); qqline(anova_residuals, col=2)

plot(anova_residuals, ylab = 'Studentized residuals',
     xlab = 'Predictions',
     main = 'Anova Studentized residuals'); qqline(0, col=3)


#Verificar que se cumplen los supuestos del modelo

shapiro.test(anova_residuals)

bartlett.test(anova_residuals, df$age_physic.age)

dwtest(physic_anova)

