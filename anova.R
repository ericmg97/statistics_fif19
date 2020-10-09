library(lmtest)

# Cargar los datos de los jugadores
players <- read.csv('./players_19.csv')

#Trabajar con los jugadores de los clubes: Rayo Vallecano, Manchester City, Herta BSC, VFL Wolfsburg
players_data <- subset(players,(players$club == "FC Barcelona" | players$club == "Valencia CF" | players$club == "Real Sociedad") & !is.na(players$passing))

#Eleccion de las variables a analizar
vars_select = c("club","passing")

dataset = players_data[vars_select]

club_passing = dataset[order(dataset$club),]


df = data.frame(club_passing$club,club_passing$passing)

boxplot(club_passing$passing ~ club_passing$club, data=df)

passing_anova <- aov(club_passing$passing ~ club_passing$club, data = df)
summary(passing_anova)

anova_residuals <- passing_anova$residuals

#1. Los 𝑒𝑖𝑗 siguen una distribución normal con media cero.
#2. Los 𝑒𝑖𝑗 son independientes entre sí.
#3. Los residuos de cada tratamiento tienen la misma varianza 𝜎
 


hist(anova_residuals)
qqnorm(anova_residuals); qqline(anova_residuals, col=2)
plot(anova_residuals)

shapiro.test(anova_residuals)

bartlett.test(anova_residuals, df$club_passing.club)

dwtest(passing_anova)

