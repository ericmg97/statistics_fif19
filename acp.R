# Cargar los datos de los jugadores
players <- read.csv('./players_19.csv')

#Trabajar unicamente con jugadores de campo del FCBarcelona con promedio superior a 75
barca_field_players <- subset(players, !is.na(players$pace) & players$club == "FC Barcelona" & players$overall >= 75)

row.names(barca_field_players) <- barca_field_players$"short_name"

#Eleccion de las variables a analizar
vars_select = c("pace","shooting","passing","dribbling", "defending")

acp_dataset = barca_field_players[vars_select]

attach(acp_dataset)

#Calcular la matriz de correlacion
tp = cor(acp_dataset)

#Chequear de forma grafica si existe correlacion
symnum(tp)

#Calculo de ACP
acp <- prcomp(acp_dataset, scale = TRUE)
summary(acp)

#Matriz de valores propios
acp$rotation[,1:5]

#Graficos
plot(acp)
biplot(acp, choices = 1:2)

detach(acp_dataset)


