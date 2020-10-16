library(rpart)
library(rpart.plot)
library(digest)

# Cargar los datos de los jugadores
players_19 <- read.csv('./players_19.csv')

l<-length(players_19$sofifa_id)

#Selección de un subconjunto de variables, manejo de valores faltantes,
#conversión de las clases objetivos a enteros y normalización de datos
subconjunto_con_id = subset(players_19, select = -c(player_url, short_name, long_name, dob, real_face, joined, contract_valid_until, nation_position, nation_jersey_number, team_jersey_number, player_traits, player_tags, player_positions, ls, st, rs, lw, lf,cf,rf,rw,lam,cam,ram, lm, lcm, cm,rcm,rm,lwb,ldm,cdm,rdm,rwb,lb,lcb,cb,rcb,rb))
subconjuntonumerico=subset(subconjunto_con_id, select=-c(nationality,club,preferred_foot, work_rate, body_type, team_position, loaned_from))
subconjuntonominal=subset(subconjunto_con_id, select=c(sofifa_id,nationality,club,preferred_foot, work_rate, body_type, team_position, loaned_from))
subconjuntoresultado=subset(subconjunto_con_id, select=c(sofifa_id,team_position))

subconjuntonumerico[is.na(subconjuntonumerico)]<-0
subconjuntonumerico<-sapply(subconjuntonumerico,as.numeric)
subconjuntonumerico[is.na(subconjuntonumerico)]<-0

subconjunto<-merge(subconjuntonumerico,subconjuntoresultado)
targetob<-digest2int(subconjunto$team_position)#Conversión de las clases objetivos de string a enteros
subconjunto$target<-targetob
subconjunto=subset(subconjunto, select = -c(sofifa_id,team_position))
subconjunto[is.na(subconjunto)]<-"No"

#Elección del subconjunto de entrenamiento
sub<-sample(1:l,10000)
sub

subconjunto[sub,]#Muestra de entrenamiento

subconjunto[-sub,]#Muestra de prueba

#Generación del CART
subconjunto.tree <- rpart(formula=target~. , data=subconjunto[sub,], cp=0.01, maxdepth=20, method = "class")

summary(subconjunto.tree)
subconjunto.tree

#Graficando el árbol resultante
rpart.plot(subconjunto.tree)


#Verificando en la muestra de prueba el CART
subconjunto.pred <- predict(subconjunto.tree, newdata = subconjunto[-sub,],type="class")
subconjunto.pred
positions<-subconjunto[-sub,]$target

#Tabla de confusión
tb <- table(subconjunto.pred,subconjunto[-sub,]$target)

error.rpart<- 1-(sum(diag(tb))/sum(tb))#Cálculo del error
tb
error.rpart

