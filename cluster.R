# Cargar los datos de los jugadores
players_19 <- read.csv('./players_19.csv')

l<-length(players_19$sofifa_id)
sub<-sample(1:l,2000)#Eleccion de una muestra de la poblaciÃ³n

#SelecciÃ³n de un subconjunto de variables, manejo de valores faltantes
#y normalizaciÃ³n de datos
subconjunto_con_id = subset(players_19, select = -c(player_url, short_name, long_name, dob, real_face, joined, contract_valid_until, nation_position, nation_jersey_number, team_jersey_number, player_traits, player_tags, player_positions, ls, st, rs, lw, lf,cf,rf,rw,lam,cam,ram, lm, lcm, cm,rcm,rm,lwb,ldm,cdm,rdm,rwb,lb,lcb,cb,rcb,rb))
subconjunto=subset(subconjunto_con_id, select=-c(sofifa_id,nationality,club,preferred_foot, work_rate, body_type, team_position, loaned_from))
subconjunto2<-subconjunto
subconjunto2[is.na(subconjunto2)]<-0
subconjunto2<-sapply(subconjunto2,as.numeric)
subconjunto2[is.na(subconjunto2)]<-0
scaled.data <- scale(subconjunto2[sub,])
head(scaled.data)

scaled.data[sub,]#Muestra

d<-dist(scaled.data, method = "euclidean")#CÃ¡lculo de la matriz de distancia
fit <- hclust(d,method = "complete")#AplicaciÃ³n de la tÃ©cnica de clÃºster jerÃ¡rquico
d2<- as.dendrogram(fit)
d2

#GraficaciÃ³n del clÃºster jerÃ¡rquico
plot(d2)
rect.hclust(fit,k=10,border = "red")

fit.kmeans<-kmeans(scaled.data, 10)#AplicaciÃ³n de la tÃ©cnica de clÃºster por k-means
fit.kmeans

#GraficaciÃ³n de los centros de los clusters atendiendo a diferentes variables
plot((fit.kmeans$centers[1:10,1]),(fit.kmeans$centers[1:10,4]), col= 1:5, pch=1:4, xlab = "age", ylab = "overall")
plot((fit.kmeans$centers[1:10,1]),(fit.kmeans$centers[1:10,7]), col= 1:5, pch=1:4, xlab = "age", ylab = "wage")
plot((fit.kmeans$centers[1:10,1]),(fit.kmeans$centers[1:10,6]), col= 1:5, pch=1:4, xlab = "age", ylab = "value")
plot((fit.kmeans$centers[1:10,4]),(fit.kmeans$centers[1:10,8]), col= 1:5, pch=1:4, xlab = "overall", ylab = "reputation")
plot((fit.kmeans$centers[1:10,2]),(fit.kmeans$centers[1:10,34]), col= 1:5, pch=1:4, xlab = "height", ylab = "speed")
plot((fit.kmeans$centers[1:10,7]),(fit.kmeans$centers[1:10,6]), col= 1:5, pch=1:4, xlab = "wage", ylab = "value")
