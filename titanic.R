setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#1
data <- read.csv("titanic.csv")
head(data)
summary(data)

#2
b = data[data$Survived==1,"Survived"]
length(b) / length(data)


#3
n <- colnames(data)
for(x in 1:length(n)){
  print( paste(n[x],":" , toString(length(data[(is.na(data[,x]))==T,x]))))
}

#4
data <- data[,!n=="Cabin"]
data

#5
prom <- median(data[,"Age"],na.rm = T)
data[is.na(data["Age"]),"Age"] = prom
data

#6
#Investigando un poco en como hallar la probabilidad, veo algo de luz
#veo datos bastente interesante usando glm con distintas combinaciones 
table(data$Sex,data$Survived)
plot(table(data$Sex,data$Survived))

binom_model1 <- glm(Sex~Survived+Pclass+Age+SibSp+Parch+Fare,family=binomial(link="logit"),data=data)
summary(binom_model1)

#Finalmente, el porcentaje de supervivencia más alto lo tienen las mujeres
aggregate(Survived~Sex,data,FUN=mean)

#7
#No es muy interpretable o algo estoy haciendo mal
aggregate(Survived~Age,data,FUN=mean)


#8
#Las personas de 80 años tuvieron tuvieron una probabilidad alta de salvarse lo mismo de personas
#entre 0:10 año a diferencia de peronas de 20, 40 y 60
data$Decade <- findInterval(data$Age, c(10,20, 30, 40, 50, 60, 70, 80, 90))
aggregate(Survived~Decade,data,FUN=mean)

#9
#Las personas de primera clase tuvieron un promedio mas alto en salvarse
aggregate(Survived~Pclass,data,FUN=mean)

#Adicionalmente, las personas (pregunta Nro 8) que no se salvaron efectivamente estuvieron 
#en la clase 3
aggregate(Survived~Pclass+Age,data,FUN=mean)

#10
#Las mujeres tuvieron prioridad en todas las clases
#Les habrán dado algun seguro, como habran hecho las mujeres con las deudas de la epoca
#Claro, a lo mejor las personas de primera clase no tuvieron problemas pero en 2° y 3° ?
aggregate(Survived~Sex+Pclass,data,FUN=mean)

#11
data["familysize"] <- data["SibSp"]	+ data["Parch"] + 1
data["sigleton"] <- data["familysize"]==1

#12
#Las que fueron solos tuvieron menos oportunidad de salvarse pero en todos los análisis anteriores
#Los hombres fueron los de más bajo porcentaje, veamos el mismo análisis según sexo
aggregate(Survived~sigleton,data,FUN=mean)

#efectivamente, daba lo mismo si viajabas solo o no, si eras hombre(En ese entonces eramos valientes)
#estabas condenad a morir por tus mujeres, Wow
aggregate(Survived~sigleton+Sex,data,FUN=mean)

#13
data["contador"] <- 1
aggregate(contador~Pclass+familysize,data,FUN=sum)
# en matriz 
table(data$Pclass,data$familysize)

#Adicionalmente de aquella clasificación, en que grupos hubo sobrevivientes
#Claramente en las familias menos numerosas, familias entre 1 y 5 miembros, evidentemente tuvieron mayor acceso a la evacución
table(data$Pclass, data$familysize, data$Survived)