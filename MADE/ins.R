
dir<-"/home/mgarenas/inves/MADE/made_nov14/";
directorioMedias <- paste(dir,"medias",sep="");
directorioTukey <- paste(dir,"tukey",sep="");
directorioBoxplot <- paste(dir,"boxplot",sep="");
directorioHist <- paste(dir,"histogramas",sep="");
dir

#z1 <- read.table("/home/mgarenas/inves/MADE/made_nov14/z1.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#z1 <- read.table(paste(dir,"z1.csv",sep=""),header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
#z2 <- read.table(paste(dir,"z2.csv",sep=""),header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
z5 <- read.table(paste(dir,"z5.csv",sep=""),header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)

#Cambiar aqui el conjunto de datos activo
datos <- z5
nombre <- "z5"

extension <- ".eps"
parametros<-c("P","D","W","F","S")

dirTukey <- paste(directorioTukey,nombre,sep = "")
dirMedias <-paste(directorioMedias,nombre,sep = "")
dirBox <-paste(directorioBoxplot,nombre,sep = "")
dirHist <-paste(directorioHist,nombre,sep = "")

x11(width=10, height=10, xpos=0, pointsize=24)
hist(datos$BEST_FITNESS, main="Histogram for Best Fitness", xlab="Best Fitness")
dir<-paste(dirHist,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

tapply(datos$BEST_FITNESS, list(datos$P), mean)
tapply(datos$BEST_FITNESS, list(datos$D), mean)
tapply(datos$BEST_FITNESS, list(datos$W), mean)
tapply(datos$BEST_FITNESS, list(datos$F), mean)
tapply(datos$BEST_FITNESS, list(datos$S), mean)
tapply(datos$BEST_FITNESS, list(datos$P), median)
tapply(datos$BEST_FITNESS, list(datos$D), median)
tapply(datos$BEST_FITNESS, list(datos$W), median)
tapply(datos$BEST_FITNESS, list(datos$F), median)
tapply(datos$BEST_FITNESS, list(datos$S), median)

# P*D*W*F*S
datos$P <- as.factor(datos$P)
datos$D <- as.factor(datos$D)
datos$W <- as.factor(datos$W)
datos$F <- as.factor(datos$F)
datos$S <- as.factor(datos$S)

#anova(lm(BEST_FITNESS ~ P*D*W*F*S, datos))
summary(aovP<-(aov(datos$BEST_FITNESS ~ P, data=datos)))
summary(aovD<-(aov(datos$BEST_FITNESS ~ D, data=datos)))
summary(aovW<-(aov(datos$BEST_FITNESS ~ W, data=datos)))
summary(aovF<-(aov(datos$BEST_FITNESS ~ F, data=datos)))
summary(aovS<-(aov(datos$BEST_FITNESS ~ S, data=datos)))


kwp<-kruskal.test(BEST_FITNESS ~ P, data=datos)
kwp
kwd<-kruskal.test(BEST_FITNESS ~ D, data=datos)
kwd
kww<-kruskal.test(BEST_FITNESS ~ W, data=datos)
kww
kwf<-kruskal.test(BEST_FITNESS ~ F, data=datos)
kwf
kws<-kruskal.test(BEST_FITNESS ~ S, data=datos)
kws
kwTodo<-kruskal.test(BEST_FITNESS ~ P*D*W*F*S, data=datos)
kwTodo
#Funcion <- (lm(BEST_FITNESS ~ P*D*W*F*S, data=datos))
#anova <- aov(Funcion)
#summary(anova)

require(graphics)
require(PMCMR)
posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$P, "Chisq")
posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$D, "Chisq")
posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$W, "Chisq")
posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$F, "Chisq")
posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$S, "Chisq")

nemeS<-posthoc.kruskal.nemenyi.test(datos$BEST_FITNESS, datos$S, "Chisq")
nemeS


plot(nemeS)
type(nemeS)

plot(TukeyHSD(aovP, parametros[1], ordered = TRUE))
dir<-paste(dirTukey,parametros[1], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

plot(TukeyHSD(aovD, "D", ordered=TRUE))
dir<-paste(dirTukey,parametros[2], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

plot(TukeyHSD(aovW, "W", ordered=TRUE))
dir<-paste(dirTukey,parametros[3], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

plot(TukeyHSD(aovF, "F", ordered=TRUE))
dir<-paste(dirTukey,parametros[4], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

plot(TukeyHSD(aovS, "S", ordered=TRUE))
dir<-paste(dirTukey,parametros[5], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)


boxplot(BEST_FITNESS~P, ylab="Best Fitness", xlab="P", data=datos)
dir<-paste(dirBox,parametros[1], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

boxplot(BEST_FITNESS~D, ylab="Best Fitness", xlab="D", data=datos)
dir<-paste(dirBox,parametros[2], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

boxplot(BEST_FITNESS~W, ylab="Best Fitness", xlab="W", data=datos)
dir<-paste(dirBox,parametros[3], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

boxplot(BEST_FITNESS~F, ylab="Best Fitness", xlab="F", data=datos)
dir<-paste(dirBox,parametros[4], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)

boxplot(BEST_FITNESS~S, ylab="Best Fitness", xlab="S", data=datos)
dir<-paste(dirBox,parametros[5], sep= "")
dir<-paste(dir,extension,sep= "")
dev.copy2eps(file=dir, width=10.0, height=10.0, pointsize=24)


shapiro.test(datos$BEST_FITNESS)

levene.test(datos$BEST_FITNESS, datos$P)
levene.test(datos$BEST_FITNESS, datos$D)
levene.test(datos$BEST_FITNESS, datos$W)
levene.test(datos$BEST_FITNESS, datos$F)
levene.test(datos$BEST_FITNESS, datos$S)


