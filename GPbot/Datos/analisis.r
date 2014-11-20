datos = read.csv("sc.csv")
labels = unique(datos$LABEL)

l = c("Number of generations","Age of outliers","Replacement rate","Fitness threshold","Fitness improvement")

generaciones = read.csv("generation.csv")

puntitos = c(0,0,0,0,3,3,3,3,2,2,2,2,5,5,5,5,1,1,1,1)
puntitos = c(11,11,11,11,0,0,0,0,15,15,15,15,8,8,8,8,2,2,2,2)

#Reordenamos para que Antonio sea feliz. Si es una chapuza. Atención que los NOMBRES sean siempre los mismos
datos$LABEL = factor(datos$LABEL, levels = c("NG_030.0","NG_050.0","NG_100.0","NG_200.0","AO_1.0","AO_1.5","AO_2.0","AO_2.5","RT_n/02","RT_n/04","RT_n/08","RT_n/16","FT_20.0","FT_22.0","FT_24.0","FT_26.0","FI_03.0","FI_07.0","FI_10.0","FI_15.0"))

#Boxplot SCORE
par(cex.axis = 0.70,las=3)
boxplot(SCORE~LABEL,data=datos,cex=0.2, ylab = "Score Best Individual",log="y")
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=l,las=0,tck=0,cex.axis=0.75)

#Boxplot GENERATIONS
par(cex.axis = 0.70,las=3)
boxplot(IT~LABEL,data=datos,cex=0.2, ylab="Generations to stop",log="y")
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=l,las=0,tck=0,cex.axis=0.75)

dev.off()
#Tasa de finalización
c_rate = table(datos$LABEL)/36
plot(c_rate, type="p", ylab="Completion Rate in experiments",pch=puntitos)
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=l,las=0,tck=0,cex.axis=0.55)

#Fitness evolution population
boxplot(SCORE~IT,data=generaciones,cex=0.2,xaxt="n",ylab="Score Population",xlab="Generation",ylim=c(0,30))
axis(1, at=seq(from = 50,to = 500,by = 50),labels=seq(from = 50,to = 500,by = 50))

#Fitness evolution best
boxplot(SCORE~IT,data=generaciones[generaciones$POSITION==0,],cex=0.2,xaxt="n",ylab="Score best individual",xlab="Generation",ylim=c(0,30))
axis(1, at=seq(from = 50,to = 500,by = 50),labels=seq(from = 50,to = 500,by = 50))

#Time evolution
boxplot(TIME/60/60~IT,data=generaciones,cex=0.2,xaxt="n",ylab="Time",xlab="Generation")
axis(1, at=seq(from = 50,to = 500,by = 50),labels=seq(from = 50,to = 500,by = 50))


library( ggplot2)
qplot(SCORE,LABEL,data=datos,ylab="Stop criterion",xlab="Score")
qplot(IT,LABEL,data=datos,ylab="Stop criterion",xlab="Generations")

#qplot(IT,LABEL,data=datos,cex=0.2,xaxt="n",ylab="Score",xlab="Generations needed",fill=SCORE)
#datos_AO = datos[datos$LABEL== c("AO_1.0","AO_1.5","AO_2.0","AO_2.5"),]
#qplot(LABEL,IT,data=datos_AO,cex=0.2,xaxt="n",ylab="LABEL",xlab="GENERATION",fill=SCORE,geom=c("boxplot", "jitter"))




label_it = aggregate(IT~LABEL,data=datos,FUN=mean)
label_score = aggregate(SCORE~LABEL,data=datos,FUN=mean)
label_rate = aggregate(VICTORIES~LABEL,data=datos,FUN=length)
names(label_rate) = c("LABEL", "RATE")
d = merge(label_it,label_score)
d = merge(d, label_rate)


#barplot(d$IT,names.arg = d$LABEL, col = gray.colors(n = 20, start = 0.8, end = 0.1 )[as.factor(d$SCORE)])
#legend("topright",col = gray.colors(n = 20, start = 0.8, end = 0.1 )[as.factor(d$SCORE)], legend=d$SCORE)

#Comparativa
par(mar=c(4,4,0,2))
plot(d[,2:3],pch=puntitos,ylab="Average SCORE of Best Individual","Average GENERATION to stop",cex=0.8,xlim=c(0,500))
text(d[,2],d[,3],paste(d[,1]," <",round(d[,4]/36*100),"%>",sep=""),cex=0.65,pos=4)
legend("bottomright",cex=0.65,pch = c(11,0,15,8,2,NA_integer_),legend=c("AO - Age of outliers","FI - Fitness improvement","FT - Fitness threshold","NG - Number of generations","RT - Replacement rate","<value> - Completion Rate in experiments"))

par(mar=c(4,4,0,0))
plot(d[,2:3],pch=puntitos,ylab="Average SCORE of Best Individual","Average GENERATION to stop",cex=0.8,log ="yx",xlim=c(1,500))
text(d[,2],d[,3],paste(d[,1]," <",round(d[,4]/36*100),"%>",sep=""),cex=0.65,pos=c(2,4,4,2,4,4,4,4,2,2,2,2,2,2,4,2,4,4,4,2))
legend("bottomright",cex=0.65,pch = c(11,0,15,8,2,NA_integer_),legend=c("AO - Age of outliers","FI - Fitness improvement","FT - Fitness threshold","NG - Number of generations","RT - Replacement rate","<value> - Completion Rate in experiments"))

#par(mfrow = c(2,3),mar = c(0,0,0,0),cex=1.1)
#plot(d[1:4,2:3],pch=puntitos[1:4],xlim = c(0,500),cex=d[,4]/24,ylim = c(0,30))
#text(d[1:4,2],d[1:4,3],paste(d[1:4,1]," <",round(d[1:4,4]/36*100),"%>",sep=""),cex=0.4,pos=4)

#plot(d[5:8,2:3],pch=puntitos[5:8],xlim = c(0,500),cex=d[,4]/24,ylim = c(0,30))
#text(d[5:8,2],d[5:8,3],paste(d[5:8,1]," <",round(d[5:8,4]/36*100),"%>",sep=""),cex=0.4,pos=4)

#plot(d[9:12,2:3],pch=puntitos[9:12],xlim = c(0,500),cex=d[,4]/24,ylim = c(0,30))
#text(d[9:12,2],d[9:12,3],paste(d[9:12,1]," <",round(d[9:12,4]/36*100),"%>",sep=""),cex=0.4,pos=4)

#plot(d[13:16,2:3],pch=puntitos[13:16],xlim = c(0,500),cex=d[,4]/24,ylim = c(0,30))
#text(d[13:16,2],d[13:16,3],paste(d[13:16,1]," <",round(d[13:16,4]/36*100),"%>",sep=""),cex=0.4,pos=4)

#plot(d[17:20,2:3],pch=puntitos[17:20],xlim = c(0,500),cex=d[,4]/24,ylim = c(0,30))
#text(d[17:20,2],d[17:20,3],paste(d[17:20,1]," <",round(d[17:20,4]/36*100),"%>",sep=""),cex=0.4,pos=4)

#plot(1, type="n", axes=F, xlab="", ylab="")

#legend("bottomright",cex=0.85,pch = c(11,0,15,8,2,NA_integer_),legend=c("AO - Age of outliers","FI - Fitness improvement","FT - Fitness threshold","NG - Number of generations","RT - Replacement rate","<value> - Completion Rate in experiments"))

#SACAMOS LOS MEJORES INDIVIDUOS A UN FICHERITO PARA LA BATERÍA DE BATALLAS
write(as.matrix(t(datos[order(datos$LABEL),c(5,12)] )),file="mejores.csv",ncolumns = 2,sep = ",")

### ESTUDIOS SCORE


#Histograma de SCORE
par(mfrow = c(5,4),mar = c(2.6,1,1,1),cex=0.5)
for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  hist(datos$SCORE[datos$LABEL == i],main = i,xlim = c(5,30))
}

#TEST DE NORMALIDAD
res_norm <- data.frame(row.names = NULL)
  
par(mfrow = c(5,4),mar = c(2.6,1,1,1),cex=0.5)
for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  qqnorm(datos$SCORE[datos$LABEL == i],main = i)
  qqline(datos$SCORE[datos$LABEL == i],main = i)
  
  print(paste(i,shapiro.test(datos$SCORE[datos$LABEL == i])[2]))
  test = shapiro.test(datos$SCORE[datos$LABEL == i])[2]
  
  nrow = data.frame(c(i,test,test<0.05),row.names = NULL)
  names(nrow) = c("SC","PVALUE","NORMAL?")
  
  res_norm <- rbind(res_norm, nrow )
}

res_norm

kruskal.test(SCORE ~ LABEL, data= datos)

res_kruskal <- matrix(nrow = length(x = unique(datos$LABEL)[ order(unique(datos$LABEL))]), ncol = length(unique(datos$LABEL)[ order(unique(datos$LABEL))]))
rownames(res_kruskal) = unique(datos$LABEL)[ order(unique(datos$LABEL))]
colnames(res_kruskal) = unique(datos$LABEL)[ order(unique(datos$LABEL))]


for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  for(j in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
    if(i != j){
    test = kruskal.test(SCORE ~ LABEL, data= datos[datos$LABEL == i | datos$LABEL == j,])
    
    res_kruskal[i,j] = test$p.value

    }
  }
}
  
res_kruskal < 0.05

levelplot(res_kruskal < 0.05, col.regions = gray.colors, scales=list(x=list(rot=90)),ylab = "", xlab="", main = "Kruskal test SCORE",border="black",colorkey=FALSE)
#contourplot(res_kruskal < 0.05, col.regions = gray.colors, scales=list(x=list(rot=90)),ylab = "", xlab="", main = "Kruskal test", at=c(0,1))



### ESTUDIOS GENERATIONS


#Histograma de SCORE
par(mfrow = c(5,4),mar = c(2.6,1,1,1),cex=0.5)
for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  hist(datos$IT[datos$LABEL == i],main = i,xlim = c(0,500))
}

#TEST DE NORMALIDAD
res_norm <- data.frame(row.names = NULL)

par(mfrow = c(5,4),mar = c(2.6,1,1,1),cex=0.5)
conta = 0
for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  
  qqnorm(datos$IT[datos$LABEL == i],main = i)
  qqline(datos$IT[datos$LABEL == i],main = i)
  
  
  
  if(conta>4 & conta!=8){
  print(paste(i,shapiro.test(datos$IT[datos$LABEL == i])[2]))
  test = shapiro.test(datos$IT[datos$LABEL == i])[2]
  
  nrow = data.frame(c(i,test,test<0.05),row.names = NULL)
  names(nrow) = c("SC","PVALUE","NORMAL?")
  
  res_norm <- rbind(res_norm, nrow )
    
  }
  
  conta = conta + 1
}

kruskal.test(IT ~ LABEL, data= datos)

res_kruskal <- matrix(nrow = length(x = unique(datos$LABEL)[ order(unique(datos$LABEL))]), ncol = length(unique(datos$LABEL)[ order(unique(datos$LABEL))]))
rownames(res_kruskal) = unique(datos$LABEL)[ order(unique(datos$LABEL))]
colnames(res_kruskal) = unique(datos$LABEL)[ order(unique(datos$LABEL))]


for(i in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
  for(j in unique(datos$LABEL)[ order(unique(datos$LABEL))]){
    if(i != j){
      test = kruskal.test(IT ~ LABEL, data= datos[datos$LABEL == i | datos$LABEL == j,])
      
      res_kruskal[i,j] = test$p.value
      
    }
  }
}

res_kruskal < 0.05

levelplot(res_kruskal < 0.05, col.regions = gray.colors, scales=list(x=list(rot=90)),ylab = "", xlab="", main = "Kruskal test Generations",border="black")
#contourplot(res_kruskal < 0.05, col.regions = gray.colors, scales=list(x=list(rot=90)),ylab = "", xlab="", main = "Kruskal test", at=c(0,1))



library(ggplot2)
mini = mini[,c(5,4,7)]
mini$TYPE = strtrim(mini$LABEL,2)
mini$SUBTYPE = substr(mini$LABEL,start = 3,stop=8)

mini$SUBTYPE[mini$LABEL=="AO_1.0"] = "A"
mini$SUBTYPE[mini$LABEL=="AO_1.5"] = "B"
mini$SUBTYPE[mini$LABEL=="AO_2.0"] = "C"
mini$SUBTYPE[mini$LABEL=="AO_2.5"] = "D"

mini$SUBTYPE[mini$LABEL=="FI_03.0"] = "A"
mini$SUBTYPE[mini$LABEL=="FI_07.0"] = "B"
mini$SUBTYPE[mini$LABEL=="FI_10.0"] = "C"
mini$SUBTYPE[mini$LABEL=="FI_15.0"] = "D"

mini$SUBTYPE[mini$LABEL=="FT_20.0"] = "A"
mini$SUBTYPE[mini$LABEL=="FT_22.0"] = "B"
mini$SUBTYPE[mini$LABEL=="FT_24.0"] = "C"
mini$SUBTYPE[mini$LABEL=="FT_26.0"] = "D"

mini$SUBTYPE[mini$LABEL=="RT_n/02"] = "A"
mini$SUBTYPE[mini$LABEL=="RT_n/04"] = "B"
mini$SUBTYPE[mini$LABEL=="RT_n/08"] = "C"
mini$SUBTYPE[mini$LABEL=="RT_n/16"] = "D"

mini$SUBTYPE[mini$LABEL=="NG_030.0"] = "A"
mini$SUBTYPE[mini$LABEL=="NG_050.0"] = "B"
mini$SUBTYPE[mini$LABEL=="NG_100.0"] = "C"
mini$SUBTYPE[mini$LABEL=="NG_200.0"] = "D"

ggplot(mini,aes(x=IT,y=SCORE,shape=SUBTYPE,colour=TYPE,size=SUBTYPE)) + scale_x_log10() + scale_y_log10() + geom_point() + facet_grid(TYPE ~ .) + scale_fill_grey(start = 0.9,end=0)

#ggplot(mini,aes(x=IT,y=SCORE,shape=TYPE,colour=TYPE, size=TYPE)) + scale_x_log10() + scale_y_log10() + geom_point() 

gp <- ggplot(mini,aes(x=IT,y=SCORE,shape=TYPE)) + scale_x_log10() + scale_y_log10() + geom_point() 




gprincipal <- ggplot(mini[mini$TYPE=="NG",],aes(x=IT,y=SCORE,shape=TYPE,colour=)) + geom_point() par(mfrow = c(2,2),mar = c(2.6,1,1,1),cex=0.5)
ggplot(mini[mini$TYPE=="FI",],aes(x=IT,y=SCORE,shape=TYPE,colour=LABEL)) + geom_point()
ggplot(mini[mini$TYPE=="FT",],aes(x=IT,y=SCORE,shape=TYPE,colour=LABEL)) + geom_point()
ggplot(mini[mini$TYPE=="AO",],aes(x=IT,y=SCORE,shape=TYPE,colour=LABEL)) + geom_point()
ggplot(mini[mini$TYPE=="RT",],aes(x=IT,y=SCORE,shape=TYPE,colour=LABEL)) + geom_point()
