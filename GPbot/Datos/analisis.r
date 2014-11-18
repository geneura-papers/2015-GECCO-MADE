datos = read.csv("sc.csv")
labels = unique(datos$LABEL)

generaciones = read.csv("generation.csv")

puntitos = c(0,0,0,0,3,3,3,3,2,2,2,2,5,5,5,5,1,1,1,1)
puntitos = c(11,11,11,11,0,0,0,0,15,15,15,15,8,8,8,8,2,2,2,2)

#Boxplot SCORE
par(cex.axis = 0.85,las=3)
boxplot(SCORE~LABEL,data=datos,cex=0.2, ylab = "Score Best Individual")
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=c("Age of outliers","Fitness improvement","Fitness threshold","Number of generations","Replacement rate"),las=0,tck=0,cex.axis=0.55)

#Boxplot GENERATIONS
par(cex.axis = 0.85,las=3)
boxplot(IT~LABEL,data=datos,cex=0.2, ylab="Generations needed to stop")
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=c("Age of outliers","Fitness improvement","Fitness threshold","Number of generations","Replacement rate"),las=0,tck=0,cex.axis=0.55)


#Tasa de finalización
c_rate = table(datos$LABEL)/36
plot(c_rate, type="p", ylab="Completion Rate in experiments",pch=puntitos)
abline(v=4.5)
abline(v=8.5)
abline(v=12.5)
abline(v=16.5)
axis(3,at=c(2,6,10,14,19),labels=c("Age of outliers","Fitness improvement","Fitness threshold","Number of generations","Replacement rate"),las=0,tck=0,cex.axis=0.55)

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
qplot(IT,LABEL,data=datos,ylab="Stop criterion",xlab="Generations needed")

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
plot(d[,2:3],pch=puntitos,xlim = c(0,500),ylab="Average SCORE of Best Individual","Average GENERATION Needed to stop")
text(d[,2],d[,3],paste(d[,1]," <",round(d[,4]/36*100),"%>",sep=""),cex=0.65,pos=4)
legend("bottomright",cex=0.85,pch = c(11,0,15,8,2,NA_integer_),legend=c("AO - Age of outliers","FI - Fitness improvement","FT - Fitness threshold","NG - Number of generations","RT - Replacement rate","<value> - Completion Rate in experiments"))


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

