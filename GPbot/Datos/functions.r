

datos = read.csv("todosMedio.csv",sep=",",dec = ".",quote = "")
datos$IT = as.numeric(datos$IT)
datos$SCORE = as.numeric(datos$SCORE!="LABEL")

labels = names(summary(datos$LABEL))

mIT = matrix(nrow = length(labels), ncol= max(summary(datos$LABEL)))

#mIT = data.frame(row.names = labels)

names(mIT) = labels
j = 1
for(i in labels){
  print(i)
  mIT[j,] = datos$IT[datos$LABEL == i]
  j = j + 1
}

rownames(mIT)
colnames(mIT)


