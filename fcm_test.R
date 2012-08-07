#Teste FCM with toy problems

x1 = matrix(rnorm(100,mean=0,sd=0.5),ncol=2)
x2 = matrix(rnorm(100,mean=2,sd=0.5),ncol=2)
y1 = rep(1,nrow(x1))
y2 = rep(2,nrow(x2))

xplot = cbind(x1[,1],x2[,1]);
yplot = cbind(x1[,2],x2[,2]);
matplot(xplot,yplot,pch='o',col=c('blue','red'))

xin = rbind(x1,x2)
result = fcm(xin,2)

points(result$centers)

clusters = apply(result$U, 1, which.max)

y = c(y1,y2)
error = sum((clusters == y) == FALSE)
print(paste('O número de erros é:',error))
