rm(list=ls()) #Clears variables
require(neuralnet) #import package


load(file ="./RELU_ACT_FUNCTION_CLOSE.rda") #load nn


#----Data that it was trained on:
x<-read.csv("hw1_data.csv")
y<-na.omit(x)

y<-as.matrix(y)

for(i in 1:ncol(y)){y[,i]<-(y[,i]-min(y[,i]))/(max(y[,i])-min(y[,i]))} #Scaling


numberpoints=50 #Number of points to train on
fitcol=1 #Column to fit

y<-as.data.frame(y)

output=y[1:numberpoints,fitcol]
input=y[1:numberpoints,-fitcol]

#----------Creating test set:

test=y[numberpoints+1:nrow(y),-fitcol]
testres=y[numberpoints+1:nrow(y),fitcol]
xvals=c(numberpoints+1:-(-nrow(y)))


#----------Calculating and plotting
Predict=compute(nn,test)
Trainpred=compute(nn,input)
trainxvals=c(1:numberpoints)


plot(xvals,testres,ylab="Scaled values", xlab="Index",type="o", col="blue",ylim=c(-0.5,1),xlim=c(1,nrow(y)))
lines(xvals,Predict$net.result,type="o", col="#000000",lty=2)
lines(trainxvals,Trainpred$net.result,type="o", col="red",lty=1)
lines(trainxvals,output,type="o", col="blue",lty=1)

legend(x="bottomleft",legend=c("Data","Fit from training","Prediction"),col=c("blue","red","#000000"),lty=c(1:2,2))

