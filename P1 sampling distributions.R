
n=c(5,15,30);
E_X_bar=V_X_bar=0;
for (i in 1:length(n))
{
  x=matrix(rnorm(1000*n[i]),1000,n[i])
  X_bar=apply(x,1,mean)
  E_X_bar[i]=mean(X_bar)
  V_X_bar[i]=var(X_bar)
}
cbind(n,E_X_bar,V_X_bar)

#Q1
rm(list=ls())
Y=matrix(rnorm(1000*100,10,1),ncol=100)
m=matrix(rep(0,12),ncol=4)
n=c(15,30,60)
T1=rep(0,1000)
T2=rep(0,1000)
df=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df_=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
for(i in 1:3)
{ 
  T1=rowMeans(Y[,1:n[i]])
  df[,i]=T1

m[i,1]=mean(T1)
m[i,2]=var(T1)

T2=apply(Y[,1:n[i]],1,var)
df_[,i]=T2
m[i,3]=mean(T2)
m[i,4]=var(T2)


}

colnames(df)=list("n=15","n=30","n=60")
head(df)
colnames(df2)=list("n=15","n=30","n=60")
head(df_)
colnames(m)=list("mean","variance","var_mean","var_var")
m

estimate=c(xbar=mean(m[,1]),xbar_var=mean(m[,2]),var_mean=mean(m[,3]),v_var=mean(m[,4]))
estimate

hist(df[,1],col="red")
hist(df[,2],col="yellow",add=TRUE)
hist(df[,3],col="orange",add=TRUE)
legend("topright",legend=c("n=15","n=30","n=60"),fill=c("red","yellow","orange"))

#Q2
rm(list=ls())
Y=matrix(runif(1000*30,0,10),ncol=30)
m1=matrix(rep(0,15),ncol=5)
n=c(5,15,30)
df1=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
df2=matrix(rep(0,3*1000),nrow = 1000,ncol=3)
for(i in 1:3)
{ 
  T1=2*rowMeans(Y[,1:n[i]])
  df1[,i]=T1
  m1[i,1]=n[i]
  m1[i,2]=mean(T1)
  m1[i,3]=var(T1)
  
  T2=apply(Y[,1:n[i]],1,max)
  df2[,i]=T2
  m1[i,4]=mean(T2)
  m1[i,5]=var(T2)
  
}

colnames(m1)=list("n","2*Xbar","var(2*Xbar)","X(n)","var(X(n))")
m1
hist(df1[,1],col="red")
hist(df1[,2],col="yellow",add=TRUE)
hist(df1[,3],col="orange",add=TRUE)
legend("topright",legend=c("n=5","n=15","n=30"),fill=c("red","yellow","orange"))

hist(df2[,1],col="red")
hist(df2[,2],col="yellow",add=TRUE)
hist(df2[,3],col="orange",add=TRUE)
legend("topright",legend=c("n=5","n=15","n=30"),fill=c("red","yellow","orange"))

hist(df1,col="lightgreen",main = "Overlayed histogram of distribution of 2*Xbar and X(n)")
hist(df2,col="lightblue",add=T)


#Q4

sum_xsq=function(x){
  n=length(x)
  sum_xsq=sum(x*x)
  return(sum_xsq/n)
}
alpha=0
beta=0
Y=matrix(rgamma(1000*30,8,1/10),ncol=30)
n=c(5,15,30)
for(i in 1:3)
{ 
  m1=rowMeans(Y[,1:n[i]])
  m2=apply(Y[,1:n[i]],1,sum_xsq)
  T1=(m1^2)/(m2-m1^2)
  beta[i]=mean(T1)
  T2=(m2-m1^2)/m1
  alpha[i]=mean(T2)
}
cbind(n,alpha,beta)









