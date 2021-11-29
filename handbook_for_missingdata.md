```
x1_na_list = sample(1:100,25)
x2_na_list = sample(1:100,25)
Xmis=Y
for (i in x1_na_list){
  Xmis[i,1]=NA
}
for (i in x2_na_list){
  Xmis[i,2]=NA
}

```

````
```{r}
y=Xmis
Estep = function(mu,sigma){
   beta21.1 = sigma[1,2]/sigma[1,1]
   beta20.1 = mu[2] - beta21.1*mu[1]
   sigma22.1=sigma[2,2]-(sigma[1,2]^2)/sigma[1,1]
   s1=sum(y[,1],na.rm = T)
   s2=sum(beta20.1+beta21.1*y[,1],na.rm = T)
   s11=sum(y[,1]^2,na.rm = T)
   s22=sum((beta20.1+beta21.1*y[,1])^2+sigma22.1,na.rm = T)
   s12=sum((beta20.1+beta21.1)*y[,1],na.rm = T)
   pra_list = c(s1,s2,s11,s22,s12)
   return(pra_list)
}
Mstep = function(mu,sigma){
   pra_list=Estep(mu,sigma)
   s1=pra_list[1]
   s2=pra_list[2]
   s11=pra_list[3]
   s22=pra_list[4]
   s12=pra_list[5]
   mu[1]=s1/n
   mu[2]=s2/n
   sigma[1]=s11/n-mu[1]^2
   sigma[2]=s22/n-mu[2]^2
   sigma[1,2]=s12/n-mu[1]*mu[2]
   pra_list_order = c(mu,sigma[1],sigma[2],sigma[1,2],sigma[1,2])
   return(pra_list_order)
}
mu<-c(2, -1) 
sigma<-matrix(c(1, 0.5,0.5, 1), nrow=2, ncol=2) 
n=length(y[!is.na(y)])
pra_list_order=Mstep(mu,sigma)
#the order mu and simga is:
mu
sigma
#the new mu[1] mu[2] and sigma[1]^2 sigma[2]^2 sigma[1.2] sigma[1.2] in next:
pra_list_order
```
````

