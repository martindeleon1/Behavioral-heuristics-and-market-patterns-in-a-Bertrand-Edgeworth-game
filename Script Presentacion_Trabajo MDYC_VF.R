C #Costo por unidad
Pa #Precio de equilibrio para el cual todas las empresas venden su maxima capacidad al mismo precio Pa
Qa #Cantidad maxima que puede producir cada empresa.
N #Numero de empresas
Sigp #Valor que amuenta P si la empresa vende todo su stock
Sigm #Valor que disminuye P si la empresa no vende
M=(Pa*Qa*N) #M es la suma de todo el dinero que piensan gastar los consumidores. El modelo supone que los consumidores actuan como una masa, por lo que se los simplifica con M.
Pedge=Pa+C/N #Definicion del precio para el cual se vuelve estrategia dominante aumentar los precios.
tmax #Periodo Final
O #Parametro que define la sensibilidad de la variacion de precios en funcion del diferencial de los beneficios pasados
E #Valor para el cual se distribuira Eta.
Hm #Probabilidad para la cual la empresa sube su precio en Sigp
Hp #Probabilidad para la cual la empresa baja su precio en Sigm
HO #Probabilidad para la cual la empresa mantiene su precio

Graficar<-function(C,Pa,Qa,N,Sigp,Sigm,tmax){
  t<-1
  M<-(Pa*Qa*N)
  Pedge<-Pa+C/N
  Pi<-runif(N,Pa,2*Pa)
  Precios<-matrix(0,tmax,N+3)
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  Precios[t,]<-Pistats
  
  while (t<tmax) {
    t<-t+1
    M1<-M
    Piaux<-Pi
    Vecaux<-rep(-Sigm,N)
    while (M1>=min(Piaux)*Qa) {
      M1<-M1-min(Piaux)*Qa
      Pi[match(min(Piaux),Piaux)]<-Pi[match(min(Piaux),Piaux)]+Sigp
      Vecaux[match(min(Piaux),Piaux)]<-0
      Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
    }
    Pi<-Pi+Vecaux
    
    Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
    Precios[t,]<-Pistats
  }
  
  colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
  rownames(Precios)<-c(paste("t",1:tmax))
  
  Precios<-as.data.frame(Precios)
  
  Pedge<-rep(Pedge,tmax)
  
  plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05))
  lines(Precios$Max,col=3)
  lines(Precios$Min,col=4)
  lines(Precios$"Firma 1",col=1)
  lines(Pedge,col=6)
  
  legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
}

#Modelo 2
Graficar2<-function(C,Pa,Qa,N,tmax,O,E){
  t<-1
  M<-(Pa*Qa*N)
  Pedge<-Pa+C/N
  Pi<-runif(N,Pa,2*Pa)
  Precios<-matrix(0,tmax,N+3)
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  Precios[t,]<-Pistats
  
  Beneficios<-matrix(0,tmax,N)
  Bi<-rep(0,N)
  for (i in 1:N) {
    Bi[i]<-Qa*(Pi[i]-C)
  }
  M1<-M
  Piaux<-Pi
  Vende<-rep(0,N)
  while (M1>=min(Piaux)*Qa) {
    M1<-M1-min(Piaux)*Qa
    Vende[match(min(Piaux),Piaux)]<-1
    Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
  }
  Bi<-Bi*Vende
  Beneficios[t,]<-Bi
  
  t<-t+1
  for (i in 1:N) {
    if (Beneficios[t-1,i]!=0) {
      Eta<-runif(1,-E,E)
    }else{
      Eta<-runif(1,-E,0)  
    }
    Pi[i]<-Precios[t-1,i]+O*(Beneficios[t-1,i]-0)*sign(Precios[t-1,i]-0)+Eta
  }
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  Precios[t,]<-Pistats
  
  for (i in 1:N) {
    Bi[i]<-Qa*(Pi[i]-C)
  }
  M1<-M
  Piaux<-Pi
  Vende<-rep(0,N)
  while (M1>=min(Piaux)*Qa) {
    M1<-M1-min(Piaux)*Qa
    Vende[match(min(Piaux),Piaux)]<-1
    Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
  }
  Bi<-Bi*Vende
  Beneficios[t,]<-Bi
  while( t < tmax ){
    t<-t+1
    for (i in 1:N) {
      if (Beneficios[t-1,i]!=0 || Beneficios[t-2,i]!=0) {
        Eta<-runif(1,-E,E)
      }else{
        Eta<-runif(1,-E,0)  
      }
      Pi[i]<-Precios[t-1,i]+O*(Beneficios[t-1,i]-Beneficios[t-2,i])*sign(Precios[t-1,i]-Precios[t-2,i])+Eta
    }
    Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
    Precios[t,]<-Pistats 
    
    for (i in 1:N) {
      Bi[i]<-Qa*(Pi[i]-C)
    }
    M1<-M
    Piaux<-Pi
    Vende<-rep(0,N)
    while (M1>=min(Piaux)*Qa) {
      M1<-M1-min(Piaux)*Qa
      Vende[match(min(Piaux),Piaux)]<-1
      Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
    }
    Bi<-Bi*Vende
    Beneficios[t,]<-Bi
    
  }
  
  colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
  rownames(Precios)<-c(paste("t",1:tmax))
  
  Precios<-as.data.frame(Precios)
  
  Pedge<-rep(Pedge,tmax)
  
  plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05))
  lines(Precios$Max,col=3)
  lines(Precios$Min,col=4)
  lines(Precios$"Firma 1",col=1)
  lines(Pedge,col=6)
  
  legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
}

Graficar3<-function(C,Pa,Qa,N,Sigp,Sigm,Hm,Hp,tmax){
t<-1
M<-(Pa*Qa*N)
Pedge<-Pa+C/N
Pi<-runif(N,Pa,2*Pa)
Precios<-matrix(0,tmax,N+3)
Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
Precios[t,]<-Pistats

while (t<tmax) {
t<-t+1
M1<-M
Piaux<-Pi
Vecaux<-rep(-Sigm,N)
while (M1>=min(Piaux)*Qa) {
  Rand<-runif(1,0,1)
  if (Rand<=Hm) {
    Aum<--Sigm
  }else{
    if (Rand<=Hm+Hp) {
      Aum<-Sigp
    }else{
      Aum<-0
    }
  }
  M1<-M1-min(Piaux)*Qa
  Pi[match(min(Piaux),Piaux)]<-Pi[match(min(Piaux),Piaux)]+Aum
  Vecaux[match(min(Piaux),Piaux)]<-0
  Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
}
Pi<-Pi+Vecaux

Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
Precios[t,]<-Pistats
}

colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
rownames(Precios)<-c(paste("t",1:tmax))

Precios<-as.data.frame(Precios)

Pedge<-rep(Pedge,tmax)

plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05))
lines(Precios$Max,col=3)
lines(Precios$Min,col=4)
lines(Precios$"Firma 1",col=1)
lines(Pedge,col=6)

legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
}

#Simulaciones

par(mfrow=c(3,3))


#Modelo 1

#Caso inicial
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.1,tmax=100)

#Cambio en costos y N
Graficar(C=0.5,Pa=1,Qa=1,N=15,Sigp=0.02,Sigm=0.1,tmax=100)

#Cambio en Sigp
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.09,Sigm=0.1,tmax=100)

#Cambio en Sigm
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.04,tmax=100)

#Sigm=Sigp
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.1,Sigm=0.1,tmax=100)

#Sigm >> Sigp
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.05,Sigm=0.5,tmax=100)

#Sigp>Sigm
Graficar(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.08,Sigm=0.05,tmax=100)


#Modelo 2

#Caso inicial
Graficar2(C=0.75,Pa=1,Qa=1,N=10,tmax=300,O=0.2,E=0.05)

#Cambio O (Ajuste)
Graficar2(C=0.75,Pa=1,Qa=1,N=10,tmax=300,O=0.4,E=0.05)

#Cambio E (Ruido aleatorio)
Graficar2(C=0.75,Pa=1,Qa=1,N=10,tmax=300,O=0.2,E=0.1)

#Cambio N
Graficar2(C=0.75,Pa=1,Qa=1,N=100,tmax=300,O=0.2,E=0.05)

#Cambio Pa
Graficar2(C=0.75,Pa=3,Qa=1,N=10,tmax=300,O=0.2,E=0.05)

#Cambio Costos
Graficar2(C=0.5,Pa=1,Qa=1,N=10,tmax=300,O=0.2,E=0.05)


#Modelo 3

#Caso inicial
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.1,Hm=0,Hp=0.55,tmax=100)

#Cambio en Hm
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.1,Hm=0.05,Hp=0.55,tmax=100)

#Cambio en Hp
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.1,Hm=0,Hp=0.9,tmax=100)

#Cambio en costos y N
Graficar3(C=0.5,Pa=1,Qa=1,N=15,Sigp=0.02,Sigm=0.1,Hm=0,Hp=0.55,tmax=100)

#Cambio en Sigp
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.09,Sigm=0.1,Hm=0,Hp=0.55,tmax=100)

#Cambio en Sigm
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.04,Hm=0.05,Hp=0.55,tmax=100)

#Sigm=Sigp
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.1,Sigm=0.1,Hm=0,Hp=0.55,tmax=100)

#Sigm >> Sigp
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.5,Hm=0,Hp=0.55,tmax=100)

#Sigm >> Sigp y Hm chico
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.02,Sigm=0.5,Hm=0.01,Hp=0.55,tmax=100)

#Sigp>Sigm
Graficar3(C=0.75,Pa=1,Qa=1,N=10,Sigp=0.1,Sigm=0.05,Hm=0,Hp=0.55,tmax=100)


