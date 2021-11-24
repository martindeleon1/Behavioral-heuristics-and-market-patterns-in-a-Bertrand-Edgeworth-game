C #Costo por unidad
Pa #Precio de equilibrio para el cual todas las empresas venden su maxima capacidad al mismo precio Pa
Qa #Cantidad maxima que puede producir cada empresa.
N #Numero de empresas
Sigp #Valor que aumenta P si la empresa vende todo su stock
Sigm #Valor que disminuye P si la empresa no vende
M=(Pa*Qa*N) #M es la suma de todo el dinero que piensan gastar los consumidores. El modelo supone que los consumidores actuan como una masa, por lo que se los simplifica con M.
Pedge=Pa+C/N #Definicion del precio para el cual se vuelve estrategia dominante aumentar los precios.
tmax #Periodo Final
O #Parametro que define la sensibilidad de la variacion de precios en funcion del diferencial de los beneficios pasados
E #Valor para el cual se distribuira Eta.
Hm #Probabilidad para la cual la empresa sube su precio en Sigp
Hp #Probabilidad para la cual la empresa baja su precio en Sigm
HO #Probabilidad para la cual la empresa mantiene su precio
sim #Numero de simulaciones deseadas
Sigms #Valor maximo de Sigm para la simulacion. Por ejemplo, si Sigms=1, la simulacion tomara valores aleatorios de Sigm de 0 a 1.
Sigps #Valor maximo de Sigp para la simulacion. Por ejemplo, si Sigps=1, la simulacion tomara valores aleatorios de Sigp de 0 a 1.
Os #Valor maximo de O para la simulacion. Por ejemplo, si Os=1, la simulacion tomara valores aleatorios de O de 0 a 1.
Es #Valor maximo de E para la simulacion. Por ejemplo, si Es=0.3, la simulacion tomara valores aleatorios de E de 0 a 0.3.

#Definimos como funcion el script para hacer mas facil su ejecucion y cambio de parametros
Graficar<-function(C,Pa,Qa,N,Sigp,Sigm,tmax){
  #Definimos el periodo inicial
  t<-1
  #Definimos M de acuerdo con lo mencionado en el articulo
  M<-(Pa*Qa*N)
  #Definimos P edge de acuerdo con lo mencionado en el articulo
  Pedge<-Pa+C/N
  #Generamos el vector de precios en t=1. Este vector ira actualizando los precios periodo a periodo
  Pi<-runif(N,Pa,2*Pa)
  #Generamos una matriz para guardar los precios de cada periodo
  Precios<-matrix(0,tmax,N+3)
  #Generamos un vector auxiliar de precios, al cual le agregamos la media, el min y el max.
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  #Guardamos este vector auxiliar en la matriz de precios
  Precios[t,]<-Pistats
  
  #Generamos un while que itera cada periodo
  while (t<tmax) {
    #Dentro de cada periodo reiniciamos los vectores auxiliares y sumamos 1 a t para avanzar el periodo
    t<-t+1
    #Este valor registra el dinero disponible de los consumidores luego de cada venta
    M1<-M
    #Este vector auxiliar registra que empresa ya vendio en este periodo
    Piaux<-Pi
    #Este vector valdra 0 para las empresas que vendieron, y -Sigm para las que no vendieron. Inicialmente se define como una repeticion de -Sigm
    Vecaux<-rep(-Sigm,N)
    #Utilizamos un while en el cual se actualiza el vector de precios segun la venta de cada empresa
    #El while funciona mientras la siguiente venta sea menor a lo que resta de M
    #La venta se define como el precio minimo en el vector de precios * las cantidades maximas
    while (M1>=min(Piaux)*Qa) {
      #Se descuenta de M1 (Valor auxiliar) cada venta que se hace
      #Esto es el registro de la cantidad de dinero que tienen los consumidores que todavia no efectuaron una compra
      M1<-M1-min(Piaux)*Qa
      #Luego, se obtiene la posicion de la empresa que vendio su totalidad, y se suma el sigma a esa empresa como un aumento a su precio. La suma se hace en el vector Pi, mientras que la posicion se busca en Paux
      Pi[match(min(Piaux),Piaux)]<-Pi[match(min(Piaux),Piaux)]+Sigp
      #Luego se remplaza por 0 la posicion de esa empresa en un vector auxiliar que sirve de guia para saber que empresas no vendieron todo su stock
      Vecaux[match(min(Piaux),Piaux)]<-0
      #Finalmente reemplaza en el vector auxiliar de precios esa posicion por el precio inicial + M. Esto no cambia el valor en Pi.
      #El reemplazo se hace sumando M, a efectos de que sin importar las unidades que se manejen (miles, millones), esta empresa nunca volvera a ser seleccionada como la que tiene el precio mas bajo
      #En el caso de que los precios iniciales sean muy elevados Ninguna empresa vendera, el while no se ejecutara y todas las empresas bajaran sus precios en el siguiente periodo
      #En el caso de que los precios iniciales sean muy bajos, y luego de que este while se ejecutara N veces, seria matematicamente imposible que se ejecutara N+1 veces dado que sumamos M a cada precio
      #De esta manera, todas las empresas venderan su stock, y todas aumentaran su precio. Pero ninguna aumentara dos veces su precio en un mismo periodo
      Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
    }
    #Finalmente se suma el vector auxiliar al vector de precios (Este vector ya tiene los aumentos de las empresas que vendieron todo su stock)
    #Este vector auxiliar tiene un 0 para las empresas que vendieron todo su stock (Por lo que sus precios no cambian) y el valor de sigma menos para las que no lograron vender la totalidad de su stock
    Pi<-Pi+Vecaux
    
    #Al final del while se calcula la media, min y max. del vector de precios, para luego registrarlo en la fila t correspondiente de la matriz de precios
    Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
    Precios[t,]<-Pistats
  }
  
  #Terminado el while, terminamos con una matriz que registro todos los precios en cada periodo, asi como la media, max. y min
  #Luego le damos nombre a las columnas y las filas
  colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
  rownames(Precios)<-c(paste("t",1:tmax))
  
  #Siguiente lo transformamos en un data frame para graficar mas facilmente
  Precios<-as.data.frame(Precios)
  
  #Nos definimos un vector que es Pedge repetido tmax veces, a efectos de graficarlo
  
  Pedge<-rep(Pedge,tmax)
  
  #Graficamos utilizando las funciones Plot y lines. Se grafica la media, el min, el max., una firma y el Pedge
  #Para definir la escala de Y utilizamos el precio maximo y minimo para todos los periodos
  plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05),main = "Regla basada en ventas",ylab="Precio",xlab = "Periodo")
  lines(Precios$Max,col=3)
  lines(Precios$Min,col=4)
  lines(Precios$"Firma 1",col=1)
  lines(Pedge,col=6)
  
  #Se le da una leyenda al grafico a efectos de identificar cada variable
  legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
  #Finalmente aca termina la funcion creada
}


#Modelo 2
#Definimos como funcion el script para hacer mas facil su ejecucion y cambio de parametros
Graficar2<-function(C,Pa,Qa,N,tmax,O,E){
  #Definimos el periodo inicial
  t<-1
  #Definimos M de acuerdo con lo mencionado en el articulo
  M<-(Pa*Qa*N)
  #Definimos P edge de acuerdo con lo mencionado en el articulo
  Pedge<-Pa+C/N
  #Generamos el vector de precios en t=1
  Pi<-runif(N,Pa,2*Pa)
  #Generamos una matriz para guardar los precios de cada periodo
  Precios<-matrix(0,tmax,N+3)
  #Generamos un vector auxiliar de precios, al cual le agregamos la media, el min y el max.
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  #Guardamos este vector auxiliar en la matriz de precios
  Precios[t,]<-Pistats
  
  #Teniendo los precios iniciales, debemos calcular el beneficio inicial en el periodo 1
  #Para esto generamos una matriz de beneficios, que registrara los beneficios en cada periodo periodos
  Beneficios<-matrix(0,tmax,N)
  #Definimos los beneficios como un vector con 0 N veces. En este vector se registraran los beneficios periodo a periodo
  Bi<-rep(0,N)
  #Generamos un for que calcula el beneficio para cada empresa y lo registra en el vector de beneficios
  for (i in 1:N) {
    Bi[i]<-Qa*(Pi[i]-C)
  }
  #Luego, debemos encontrar las empresas que no venden en este periodo, ya que sus beneficios deberian ser 0
  #Para esto seguimos una estrategia similar al otro modelo, donde nos definimos parametros y vectores auxiliares.
  #Este valor registra el dinero disponible de los consumidores luego de cada venta
  M1<-M
  #Este vector auxiliar registra que empresa ya vendio en este periodo
  Piaux<-Pi
  #Este vector valdra 1 para las empresas que vendieron, y 0 para las que no vendieron. Inicialmente vale 0 para todas las empresas
  Vende<-rep(0,N)
  #Utilizamos un while en el cual se actualiza el vector de precios segun la venta de cada empresa
  #El while funciona mientras la siguiente venta sea menor a lo que resta de M
  #La venta se define como el precio minimo en el vector de precios * las cantidades maximas
  while (M1>=min(Piaux)*Qa) {
    #Se descuenta de M1 (Valor auxiliar) cada venta que se hace
    #Esto es el registro de la cantidad de dinero que tienen los consumidores que todavia no efectuaron una compra
    M1<-M1-min(Piaux)*Qa
    #Luego, se obtiene la posicion de la empresa que vendio su totalidad, y se le asigna un 1 en dicha posicion
    Vende[match(min(Piaux),Piaux)]<-1
    #Finalmente reemplaza en el vector auxiliar de precios esa posicion por el precio inicial + M. Esto no cambia el valor en Pi.
    #El reemplazo se hace sumando M, a efectos de que sin importar las unidades que se manejen (miles, millones), esta empresa nunca volvera a ser seleccionada como la que tiene el precio mas bajo
    #En el caso de que los precios iniciales sean muy elevados Ninguna empresa vendera, el while no se ejecutoria y todas las empresas tendran beneficio 0
    #En el caso de que los precios iniciales sean muy bajos, y luego de que este while se ejecutara N veces, seria matematicamente imposible que se ejecutara N+1 veces dado que sumamos M a cada precio
    #De esta manera, todas las empresas venderan su stock, y todas tendran beneficios dif a 0. Pero ninguna vendera 2 veces en el mismo periodo
    Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
  }
  #Terminado esto, se multiplica el vector de beneficios por el vector vende, por lo que solo las empresas que vendieron tendran beneficios diferentes a 0
  Bi<-Bi*Vende
  #Finalmnete se registra este vector de beneficios en los beneficios del primer periodo
  Beneficios[t,]<-Bi
  
  #En este punto del script tenemos el beneficio y precio de t=1
  
  #Ahora el script calcula el beneficio y precio del segundo periodo. 
  #No se hace dentro de un while dado que la formula de calculo general tiene B[t-2], por lo que en este periodo no seria correcto. 
  #Aumentamos en uno el t
  t<-t+1
  #Generamos un for para calcular el cambio de precio en cada empresa
  for (i in 1:N) {
    #Dado que el termino aleatorio cambia su distribucion si se vendio en t-1 o t-2, utilizamos un if para definir este Eta. Como no tenemos Beneficios en t-2, utilizamos solo t-1.
    if (Beneficios[t-1,i]!=0) {
      Eta<-runif(1,-E,E)
    }else{
      Eta<-runif(1,-E,0)  
    }
    #Luego de definido el if, utilizamos la formula brindada por el articulo para calcular el precio en t. En este caso t-2 es 0 tanto para los precios como los beneficios
    Pi[i]<-Precios[t-1,i]+O*(Beneficios[t-1,i]-0)*sign(Precios[t-1,i]-0)+Eta
  }
  # final del for se calcula la media, min y max. del vector de precios, para luego registrarlo en la fila t correspondiente de la matriz de precios
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  Precios[t,]<-Pistats
  
  #En este punto ya tenemos los precios en t=2, por lo que repetimos la estrategia de mas arriba para el calculo de los beneficios en t=2
  
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
  #Hasta este punto se repite la misma estrategia que esta mas arriba 
  
  #Llegados a esta punto del script, ya tenemos precio y beneficio en t=1 y t=2
  #Ahora podemos definir un while que calcule los periodos restantes
  #Este while calcula tanto el precio como los beneficios en los periodos restantes
  while( t < tmax ){
    #Primero suma 1 a t para pasar al siguiente periodo
    t<-t+1
    #Luego, utiliza un for que itera una vez por cada firma para calcular cada precio y agregarlo al vector Pi
    for (i in 1:N) {
      #Dado que tenemos dos periodos para atras, la condicion cambia a requerir que los beneficios en alguno de los periodos pasados sean diferentes a 0
      #Asi se define como sera la distribucion de Eta
      if (Beneficios[t-1,i]!=0 || Beneficios[t-2,i]!=0) {
        Eta<-runif(1,-E,E)
      }else{
        Eta<-runif(1,-E,0)  
      }
      #Luego se utiliza la ecuacion del articulo para calcular el precio en este periodo, dependiendo de los periodos pasados.
      #Los datos de beneficios se extraen de la matriz de beneficios.
      #Dado que ahora se cuenta con beneficios en t-1 y t-2, se puede utilizar la ecuacion completa
      Pi[i]<-Precios[t-1,i]+O*(Beneficios[t-1,i]-Beneficios[t-2,i])*sign(Precios[t-1,i]-Precios[t-2,i])+Eta
    }
    #Al final del for se calcula la media, min y max. del vector de precios, para luego registrarlo en la fila t correspondiente de la matriz de precios
    Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
    Precios[t,]<-Pistats 
    
    #Teniendo los precios en el t esimo periodo, calculamos los beneficios en dicho periodo
    #El calculo se realiza siguiendo la misma estrategia que en los periodos pasados
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
    #Finalmente se registra el vector de beneficios en el periodo t, el cual sera utilizado para el calculo de P en el periodo t+1 y t+2
    Beneficios[t,]<-Bi
    
  }
  
  #Terminado este while, quedamos con una matriz que registro todos los precios en cada periodo, asi como la media, max. y min
  #Luego le damos nombre a las columnas y las filas
  colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
  rownames(Precios)<-c(paste("t",1:tmax))
  
  #Siguiente lo transformamos en un data frame para graficar mas facilmente
  Precios<-as.data.frame(Precios)
  
  #Nos definimos un vector que es Pedge repetido tmax veces, a efectos de graficarlo
  
  Pedge<-rep(Pedge,tmax)
  
  #Graficamos utilizando las funciones Plot y lines. Se grafica la media, el min, el max., una firma y el Pedge
  #Para definir la escala de Y utilizamos el precio maximo y minimo para todos los periodos
  plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05),main = "Regla basada en beneficios",ylab="Precio",xlab = "Periodo")
  lines(Precios$Max,col=3)
  lines(Precios$Min,col=4)
  lines(Precios$"Firma 1",col=1)
  lines(Pedge,col=6)
  
  #Se le da una leyenda al grafico a efectos de identificar cada variable
  legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
}


#Modelo 3
Graficar3<-function(C,Pa,Qa,N,Sigp,Sigm,Hm,Hp,tmax){
  #Definimos el periodo inicial
  t<-1
  #Definimos M de acuerdo con lo mencionado en el articulo
  M<-(Pa*Qa*N)
  #Definimos P edge de acuerdo con lo mencionado en el articulo
  Pedge<-Pa+C/N
  #Generamos el vector de precios en t=1
  Pi<-runif(N,Pa,2*Pa)
  #Generamos una matriz para guardar los precios de cada periodo
  Precios<-matrix(0,tmax,N+3)
  #Generamos un vector auxiliar de precios, al cual le agregamos la media, el min y el max.
  Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
  #Guardamos este vector auxiliar en la matriz de precios
  Precios[t,]<-Pistats
  
  
  #Generamos un while que itera cada periodo
  while (t<tmax) {
    #Dentro de cada periodo reiniciamos los vectores auxiliares y sumamos 1 a t para avanzar el periodo
    t<-t+1
    #Este valor registra el dinero disponible de los consumidores luego de cada venta
    M1<-M
    #Este vector auxiliar registra que empresa ya vendio en este periodo
    Piaux<-Pi
    #Este vector valdra 0 para las empresas que vendieron, y -Sigm para las que no vendieron
    Vecaux<-rep(-Sigm,N)
    #Utilizamos un while en el cual se actualiza el vector de precios segun la venta de cada empresa
    #El while funciona mientras la siguiente venta sea menor a lo que resta de M
    #La venta se define como el precio minimo en el vector de precios * las cantidades maximas
    while (M1>=min(Piaux)*Qa) {
      #Para incluir el factor aleatorio en la suma, primero generamos un numero aleatorio de la distribucion uniforme entre 0 y 1
      Rand<-runif(1,0,1)
      #Luego de tener dicho numero, lo clasificamos segun las probabilidades deseadas, dado que todos los numeros tienen igual probabilidad de ser elegidos
      #Como todos los numeros entre 0 y 1 tienen la misma probabilidad, si utilizo un intervalo de 0 a 0.20 por ejemplo, la probabilidad de que mi numero este dentro es de un 20%
      #De esta manera, si Rand es menos a la probabilidad de Hm, define el aumento como Sigm
      if (Rand<=Hm) {
        Aum<--Sigm
      }else{
        #Si Rand esta entre Hm+Hp y Hp (Es decir, que el intervalo es de magnitud Hp), el aumento se define como Sigp
        if (Rand<=Hm+Hp) {
          Aum<-Sigp
        }else{
          #En caso contrario (Rand entre Hp y 1), se define el aumento como 0
          Aum<-0
          #Como resumen, esta estrategia parte el intervalo [0,1] en tres secciones, donde el tamaño de cada seccion es la probabilidad de que se de cada aumento.
        }
      }
      #Luego de definido el valor aleatorio, se descuenta de M1 (Valor auxiliar) cada venta que se hace
      #Esto es el registro de la cantidad de dinero que tienen los consumidores que todavia no efectuaron una compra
      M1<-M1-min(Piaux)*Qa
      #Luego, se obtiene la posicion de la empresa que vendio su totalidad, y se suma el aumento definido previamente. La suma se hace en el vector Pi, mientras que la posicion se busca en Paux
      Pi[match(min(Piaux),Piaux)]<-Pi[match(min(Piaux),Piaux)]+Aum
      #Luego se remplaza por 0 la posicion de esa empresa en un vector auxiliar que sirve de guia para saber que empresas no vendieron todo su stock
      Vecaux[match(min(Piaux),Piaux)]<-0
      #Finalmente reemplaza en el vector auxiliar de precios esa posicion por el precio inicial + M. Esto no cambia el valor en Pi.
      #El reemplazo se hace sumando M, a efectos de que sin importar las unidades que se manejen (miles, millones), esta empresa nunca volvera a ser seleccionada como la que tiene el precio mas bajo
      #En el caso de que los precios iniciales sean muy elevados Ninguna empresa venderia, el while no se ejecutara y todas las empresas bajaran sus precios en el siguiente periodo
      #En el caso de que los precios iniciales sean muy bajos, y luego de que este while se ejecutara N veces, seria matematicamente imposible que se ejecutara N+1 veces dado que sumamos M a cada precio
      #De esta manera, todas las empresas venderan su stock, y todas aumentaran su precio. Pero ninguna aumentara dos veces su precio en un mismo periodo
      Piaux[match(min(Piaux),Piaux)]<-Piaux[match(min(Piaux),Piaux)]+M
    }
    #Finalmente se suma el vector auxiliar al vector de precios (Este vector ya tiene los aumentos de las empresas que vendieron todo su stock)
    #Este vector auxiliar tiene un 0 para las empresas que vendieron todo su stock (Por lo que sus precios no cambian) y el valor de sigma menos para las que no lograron vender la totalidad de su stock
    Pi<-Pi+Vecaux
    
    #Al final del while se calcula la media, min y max. del vector de precios, para luego registrarlo en la fila t correspondiente de la matriz de precios
    Pistats<-c(Pi,mean(Pi),max(Pi),min(Pi))
    Precios[t,]<-Pistats
  }
  
  #Terminado el while, terminamos con una matriz que registro todos los precios en cada periodo, asi como la media, max. y min
  #Luego le damos nombre a las columnas y las filas
  colnames(Precios)<-c(paste("Firma",1:N),"Media","Max","Min")
  rownames(Precios)<-c(paste("t",1:tmax))
  
  #Siguiente lo transformamos en un data frame para graficar mas facilmente
  Precios<-as.data.frame(Precios)
  
  #Nos definimos un vector que es Pedge repetido tmax veces, a efectos de graficarlo
  
  Pedge<-rep(Pedge,tmax)
  
  #Graficamos utilizando las funciones Plot y lines. Se grafica la media, el min, el max., una firma y el Pedge
  #Para definir la escala de Y utilizamos el precio maximo y minimo para todos los periodos
  plot(Precios$Media,type="l",col=2,ylim = c(min(Precios$Min)*0.95, max(Precios$Max)*1.05),main = "Regla basada en ventas con probabilidades",ylab="Precio",xlab = "Periodo")
  lines(Precios$Max,col=3)
  lines(Precios$Min,col=4)
  lines(Precios$"Firma 1",col=1)
  lines(Pedge,col=6)
  
  #Se le da una leyenda al grafico a efectos de identificar cada variable
  legend("topright",inset = 0, legend = c("Firma 1","Media","Max","Min","P Edge"),pch = 15, col=c(1,2,3,4,6))
}

#A continuacion planteamos algunos casos particulares de los modelos

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


#Simulaciones para analisis de volatilidad

#En esta seccion nos planteamos dos funciones que nos permiten analizar la volatilidad del comportamiento de los precios

#Simulacion para regla basada en ventas
#Primero nos definimos una funcion que nos permitira correr mas facilmente las simulaciones
Simulacion1<-function(sim,Pa,Qa,C,N,tmax,Sigms,Sigps){
  #Definimos una matriz que tendra 3 columnas, dos para guardar los parametros y una para guardar los resultados de cada simulacion. Tendra tantas filas como simulaciones hagamos
  par<-matrix(0,ncol=3,nrow=sim)
  #Luego nos definimos un for que iterara cada simulacion. El mismo se define en base a sim, que es el numero maximo de simulaciones
  for (i in 1:sim){
    #Dentro de este for seleccionamos valores aleatorios de los parametros para cada simulacion. Los mismos van de 0 a un valor maximo que se define en la funcion. De esta manera se puede controlar el alcance de las simulaciones
    par[i,1]<-runif(1,0,Sigms)                      
    par[i,2]<-runif(1,0,Sigps)
    #Se guardan estos valores aleatorios como Sigm y Sigp para ser utilizados en el modelo
    Sigm<-par[i,1]
    Sigp<-par[i,2]
    #A partir de este punto se utiliza el mismo modelo visto mas arriba. Para comentarios sobre su funcionamiento, ir a la linea 18
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
    #Llegado a este punto del script, ya quedo calculada la matriz de precios del modelo
    #Tenemos un data frame con los precios en todos los periodos para estos parametros aleatorios
    #Generamos un vector de N veces 0 para guardar la varianza de cada empresa
    var=rep(0,N)
    #Utilizamos un for que calcula la varianza de todos los periodos para cada empresa, y los guarda en var
    for (j in 1:N) {
      var[j]<-var(Precios[,j])
    }
    #Luego, nos guardamos en la tercer columna de la matriz par la media de estas varianzas dividido por el promedio de la media de los precios
    par[i,3]<-mean(var)/mean(Precios$Media) #Tomamos el promedio de la varianza de precios de todas las empresas dividido por el promedio de medias, ya que consideramos que es una buena medida para la volatilidad, sin importar la magnitud de los precios 
  }
  #Llegado a este punto se ejecutaron todas las simulaciones, por lo que queda es graficar
  #Para esto utilizamos la formula plot
  plot(par[,2],par[,1],main="Comportamiento de la volatilidad de Precios", #Incluimos el nombre
       col=ifelse(par[,3] > quantile(par[,3],0.85)   #Escala fue definida como el percentil 85 de las varianzas estandarizadas por la media
       ,"blue",ifelse(par[,3] > quantile(par[,3],0.4) #Escala fue definida como el percentil 40 de las varianzas estandarizadas por la media
       #Estos percentiles para la division fueron definidos de esta manera por nosotros, porque entendemos que es una buena division, pero se podrian cambiar dependiendo de los criterios buscados
       ,"green","red")),
       pch=18,ylab="Sigm",xlab = "Sigp",cex=0.7)
  legend(x="topleft",c("Alta volatilidad", "Volatilidad moderada", 
       "Baja volatilidad"),col=c("blue","green","red"),
       cex=0.8,pch=18)
}

#Finalizada la funcion de simulacion podemos utilizar la siguiente expresion para ejecutar
Simulacion1(sim=10000,Pa=1,Qa=3,C=0.75,N=10,tmax=50,Sigms=1,Sigps=1)


#Simulacion regla basada en beneficios
#Primero nos definimos una funcion que nos permitira correr mas facilmente las simulaciones
Simulacion2<-function(sim,Pa,Qa,C,N,tmax,Os,Es){
  #Definimos una matriz que tendra 3 columnas, dos para guardar los parametros y una para guardar los resultados de cada simulacion. Tendra tantas filas como simulaciones hagamos
  par=matrix(0,ncol=3,nrow=sim)          
  #Luego nos definimos un for que iterara cada simulacion. El mismo se define en base a sim, que es el numero maximo de simulaciones
  for (z in 1:sim){
    #Dentro de este for seleccionamos valores aleatorios de los parametros para cada simulacion. Los mismos van de 0 a un valor maximo que se define en la funcion. De esta manera se puede controlar el alcance de las simulaciones
    par[z,1]=runif(1,0,Os)                      
    par[z,2]=runif(1,0,Es)                     
    #Se guardan estos valores aleatorios como O y E para ser utilizados en el modelo
    O=par[z,1]
    E=par[z,2]
    #A partir de este punto se utiliza el mismo modelo visto mas arriba. Para comentarios sobre su funcionamiento, ir a la linea 99
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
    #Llegado a este punto del script, ya quedo calculada la matriz de precios del modelo
    #Tenemos un data frame con los precios en todos los periodos para estos parametros aleatorios
    #Generamos un vector de N veces 0 para guardar la varianza de cada empresa
    var=rep(0,N)
    #Utilizamos un for que calcula la varianza de todos los periodos para cada empresa, y los guarda en var
    for (j in 1:N) {
      var[j]=var(Precios[,j])
    }
    #Luego, nos guardamos en la tercer columna de la matriz par la media de estas varianzas dividido por el promedio de la media de los precios
    par[z,3]=mean(var)/mean(Precios$Media) #Tomamos el promedio de la varianza de precios de todas las empresas dividido por el promedio de medias, ya que consideramos que es una buena medida para la volatilidad, sin importar la magnitud de los precios 
  }
  #Llegado a este punto se ejecutaron todas las simulaciones, por lo que queda es graficar
  #Para esto utilizamos la formula plot
  plot(par[,2],par[,1],main="Comportamiento de la volatilidad de Precios",#Incluimos el nombre
       col=ifelse(par[,3] > quantile(par[,3],0.85)   #Escala fue definida como el percentil 85 de las varianzas estandarizadas por la media
       ,"blue",ifelse(par[,3] > quantile(par[,3],0.4) #Escala fue definida como el percentil 40 de las varianzas estandarizadas por la media
       #Estos percentiles para la division fueron definidos de esta manera por nosotros, porque entendemos que es una buena division, pero se podrian cambiar dependiendo de los criterios buscados
       ,"green","red")),
       pch=18,ylab="O",xlab = "E",cex=0.7)
  legend(x="topleft",c("Alta volatilidad", "Volatilidad moderada", 
       "Baja volatilidad"),col=c("blue","green","red"),
       cex=0.8,pch=18)
}

#Finalizada la funcion de simulacion podemos utilizar la siguiente expresion para ejecutar
Simulacion2(sim=10000,Pa=1,Qa=3,C=3,N=10,tmax=50,Os=1,Es=0.3)




