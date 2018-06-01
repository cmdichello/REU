H<-1; #arbitrary 
S1<-40.9; #PW
A<--49.7; #PW
B<-0.29; #PW 
S2<-24.1; #PW
alpha<-0.29; 
F0<-3; #PW
gamma1<-0.035;
gamma2<-0.002852;
Tice<-271.15;

dt<-1;
T1<-300; 
T2<-288; 
t<-0;
n<-1000; #arbritrary
tA =200

eps1 <- function (t) ifelse(t < tA, 0, 0) #forcing function 
eps2 <- function (t) ifelse(t < tA, 0, 0) #forcing function 

phiice <- function (T1, T2) pi/6 +(pi/6)*(T1-Tice)/(T1-T2)
a <- function (T1, T2) 1-sin(phiice(T1,T2))
CCeq <- function (T1) 6.11*exp(17.23*((T1-273.25)/(T1-35.86)))
AHT <- function (T1, T2) F0+gamma1*(T1-T2)+gamma2*CCeq(T1)*(T1-T2)

dT1 <- function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1(t))/H
dT2 <- function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHT(T1, T2)-(A+B*T2)+eps2(t))/H


list1<- vector("list", n)
list2<- vector("list",n)
listt<- vector ("list",n)

list1[[1]] <- T1
list2[[1]] <- T2
listt[[1]] <- t

for (k in 1: n){
  Var1=T1+dT1(T1,T2,t)*dt
  Var2=T2+dT2(T1,T2,t)*dt
  t=t+dt
  T1=Var1
  T2=Var2
  list1[[k+1]] <- T1
  list2[[k+1]] <- T2
  listt[[k+1]] <- t
}

plot(listt, list1, ylim = c(240,300), type="l", col="red")
lines(listt, list2, col="blue")
title("Graph 1")

T1diff = list1[[n]]-list1[[tA]]
T2diff = list2[[n]]-list2[[tA]]
T1eq = list1[[n]]
T2eq = list2[[n]]
aeq = a(T1eq,T2eq)
phiiceeq = phiice(T1eq,T2eq)
phiiceeqdeg = phiiceeq*180/pi
AHTeq = AHT(T1eq,T2eq)
dT1eq= function(T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1(t))/H
dT2eq=function (T1,T2,t) S2*(1-2*alpha*aeq+AHTeq-(A+B*T2)+eps2(t))/H



#loop 2 with equilibrium no forcing
list1eq= vector ("list", n)
list2eq= vector ("list", n)

list1eq[[1]]=300
list2eq[[1]]=288

T1=300
T2=288
t=0

eps1eq <- function (t) ifelse(t < tA ,0, 0) #forcing function
eps2eq <- function (t) ifelse(t < tA ,0, 0) #forcing function
dT1eq= function(T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1eq(t))/H
dT2eq=function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHTeq- (A+B*T2)+eps2eq(t))/H

for (k in 1:n){
  Var1=T1+dT1eq(T1,T2,t)*dt
  Var2=T2+dT2eq(T1,T2,t)*dt
  t=t+dt
  T1=Var1
  T2=Var2
  list1eq[[k+1]]= T1
  list2eq[[k+1]]= T2
}

plot(listt, list1eq, ylim=c(240,300), type="l", col="red")
lines(listt, list2eq, col="blue")
title("equilibrum plot")
T1diffeq = list1[[n]]-list1[[tA]]
T2diffeq = list2[[n]]-list2[[tA]]



#forcing feedback not active- base case (dT1eq and dT2eq) (esp1=1, eps2=0)
list1eps1= vector("list",n)
list2eps0=vector("list", n)

list1eps1[[1]]=300
list2eps0[[1]]=288

T1=300
T2=288
t=0

 eps1_1 = function (t) ifelse(t < tA, 0, 1) #forcing function 
 eps2_0 = function (t) ifelse(t < tA, 0, 0) #forcing function 
 dT1_eps1 = function (T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1_1(t))/H
 dT2_eps0 = function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHTeq-(A+B*T2)+eps2_0(t))/H #using AHT function and a function
 

 for (k in 1: n){
   Var1=T1+dT1_eps1(T1,T2,t)*dt
   Var2=T2+dT2_eps0(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps1[[k+1]] = T1
   list2eps0[[k+1]] = T2
   listt[[k]] = t
 }
 plot(listt, list1eps1, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps0, col="blue")
 title( "base case forcing, eps1=1, eps2=0")
 
 T1diffbaseeps1 = list1eps1[[n]]-list1eps1[[tA]]
 T2diffbaseeps0 = list2eps0[[n]]-list2eps0[[tA]]

 #forcing feedback not active-base case (dT1eq and dT2eq) (esp1=0, eps2=1)
 list1eps0= vector("list",n)
 list2eps1=vector("list", n)
 T1=300
 T2=288
 t=0
 
 list1eps0[[1]]=T1
 list2eps1[[1]]=T2
 
 eps1_0 = function (t) ifelse(t < tA, 0, 0) #forcing function 
 eps2_1 = function (t) ifelse(t < tA, 0, 1) #forcing function 
 dT1_eps0 = function (T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1_0(t))/H
 dT2_eps1 = function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHTeq-(A+B*T2)+eps2_1(t))/H 
 
 for (k in 1: n){
   Var1=T1+dT1_eps0(T1,T2,t)*dt
   Var2=T2+dT2_eps1(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps0[[k+1]] = T1
   list2eps1[[k+1]] = T2
   listt[[k]] = t
 }
 plot(listt, list1eps0, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps1, col="blue")
 title( "base case forcing, eps1=0, eps2=1")
 
 T1diffbaseeps0 = list1eps0[[n]]-list1eps0[[tA]]
 T2diffbaseeps1 = list2eps1[[n]]-list2eps1[[tA]]
 
 #Forcing feedback all active (dT1, dT2) (eps1=1, eps2=0)
 list1eps1a=vector("list",n)
 list2eps0a=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps1a[[1]]=T1
 list2eps0a[[1]]=T2
 
 #eps1_1 and eps2_0 are defined above 
 dT1_eps1a = function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1_1(t))/H
 dT2_eps0a = function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHT(T1,T2)-(A+B*T2)+eps2_0(t))/H 
 for (k in 1: n){
   Var1=T1+dT1_eps1(T1,T2,t)*dt
   Var2=T2+dT2_eps0(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps1a[[k+1]] = T1
   list2eps0a[[k+1]] = T2
   listt[[k]] = t
 }
 plot(listt, list1eps1a, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps0a, col="blue")
 title( "all active forcing, eps1=1, eps2=0")

 T1diff1a = list1eps1a[[n]]-list1eps1a[[tA]]
 T2diff0a = list2eps0a[[n]]-list2eps0a[[tA]]
 
 #Forcing feedback all active (dT1, dT2) (eps1=0, eps2=1)
 list1eps0a=vector("list",n)
 list2eps1a=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps0a[[1]]=T1
 list2eps1a[[1]]=T2
 
 #eps1_0 and eps2_1 defined above 
 dT1_eps0a = function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1_0(t))/H
 dT2_eps1a = function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHT(T1,T2)-(A+B*T2)+eps2_1(t))/H 
 for (k in 1: n){
   Var1=T1+dT1_eps0a(T1,T2,t)*dt
   Var2=T2+dT2_eps1a(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps0a[[k+1]] = T1
   list2eps1a[[k+1]] = T2
   listt[[k]] = t
  } 
 plot(listt, list1eps0a, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps1a, col="blue")
 title( "all active forcing, eps1=0, eps2=1") 

 T1diffeps0a = list1eps0a[[n]]-list1eps0a[[tA]]
 T2diffeps1a = list2eps1a[[n]]-list2eps1a[[tA]]
 
 #Top down in reference to a (ice albedo) use AHT(T1,T2) and aeq (eps1=1, eps2=0)
 list1eps1t=vector("list",n)
 list2eps0t=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps1t[[1]]=T1
 list2eps0t[[1]]=T2
 
 #eps1_1 and eps2_0 defined above 
 dT1_eps1t = function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1_1(t))/H
 dT2_eps0t = function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHT(T1,T2)-(A+B*T2)+eps2_0(t))/H 
 for (k in 1: n){
   Var1=T1+dT1_eps1t(T1,T2,t)*dt
   Var2=T2+dT2_eps0t(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps1t[[k+1]] = T1
   list2eps0t[[k+1]] = T2
   listt[[k]] = t
 } 
 plot(listt, list1eps1t, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps0t, col="blue")
 title( "AHT active forcing, a is not active, eps1=1, eps2=0")  
 
 T1diffeps1t = list1eps1t[[n]]-list1eps1t[[tA]]
 T2diffeps0t = list2eps0t[[n]]-list2eps0t[[tA]]
 
 #Top down in reference to a (ice albedo) use AHT(T1,T2) and aeq (eps1=0, eps2=1)
 list1eps0t=vector("list",n)
 list2eps1t=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps0t[[1]]=T1
 list2eps1t[[1]]=T2
 
 #eps1_0 and eps2_1 defined above 
 dT1_eps0t = function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1_0(t))/H
 dT2_eps1t = function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHT(T1,T2)-(A+B*T2)+eps2_1(t))/H
 for (k in 1: n){
   Var1=T1+dT1_eps0t(T1,T2,t)*dt
   Var2=T2+dT2_eps1t(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps0t[[k+1]] = T1
   list2eps1t[[k+1]] = T2
   listt[[k]] = t
 }  
 plot(listt, list1eps0t, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps1t, col="blue")
 title( "AHT active forcing, a is not active, eps1=0, eps2=1")  

 T1diffeps0t = list1eps0t[[n]]-list1eps0t[[tA]]
 T2diffeps1t = list2eps1t[[n]]-list2eps1t[[tA]]
 
 #Bottom up in reference to a (ice albedo) use AHTeq and a(T1,T2) (eps1=1, eps2=0) 
 list1eps1b=vector("list",n)
 list2eps0b=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps1b[[1]]=T1
 list2eps0b[[1]]=T2
 
 #eps1_1 and eps2_0 defined above 
 dT1_eps1b = function (T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1_1(t))/H
 dT2_eps0b = function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHTeq-(A+B*T2)+eps2_0(t))/H 
 for (k in 1: n){
   Var1=T1+dT1_eps1b(T1,T2,t)*dt
   Var2=T2+dT2_eps0b(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps1b[[k+1]] = T1
   list2eps0b[[k+1]] = T2
   listt[[k]] = t
 }
 plot(listt, list1eps1b, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps0b, col="blue")
 title( "AHT not active, a is active, eps1=1, eps2=0")  

 T1diffeps1b = list1eps1b[[n]]-list1eps1b[[tA]]
 T2diffeps0b = list2eps0b[[n]]-list2eps0b[[tA]]
 
 #Bottom up in reference to a (ice albedo) use AHTeq and a(T1,T2) (eps1=0, eps2=1)
 list1eps0b=vector("list",n)
 list2eps1b=vector("list",n)
 T1=300
 T2=288
 t=0
 
 list1eps0b[[1]]=T1
 list2eps1b[[1]]=T2
 
 #eps1_0 and eps2_1 defined above 
 dT1_eps0b = function (T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1_0(t))/H
 dT2_eps1b = function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHTeq-(A+B*T2)+eps2_1(t))/H
 for (k in 1: n){
   Var1=T1+dT1_eps0b(T1,T2,t)*dt
   Var2=T2+dT2_eps1b(T1,T2,t)*dt
   t=t+dt
   T1=Var1
   T2=Var2
   list1eps0b[[k+1]] = T1
   list2eps1b[[k+1]] = T2
   listt[[k]] = t
 }
 plot(listt, list1eps0b, ylim = c(240,300), type="l", col="red")
 lines(listt, list2eps1b, col="blue")
 title( "AHT not active, a is active, eps1=0, eps2=1")
  
 T1diffeps0b = list1eps0b[[n]]-list1eps0b[[tA]]
 T2diffeps1b = list2eps1b[[n]]-list2eps1b[[tA]]
 
 
