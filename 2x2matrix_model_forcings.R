
H=1; #arbitrary 
S1=40.9; #PW
A=-49.7; #PW
B=0.29; #PW 
S2=24.1; #PW
alpha=0.29; 
F0=3; #PW
gamma1=0.035;
gamma2=0.002852;
Tice=271.15;

dt=1;
T1=300; 
T2=288; 
t<-0;
n<-1000; #arbritrary
tA =200


dT0= vector("list")
dTAlpha=vector("list")

for (theta in seq(from=0, to =1, by=.1)){
  epsilon= c(theta, 1-theta)
  
#############################################################Code taken from model before#############################################
  
  eps1 = function (t) ifelse(t < tA, 0, 0) #forcing function 
  eps2 = function (t) ifelse(t < tA, 0, 0) #forcing function 
  
  phiice = function (T1, T2) pi/6 +(pi/6)*(T1-Tice)/(T1-T2)
  a = function (T1, T2) 1-sin(phiice(T1,T2))
  CCeq = function (T1) 6.11*exp(17.23*((T1-273.25)/(T1-35.86)))
  AHT = function (T1, T2) F0+gamma1*(T1-T2)+gamma2*CCeq(T1)*(T1-T2)
  
  dT1 = function (T1,T2,t) (S1-AHT(T1,T2)-(A+B*T1)+eps1(t))/H
  dT2 = function (T1,T2,t) (S2*(1-2*alpha*a(T1,T2))+AHT(T1, T2)-(A+B*T2)+eps2(t))/H
  
  # aeq = a(T1eq,T2eq)
  # phiiceeq = phiice(T1eq,T2eq)
  # phiiceeqdeg = phiiceeq*180/pi
  # AHTeq = AHT(T1eq,T2eq)
  ###dT1eq= function(T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1(t))/H
  ###dT2eq=function (T1,T2,t) S2*(1-2*alpha*aeq+AHTeq-(A+B*T2)+eps2(t))/H
  
  
  eps1 = function (t) ifelse(t < tA, 0, -.9 )
  eps2 = function (t) ifelse(t < tA, 0, .1)
  
  
  #forcing feedback not active- base case (dT1eq and dT2eq) (esp1=1, eps2=0) 
  list1eps1= vector("list",n)
  list2eps0=vector("list", n)
  
  list1eps1[[1]]=300
  list2eps0[[1]]=288
  
  T1=300
  T2=288
  t=0
  
  eps1 = function (t) ifelse(t < tA, 0, -.9 )
  eps2 = function (t) ifelse(t < tA, 0, .1)
  dT1_eps1 = function (T1,T2,t) (S1-AHTeq-(A+B*T1)+eps1(t))/H
  dT2_eps0 = function (T1,T2,t) (S2*(1-2*alpha*aeq)+AHTeq-(A+B*T2)+eps2(t))/H #using AHT function and a function

  
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
  
  
  
}