#make a triangle then use random gen to pick a point then pick a random corrner and make a new point halfway between the random point and the 
#corrner of the triangle 


V1= vector()
V2= vector()
V3= vector()

V1=c(0, 1)
V2=c(1,0)
V3=c(1,1)
xvector= c(0,1,.5,0)
yvector=c(0,0,1,0)

plot(V1, V2,V3,V1, type="o")

for (k in 1:10){
  Vnew= vector()
randpoint= runif(2)
if V1<= randpoint <= V3 
vertex= floor(runif(1, min=1, max=4))
Vnew=c()
}