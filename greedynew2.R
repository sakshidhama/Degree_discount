library(igraph)
dd <- read.table("facebook_combined.txt")
gg <- graph.data.frame(dd, directed=FALSE)
#plot(gg)
mylist<-vector()
mylist<-V(gg)
#print(V(gg))
#V(gg)$influence<-FALSE
#V(gg)$activ_prob<-runif(length(V(gg)))
V(gg)$tv=vector()#initialise tv to 0
V(gg)$deg=vector()
S=list()
i<-1
while (i<=length(V(gg)))
{
  #ddv[[i]]<-degree(gg,V(gg)[i],mode="all")
  V(gg)$tv<-0
  V(gg)$deg[i]<-degree(gg,V(gg)[i],mode="all")
  i<-i+1
}
h<-1
while(h<=3)#7
{
t=vector()  
maxval<-which.max(mylist$deg)
m<-mylist[maxval]#8
mylist<-mylist[-which(mylist==m)]#updating V/S:mylist stores V/S values
s<-c(s,m)#9
t<-neighbors(gg,m)#selecting neighbors : 10
i<-1
while(i<=length(t))#10
{
  if(t[i]%in%mylist)
  {
    V(gg)[t[i]]$tv<-V(gg)[t[i]]$tv+1#11
    V(gg)[t[i]]$deg<-V(gg)[t[i]]$deg-2*(V(gg)[t[i]]$tv)-(V(gg)[t[i]]$deg-V(gg)[t[i]]$tv)*((V(gg)[t[i]]$tv)*0.01) #12
  }
  i<-i+1
  }
h<-h+1
}
print("final value of s :")
print(s)
s<-NULL 



