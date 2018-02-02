#the pretation for route including dem,slope,step,area 
setwd("F:\R-program\R-BW\BlueWater")
gn=311        #the number of grids
drn=21       #the rows number of FLOWDRIC 
dcn=35       #the clows number of FLOWDRIC 
cro=13       #the order row of hydro station
cco=32       #the order clow of hydro station
OUT="input\\acculate.txt"
check="output\\check.txt"
FLOWDRIC=matrix(scan("input\\cuntanDir.txt"),drn,dcn,byrow=TRUE)  #the direction of flow, in order to get the UHS and UHB of different grids
GRIDID=matrix(scan("input\\cuntanID.txt"),drn,dcn,byrow=TRUE)  #the ID of grid
FTGID=matrix(0,gn,2)
ACLT=diag(gn)  #acculate 
##find the father grid
#############make sure that the oder is same with runoffname########
i=1
for(m in 1:dcn){
  for(l in 1:drn){
    #find the way to station	
    judge=FLOWDRIC[l,m]
    if((judge!=-9999)){
      FTGID[i,1]=GRIDID[l,m]
      f=FLOWDRIC[l,m]
      if(f==1) {
        j=l-1
        k=m
      }
      if(f==2) {
        j=l-1
        k=m+1
      }
      if(f==3) {
        k=m+1
        j=l
      }
      if(f==4) {
        j=l+1
        k=m+1
      }
      if(f==5) {
        j=l+1
        k=m
      }
      if(f==6) {
        j=l+1
        k=m-1
      }
      if(f==7) {
        k=m-1
        j=l
      }
      if(f==8) {
        j=l-1
        k=m-1
      }
      FTGID[i,2]=GRIDID[j,k]
      if(l==cro&m==cco) FTGID[i,2]=0
      i=i+1
    }
  }
}
for(k in 1:gn){
  l=k
  while(FTGID[l,2]!=0) {
    tem1=FTGID[l,1]
    tem2=FTGID[l,2]
    ACLT[k,tem2]=1
    l=tem2
  }
}
write.table(ACLT,OUT,row.names = F,col.names = F)
write.table(FTGID,check,row.names = F,col.names = F)
