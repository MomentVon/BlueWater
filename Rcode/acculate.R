setwd("F:\\R-program\\R-BW\\BlueWater")
#pn=13      #the number of params

#inpute and base settings
#PARAM=matrix(scan("param.prn"),pn,1,byrow=TRUE)  #the params of model
dn=3287        #the number of days
gn=311        #the number of grids
subuhsn=4    #how many time step have on one day 
subuhbn=2
drn=21       #the rows number of FLOWDRIC 
dcn=35       #the clows number of FLOWDRIC 
cro=13       #the order row of hydro station
cco=32       #the order clow of hydro station
so=6       #the clow order of surfaceflow from VIC
bo=7      #the clow order of baseflow from VIC
runrn=3287    #the rows number of RUNOFF files
runcn=25    #the clows number of RUNOFF files
check="output\\check.txt"
FLOWDRIC=matrix(scan("input\\cuntanDir.txt"),drn,dcn,byrow=TRUE)  #the direction of flow, in order to get the UHS and UHB of different grids
GRIDID=matrix(scan("input\\cuntanID.txt"),drn,dcn,byrow=TRUE)  #the ID of grid
FTGID=matrix(0,gn,2)
ACLT=diag(gn)  #acculate 
BWname="output\\bw.txt"
RUNOFF=matrix(,dn,gn)       #the surfaceflow from VIC 
DSCG=matrix(,dn,gn)
Qall=matrix(,dn,1)
Rb=matrix(,dn,1)
BW=matrix(,dn,gn)  #daily blue water resource
RUNOFFNAME=c()
RUNOFFNAMEE<-read.table("input\\fluxesname.txt",sep=" ")    #the files names of runoff from VIC
for(i in 1:gn)RUNOFFNAME[i]=as.character(RUNOFFNAMEE[i,1])    #the character form in R about files names of runoff from VIC
setwd("F:\\Cuntan2017\\vic_result")
#get the information of surfaceflow and baseflow
for(j in 1:gn){
  RUNTEM=read.table(RUNOFFNAME[j],sep=" ")
  RUNOFF[,j]=RUNTEM[,so]+RUNTEM[,bo]
}
j=0
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

Rb=rowSums(RUNOFF,na.rm = FALSE, dims = 1)
DSCG=RUNOFF%*%ACLT
Qall=rowSums(DSCG,na.rm = FALSE, dims = 1)
for(i in 1:dn){
  BW[i,]=DSCG[i,]*Rb[i]/Qall[i]
}


setwd("F:\\R-program\\R-BW\\BlueWater")
write.table(BW,BWname,row.names = F,col.names = F)
write.table(FTGID,check,row.names = F,col.names = F)

