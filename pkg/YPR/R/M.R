m=function(k,t0,data){
   t= data@base$age+3
   sel=which(data@base$age<16)
   sel2=which(data@base$age>=16)
   
   mt=k/(1-exp(-k*(t-t0)))
   
   #tm=-(1/k)*log(1-exp(k*t0))+t0
   tm=16
   a0=(1/(max(data@base$age+3)-t))
   a1=log((exp(k)*max(data@base$age+3)-exp(k)*t0)/(exp(k)*t-exp(k)*t0))

   mt2=a0*a1
   
   mmt=data@base
   mmt$mt=c(mt[sel],mt2[sel2])

   mmt
   }
   
   plot(mt~age,data=res,ylim=c(0,1))
   mean(res$mt)
   
   27.0 2153.4114 129.4215365