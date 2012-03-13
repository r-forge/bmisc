rivard <- function(data, pred=FALSE, K=2, plus.gr=FALSE)
{
    n.age=dim(data)[1]
    n.an=dim(data)[2]
    pds1=log(data)
    pds2=pds1
    
    
    if(!plus.gr){
        for(i in 2:n.age){
            for(j in 2:n.an){
                pds2[i,j]=(pds1[i,j]+pds1[i-1,j-1])/2
            }
        }
        
        for(i in 1:(n.an-1)){
            pds2[1,i]=2*pds1[1,i]-pds2[2,i+1]
        }
        
        for(i in 1:(n.age-1)){
            pds2[i,1]=2*pds1[i,1]-pds2[i+1,2]
        }
        
        pds2[n.age,1]=(pds1[n.age,1]+pds1[n.age-1,1])/2
        
        pds2[1,n.an]=2*pds1[1,n.an]-pds2[2,n.an]
        
        pds3=exp(pds2)
        
        if(pred){
            if(K<=1)stop("K can not be smaller than 1. See help('rivard').")
            if(K>n.an)stop("K can not be greater than the number of years in\n  the data.frame submitted to this function. See help('rivard').")
            last.name="last.year+1"
            pds3$pred.next=rowMeans(pds3[,(n.an-K+1):n.an])
            names(pds3)[dim(pds3)[2]]=last.name
        }
        
    }else{
        for(i in 2:n.age-1){
            for(j in 2:n.an){
                pds2[i,j]=(pds1[i,j]+pds1[i-1,j-1])/2
            }
        }
        
        for(i in 1:(n.an-1)){
            pds2[1,i]=2*pds1[1,i]-pds2[2,i+1]
        }
        
        for(i in 1:(n.age-2)){
            pds2[i,1]=2*pds1[i,1]-pds2[i+1,2]
        }
        
        pds2[n.age-1,1]=(pds1[n.age-1,1]+pds1[n.age-2,1])/2
        
        pds2[1,n.an]=2*pds1[1,n.an]-pds2[2,n.an]
        
        for(i in 1:n.an){
            pds2[n.age,i]=pds1[n.age,i]
        }
        
        pds3=exp(pds2)
        if(pred){
            if(K<=1)stop("K can not be smaller than 1. See help('rivard').")
            if(K>n.an)stop("K can not be greater than the number of years in\n  the data.frame submitted to this function. See help('rivard').")
            last.name=as.character((as.numeric(names(pds3)[n.an])+1))
            pds3$pred.next=rowMeans(pds3[,(n.an-K+1):n.an])
            names(pds3)[dim(pds3)[2]]=last.name
        }
        
    }
    return(pds3)
}


