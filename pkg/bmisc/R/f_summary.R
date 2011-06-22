s.var <- function(data){

    if('pds_tot..g.'%in% names(data)){
      var=c('age','longueur..mm.','pds_tot..g.')
    }else{
       var=c('age','longueur..mm.','pds_tot..kg.')
    }

    for(i in var){

        form=formula(paste(i,'~age+year',sep=""))

        all=data.frame()
        for( j in unique(data$sex)){

            dat=data[data$age!=99 & !is.na(data$age) & data$sex==j,]
            ages=sort.vdf(unique(dat$age))
            years=sort.vdf(unique(dat$year))

            key=data.frame(age=rep(ages,n(years)),year=rep(years,each=n(ages)))
            if(i=='age'){
              ay=summaryBy(form, data=dat, FUN=n)
            }else{
              ay=summaryBy(form, data=dat, FUN=mean, na.rm=T)
            }
            merg=merge(key,ay,all=T)
            merg$sex=j
            all=rbind(all,merg)
        }

      assign(paste('all',i,sep='.'),all)
      }
    if('pds_tot..g.'%in% names(data)){
      summary.all=list(all.age, all.longueur..mm., all.pds_tot..g.)
    }else{
      summary.all=list(all.age, all.longueur..mm., all.pds_tot..kg.)
    }
    names(summary.all)=var
    summary.all
}