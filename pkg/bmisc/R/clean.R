clean <- function (data,...) UseMethod("clean")

clean.default = function (data,  col.start =1,  min.val=NULL) {

	names = names(data)
	n.col=length(names)
	deb.col= 1:(col.start-1)

	test=data < min.val
	col= col.start:n.col
	
	subset1= data [, 1 : col.start]
	
	subset2= data [, col.start : n.col]
	
	names.sub2=names(subset2)
	test = subset2 >= min.val
	res=colSums(test,na.rm=T)
	
	if(col.start==1){	res2= subset2[, res>0]}
	
	if(col.start!=1){	res2= cbind(data[,deb.col],subset2[,  res>0])
						names(res2)=c(names(data)[deb.col], names(subset2)[res>0])
						}
	
	return(res2)	
}
