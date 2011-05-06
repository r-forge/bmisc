r.colors <- function (file=NULL) UseMethod("r.colors")

r.colors.default <- function(file=NULL) {
  if(is.null(file)){
    if (.Platform$OS.type== "unix") {file=file.path("~/")}
    if (.Platform$OS.type=="windows") {file=file.path("C:/")} }
    z=colors()
    num=c(seq(0,657,12),657)
    num2=num+1
    nn=sort.vdf(c(num,num2))
    nn=nn[nn!=0 & nn!= 658]
   ss= seq(1,110,2)
   if(!is.null(file) & substr(file,nchar(file),nchar(file))=="/"){
        pdf(file=paste(file,"R_colors.pdf",sep=""),width=11, height=8.5)
   }else{pdf(file=paste(file,"R_colors.pdf",sep="/"),width=11, height=8.5) }
   
  try(
    for(j in ss){
         n=nn[j+1]-nn[j]+1
        pie(rep(1,n), col=z[nn[j]:nn[j+1]], labels=z[nn[j]:nn[j+1]])

    } ,silent=T)
  graphics.off()
}