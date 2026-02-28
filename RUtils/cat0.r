#===+===
#===+===
#   This function is similar to paste0, but for printing stuff on screen.
#---~---
cat0 <<- function(...,file="",fill=FALSE,labels=NULL,append=FALSE){
   cat(...,"\n",file=file,fill=fill,sep="",labels=labels,append=append)
}#end cat0
#---~---
