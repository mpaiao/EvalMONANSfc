#===+===
#===+===
#   This function summarises the vector with the most appropriate function depending on 
# the variable type.
#---~---
summarise_by_type <<- function(x,na.rm=FALSE){
   #---~---
   #   We decide how to summarise based on the variable type. However, we first check for
   # edge cases in which eall values are undefined.
   #---~---
   if (all(is.na(x))){
      ans = x[1L]
   }else if ( is.numeric(x) && ( ! is.integer(x) ) ){
      #--- Numeric, take the average
      ans = mean(x=x,na.rm=na.rm)
      #---~---
   }else if ( is.integer(x) ){
      #--- Integer, take the median
      ans = median(x=x,na.rm=na.rm)
      #---~---
   }else if ( inherits(x,"Date") || inherits(x,"POSIXt") ){
      #--- Date, take the median
      ans = median(x=x,na.rm=na.rm)
      #---~---
   }else if ( is.character(x) ){
      #--- String, take the commonest
      ans = commonest(x=x,na.rm=na.rm)
      #---~---
   }else if ( is.character(x) || is.logical(x) ){
      #--- Logical, take the median
      ans = as.logical(median(x=as.integer(x),na.rm=na.rm))
      #---~---
   }else if ( is.ordered(x) ){
      #--- Ordered categorical, take the median
      xlev = levels(x)
      xmid = xlev[median(as.integer(x),na.rm=na.rm)]
      ans  = ordered(xmid,levels=xlev)
      #---~---
   }else if ( is.factor(x) ){
      #--- Non-ordered categorical, take the commonest
      xlev = levels(x)
      xmid = commonest(as.character(x),na.rm=na.rm)
      ans  = factor(xmid,levels=xlev)
      #---~---
   }else{
      stop(" Unrecognised variable type!")
   }#end if (all(is.na(x)))
   #---~---

   return(ans)
}#end function summarise_by_type
#---~---
