#===+===
#===+===
#   This routine checks whether or not the values are within reasonable bounds. In case they
# are not, discard data.
#---~---
check_bounds <<- function(x,xBounds){
   ans = ifelse( test = is.finite(x) & (x >= xBounds[1L]) & (x <= xBounds[2L])
               , yes  = x
               , no   = NA_real_
               )#end ifelse
   return(ans)
}#end check_bounds
#---~---
