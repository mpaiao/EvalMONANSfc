#===+===
#===+===
#   This function converts continuously accumulated quantities into accumulated quantities
# between two consecutive time steps. This will compute the time difference only when the
# time steps are consecutive, otherwise, it will return NA_real_.
#---~---
accum_one_step <<- function(x,when){
   #--- Find out the time difference between consecutive times, and the nominal time step.
   elapsed     = as.numeric(when)
   dt_one_step = min(diff(elapsed))
   dt_when     = c(dt_one_step,diff(elapsed))
   #---~---


   #--- Find the values at the beginning and end of the time accumulation.
   acc_begin = c(0,x[-length(x)])
   acc_end   = x
   #---~---
   
   
   #---~---
   #   We first check whether or not we have valid data over a valid step. This means that
   # the time between two consecutive data points corresponds to a single time step, and 
   # that both the initial and final accumulated values are valid. In case everything looks
   # good, we further check whether or not the accumulated at the end of the step is less
   # than at the beginning. If so, we keep the accumulated at the end of the time step, as
   # this is likely when the accumulation was reset.
   #---~---
   ans = ifelse( test = ( dt_when > dt_one_step ) | is.na(acc_begin) | is.na(acc_end) 
               , yes  = NA_real_
               , no   = ifelse( test = acc_end < acc_begin
                              , yes  = acc_end
                              , no   = acc_end - acc_begin
                              )#end ifelse
               )#end ifelse
   #---~---

   #--- The answer is the vector with accumulated values between consecutive steps.
   return(ans)   
   #---~---
}#end accum_one_step
#---~---
