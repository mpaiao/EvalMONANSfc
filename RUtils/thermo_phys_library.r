#---~---
#   Physical and thermodynamic constants.
#---~---
grav    <<- 9.80665     # Gravity acceleration                       [   m/s2]
rmol    <<- 8.314510    # Molar gas constant                         [J/mol/K]
mmdry   <<- 0.02897     # Mean dry air molar mass                    [ kg/mol]
mmh2o   <<- 0.01801505  # Mean water molar mass                      [ kg/mol]
rdry    <<- rmol/mmdry  # Gas constant for dry air                   [ J/kg/K]
ep      <<- mmh2o/mmdry # Water:Dry-air molar mass ratio (epsilon)   [  kg/kg]
t00     <<- 273.15      # Temperature at 0 degC                      [      K]
pvsat00 <<- 611.2       # Saturation water vapour pressure at 0 degC [     Pa]
#---~---


#---~---
#   Unit conversions.
#---~---
rad_to_deg = 180. / pi
deg_to_rad = 1. / rad_to_deg
kJ_to_J    = 1000.
J_to_kJ    = 1. / kJ_to_J
hPa_to_Pa  = 100.
Pa_to_hPa  = 1. / hPa_to_Pa
#---~---


#---~---
#   Wrapper function that will solve the root to obtain surface pressure [Pa] given 
# mean sea level pressure [Pa], 2-metre temperature [K], 2-metre vapour pressure [Pa] and
# terrain elevation [m].
#---~---
find_sfc_pressure <<- function(mslp,t2m,pvap2m,ter){
   #---~---
   #   Internal function that computes how far the pressure guess (x) is from the actual
   # solution.
   #---~---
   zero_sfc_pressure = function(x,mslp,t2m,pvap2m,ter){
      tv2m = find_tv2m( surface_pressure = x, t2m = t2m, pvap2m = pvap2m )
      pres = mslp * exp( - grav * ter / ( rdry * tv2m ) )
      ans  = pres - x
      return(ans)
   }#end zero_sfc_pressure
   #---~---

   #---~---
   #   If any argument is a vector, make sure all arguments are either scalars or vectors
   # of the same length.
   #---~---
   n_max    = max(c(length(mslp),length(t2m),length(pvap2m),length(ter)))
   if (! all(c(length(mslp),length(t2m),length(pvap2m),length(ter)) %in% c(1,n_max))){
      cat("---~---\n")
      cat("   FATAL ERROR! \n")
      cat("---~---\n")
      cat("   Vector lengths are not of the same length (or scalars)!\n")
      cat("   - length(mslp)   = ",length(mslp  ),".\n",sep="")
      cat("   - length(t2m)    = ",length(t2m   ),".\n",sep="")
      cat("   - length(pvap2m) = ",length(pvap2m),".\n",sep="")
      cat("   - length(ter)    = ",length(ter   ),".\n",sep="")
      cat("---~---\n")
      stop(" Input arguments must have the same length (or be scalars).")
   }else{
      if (length(mslp  ) == 1L) mslp   = rep(mslp  ,times=n_max)
      if (length(t2m   ) == 1L) t2m    = rep(t2m   ,times=n_max)
      if (length(pvap2m) == 1L) pvap2m = rep(pvap2m,times=n_max)
      if (length(ter   ) == 1L) ter    = rep(ter   ,times=n_max)
   }#end if (! all(c(length(mslp),length(t2m),length(pvap2m),length(ter)) %in% c(1,n_max)))
   #---~---


   #---~---
   #   Decide how to call this routine based on inputs.
   #---~---
   if (n_max > 1L){
      #--- Vectors provided. Recursively call this function using mapply.
      surface_pressure = mapply( FUN      = find_sfc_pressure
                               , mslp     = mslp
                               , t2m      = t2m
                               , pvap2m   = pvap2m
                               , ter      = ter
                               , SIMPLIFY = TRUE
                               )#end mapply
      #---~---
   }else if (any(is.na(c(mslp,t2m,pvap2m,ter)))){
      #--- Skip function if any of the values are NA.
      surface_pressure = NA_real_
      #---~---
   }else{
      #---~---
      #   Arguments are scalars, proceed with root-finding.
      #---~---

      #---~---
      #   Define interval as the minimum and maximum possible values of pressure, by
      # varying virtual temperature from t2m (zero humidity) to tv2m / ep (air is pure
      # air vapour).
      #---~---
      pbnd_one  = mslp * exp( - grav * ter / ( rdry * t2m      ) )
      pbnd_two  = mslp * exp( - grav * ter / ( rdry * t2m / ep ) )
      pinterval = sort(c(pbnd_one,pbnd_two))
      if (length(pinterval) != 2) browser()
      #---~---



      #---~---
      #   Find the solution.
      #---~---
      solve_pressure = try( uniroot( f        = zero_sfc_pressure
                                   , interval = pinterval
                                   , mslp     = mslp
                                   , t2m      = t2m
                                   , pvap2m   = pvap2m
                                   , ter      = ter
                                   )#end uniroot
                          , silent = TRUE
                          )#end try

      if ("try-error" %in% is(solve_pressure)){
         surface_pressure = NA_real_
      }else{
         surface_pressure = solve_pressure$root
      }#end if ("try-error" %in% is(solve_pressure))
      #---~---
   }#end if (any(is.na(c(mslp,t2m,pvap2m,ter))))
   #---~---

   #--- Return the solution.
   return(surface_pressure)
   #---~---
}#end function find_sfc_pressure
#---~---



#---~---
#   Function that finds mean sea level pressure [Pa] given the surface pressure [Pa], 
# 2-metre temperature [K], 2-metre vapour pressure [Pa] and terrain elevation [m].
#---~---
find_mslp <<- function(surface_pressure, t2m, pvap2m, ter){
   tv2m = find_tv2m(surface_pressure,t2m,pvap2m)
   ans  = surface_pressure * exp ( grav * ter / (rdry * tv2m) )
   return(ans)
}#end function find_mslp
#---~---



#---~---
#   Function that finds the 2-metre virtual temperature [K] given the surface pressure 
# [Pa], the 2-metre temperature [K] and the 2-metre water vapour pressure [Pa].
#---~---
find_tv2m <<- function(surface_pressure,t2m,pvap2m){
   ans = t2m / ( 1. - pvap2m / surface_pressure * ( 1. - ep ) )
   return(ans)
}#end find_tv2m
#---~---



#---~---
#   Function that finds the 2-metre specific humidity [kg/kg] given the surface pressure 
# [Pa], the 2-metre temperature [K] and the 2-metre water vapour pressure [Pa].
#---~---
find_q2m <<- function(surface_pressure,t2m,pvap2m){
   ans = ep * pvap2m / ( surface_pressure - (1. - ep) * pvap2m )
   return(ans)
}#end find_q2m
#---~---



#---~---
#   Functions that find saturation vapour pressure [Pa] from temperature [K] and 
# vice-versa, based on Bolton (1980).
#
# Reference:
# 
# Bolton D. 1980. The computation of equivalent potential temperature. Mon. Wea. Rev.
#   108: 1046-1053, doi:10.1175/1520-0493(1980)108<1046:TCOEPT>2.0.CO;2.
#---~---
t2m_to_pvsat2m <<- function(t2m){
   ans = pvsat00 * exp( 17.67 * (t2m - t00) / ( t2m - t00 + 243.5 ) )
   return(ans)
}#end function t2m_to_pvsat2m
pvsat2m_to_t2m <<- function(pvsat2m){
   ans = t00 + ( 243.5 * log( pvsat2m / pvsat00 ) ) / ( 17.67 - log ( pvsat2m / pvsat00 ) )
   return(ans)
}#end function pvsat2m_to_t2m
#---~---



#---~---
#   Functions that find the zonal and meridional wind speed. These are equivalent to 
# trigonometric decomposition, but using the information that wind direction is 
# 270 - trigonometric angle.
#---~---
find_u10 <<- function(ws10,wd10){
   ans = - ws10 * sin( wd10 * deg_to_rad)
   return(ans)
}#end function find_u10
find_v10 <<- function(ws10,wd10){
   ans = - ws10 * cos( wd10 * deg_to_rad)
   return(ans)
}#end function find_v10
#---~---
