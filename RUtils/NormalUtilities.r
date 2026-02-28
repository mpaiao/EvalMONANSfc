#===+===
#===+===
#   These global parameters are used for streamlining the distribution fitting.
#---~---
#--- Number of parameters.
distrNParams <<- c( normal       = 2L
                  , logistic     = 2L
                  , lsTStudent   = 3L
                  , skewNormal   = 3L
                  , logNormal    = 2L
                  , weibull      = 2L
                  , gamma        = 2L
                  , negLogNormal = 2L
                  )#end c
#--- Name of the first parameter.
distrFirst <<- c( normal       = "mean"
                , logistic     = "location"
                , lsTStudent   = "mu"
                , skewNormal   = "xi"
                , logNormal    = "meanlog"
                , weibull      = "shape"
                , gamma        = "shape"
                , negLogNormal = "meannlog"
                )#end c
distrSecond <<- c( normal       = "sd"
                 , logistic     = "scale"
                 , lsTStudent   = "sigma"
                 , skewNormal   = "omega"
                 , logNormal    = "sdlog"
                 , weibull      = "scale"
                 , gamma        = "rate"
                 , negLogNormal = "sdnlog"
                 )#end 
distrThird  <<- c( normal       = NA_character_
                 , logistic     = NA_character_
                 , lsTStudent   = "df"
                 , skewNormal   = "alpha"
                 , logNormal    = NA_character_
                 , weibull      = NA_character_
                 , gamma        = NA_character_
                 , negLogNormal = NA_character_
                 )#end 
#===+===
#===+===



#===+===
#===+===
#   This subroutine finds the best distribution to fit the data amongst the likely 
# functions. It also returns nothing in case the number of observations is less than the
# required to fit distributions.
#
# Arguments:
#
# x        -- The vector with values for which a distribution is to be fitted.
# nx_min   -- Minimum number of observations for which we fit a distribution.
# altDistr -- The function will always fit the normal distribution, and test it against
#             all alternative distributions listed in this argument. If only normal 
#             distribution is sought, set altDistr to NA.
# whichIC  -- Which information criterion to use?
#---~---
getDistrStat <<- function( x
                         , nx_min     = 30L
                         , altDistr   = c( "logistic"    , "lsTStudent"  , "skewNormal"
                                         , "logNormal"   , "weibull"     , "gamma"
                                         , "negLogNormal")
                         , skewThresh = 0.2
                         , kurtThresh = 1.0
                         , whichIC    = c("AIC","BIC")
                         ){

   #---~---
   #   Make sure "whichIC" is a valid argument
   #---~---
   whichIC = match.arg(whichIC)
   useAIC  = whichIC %in% "AIC"
   #---~---

   #---~---
   #   Make use altDistr is either NA or a valid argument.
   #---~---
   if (all(is.na(altDistr))){
      altDistr = "none"
   }else{
      for (a in seq_along(altDistr)){
         altDistr[a] = 
            match.arg( arg     = altDistr[a]
                     , choices = c( "logistic"    , "lsTStudent"  , "skewNormal"
                                  , "logNormal"   , "weibull"     , "gamma"
                                  , "negLogNormal")
                     )#end match.arg
      }#end for (a in seq_along(altDistr))
   }#end if (is.na(altDistr)
   #---~---


   #---~---
   #   Remove non-finite points 
   #---~---
   xfit     = x[is.finite(x)]
   nxfit    = length(xfit)
   #---~---


   #---~---
   #   Initialise the answer with no information.
   #---~---
   ans = list( N               = nxfit
             , Distr           = NA_character_
             , First           = NA_real_
             , Second          = NA_real_
             , Third           = NA_real_
             , XIC             = +Inf
             , stringAsFactors = FALSE
             )#end list
   #---~---



   #---~---
   #   Skip fitting if the vector does not have valid data.
   #---~---
   if (nxfit < nx_min) return(ans)
   #---~---


   #--- Ancillary values
   positive = all(xfit > 0)
   negative = all(xfit < 0)
   lognxfit = log(nxfit)
   #---~---



   #---~---
   #   Verify whether or not to fit the costly skew-normal distribution.
   #---~---
   if ("skewNormal" %in% altDistr){
      mu    = mean(xfit)
      sigma = sd  (xfit)
      if (sigma > 0){
         #--- Estimate skewness and compare it with the threshold.
         skewEst       = mean( (xfit-mu)^3 ) / sigma^3
         fitSkewNormal = abs(skewEst) > skewThresh
         #---~---
      }else{
         fitSkewNormal = FALSE
      }#end if (sigma > 0)
   }else{
      fitSkewNormal = FALSE
   }#end if ("skewNormal" %in% altDistr)
   #---~---



   #---~---
   #   Verify whether or not to fit the costly location-scale Student's t distribution.
   #---~---
   if ("lsTStudent" %in% altDistr){
      mu    = mean(xfit)
      sigma = sd  (xfit)
      if (sigma > 0){
         #--- Estimate excess Kurtosis and compare it with the threshold.
         excessKurt    = mean( (xfit-mu)^4 ) / sigma^4 - 3
         fitLsTStudent = abs(excessKurt) > kurtThresh
         #---~---
      }else{
         fitLsTStudent = FALSE
      }#end if (sigma > 0)
   }else{
      fitLsTStudent = FALSE
   }#end if ("skewNormal" %in% altDistr)
   #---~---


   #---~---
   #   Normal distribution. This is the first distribution tested, so we always copy 
   # the parameters. For all other distributions tested, we will pick them only if they
   # represent an improvement.
   #---~---
   this_fit   = MASS::fitdistr(x=xfit,densfun="normal")
   ans        = testNewFit( best      = ans
                          , newDistr  = "normal"
                          , newFit    = this_fit
                          , useAIC    = useAIC
                          , lognxfit  = lognxfit
                          )#end testNewFit
   #---~---



   #---~---
   #   Logistic distribution.
   #---~---
   if ( "logistic" %in% altDistr){
      this_fit = try( expr = MASS::fitdistr(x=xfit,densfun="logistic"), silent = TRUE)
      if ( ! inherits(this_fit, "try-error") ){
         #--- Test if this fit is the best so far.
         ans = testNewFit( best      = ans
                         , newDistr  = "logistic"
                         , newFit    = this_fit
                         , useAIC    = useAIC
                         , lognxfit  = lognxfit
                         )#end testNewFit
         #---~---
      }#end if ( ! inherits(this_fit, "try-error") )
      #---~---
   }#end if ("logistic" %in% altDistr)
   #---~---



   #---~---
   #   Location-scale Student's t distribution.
   #---~---
   if (fitLsTStudent){
      lst_1st = list(mu=mean(xfit),sigma=sd(xfit),df=10)
      this_fit = try( expr   = MASS::fitdistr( x       = xfit
                                             , densfun = extraDistr::dlst
                                             , start   = lst_1st
                                             )#end MASS::fitdistr
                    , silent = TRUE
                    )#end try
      if ( ! inherits(this_fit, "try-error") ){
         #--- Test if this fit is the best so far.
         ans = testNewFit( best      = ans
                         , newDistr  = "lsTStudent"
                         , newFit    = this_fit
                         , useAIC    = useAIC
                         , lognxfit  = lognxfit
                         )#end testNewFit
         #---~---
      }#end if ( ! inherits(this_fit, "try-error") )
      #---~---
   }#end if (fitLsTStudent)
   #---~---



   #---~---
   #   Skew-normal distribution. First, use function sn_guess (see below) to define
   # first guesses based on the estimator from package "sn".
   #---~---
   if (fitSkewNormal){
      sn_1st   = sn_guess(xfit)
      this_fit = try( expr   = MASS::fitdistr(x=xfit,densfun=sn::dsn,start=sn_1st)
                    , silent = TRUE
                    )#end try
      if ( ! inherits(this_fit, "try-error") ){
         #--- Test if this fit is the best so far.
         ans = testNewFit( best      = ans
                         , newDistr  = "skewNormal"
                         , newFit    = this_fit
                         , useAIC    = useAIC
                         , lognxfit  = lognxfit
                         )#end testNewFit
         #---~---
      }#end if ( ! inherits(this_fit, "try-error") )
      #---~---
   }#end if (fitSkewNormal)
   #---~---



   #---~---
   #   The following distributions can be tested only if data are all positive or all 
   # negative.
   #---~---
   if (positive){
      #---~---
      #   Log-normal.
      #---~---
      if ("logNormal" %in% altDistr){
         this_fit = try( MASS::fitdistr(x=xfit,densfun="log-normal"), silent= TRUE )
         if ( ! inherits(this_fit, "try-error") ){
            #--- Test if this fit is the best so far.
            ans = testNewFit( best      = ans
                            , newDistr  = "logNormal"
                            , newFit    = this_fit
                            , useAIC    = useAIC
                            , lognxfit  = lognxfit
                            )#end testNewFit
            #---~---
         }#end if ( ! inherits(this_fit, "try-error") )
         #---~---
      }#end if ("logNormal" %in% altDistr)
      #---~---


      #---~---
      #   Weibull.
      #---~---
      if ("weibull" %in% altDistr){
         this_fit = try( MASS::fitdistr(x=xfit,densfun="weibull"), silent= TRUE )
         if ( ! inherits(this_fit, "try-error") ){
            #--- Test if this fit is the best so far.
            ans = testNewFit( best      = ans
                            , newDistr  = "weibull"
                            , newFit    = this_fit
                            , useAIC    = useAIC
                            , lognxfit  = lognxfit
                            )#end testNewFit
            #---~---
         }#end if ( ! inherits(this_fit, "try-error") )
         #---~---
      }#end if ("weibull" %in% altDistr)
      #---~---


      #---~---
      #   Gamma.
      #---~---
      if ("gamma" %in% altDistr){
         this_fit = try( MASS::fitdistr(x=xfit,densfun="gamma"), silent= TRUE )
         if ( ! inherits(this_fit, "try-error") ){
            #--- Test if this fit is the best so far.
            ans = testNewFit( best      = ans
                            , newDistr  = "gamma"
                            , newFit    = this_fit
                            , useAIC    = useAIC
                            , lognxfit  = lognxfit
                            )#end testNewFit
            #---~---
         }#end if ( ! inherits(this_fit, "try-error") )
         #---~---
      }#end if ("gamma" %in% altDistr)
      #---~---
   }else if (negative && ("negLogNormal" %in% altDistr)){
      #---~---
      #   Negative log-normal distribution. This is similar to the log-normal distribution,
      # but because it is a non-standard distribution, we must provide a first guess.
      #---~---
      xtrans   = -log(-xfit)
      nl_1st   = list(meannlog=mean(xtrans),sdnlog=sd(xtrans))
      this_fit = try( MASS::fitdistr(x=xfit,densfun=dnlnorm,start=nl_1st), silent = TRUE )
      if ( ! inherits(this_fit, "try-error") ){
         #--- Test if this fit is the best so far.
         ans = testNewFit( best      = ans
                         , newDistr  = "negLogNormal"
                         , newFit    = this_fit
                         , useAIC    = useAIC
                         , lognxfit  = lognxfit
                         )#end testNewFit
         #---~---
      }#end if ( ! inherits(this_fit, "try-error") )
      #---~---
   }#end if (positive)
   #---~---


   #---~---
   #   Return the answer.
   #---~---
   return(ans)
   #---~---
}#end function getDistrStat
#===+===
#===+===




#===+===
#===+===
#   Function that tests a new fit and updates the best fit it is indeed a better one.
#---~---
testNewFit <<- function(best,newDistr,newFit,useAIC,lognxfit){
   #--- By default, we keep the current estimate.
   ans = best
   #---~---

   #--- Get information criteria.
   if (useAIC){
      this_XIC = 2. * ( distrNParams[[newDistr]] - newFit$loglik )
   }else{
      this_XIC = distrNParams[[newDistr]] * lognxfit - 2. * newFit$loglik
   }#end if (useAIC
   #---~---

   #---~---
   #   If this is a better fit, update ans.
   #---~---
   if (this_XIC < best$XIC){
      ans$Distr  = newDistr
      ans$First  = newFit$estimate[[distrFirst [[newDistr]]]]
      ans$Second = newFit$estimate[[distrSecond[[newDistr]]]]
      if (is.na(distrThird[[newDistr]])){
         ans$Third = NA_real_
      }else{
         ans$Third = newFit$estimate[[distrThird[[newDistr]]]]
      }#end if (is.na(distrThird[[newDistr]]))
      ans$XIC = this_XIC
   }#end if (newXIC$XIC < ans$XIC)
   #---~---

   return(ans)
}#end testNewFit
#---~---






#===+===
#===+===
#   This function normalises the data given the best distribution. This is equivalent to
# the traditional z-score normalisation ( Z = (x - mu) / sigma ), but generalised to more
# distributions
#---~---
xToNormalised <<- function(x,Distr,First,Second,Third,N=length(x)
                          ,pLwr=min(0.0001,1./(2.*N)),pUpr=1.-pLwr){

   #--- Initialise cumulative distribution function with non-informative values.
   xcdf = rep(NA_real_,times=length(x))
   #---~---

   #--- Select distributions.
   isN   = Distr %in% "normal"
   isL   = Distr %in% "logistic"
   isLN  = Distr %in% "logNormal"
   isNLN = Distr %in% "negLogNormal"
   isW   = Distr %in% "weibull"
   isG   = Distr %in% "gamma"
   isLST = Distr %in% "lsTStudent"
   isSN  = Distr %in% "skewNormal"
   #---~---

   #--- Apply cumulative distribution functions accordingly.
   xcdf[isN  ] = pnorm   (q=x[isN  ],mean    =First[isN  ],sd    =Second[isN  ])
   xcdf[isL  ] = plogis  (q=x[isL  ],location=First[isL  ],scale =Second[isL  ])
   xcdf[isLN ] = plnorm  (q=x[isLN ],meanlog =First[isLN ],sdlog =Second[isLN ])
   xcdf[isNLN] = pnlnorm (q=x[isNLN],meannlog=First[isNLN],sdnlog=Second[isNLN])
   xcdf[isW  ] = pweibull(q=x[isW  ],shape   =First[isW  ],scale =Second[isW  ])
   xcdf[isG  ] = pgamma  (q=x[isG  ],shape   =First[isG  ],rate  =Second[isG  ])
   xcdf[isLST] = extraDistr::plst( q     = x     [isLST]
                                 , mu    = First [isLST]
                                 , sigma = Second[isLST]
                                 , df    = Third [isLST]
                                 )#end extraDistr::plst
   xcdf[isSN ] = sn::psn         ( x     = x     [isSN ]
                                 , xi    = First [isSN ]
                                 , omega = Second[isSN ]
                                 , alpha = Third [isSN ]
                                 )#end sn::psn
   #---~---

   #---~---
   #   Find the equivalent quantile in the normal distribution with mean=0 and sd=1. To
   # avoid infinity, we set bounds for CDFs.
   #---~---
   xcdf = pmax(pLwr,pmin(pUpr,xcdf))
   ans  = qnorm(p=xcdf,mean=0.,sd=1.)
   #---~---

   #--- Return answer
   return(ans)
   #---~---
}#end function xToNormalised
#===+===
#===+===






#===+===
#===+===
#   This function converts back normalised data given the best distribution. This is 
# equivalent to the traditional z-score un-normalisation ( x = mu + Z * sigma ), but 
# generalised to more distributions.
#---~---
normalisedToX <<- function(z,Distr,First,Second,Third,N=length(z)
                          ,pLwr=min(0.0001,1/(2.*N)),pUpr=1.-pLwr){

   #---~---
   #   Find the equivalent quantile in the normal distribution with mean=0 and sd=1.
   #---~---
   xcdf = pnorm(q=z,mean=0.,sd=1.)
   xcdf = pmax(pLwr,pmin(pUpr,xcdf))
   #---~---

   #--- Select distributions.
   isN   = Distr %in% "normal"
   isL   = Distr %in% "logistic"
   isLN  = Distr %in% "logNormal"
   isNLN = Distr %in% "negLogNormal"
   isW   = Distr %in% "weibull"
   isG   = Distr %in% "gamma"
   isLST = Distr %in% "lsTStudent"
   isSN  = Distr %in% "skewNormal"
   #---~---

   #--- Apply cumulative distribution functions accordingly.
   ans        = rep(NA_real_,times=length(z))
   ans[isN  ] = qnorm   (p=xcdf[isN  ],mean    =First[isN  ],sd    =Second[isN  ])
   ans[isL  ] = qlogis  (p=xcdf[isL  ],location=First[isL  ],scale =Second[isL  ])
   ans[isLN ] = qlnorm  (p=xcdf[isLN ],meanlog =First[isLN ],sdlog =Second[isLN ])
   ans[isNLN] = qnlnorm (p=xcdf[isNLN],meannlog=First[isNLN],sdnlog=Second[isNLN])
   ans[isW  ] = qweibull(p=xcdf[isW  ],shape   =First[isW  ],scale =Second[isW  ])
   ans[isG  ] = qgamma  (p=xcdf[isG  ],shape   =First[isG  ],rate  =Second[isG  ])
   ans[isLST] = extraDistr::qlst( p     = xcdf  [isLST]
                                , mu    = First [isLST]
                                , sigma = Second[isLST]
                                , df    = Third [isLST]
                                )#end extraDistr::qlst
   ans[isSN ] = sn::qsn         ( p     = xcdf  [isSN ]
                                , xi    = First [isSN ]
                                , omega = Second[isSN ]
                                , alpha = Third [isSN ]
                                )#end sn::qsn
   #---~---

   #--- Return answer
   return(ans)
   #---~---
}#end function NormalisedToX
#===+===
#===+===
