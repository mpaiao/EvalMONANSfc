#!/bin/bash

#---~---
#   Path settings.
# 
# MAIN_PATH -- Path where this script is located.
# OPER_PATH -- Path where the post-processed MONAN simulations are located.
# ARCH_PATH -- Path where we archive the trimmed NetCDF files.
#---~---
MAIN_PATH="/path/to/EvalMONANSfc"
OPER_PATH="/path/to/MONAN_Forecasts/scripts_CD-CT/dataout"
ARCH_PATH="${MAIN_PATH}/Archive_MASTER"
#---~---



#---~---
#   Operational settings
#
# UNIQ_LABEL -- Descriptive label for archive and to MASTER. Select a unique name for
#               this operational run, and refrain from using spaces or other non-standard
#               characters.
# STEP       -- Which step should the script process? Current options are:
#               - "Model" -- The raw output from MONAN (not reprojected). Currently not
#                            supported, but the idea is to make this available too.
#               - "Post"  -- The output reprojected to a regular longitude/latitude grid.
# STEP_PATH  -- Should the step be appended to the path? Depending on the directory, 
#               MONAN output files are in a sub-directory with the name of the step.
# EXP        -- Which meteorological drivers were used for this run? Current options are
#               - "GFS"   -- Simulations initialised with GFS reanalyses.
# RES        -- Which resolution was used? Typical values are:
#               - 65536002 -- ~ 3 km
#               - 5898242  -- ~ 10 km
#               - 2621442  -- ~ 15 km
#               - 1024002  -- ~ 24 km
#               - 655362   -- ~ 30 km
#               - 163842   -- ~ 60 km
#               - 40962    -- ~ 120 km
# NLEV       -- Number of levels in the output files?
#---~---
UNIQ_LABEL="MONAN-1.4.3rc_10km"
STEP="Post"
STEP_PATH=false
EXP="GFS"
RES=5898242
NLEV=55
#---~---



#---~---
#   Period for which we run the script:
#
# WHEN_FRST -- First time to process MONAN
# WHEN_LAST -- Last  time to process MONAN
# WHEN_STEP -- Time step
# 
# NOTE: All these variables must be set so they are recognised by command `date`. For
#       WHEN_FRST/WHEN_LAST, format "YYYY-MM-DD HH UTC" works fine. For time step, provide
#       the value and the units, minding that the value must be integer. Possible values
#       are: "3600 seconds"; "180 minutes"; "6 hours"; "1 day".
#---~---
WHEN_FRST="2025-11-01 00 UTC" # Format must be recognised by command `date`
WHEN_LAST="2026-02-21 00 UTC" # Format must be recognised by command `date`
WHEN_STEP="12 hours"          # Format must be recognised by command `date`
#---~---



#---~---
#   Bounding box for the intercomparison files.
#---~---
WEST_LON=-120   # Westernmost  edge
EAST_LON=-20    # Easternmost  edge
SOUTH_LAT=-60   # Southernmost edge
NORTH_LAT=30    # Northernmost edge
#---~---


#---~---
#   Vector of variables to be included in the simplified output. Please only include
# variables that exist in the original operation output files. The derived quantities will
# be automatically added.
#---~---
VAR_LIST=("surface_pressure"  "mslp"              "precipw"           "rainnc"
          "rainc"             "cape"              "cin"               "acswdnb"
          "aclwupb"           "aclwupt"           "u10"               "v10"
          "q2"                "t2m"               "hfx"               "lh"
          "ter"               "landmask"        )
#---~---



#---~---
#   Include commands that will load CDO and NCL. These are necessary for running this
# script and are machine-specific. If these have been installed with apt-get (Ubuntu),
# HomeBrew (MacOS) or similars, you can comment this part out and run the script directly.
# To figure out whetehr or not CDO and NCL are readily available, try `which cdo` and 
# `which ncks` on your terminal. If they are available, you should see the full-path 
# location of these executables. Otherwise, if you get nothing back and don't know what
# to do, maybe check with your IT support team.
#---~---
module load cdo
module load ncl
#---~---




#---~---
#---~---
#---~---
#---~---
#---~---
#   No need to edit the script after this point, unless you are developing it.
#---~---
#---~---
#---~---
#---~---
#---~---
#---~---


#---- Set formats for times.
WHEN_FMT="%Y-%m-%d %H %Z" # Format for display and time stepping
ELAPSED_FMT="%s"          # Format for time since epoch
OPER_FMT="%Y%m%d%H"       # Time format used in the operational directory.
INIT_FMT="%H"             # Time format used for initialisation time.
#---~---


#---~---
#   Find the time bounds in seconds since epoch. 
#---~---
echo " + Find time bounds in seconds since epoch."
TZ="UTC" ELAPSED_FRST=$(date +"${ELAPSED_FMT}" -d "${WHEN_FRST}")
TZ="UTC" ELAPSED_LAST=$(date +"${ELAPSED_FMT}" -d "${WHEN_LAST}")
#---~---

#--- Set initial time (minus offset, which will be added back inside the loop).
echo " + Set initial time."
WHEN=$(date +"${WHEN_FMT}" -d "${WHEN_FRST} - ${WHEN_STEP}")
ELAPSED=$(date +%s -d "${WHEN}")
#---~---


#---- Set file prefix and suffix
case "${STEP}" in
Model)
   #--- Native output.
   OPER_PREFIX="MONAN_DIAG_G_MOD_${EXP}"
   OPER_SUFFIX="x${RES}L${NLEV}"
   #---~---
   ;;
Post)
   #--- Reprojected output.
   OPER_PREFIX="MONAN_DIAG_G_POS_${EXP}"
   OPER_SUFFIX="x${RES}L${NLEV}"
   #---~---
   ;;
*)
   #--- Invalid setting, stop.
   echo "---~---"
   echo "   FATAL ERROR!!!"
   echo "---~---"
   echo " - Step requested (\"${STEP}\") is invalid."
   echo " - Please set variable \"STEP\" to either \"Model\" or \"Post\"."
   echo "---~---"
   exit 1
   #---~---
   ;;
esac
#---~---




#--- Concatenate variables into a comma-separated string.
VAR_LIST=$(printf ",%s" "${VAR_LIST[@]}")
VAR_LIST=${VAR_LIST:1}
#---~---



#--- Make sure the output path exists.
mkdir -p ${ARCH_PATH}
#---~---



#---~---
#   Loop through times.
#---~---
while [[ ${ELAPSED} -lt ${ELAPSED_LAST} ]]
do
   #--- Update time and get information.
   WHEN=$(date +"${WHEN_FMT}" -d "${WHEN} + ${WHEN_STEP}")
   ELAPSED=$(date +%s -d "${WHEN}")
   OPER_WHEN=$(date +"${OPER_FMT}" -d "${WHEN}")
   INIT_WHEN="$(date +"${INIT_FMT}" -d "${WHEN}")Z"
   if ${STEP_PATH}
   then
      WHEN_PATH="${OPER_PATH}/${OPER_WHEN}/Post"
   else
      WHEN_PATH="${OPER_PATH}/${OPER_WHEN}"
   fi
   #---~---



   #--- Define archive file name
   INIT_PATH="${ARCH_PATH}/${UNIQ_LABEL}_${INIT_WHEN}"
   mkdir -p ${INIT_PATH}
   INIT_BASE="${UNIQ_LABEL}_${INIT_WHEN}_${OPER_WHEN}_SurfaceEval_${OPER_SUFFIX}.nc"
   INIT_FILE="${INIT_PATH}/${INIT_BASE}"
   #---~---


   #---~---
   #   Check if this operational run exists.
   #---~---
   if [[ -s ${ARCH_FILE} ]] || [[ -s "${ARCH_FILE}.xz" ]]
   then
      echo " + File $(basename ${ARCH_FILE}) already processed, skip it."

   elif [[ -d ${WHEN_PATH} ]]
   then
      echo " + Processing forecast initialised on ${WHEN}."

      #---~---
      #   Set a path to store temporary NetCDF files.
      #---~---
      TEMP_PATH="${MAIN_PATH}/DeleteMe"
      mkdir -pv ${TEMP_PATH}
      /bin/rm -f ${TEMP_PATH}/*
      #---~---



      #---~---
      #   Crop and downselect variables.
      #---~---
      echo "   - Crop and downselect variables."
      BASE_ORIG="${OPER_PREFIX}_${OPER_WHEN}_??????????.??.??.${OPER_SUFFIX}.nc"
      WHEN_ORIG="${WHEN_PATH}/${BASE_ORIG}"
      for THIS_ORIG in $(/bin/ls -1 ${WHEN_ORIG})
      do
         echo "     ~ File $(basename ${THIS_ORIG})"
         THIS_TEMP="${TEMP_PATH}/$(basename ${THIS_ORIG})"
         cdo -s -O selvar,${VAR_LIST} ${THIS_ORIG} ${TEMP_PATH}/down_select.nc
         cdo -s -O sellonlatbox,-180,180,-90,90                                            \
            ${TEMP_PATH}/down_select.nc ${TEMP_PATH}/180west180east.nc
         cdo -s -O sellonlatbox,${WEST_LON},${EAST_LON},${SOUTH_LAT},${NORTH_LAT}          \
            ${TEMP_PATH}/180west180east.nc ${THIS_TEMP}
      done
      #---~---



      #---~---
      #   Merge times so we only handle one file.
      #---~---
      echo "   - Merge all times into a single NetCDF"
      WHEN_WILD="${WHEN_PATH}/${OPER_PREFIX}_${OPER_WHEN}"
      WHEN_WILD="${WHEN_WILD}_??????????.??.??.${OPER_SUFFIX}.nc"
      cdo -s -O mergetime ${TEMP_PATH}/${BASE_ORIG} ${TEMP_PATH}/mergetime.nc
      #---~---


      #--- Add vapour pressure, which is needed for computing dew point temperature.
      echo "   - Add vapour pressure."
      cdo -s -O aexpr,"pvap2m=surface_pressure*q2/(q2+(1-q2)*0.6218519)"                   \
         ${TEMP_PATH}/mergetime.nc ${TEMP_PATH}/stepping_stone.nc
      cdo -s -O                                                                            \
        setattribute,pvap2m@units="Pa",pvap2m@longname="2-metre vapour pressure"           \
        ${TEMP_PATH}/stepping_stone.nc ${TEMP_PATH}/pvap2m.nc 
      #---~---


      #--- Add dew point temperature, based on Bolton (1980).
      echo "   - Add dew point temperature."
      cdo -s -O aexpr,'td2m=273.15+(243.5*ln(pvap2m/611.2))/(17.67-ln(pvap2m/611.2))'      \
         ${TEMP_PATH}/pvap2m.nc ${TEMP_PATH}/stepping_stone.nc
      cdo -s -O setattribute,td2m@units="K",td2m@longname="2-metre dew point temperature"  \
         ${TEMP_PATH}/stepping_stone.nc ${TEMP_PATH}/td2m.nc
      #---~---


      #--- Add 10-m wind speed.
      echo "   - Add wind speed at 10 m."
      cdo -s -O aexpr,'ws10=sqrt(u10*u10+v10*v10)'                                         \
         ${TEMP_PATH}/td2m.nc ${TEMP_PATH}/stepping_stone.nc
      cdo -s -O setattribute,ws10@units="m s-1",td2m@longname="10-metre wind speed"        \
         ${TEMP_PATH}/stepping_stone.nc ${TEMP_PATH}/ws10.nc
      #---~---


      #--- Add total rainfall.
      echo "   - Add total rainfall."
      cdo -s -O aexpr,'rain=rainnc+rainc'                                                  \
         ${TEMP_PATH}/ws10.nc ${TEMP_PATH}/stepping_stone.nc
      cdo -s -O                                                                            \
         setattribute,rain@units="mm",rain@longname="accumulated total precipitation"      \
         ${TEMP_PATH}/stepping_stone.nc ${TEMP_PATH}/rain.nc
      #---~---


      #--- Copy simplified file to output.
      echo " + Archive simplified file."
      /bin/mv ${TEMP_PATH}/rain.nc ${ARCH_FILE}
      #---~---

      #--- Clear the temporary path.
      echo " + Clear the temporary path."
      /bin/rm -fr ${TEMP_PATH}/*
      #---~---
   else
      echo " + No forecast was initialised on ${WHEN}, skip it."
   fi
   #---~---
done
#---~---




#--- Delete the temporary path.
echo " + Delete the temporary path."
/bin/rm -fr ${TEMP_PATH}
#---~---
