#!/bin/bash

#===+===
#===+===
#   script extract_MONAN.sh
#
#   Developed by:  Marcos Longo 
#                  m a r c o s -d.o.t- l o n g o -a.t- i n p e -d.o.t- b r
#   First version: 2026-02-19 
#
#   This script extracts time series from MONAN NetCDF files for each of the sites and 
# output the data into a text file containing the station code, time stamp and time step, 
# and a list of selected variables.
#===+===


#---~---
#   Path settings.
#---~---
MAIN_PATH="/p/projetos/monan_sfc/Avaliacao/2026-01_MONAN-1.4.3.rc/PrepareIntercomp"
OBSER_PATH="${MAIN_PATH}/Observations"
IN_MONAN_PATH="${MAIN_PATH}/ArchiveMASTER"
OUT_MONAN_PATH="${MAIN_PATH}/SiteForecast"
#---~---


#---~---
#   Prefix, times and and suffix for MONAN simulations.
#---~---
MONAN_PREFIX="MONAN-1.4.3rc_10km"
MONAN_SUFFIX="SurfaceEval_x5898242L55"
#---~---


#---~---
#   The following variables are the column indices corresponding to the site identity,
# site longitude and site latitude in the site lists.
#---~---
SITE_IDENT_POS=1
SITE_LON_POS=2
SITE_LAT_POS=3
#---~---



#---~---
#   List of variables to be included on the table. Use vector arrays.
#---~---
VAR_LIST=("surface_pressure" "mslp" "acswdnb" "u10" "v10" "q2" "t2m"
          "pvap2m" "td2m" "rain" "ter")
#---~---

#--- Load required modules.
module purge
module load ncl
module load nco
module load cdo
#---~---


#---~---
#---~---
#---~---
#---~---
#---~---
#---~---
#   No need to change anything beyond this point, unless you are developing the script.
#---~---
#---~---
#---~---
#---~---
#---~---
#---~---



#---~---
#   Create output path.
#---~---
mkdir -pv ${OUT_MONAN_PATH}
#---~---



#---~---
#   Make list of variables separated by commas.
#---~---
SELVAR_COMMA=$(IFS=","; echo "${VAR_LIST[*]}")
#---~---



#---~---
#   Create a temporary path.
#---~---
TEMP_PATH="${MAIN_PATH}/DeleteMe"
/bin/rm -fr ${TEMP_PATH}
mkdir -pv ${TEMP_PATH}
#---~---



#---~---
#   Find all sites and all MONAN forecasts
#---~---
MONAN_BASE=${MONAN_PREFIX}_???_??????????_${MONAN_SUFFIX}.nc
SITE_LIST=$(/bin/ls ${OBSER_PATH}/*/*_SiteInfo.csv      2> /dev/null)
MONAN_LIST=$(/bin/ls ${IN_MONAN_PATH}/*/${MONAN_BASE}   2> /dev/null)
N_SITE_LIST=$(/bin/ls ${OBSER_PATH}/*/*_SiteInfo.csv    2> /dev/null | wc -l)
N_MONAN_LIST=$(/bin/ls ${IN_MONAN_PATH}/*/${MONAN_BASE} 2> /dev/null | wc -l)
#---~---


#---~---
#   Stop if we fail to find site lists or forecasts.
#---~---
if [[ ${N_SITE_LIST} -eq 0 ]] || [[ ${N_MONAN_LIST} -eq 0 ]]
then
   echo "---~---"
   echo "   FATAL ERROR!!!"
   echo "---~---"
   echo ""
   echo "   Failed finding input files needed. Check settings below:"
   echo ""
   echo "   - Path to reference site files:         \"${SITE_PATH}\""
   echo "   - Path to MONAN forecast files:         \"${IN_MONAN_PATH}\""
   echo "   - Prefix for MONAN forecasts:           \"${MONAN_PREFIX}\""
   echo "   - Suffix for MONAN forecasts:           \"${MONAN_SUFFIX}\""
   echo "   - Number of reference site files found: ${N_SITE_LIST}"
   echo "   - Number of MONAN forecast files found: ${N_MONAN_LIST}"
   echo "---~---"
   exit -1   
fi
#---~---



#---~---
#   Loop through sites.
#---~---
for SITE_FILE in ${SITE_LIST}
do
   #--- Retrieve site list.
   SITE_BASE=$(basename ${SITE_FILE})
   SITE_TYPE=$(basename ${SITE_BASE} .csv | sed s@"_SiteInfo"@""@g)
   SITE_PREFIX=$(echo ${SITE_TYPE} | tr '[:upper:]' '[:lower:]')
   echo " + Processing ${SITE_TYPE} sites listed in \"${SITE_BASE}\":"
   #---~---


   #---~---
   #    Extract information from file.
   #---~---
   IDENT_LIST=$(tail -n +2 "${SITE_FILE}" | cut -d',' -f${SITE_IDENT_POS} | tr '\n' ' ')
   LON_LIST=$(tail   -n +2 "${SITE_FILE}" | cut -d',' -f${SITE_LON_POS}   | tr '\n' ' ')
   LAT_LIST=$(tail   -n +2 "${SITE_FILE}" | cut -d',' -f${SITE_LAT_POS}   | tr '\n' ' ')
   #---~---


   #---~---
   #    Extract information from file.
   #---~---
   read -r -a IDENT_VEC <<< "${IDENT_LIST}"
   read -r -a LON_VEC   <<< "${LON_LIST}"
   read -r -a LAT_VEC   <<< "${LAT_LIST}"
   #---~---



   #--- Create file name for site coordinates.
   COORD_FILE="${TEMP_PATH}/${SITE_TYPE}_Coordinates.txt"
   #---~---


   #--- Tally the site list.
   N_SITES=$(cat ${SITE_FILE} | tail -n +2| wc -l)
   #---~---


   #--- Create a grid file with all sites.
   if [[ -s ${COORD_FILE} ]]
   then
      /bin/rm -f ${COORD_FILE}
   fi
   touch ${COORD_FILE}
   #---~---

   #---~---
   #   Append information for site extraction.
   #---~---
   echo "gridtype = unstructured" >> ${COORD_FILE}
   echo "gridsize = ${N_SITES}"   >> ${COORD_FILE}
   echo "xvals    = ${LON_LIST}"  >> ${COORD_FILE}
   echo ""                        >> ${COORD_FILE}
   echo "yvals    = ${LAT_LIST}"  >> ${COORD_FILE}
   echo ""                        >> ${COORD_FILE}
   #---~---



   #---~---
   #   Loop through every MONAN forecast file and extract site list.
   #---~---
   for MONAN_FILE in ${MONAN_LIST}
   do
      #--- Load forecast information.
      MONAN_BASE=$(basename ${MONAN_FILE} .nc)
      MONAN_ZERO=$( echo ${MONAN_BASE} \
                  | sed s@"${MONAN_PREFIX}_"@""@g )
      MONAN_INIT=${MONAN_ZERO:0:3}
      MONAN_ZERO=$( echo ${MONAN_BASE} \
                  | sed s@"${MONAN_PREFIX}_"@""@g \
                  | sed s@"${MONAN_INIT}_"@""@g   \
                  | sed s@"_${MONAN_SUFFIX}"@""@g )
      MONAN_ZLAB="${MONAN_ZERO:0:4}-${MONAN_ZERO:4:2}-${MONAN_ZERO:6:2}-${MONAN_ZERO:8-2}"
      echo "   - Process forecasts initialised on ${MONAN_ZLAB}."
      #---~---



      #--- Create temporary NetCDF with extracted sites.
      echo "     ~ Extract site information."
      MONAN_SITE_NC="${TEMP_PATH}/${SITE_TYPE}_SiteData.nc"
      cdo -s -O -L remapnn,"${COORD_FILE}" ${MONAN_FILE} ${MONAN_SITE_NC}
      #---~---



      #--- Create file with the basic data (site, time, longitude, latitude).
      MONAN_SITE_TXT="${TEMP_PATH}/${SITE_TYPE}_SiteData.txt"
      #---~---


      #---~---
      #   Loop through variables, and create one file for each variable.
      #---~---
      for v in ${!VAR_LIST[*]}
      do
         #--- Pick variable.
         VAR_NOW=${VAR_LIST[v]}
         VAR_FILE_NC="${TEMP_PATH}/${SITE_TYPE}_${VAR_NOW}.nc"
         VAR_FILE_TXT="${TEMP_PATH}/${SITE_TYPE}_${VAR_NOW}.txt"
         #---~---


         #--- Extract variable
         cdo -s -O -selname,"${VAR_NOW}" ${MONAN_SITE_NC} ${VAR_FILE_NC}
         #---~---



         #---~---
         #   If this is the first variable, first initialise table data with basic
         # information.
         #---~---
         if [[ ! -s ${MONAN_SITE_TXT} ]]
         then
            echo "     ~ Initialise table data."
            cdo -s -O -outputtab,xind,date,time,timestep,lon,lat ${VAR_FILE_NC}            \
               > ${MONAN_SITE_TXT}
            sed -i.bck s@"#"@""@g               ${MONAN_SITE_TXT}
            /bin/rm -f ${MONAN_SITE_TXT}.bck
         fi
         #---~---



         #---~---
         #   Extract variable data into a table. 
         #---~---
         cdo -s -O -outputtab,value ${VAR_FILE_NC}   > ${VAR_FILE_TXT}
         sed -i.bck s@"#"@""@g               ${VAR_FILE_TXT}
         sed -i.bck s@"value"@"${VAR_NOW}"@g ${VAR_FILE_TXT}
         /bin/rm -f ${VAR_FILE_TXT}.bck
         #---~---




         #--- Append variable.
         STEP_FILE=${TEMP_PATH}/${SITE_TYPE}_Step.txt
         echo "     ~ Append variable \"${VAR_NOW}\"."
         paste ${MONAN_SITE_TXT} ${VAR_FILE_TXT} > ${STEP_FILE}
         /bin/mv -f ${STEP_FILE} ${MONAN_SITE_TXT}
         #---~---
      done
      #---~---


      #---~---
      #   Replace all tabs with spaces
      #---~---
      echo "     ~ Make sure all tabs become spaces and leading spaces are removed."
      sed -i.bck s@"\t"@" "@g            ${MONAN_SITE_TXT}
      sed -i.bck s@"^[[:space:]]*"@""@g  ${MONAN_SITE_TXT}
      sed -i.bck s@"^xind\ "@"ident "@g  ${MONAN_SITE_TXT}
      /bin/rm -f ${MONAN_SITE_TXT}.bck
      #---~---


      #---~---
      #   Loop through each site and replace index with station name.
      #---~---
      echo "     ~ Replace indices with station identifiers."
      THIS_INDEX=0
      for s in ${!IDENT_VEC[*]}
      do
         #---~----
         #   Retrieve information for the current site.
         #---~----
         let THIS_INDEX=${THIS_INDEX}+1
         THIS_IDENT=${IDENT_VEC[s]}
         sed -i.bck s@"^${THIS_INDEX}\ "@"${THIS_IDENT} "@g  ${MONAN_SITE_TXT}
         /bin/rm -f ${MONAN_SITE_TXT}.bck
         #---~----
      done
      #---~---

      #--- Make sure the path for the initialisation time exists.
      OUT_INIT_PATH=${OUT_MONAN_PATH}/${MONAN_PREFIX}_${MONAN_INIT}
      mkdir -pv ${OUT_INIT_PATH}
      #---~---


      #--- Save the file to the permanent archive
      echo "     ~ Save file to the permanent archive."
      MONAN_SITE_ARCH="${OUT_INIT_PATH}/${SITE_TYPE}_${MONAN_BASE}.txt"
      /bin/mv ${MONAN_SITE_TXT} ${MONAN_SITE_ARCH}
      #---~---
   done
   #---~---
done
#---~---



#--- Clean the temporary directory
echo " + Remove the temporary directory."
/bin/rm -fr ${TEMP_PATH}
#---~---
