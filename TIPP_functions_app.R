################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshuajamesdavidthompson@gmail.com
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: Functions for the Shiny App Implementation of TMDL Implementation Progress and Planning (TIPP) Tool
#
# PROJECT INFORMATION:
#   Name: Shiny App of TIPP Tool (functions)
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 04/09/2022    Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================

#load libraries
library(tidyverse)
library(readxl)
library(magrittr)


# calculate loads 
baselineloads <- function(landuse,acres,county,md8digit_cbseg){
  message("Calculating baseline loads based on land use, acres, and bay segment.")
  tss.eos=acres*as.numeric(app_data %>%extract2("tss.lr") %>%
                             dplyr::filter(County==county,
                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                             dplyr::select(landuse))
  tp.eos=acres*as.numeric(app_data %>%extract2("tp.lr") %>%
                            dplyr::filter(County==county,
                                          MD8digit_CBSeg==md8digit_cbseg) %>%
                            dplyr::select(landuse)) 
  tn.eos=acres*as.numeric(app_data %>%extract2("tn.lr") %>%
                            dplyr::filter(County==county,
                                          MD8digit_CBSeg==md8digit_cbseg) %>%
                            dplyr::select(landuse))
  tibble(landuse,tss.eos,tp.eos,tn.eos,tss.eot=tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                    dplyr::select(TSS)),
         tp.eot=tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                    dplyr::select(TP)),
         tn.eot=tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                    dplyr::select(TN)))
}

# calculate septic loads reductions
septic <- function(septictype,number,county,md8digit_cbseg){
  message("Calculating septic nitrogen reductions.")
  septic.eos  <- as.numeric(app_data %>%extract2("bmp") %>%
                              dplyr::filter(BMPShortName==septictype) %>%
                              dplyr::select(AvgNitrogenEfficiencyPct)) *
    as.numeric(app_data %>%extract2("tn.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(Septic)) * number
  tibble(septictype,septic.eos,septic.eot = septic.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                    dplyr::select(TN)))
} 

# calculate shoreline load reductions 
shorelinemanagement <- function(calctype,length,tn,tp,tss,county,md8digit_cbseg){
  if(calctype=="Default Planning"){
    message("Calculating reductions using the default rate, based on length.")
    shore.tss.eos<-length*164
    shore.tp.eos<-length*0.061
    shore.tn.eos<-length*0.086
    tibble(Type=calctype,"Total Feet"=length,shore.tss.eos,shore.tp.eos,shore.tn.eos)
  }else if(calctype=="Manual Input"){
    message("Calculating reductions using user inputs, based on protocol reductions.")
    tibble(Type=calctype,"Total Feet"=length,
           shore.tss.eos=tss,
           shore.tp.eos=tp,
           shore.tn.eos=tn)
  }
  tibble(Type=calctype,"Total Feet"=length,shore.tss.eos=tss,shore.tp.eos=tp,shore.tn.eos=tss, 
         shore.tss.eos=tss*as.numeric(app_data %>%extract2("dfs") %>%
                                        dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                        dplyr::select(TSS)),
         shore.tp.eos=tp*as.numeric(app_data %>%extract2("dfs") %>%
                                      dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                      dplyr::select(TP)),
         shore.tn.eos=tn*as.numeric(app_data %>%extract2("dfs") %>%
                                      dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                      dplyr::select(TN)))
}

# calculate stream restoration load reductions
streamrestoration <- function(calctype,length,tn,tp,tss,county,md8digit_cbseg){
  if(calctype=="Default Planning"){
    message("Calculating reductions using the default rate, based on length.")
    stream.tss.eos<-length*248
    stream.tp.eos<-length*0.068
    stream.tn.eos<-length*0.075
    tibble(Type=calctype,`Total Feet`=length,stream.tss.eos,stream.tp.eos,stream.tn.eos, 
           stream.tss.eot=stream.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                      dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                      dplyr::select(TSS)),
           stream.tp.eot=stream.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                    dplyr::select(TP)),
           stream.tn.eot=stream.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                    dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                    dplyr::select(TN)))
  }else if(calctype=="Manual Input"){
    message("Calculating reductions using user inputs, based on protocol reductions.")
    tibble(Type=calctype,`Total Feet`=length,stream.tss.eos=tss,stream.tp.eos=tp,stream.tn.eos=tss, 
           stream.tss.eot=tss*as.numeric(app_data %>%extract2("dfs") %>%
                                           dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                           dplyr::select(TSS)),
           stream.tp.eot=tp*as.numeric(app_data %>%extract2("dfs") %>%
                                         dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                         dplyr::select(TP)),
           stream.tn.eot=tn*as.numeric(app_data %>%extract2("dfs") %>%
                                         dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                         dplyr::select(TN)))
  }
}

# calculate street sweeping load reductions
streetsweeping <- function(streettype,lanemiles,county,md8digit_cbseg){
  message("Calculating reductions using miles swept.")
  street.tn.eos=as.numeric(app_data %>%extract2("bmp") %>%
                             dplyr::filter(BMPShortName==streettype) %>%
                             dplyr::select(AvgNitrogenEfficiencyPct))*lanemiles*
    as.numeric(app_data %>%extract2("tn.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Impervious Road`))
  
  street.tp.eos=as.numeric(app_data %>%extract2("bmp") %>%
                             dplyr::filter(BMPShortName==streettype) %>%
                             dplyr::select(AvgPhosphorusEfficiencyPct))*lanemiles*
    as.numeric(app_data %>%extract2("tp.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Impervious Road`))
  
  street.tss.eos=as.numeric(app_data %>%extract2("bmp") %>%
                              dplyr::filter(BMPShortName==streettype) %>%
                              dplyr::select(AvgSedimentEfficiencyPct))*lanemiles*
    as.numeric(app_data %>%extract2("tss.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Impervious Road`))
  tibble(streettype,street.tss.eos,street.tp.eos,street.tn.eos,street.tss.eot=street.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                                                          dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                                                          dplyr::select(TSS)),
         street.tp.eot=street.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                  dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                  dplyr::select(TP)),
         street.tn.eot=street.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                  dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                  dplyr::select(TN)))
}

# calculate storm drain cleaning load reductions
stormdraincleaning <- function(material,defaults,tn,tp,lbs,county,md8digit_cbseg){
  storm.tn.eos=if(defaults == "No"){
    message("Calculating reductions using user specified enrichment factors.")
    if(material == "Inorganic"){
      lbs*0.7*tn
    }else if(material == "Organic"){
      lbs*0.2*tn
    }
  }else if(defaults == "Yes"){
    message("Calculating reductions using user default enrichment factors.")
    if(material == "Inorganic"){
      lbs*0.7*0.0027
    }else if(material == "Organic"){
      lbs*0.2*0.0111
    }
  }
  storm.tp.eos=if(defaults == "No"){
    if(material == "Inorganic"){
      lbs*0.7*tp
    }else if(material == "Organic"){
      lbs*0.2*tp
    }
  }else if(defaults == "Yes"){
    if(material == "Inorganic"){
      lbs*0.7*0.0006
    }else if(material == "Organic"){
      lbs*0.2*0.0012
    }
  }
  storm.tss.eos=if(material == "Inorganic"){
    lbs*0.7
  }else if(material == "Organic"){
    lbs*0.2
  }
  tibble(material,storm.tss.eos,storm.tp.eos,storm.tn.eos,storm.tss.eot=storm.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                                                   dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                                                   dplyr::select(TSS)),
         storm.tp.eot=storm.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                dplyr::select(TP)),
         storm.tn.eot=storm.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                dplyr::select(TN)))  
}


# calculate urban nutrient management load reductions
urbannutrientmanagement <- function(urbanname,fert_act,treatturfac,county,md8digit_cbseg){
  urbnm.tn.eos=as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==urbanname) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))*treatturfac*
    as.numeric(app_data %>%extract2("tn.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Turf`))
  urbnm.tp.eos=as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==urbanname) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))*treatturfac*
    as.numeric(app_data %>%extract2("tp.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Turf`))
  if(fert_act=="Yes"){
    message("Calculating reductions using Fert Act - Phosphate Free Ferts.")
    urbnm.tp.eos=urbnm.tp.eos+(0.0413*as.numeric(app_data %>%extract2("tp.lr") %>%
                                                   dplyr::filter(County==county,
                                                                 MD8digit_CBSeg==md8digit_cbseg) %>%
                                                   dplyr::select(`Turf`))*treatturfac)
  }else if(fert_act == "No"){
    message("Calculating reductions by ignoring Fert Act - Assuming Ferts are not Phosphase Free.")
    urbnm.tp.eos = urbnm.tp.eos
  }
  urbnm.tss.eos=as.numeric(app_data %>%extract2("bmp") %>%
                             dplyr::filter(BMPShortName==urbanname) %>%
                             dplyr::select(AvgSedimentEfficiencyPct))*treatturfac*
    as.numeric(app_data %>%extract2("tss.lr") %>%
                 dplyr::filter(County==county,
                               MD8digit_CBSeg==md8digit_cbseg) %>%
                 dplyr::select(`Turf`))
  tibble(urbanname,fert_act,treatturfac,urbnm.tss.eos,urbnm.tp.eos,urbnm.tn.eos,urbnm.tss.eot=urbnm.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                                                                         dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                                                                         dplyr::select(TSS)),
         urbnm.tp.eot=urbnm.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                dplyr::select(TP)),
         urbnm.tn.eot=urbnm.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                dplyr::select(TN))) 
}



# calculate land cover conversion load reductions 
landcoverconversions <- function(lufrom,luto,ac,county,md8digit_cbseg){
  if(luto == "Forest with Buffer") {
    lcc.tn.eos=(as.numeric(app_data %>%extract2("tn.lr") %>%
                             dplyr::filter(County==county,
                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                             dplyr::select(`Turf`)) - 
                  as.numeric(app_data %>%extract2("tn.lr") %>%
                               dplyr::filter(County==county,
                                             MD8digit_CBSeg==md8digit_cbseg) %>%
                               dplyr::select(`Forest`))+(0.25*
                                                           as.numeric(app_data %>%extract2("tn.lr") %>%
                                                                        dplyr::filter(County==county,
                                                                                      MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                        dplyr::select(`Urban`))))*ac
    lcc.tp.eos=(as.numeric(app_data %>%extract2("tp.lr") %>%
                             dplyr::filter(County==county,
                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                             dplyr::select(`Turf`)) - 
                  as.numeric(app_data %>%extract2("tp.lr") %>%
                               dplyr::filter(County==county,
                                             MD8digit_CBSeg==md8digit_cbseg) %>%
                               dplyr::select(`Forest`))+(0.5*
                                                           as.numeric(app_data %>%extract2("tp.lr") %>%
                                                                        dplyr::filter(County==county,
                                                                                      MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                        dplyr::select(`Urban`))))*ac
    lcc.tss.eos=(as.numeric(app_data %>%extract2("tss.lr") %>%
                              dplyr::filter(County==county,
                                            MD8digit_CBSeg==md8digit_cbseg) %>%
                              dplyr::select(`Turf`)) - 
                   as.numeric(app_data %>%extract2("tss.lr") %>%
                                dplyr::filter(County==county,
                                              MD8digit_CBSeg==md8digit_cbseg) %>%
                                dplyr::select(`Forest`))+(0.5*
                                                            as.numeric(app_data %>%extract2("tss.lr") %>%
                                                                         dplyr::filter(County==county,
                                                                                       MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                         dplyr::select(`Urban`))))*ac
  }else if(luto== "Mixed Open with Buffer"){
    lcc.tn.eos=(as.numeric(app_data %>%extract2("tn.lr") %>%
                             dplyr::filter(County==county,
                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                             dplyr::select(`Turf`)) - 
                  as.numeric(app_data %>%extract2("tn.lr") %>%
                               dplyr::filter(County==county,
                                             MD8digit_CBSeg==md8digit_cbseg) %>%
                               dplyr::select(`Mixed Open`))+(0.12*
                                                               as.numeric(app_data %>%extract2("tn.lr") %>%
                                                                            dplyr::filter(County==county,
                                                                                          MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                            dplyr::select(`Urban`))))*ac
    lcc.tp.eos=(as.numeric(app_data %>%extract2("tp.lr") %>%
                             dplyr::filter(County==county,
                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                             dplyr::select(`Turf`)) - 
                  as.numeric(app_data %>%extract2("tp.lr") %>%
                               dplyr::filter(County==county,
                                             MD8digit_CBSeg==md8digit_cbseg) %>%
                               dplyr::select(`Mixed Open`))+(0.15*
                                                               as.numeric(app_data %>%extract2("tp.lr") %>%
                                                                            dplyr::filter(County==county,
                                                                                          MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                            dplyr::select(`Urban`))))*ac
    lcc.tss.eos=(as.numeric(app_data %>%extract2("tss.lr") %>%
                              dplyr::filter(County==county,
                                            MD8digit_CBSeg==md8digit_cbseg) %>%
                              dplyr::select(`Turf`)) - 
                   as.numeric(app_data %>%extract2("tss.lr") %>%
                                dplyr::filter(County==county,
                                              MD8digit_CBSeg==md8digit_cbseg) %>%
                                dplyr::select(`Mixed Open`))+(0*
                                                                as.numeric(app_data %>%extract2("tss.lr") %>%
                                                                             dplyr::filter(County==county,
                                                                                           MD8digit_CBSeg==md8digit_cbseg) %>%
                                                                             dplyr::select(`Urban`))))*ac
  }else{
    lcc.tn.eos=(ac*as.numeric(app_data %>%extract2("tn.lr") %>%
                                dplyr::filter(County==county,
                                              MD8digit_CBSeg==md8digit_cbseg) %>%
                                dplyr::select(lufrom)))- 
      (ac*as.numeric(app_data %>%extract2("tn.lr") %>%
                       dplyr::filter(County==county,
                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                       dplyr::select(luto)))
    lcc.tp.eos=(ac*as.numeric(app_data %>%extract2("tp.lr") %>%
                                dplyr::filter(County==county,
                                              MD8digit_CBSeg==md8digit_cbseg) %>%
                                dplyr::select(lufrom)))- 
      (ac*as.numeric(app_data %>%extract2("tp.lr") %>%
                       dplyr::filter(County==county,
                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                       dplyr::select(luto)))
    lcc.tss.eos=(ac*as.numeric(app_data %>%extract2("tss.lr") %>%
                                 dplyr::filter(County==county,
                                               MD8digit_CBSeg==md8digit_cbseg) %>%
                                 dplyr::select(lufrom)))- 
      (ac*as.numeric(app_data %>%extract2("tss.lr") %>%
                       dplyr::filter(County==county,
                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                       dplyr::select(luto)))
  }
  tibble(lufrom,luto,ac,lcc.tss.eos,lcc.tp.eos,lcc.tn.eos,lcc.tss.eot=lcc.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                                                               dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                                                               dplyr::select(TSS)),
         lcc.tp.eot=lcc.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                            dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                            dplyr::select(TP)),
         lcc.tn.eot=lcc.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                            dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                            dplyr::select(TN))) 
}


# calculate stormwater management BMP load reductions 
stormwatermanagement <- function(stormwatertype,pe,ftw,agimp,improad,impnonroad,turf,county,md8digit_cbseg){
  if(missing(improad)&missing(impnonroad)){
    message("User did not select Impervious Roads and Impervious Non-Roads - using Aggregate Impervious for Calculations")
    if(stormwatertype=="RR"){
      message(paste0("Using RR with Aggregate Impervious. Pe is: ",pe, ". Agregate Impervious is: ",agimp,". Turf is: ",turf))
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0308*pe^5)-(0.2562*pe^4)+(0.8634*pe^3)-(1.5285*pe^2)+(1.501*pe)-0.013))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0308*pe^5)-(0.2562*pe^4)+(0.8634*pe^3)-(1.5285*pe^2)+(1.501*pe)-0.013))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct))))
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0326*pe^5)-(0.2806*pe^4)+(0.9816*pe^3)-(1.8039*pe^2)+(1.8292*pe)-0.0098))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0326*pe^5)-(0.2806*pe^4)+(0.9816*pe^3)-(1.8039*pe^2)+(1.8292*pe)-0.0098))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct))))           
      
    }else if(stormwatertype=="ST"){
      message(paste0("Using ST with Aggregate Impervious. Pe is: ",pe, ". Agregate Impervious is: ",agimp,". Turf is: ",turf))
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0152*pe^5)-(0.131*pe^4)+(0.4581*pe^3)-(0.8418*pe^2)+(0.8536*pe)-0.0046))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0152*pe^5)-(0.131*pe^4)+(0.4581*pe^3)-(0.8418*pe^2)+(0.8536*pe)-0.0046))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct))))
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0239*pe^5)-(0.2058*pe^4)+(0.7198*pe^3)-(1.3229*pe^2)+(1.3414*pe)-0.0072))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0239*pe^5)-(0.2058*pe^4)+(0.7198*pe^3)-(1.3229*pe^2)+(1.3414*pe)-0.0072))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct))))       
      
    }else{
      message(paste0("Using BMP Removal Efficiency with Aggregate Impervious. BMP Type is: ",stormwatertype, ". Agregate Impervious is: ",agimp,". Turf is: ",turf))
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct)))))
      
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct)))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Aggregate Impervious`))*agimp*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgSedimentEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))) 
    }
  }else{
    message("User did not select Aggregate Impervious - using Impervious Roads and Impervious Non-Roads for Calculations")
    if(stormwatertype=="RR"){
      message(paste0("Using RR with Aggregate Impervious. Pe is: ",pe, ". Impervious Roads and Impervious Non-Roads are: ",improad, " and",impnonroad,". Turf is: ",turf))
      
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-((0.0308*pe^5)-(0.2562*pe^4)+(0.8634*pe^3)-(1.5285*pe^2)+(1.501*pe)-0.013))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-((0.0308*pe^5)-(0.2562*pe^4)+(0.8634*pe^3)-(1.5285*pe^2)+(1.501*pe)-0.013))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0308*pe^5)-(0.2562*pe^4)+(0.8634*pe^3)-(1.5285*pe^2)+(1.501*pe)-0.013))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct))))
      
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Impervious Road`))*improad*
        (1-(1-(((0.0326*pe^5)-(0.2806*pe^4)+(0.9816*pe^3)-(1.8039*pe^2)+(1.8292*pe)-0.0098)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(((0.0326*pe^5)-(0.2806*pe^4)+(0.9816*pe^3)-(1.8039*pe^2)+(1.8292*pe)-0.0098)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(((0.0326*pe^5)-(0.2806*pe^4)+(0.9816*pe^3)-(1.8039*pe^2)+(1.8292*pe)-0.0098)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct))))      
      
    }else if(stormwatertype=="ST"){
      message(paste0("Using ST with Aggregate Impervious. Pe is: ",pe, ". Impervious Roads and Impervious Non-Roads are: ",improad, " and",impnonroad,". Turf is: ",turf))
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-((0.0152*pe^5)-(0.131*pe^4)+(0.4581*pe^3)-(0.8418*pe^2)+(0.8536*pe)-0.0046))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-((0.0152*pe^5)-(0.131*pe^4)+(0.4581*pe^3)-(0.8418*pe^2)+(0.8536*pe)-0.0046))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-((0.0152*pe^5)-(0.131*pe^4)+(0.4581*pe^3)-(0.8418*pe^2)+(0.8536*pe)-0.0046))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgNitrogenEfficiencyPct))))
      
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-(((0.0239*pe^5)-(0.2058*pe^4)+(0.7198*pe^3)-(1.3229*pe^2)+(1.3414*pe)-0.0072)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(((0.0239*pe^5)-(0.2058*pe^4)+(0.7198*pe^3)-(1.3229*pe^2)+(1.3414*pe)-0.0072)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(((0.0239*pe^5)-(0.2058*pe^4)+(0.7198*pe^3)-(1.3229*pe^2)+(1.3414*pe)-0.0072)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgPhosphorusEfficiencyPct))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Impervious Road`))*improad*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct)))) +
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(((0.0304*pe^5)-(0.2619*pe^4)+(0.9161*pe^3)-(1.6837*pe^2)+(1.7072*pe)-0.0091)))*
           (1-as.numeric(app_data %>%extract2("bmp") %>%
                           dplyr::filter(BMPShortName==ftw) %>%
                           dplyr::select(AvgSedimentEfficiencyPct))))      
    }else{
      message(paste0("Using BMP Removal Efficiency with Aggregate Impervious. BMP Type is: ",stormwatertype, "Impervious Roads and Impervious Non-Roads are: ",improad, " and",impnonroad,". Turf is: ",turf))
      ##### total nitrogen eos
      uplandstorm.tn.eos=as.numeric(app_data %>%extract2("tn.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgNitrogenEfficiencyPct)))))
      
      ##### total phosphorus eos
      uplandstorm.tp.eos=as.numeric(app_data %>%extract2("tp.lr") %>%
                                      dplyr::filter(County==county,
                                                    MD8digit_CBSeg==md8digit_cbseg) %>%
                                      dplyr::select(`Impervious Road`))*improad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tp.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgPhosphorusEfficiencyPct)))))      
      
      ##### total suspended sediment eos
      uplandstorm.tss.eos=as.numeric(app_data %>%extract2("tss.lr") %>%
                                       dplyr::filter(County==county,
                                                     MD8digit_CBSeg==md8digit_cbseg) %>%
                                       dplyr::select(`Impervious Road`))*improad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgSedimentEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Impervious NonRoad`))*impnonroad*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgSedimentEfficiencyPct)))))+
        as.numeric(app_data %>%extract2("tss.lr") %>%
                     dplyr::filter(County==county,
                                   MD8digit_CBSeg==md8digit_cbseg) %>%
                     dplyr::select(`Turf`))*turf*
        (1-(1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==stormwatertype) %>%
                            dplyr::select(AvgSedimentEfficiencyPct))))*
           (1-(as.numeric(app_data %>%extract2("bmp") %>%
                            dplyr::filter(BMPShortName==ftw) %>%
                            dplyr::select(AvgSedimentEfficiencyPct)))))      
    }  
  }
  
  tibble(stormwatertype,pe,ftw,uplandstorm.tss.eos,uplandstorm.tp.eos,uplandstorm.tn.eos,
         uplandstorm.tss.eot=uplandstorm.tss.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                              dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                              dplyr::select(TSS)),
         uplandstorm.tp.eot=uplandstorm.tp.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                            dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                            dplyr::select(TP)),
         uplandstorm.tn.eot=uplandstorm.tn.eos*as.numeric(app_data %>%extract2("dfs") %>%
                                                            dplyr::filter(MD8digit_CBSeg==md8digit_cbseg)%>%
                                                            dplyr::select(TN))) 
  
}

