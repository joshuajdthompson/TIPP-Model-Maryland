################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  joshuajamesdavidthompson@gmail.com
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: TMDL Implementation Progress and Planning (TIPP) Tool
#
# PROJECT INFORMATION:
#   Name: TMDL Implementation Progress and Planning (TIPP) Tool
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 04/10/2022    Created script                                   JThompson (JT)
#===============================  Environment Setup  ===========================
#==========================================================================================

library(shiny)
library(gfonts)
library(stringr)
library(tidyverse)
library(magrittr)
library(shinythemes)
library(tidyselect)
library(wesanderson)
library(leaflet)
library(ggtext)
library(sf)
library(sp)
library(DT)
library(rmapshaper)
library(shinycssloaders)
library(Cairo)
library(leaflet.extras)
library(tinytex)
library(rmarkdown)

# load data
load("app_data.RData")

# load functions 
source("TIPP_functions_app.R")

#'*things to do*
#'*add years to permit, current, future years*
#'*ggplot summary of progress vs. time*
#'*summary of bmp types per scenario*
#'*summary of progress

# geospatial 

#load shapefile
#md8 <- st_read("geospatial/HYDR_Watersheds8Digit_DNR.shp") %>%
#  st_transform(4326)
#md8_simp <- ms_simplify(md8)
#save(md8_simp, file = "md8_simp.RData")
load("md8_simp.RData")


# set up tables
baselinelanduse_table <- data.frame()
bmps_table <- data.frame()


server <- function(input, output, session){
  hostess <- Hostess$new("loader")
  
  Sys.sleep(1)
  
  for(i in 1:10){
    Sys.sleep(runif(1) / 2)
    hostess$set(i * 10)
  }
  waiter_hide()
  
  
  
  
  #######################
  # update dropdowns
  #######################
  # Update Watershed Segs IDS Based on County Selection ----------------
  observeEvent(input$county, {
    watersheds=unique(app_data %>%extract2("tn.lr") %>%
                     dplyr::filter(County==toupper(input$county)) %>%
                     pull(MD8digit_CBSeg))
    updateSelectizeInput(session,
                         "watershed",
                         selected = NULL,
                         choices = watersheds)
  }) 
  # Update Land Use based on aggregate/disaggregate selection ---------
  observeEvent(input$ag_disag_imp, {
    landuse= if(input$ag_disag_imp == "Yes"){
      c("Impervious Road",
        "Impervious NonRoad","Turf","Septic")  
    }else if(input$ag_disag_imp == "No"){
      c("Aggregate Impervious","Turf","Septic")   
    }
    updateSelectizeInput(session,
                         "baseline_landuse",
                         selected = NULL,
                         choices = landuse)
  })  
  # Update Land Use conversion BMP selection based on aggregate/disaggregate selection ---------
  observeEvent(input$ag_disag_imp, {
    landuse= if(input$ag_disag_imp == "Yes"){
      c("Impervious Road", "Impervious NonRoad", "Turf", "Forest") 
    }else if(input$ag_disag_imp == "No"){
      c("Aggregate Impervious", "Turf", "Forest")   
    }
    updateSelectizeInput(session,
                         "baselinelandconversionconvertingfrom",
                         selected = NULL,
                         choices = landuse)
  }) 
  # Update Land Use conversion BMP selection based on aggregate/disaggregate selection ---------
  observeEvent(input$ag_disag_imp, {
    landuse= if(input$ag_disag_imp == "Yes"){
      c("Turf", "Tree Canopy over Impervious Road", "Tree Canopy over Impervious NonRoad", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer")
    }else if(input$ag_disag_imp == "No"){
      c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer")
    }
    updateSelectizeInput(session,
                         "baselinelandconversionconvertingto",
                         selected = NULL,
                         choices = landuse)
  }) 
  
  #########################################################################
  
  bmps_table <- reactiveVal(bmps_table)
  
  #########################################################################
  # baseline bmps 
  #########################################################################
  
  # baseline - septic
  observeEvent(input$baselineseptic_add_btn, {
    bmpreduction = tibble(bmpid = input$baselinesepticid,feet=NA_real_,acres=NA_real_, lbs = NA_real_, ftw = NA_character_, pe = NA_real_,septic(septictype = input$baselinesepticbmptype, number = input$baselinesepticnumber, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline",
                          tss.red.eos = NA_real_, tp.red.eos = NA_real_, tss.red.eot = NA_real_, tp.red.eot = NA_real_) %>%
      rename(bmptype = septictype,                          
             tn.red.eos = septic.eos,
             tn.red.eot =septic.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # baseline - shoreline
  observeEvent(input$baselineshoreline_add_btn, {
    if(input$baselineshorelinebmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$baselineshorelineid,shorelinemanagement(calctype = input$baselineshorelinebmptype, length = input$baselineshorelinelength, tn = input$baselineshorelinetnprotocol, tp = input$baselineshorelinetpprotocol, tss = input$baselineshorelinetssprotocol, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(Type = "Shoreline Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot)
      
    }else if(input$baselineshorelinebmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$baselineshorelineid,shorelinemanagement(calctype = input$baselineshorelinebmptype, length = input$baselineshorelinelength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(Type = "Shoreline Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot) 
    }
    
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # baseline - stream
  observeEvent(input$baselinestream_add_btn, {
    if(input$baselinestreambmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$baselinestreamid,streamrestoration(calctype = input$baselinestreambmptype, length = input$baselinestreamlength, tn = input$baselinestreamtnprotocol, tp = input$baselinestreamtpprotocol, tss = input$baselinestreamtssprotocol,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(Type = "Stream Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos,
               tp.red.eos =  stream.tp.eos,
               tn.red.eos =  stream.tn.eos,
               tss.red.eot = stream.tss.eot,
               tp.red.eot =  stream.tp.eot,
               tn.red.eot =  stream.tn.eot)
      
    }else if(input$baselinestreambmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$baselinestreamid,streamrestoration(calctype = input$baselinestreambmptype, length = input$baselinestreamlength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(Type = "Stream Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos, 
               tp.red.eos = stream.tp.eos,  
               tn.red.eos = stream.tn.eos,  
               tss.red.eot = stream.tss.eot, 
               tp.red.eot = stream.tp.eot,  
               tn.red.eot = stream.tn.eot)  
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    print(1)
    bmps_table(t)
    
  })
  
  
  # baseline - street sweeping
  observeEvent(input$baselinesweep_add_btn, {
    bmpreduction = tibble(bmpid = input$baselinesweepid,feet=input$baselinesweepmiles*5280,ftw = NA_character_, pe = NA_real_,streetsweeping(streettype = input$baselinesweepbmptype,lanemiles = input$baselinesweepmiles,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
      mutate(acres = NA_real_, lbs = NA_real_,  
             number = 1)%>%
      rename(bmptype = streettype,  
             tss.red.eos = street.tss.eos,
             tp.red.eos =  street.tp.eos,
             tn.red.eos =  street.tn.eos,
             tss.red.eot = street.tss.eot,
             tp.red.eot =  street.tp.eot,
             tn.red.eot =  street.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # baseline - storm drain cleaning
  observeEvent(input$baselinestormdrain_add_btn, {
    if(input$baselinestormdrainenrich == "Yes"){
      bmpreduction = tibble(bmpid = input$baselinestormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$baselinestormdraintype, enrich = input$baselinestormdrainenrich, tn = input$baselinestormdrainenrichtn, tp = input$baselinestormdrainenrichtp, lbs = input$baselinestormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$baselinestormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }else if(input$baselinestormdrainenrich == "No"){
      bmpreduction = tibble(bmpid = input$baselinestormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$baselinestormdraintype, enrich = input$baselinestormdrainenrich, tn = NA_real_, tp = NA_real_, lbs = input$baselinestormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$baselinestormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  # baseline - urban nutrient management
  observeEvent(input$baselineurbnut_add_btn, {
    bmpreduction = tibble(bmpid = input$baselineurbnutid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,urbannutrientmanagement(urbanname = input$baselineurbnutbmptype, fert_act = input$baselineurbnutfertact, treatturfac = input$baselineurbnutacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
      mutate(number = 1, lbs = NA_real_, 
             urbanname=paste0(urbanname, ifelse(input$baselineurbnutfertact == "Yes", " (With Fert. Act)", " (Without Fert. Act)")))%>%
      rename(bmptype = urbanname,
             acres = treatturfac,
             tss.red.eos = urbnm.tss.eos,
             tp.red.eos =  urbnm.tp.eos,
             tn.red.eos =  urbnm.tn.eos,
             tss.red.eot = urbnm.tss.eot,
             tp.red.eot =  urbnm.tp.eot,
             tn.red.eot =  urbnm.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # baseline - land cover conversion
  observeEvent(input$baselinelandconversion_add_btn, {
    bmpreduction = tibble(bmpid = input$baselinelandconversionid,feet=NA_real_, ftw = NA_character_, pe = NA_real_, landcoverconversions(lufrom = input$baselinelandconversionconvertingfrom, luto = input$baselinelandconversionconvertingto, ac = input$baselinelandconversionacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Baseline") %>%
      mutate(number = 1)%>%
      rename(acres = ac,
             tss.red.eos = lcc.tss.eos,
             tp.red.eos =  lcc.tp.eos,
             tn.red.eos =  lcc.tn.eos,
             tss.red.eot = lcc.tss.eot,
             tp.red.eot =  lcc.tp.eot,
             tn.red.eot =  lcc.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # baseline - stormwater management
  observeEvent(input$baselinestormwatermanagement_add_btn, {
    if(input$ag_disag_imp == 'No'){
      bmpreduction = tibble(bmpid = input$baselinestormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$baselinestormwatermanagementbmptype,pe = input$baselinestormwatermanagementpe,ftw = input$baselinestormwatermanagementftw,agimp = input$baselinestormwatermanagementagimp, turf = input$baselinestormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Baseline") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    } else if (input$ag_disag_imp == 'Yes'){
      bmpreduction = tibble(bmpid = input$baselinestormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$baselinestormwatermanagementbmptype,pe = input$baselinestormwatermanagementpe,ftw = input$baselinestormwatermanagementftw,improad = input$baselinestormwatermanagementimproad,impnonroad = input$baselinestormwatermanagementimpnonroad,turf = input$baselinestormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Baseline") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  #########################################################################
  # permit bmps 
  #########################################################################
  
  # permit - septic
  observeEvent(input$permitseptic_add_btn, {
    bmpreduction = tibble(bmpid = input$permitsepticid,feet=NA_real_,acres=NA_real_, lbs = NA_real_, ftw = NA_character_, pe = NA_real_,septic(septictype = input$permitsepticbmptype, number = input$permitsepticnumber, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year",
                          tss.red.eos = NA_real_, tp.red.eos = NA_real_, tss.red.eot = NA_real_, tp.red.eot = NA_real_) %>%
      rename(bmptype = septictype,                          
             tn.red.eos = septic.eos,
             tn.red.eot =septic.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # permit - shoreline
  observeEvent(input$permitshoreline_add_btn, {
    if(input$permitshorelinebmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$permitshorelineid,shorelinemanagement(calctype = input$permitshorelinebmptype, length = input$permitshorelinelength, tn = input$permitshorelinetnprotocol, tp = input$permitshorelinetpprotocol, tss = input$permitshorelinetssprotocol, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(Type = "Shoreline Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot)
      
    }else if(input$permitshorelinebmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$permitshorelineid,shorelinemanagement(calctype = input$permitshorelinebmptype, length = input$permitshorelinelength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(Type = "Shoreline Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot) 
    }
    
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # permit - stream
  observeEvent(input$permitstream_add_btn, {
    if(input$permitstreambmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$permitstreamid,streamrestoration(calctype = input$permitstreambmptype, length = input$permitstreamlength, tn = input$permitstreamtnprotocol, tp = input$permitstreamtpprotocol, tss = input$permitstreamtssprotocol,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(Type = "Stream Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos,
               tp.red.eos =  stream.tp.eos,
               tn.red.eos =  stream.tn.eos,
               tss.red.eot = stream.tss.eot,
               tp.red.eot =  stream.tp.eot,
               tn.red.eot =  stream.tn.eot)
      
    }else if(input$permitstreambmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$permitstreamid,streamrestoration(calctype = input$permitstreambmptype, length = input$permitstreamlength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(Type = "Stream Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos, 
               tp.red.eos = stream.tp.eos,  
               tn.red.eos = stream.tn.eos,  
               tss.red.eot = stream.tss.eot, 
               tp.red.eot = stream.tp.eot,  
               tn.red.eot = stream.tn.eot)  
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
    
  })
  
  
  # permit - street sweeping
  observeEvent(input$permitsweep_add_btn, {
    bmpreduction = tibble(bmpid = input$permitsweepid,feet=input$permitsweepmiles*5280,ftw = NA_character_, pe = NA_real_,streetsweeping(streettype = input$permitsweepbmptype,lanemiles = input$permitsweepmiles,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
      mutate(acres = NA_real_, lbs = NA_real_,  
             number = 1)%>%
      rename(bmptype = streettype,  
             tss.red.eos = street.tss.eos,
             tp.red.eos =  street.tp.eos,
             tn.red.eos =  street.tn.eos,
             tss.red.eot = street.tss.eot,
             tp.red.eot =  street.tp.eot,
             tn.red.eot =  street.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # permit - storm drain cleaning
  observeEvent(input$permitstormdrain_add_btn, {
    if(input$permitstormdrainenrich == "Yes"){
      bmpreduction = tibble(bmpid = input$permitstormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$permitstormdraintype, enrich = input$permitstormdrainenrich, tn = input$permitstormdrainenrichtn, tp = input$permitstormdrainenrichtp, lbs = input$permitstormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$permitstormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }else if(input$permitstormdrainenrich == "No"){
      bmpreduction = tibble(bmpid = input$permitstormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$permitstormdraintype, enrich = input$permitstormdrainenrich, tn = NA_real_, tp = NA_real_, lbs = input$permitstormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$permitstormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  # permit - urban nutrient management
  observeEvent(input$permiturbnut_add_btn, {
    bmpreduction = tibble(bmpid = input$permiturbnutid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,urbannutrientmanagement(urbanname = input$permiturbnutbmptype, fert_act = input$permiturbnutfertact, treatturfac = input$permiturbnutacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
      mutate(number = 1, lbs = NA_real_, 
             urbanname=paste0(urbanname, ifelse(input$permiturbnutfertact == "Yes", " (With Fert. Act)", " (Without Fert. Act)")))%>%
      rename(bmptype = urbanname,
             acres = treatturfac,
             tss.red.eos = urbnm.tss.eos,
             tp.red.eos =  urbnm.tp.eos,
             tn.red.eos =  urbnm.tn.eos,
             tss.red.eot = urbnm.tss.eot,
             tp.red.eot =  urbnm.tp.eot,
             tn.red.eot =  urbnm.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # permit - land cover conversion
  observeEvent(input$permitlandconversion_add_btn, {
    bmpreduction = tibble(bmpid = input$permitlandconversionid,feet=NA_real_, ftw = NA_character_, pe = NA_real_, landcoverconversions(lufrom = input$permitlandconversionconvertingfrom, luto = input$permitlandconversionconvertingto, ac = input$permitlandconversionacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Permit Year") %>%
      mutate(number = 1)%>%
      rename(acres = ac,
             tss.red.eos = lcc.tss.eos,
             tp.red.eos =  lcc.tp.eos,
             tn.red.eos =  lcc.tn.eos,
             tss.red.eot = lcc.tss.eot,
             tp.red.eot =  lcc.tp.eot,
             tn.red.eot =  lcc.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # permit - stormwater management
  observeEvent(input$permitstormwatermanagement_add_btn, {
    if(input$ag_disag_imp == 'No'){
      bmpreduction = tibble(bmpid = input$permitstormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$permitstormwatermanagementbmptype,pe = input$permitstormwatermanagementpe,ftw = input$permitstormwatermanagementftw,agimp = input$permitstormwatermanagementagimp, turf = input$permitstormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Permit Year") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    } else if (input$ag_disag_imp == 'Yes'){
      bmpreduction = tibble(bmpid = input$permitstormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$permitstormwatermanagementbmptype,pe = input$permitstormwatermanagementpe,ftw = input$permitstormwatermanagementftw,improad = input$permitstormwatermanagementimproad,impnonroad = input$permitstormwatermanagementimpnonroad,turf = input$permitstormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Permit Year") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  #########################################################################
  # current bmps 
  #########################################################################
  
  # current - septic
  observeEvent(input$currentseptic_add_btn, {
    bmpreduction = tibble(bmpid = input$currentsepticid,feet=NA_real_,acres=NA_real_, lbs = NA_real_, ftw = NA_character_, pe = NA_real_,septic(septictype = input$currentsepticbmptype, number = input$currentsepticnumber, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year",
                          tss.red.eos = NA_real_, tp.red.eos = NA_real_, tss.red.eot = NA_real_, tp.red.eot = NA_real_) %>%
      rename(bmptype = septictype,                          
             tn.red.eos = septic.eos,
             tn.red.eot =septic.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # current - shoreline
  observeEvent(input$currentshoreline_add_btn, {
    if(input$currentshorelinebmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$currentshorelineid,shorelinemanagement(calctype = input$currentshorelinebmptype, length = input$currentshorelinelength, tn = input$currentshorelinetnprotocol, tp = input$currentshorelinetpprotocol, tss = input$currentshorelinetssprotocol, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(Type = "Shoreline Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot)
      
    }else if(input$currentshorelinebmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$currentshorelineid,shorelinemanagement(calctype = input$currentshorelinebmptype, length = input$currentshorelinelength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(Type = "Shoreline Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot) 
    }
    
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # current - stream
  observeEvent(input$currentstream_add_btn, {
    if(input$currentstreambmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$currentstreamid,streamrestoration(calctype = input$currentstreambmptype, length = input$currentstreamlength, tn = input$currentstreamtnprotocol, tp = input$currentstreamtpprotocol, tss = input$currentstreamtssprotocol,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(Type = "Stream Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos,
               tp.red.eos =  stream.tp.eos,
               tn.red.eos =  stream.tn.eos,
               tss.red.eot = stream.tss.eot,
               tp.red.eot =  stream.tp.eot,
               tn.red.eot =  stream.tn.eot)
      
    }else if(input$currentstreambmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$currentstreamid,streamrestoration(calctype = input$currentstreambmptype, length = input$currentstreamlength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(Type = "Stream Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos, 
               tp.red.eos = stream.tp.eos,  
               tn.red.eos = stream.tn.eos,  
               tss.red.eot = stream.tss.eot, 
               tp.red.eot = stream.tp.eot,  
               tn.red.eot = stream.tn.eot)  
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
    
  })
  
  
  # current - street sweeping
  observeEvent(input$currentsweep_add_btn, {
    bmpreduction = tibble(bmpid = input$currentsweepid,feet=input$currentsweepmiles*5280,ftw = NA_character_, pe = NA_real_,streetsweeping(streettype = input$currentsweepbmptype,lanemiles = input$currentsweepmiles,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
      mutate(acres = NA_real_, lbs = NA_real_,  
             number = 1)%>%
      rename(bmptype = streettype,  
             tss.red.eos = street.tss.eos,
             tp.red.eos =  street.tp.eos,
             tn.red.eos =  street.tn.eos,
             tss.red.eot = street.tss.eot,
             tp.red.eot =  street.tp.eot,
             tn.red.eot =  street.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # current - storm drain cleaning
  observeEvent(input$currentstormdrain_add_btn, {
    if(input$currentstormdrainenrich == "Yes"){
      bmpreduction = tibble(bmpid = input$currentstormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$currentstormdraintype, enrich = input$currentstormdrainenrich, tn = input$currentstormdrainenrichtn, tp = input$currentstormdrainenrichtp, lbs = input$currentstormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$currentstormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }else if(input$currentstormdrainenrich == "No"){
      bmpreduction = tibble(bmpid = input$currentstormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$currentstormdraintype, enrich = input$currentstormdrainenrich, tn = NA_real_, tp = NA_real_, lbs = input$currentstormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$currentstormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  # current - urban nutrient management
  observeEvent(input$currenturbnut_add_btn, {
    bmpreduction = tibble(bmpid = input$currenturbnutid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,urbannutrientmanagement(urbanname = input$currenturbnutbmptype, fert_act = input$currenturbnutfertact, treatturfac = input$currenturbnutacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
      mutate(number = 1, lbs = NA_real_, 
             urbanname=paste0(urbanname, ifelse(input$currenturbnutfertact == "Yes", " (With Fert. Act)", " (Without Fert. Act)")))%>%
      rename(bmptype = urbanname,
             acres = treatturfac,
             tss.red.eos = urbnm.tss.eos,
             tp.red.eos =  urbnm.tp.eos,
             tn.red.eos =  urbnm.tn.eos,
             tss.red.eot = urbnm.tss.eot,
             tp.red.eot =  urbnm.tp.eot,
             tn.red.eot =  urbnm.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # current - land cover conversion
  observeEvent(input$currentlandconversion_add_btn, {
    bmpreduction = tibble(bmpid = input$currentlandconversionid,feet=NA_real_, ftw = NA_character_, pe = NA_real_, landcoverconversions(lufrom = input$currentlandconversionconvertingfrom, luto = input$currentlandconversionconvertingto, ac = input$currentlandconversionacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario="Current Year") %>%
      mutate(number = 1)%>%
      rename(acres = ac,
             tss.red.eos = lcc.tss.eos,
             tp.red.eos =  lcc.tp.eos,
             tn.red.eos =  lcc.tn.eos,
             tss.red.eot = lcc.tss.eot,
             tp.red.eot =  lcc.tp.eot,
             tn.red.eot =  lcc.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # current - stormwater management
  observeEvent(input$currentstormwatermanagement_add_btn, {
    if(input$ag_disag_imp == 'No'){
      bmpreduction = tibble(bmpid = input$currentstormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$currentstormwatermanagementbmptype,pe = input$currentstormwatermanagementpe,ftw = input$currentstormwatermanagementftw,agimp = input$currentstormwatermanagementagimp, turf = input$currentstormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Current Year") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    } else if (input$ag_disag_imp == 'Yes'){
      bmpreduction = tibble(bmpid = input$currentstormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$currentstormwatermanagementbmptype,pe = input$currentstormwatermanagementpe,ftw = input$currentstormwatermanagementftw,improad = input$currentstormwatermanagementimproad,impnonroad = input$currentstormwatermanagementimpnonroad,turf = input$currentstormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario="Current Year") %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  #########################################################################
  # future bmps 
  #########################################################################
  
  # future - septic
  observeEvent(input$futureseptic_add_btn, {
    bmpreduction = tibble(bmpid = input$futuresepticid,feet=NA_real_,acres=NA_real_, lbs = NA_real_, ftw = NA_character_, pe = NA_real_,septic(septictype = input$futuresepticbmptype, number = input$futuresepticnumber, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario,
                          tss.red.eos = NA_real_, tp.red.eos = NA_real_, tss.red.eot = NA_real_, tp.red.eot = NA_real_) %>%
      rename(bmptype = septictype,                          
             tn.red.eos = septic.eos,
             tn.red.eot =septic.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # future - shoreline
  observeEvent(input$futureshoreline_add_btn, {
    if(input$futureshorelinebmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$futureshorelineid,shorelinemanagement(calctype = input$futureshorelinebmptype, length = input$futureshorelinelength, tn = input$futureshorelinetnprotocol, tp = input$futureshorelinetpprotocol, tss = input$futureshorelinetssprotocol, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(Type = "Shoreline Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot)
      
    }else if(input$futureshorelinebmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$futureshorelineid,shorelinemanagement(calctype = input$futureshorelinebmptype, length = input$futureshorelinelength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(Type = "Shoreline Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = shore.tss.eos,
               tp.red.eos =  shore.tp.eos,
               tn.red.eos =  shore.tn.eos,
               tss.red.eot = shore.tss.eot,
               tp.red.eot =  shore.tp.eot,
               tn.red.eot =  shore.tn.eot) 
    }
    
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # future - stream
  observeEvent(input$futurestream_add_btn, {
    if(input$futurestreambmptype == "Protocol Reductions"){
      bmpreduction = tibble(bmpid = input$futurestreamid,streamrestoration(calctype = input$futurestreambmptype, length = input$futurestreamlength, tn = input$futurestreamtnprotocol, tp = input$futurestreamtpprotocol, tss = input$futurestreamtssprotocol,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(Type = "Stream Restoration (Protocol Reductions)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos,
               tp.red.eos =  stream.tp.eos,
               tn.red.eos =  stream.tn.eos,
               tss.red.eot = stream.tss.eot,
               tp.red.eot =  stream.tp.eot,
               tn.red.eot =  stream.tn.eot)
      
    }else if(input$futurestreambmptype == "Default Rate"){
      bmpreduction = tibble(bmpid = input$futurestreamid,streamrestoration(calctype = input$futurestreambmptype, length = input$futurestreamlength, tn = NA_real_, tp = NA_real_, tss = NA_real_, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(Type = "Stream Restoration (Planning Rate)",
               acres = NA_real_, 
               number = 1, lbs = NA_real_, ftw = NA_character_, pe = NA_real_)%>%
        rename(bmptype = Type,  
               feet = `Total Feet`,
               tss.red.eos = stream.tss.eos, 
               tp.red.eos = stream.tp.eos,  
               tn.red.eos = stream.tn.eos,  
               tss.red.eot = stream.tss.eot, 
               tp.red.eot = stream.tp.eot,  
               tn.red.eot = stream.tn.eot)  
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
    
  })
  
  
  # future - street sweeping
  observeEvent(input$futuresweep_add_btn, {
    bmpreduction = tibble(bmpid = input$futuresweepid,feet=input$futuresweepmiles*5280,ftw = NA_character_, pe = NA_real_,streetsweeping(streettype = input$futuresweepbmptype,lanemiles = input$futuresweepmiles,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
      mutate(acres = NA_real_, lbs = NA_real_,  
             number = 1)%>%
      rename(bmptype = streettype,  
             tss.red.eos = street.tss.eos,
             tp.red.eos =  street.tp.eos,
             tn.red.eos =  street.tn.eos,
             tss.red.eot = street.tss.eot,
             tp.red.eot =  street.tp.eot,
             tn.red.eot =  street.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  # future - storm drain cleaning
  observeEvent(input$futurestormdrain_add_btn, {
    if(input$futurestormdrainenrich == "Yes"){
      bmpreduction = tibble(bmpid = input$futurestormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$futurestormdraintype, enrich = input$futurestormdrainenrich, tn = input$futurestormdrainenrichtn, tp = input$futurestormdrainenrichtp, lbs = input$futurestormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$futurestormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }else if(input$futurestormdrainenrich == "No"){
      bmpreduction = tibble(bmpid = input$futurestormdrainid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,stormdraincleaning(material = input$futurestormdraintype, enrich = input$futurestormdrainenrich, tn = NA_real_, tp = NA_real_, lbs = input$futurestormdrainlbs,county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
        mutate(material = paste0("Storm Drain Cleaning (",input$futurestormdraintype,")"),
               acres = NA_real_, 
               number = 1)%>%
        rename(bmptype = material,  
               tss.red.eos = storm.tss.eos,
               tp.red.eos =  storm.tp.eos,
               tn.red.eos =  storm.tn.eos,
               tss.red.eot = storm.tss.eot,
               tp.red.eot =  storm.tp.eot,
               tn.red.eot =  storm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  # future - urban nutrient management
  observeEvent(input$futureurbnut_add_btn, {
    bmpreduction = tibble(bmpid = input$futureurbnutid,feet=NA_real_,ftw = NA_character_, pe = NA_real_,urbannutrientmanagement(urbanname = input$futureurbnutbmptype, fert_act = input$futureurbnutfertact, treatturfac = input$futureurbnutacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
      mutate(number = 1, lbs = NA_real_, 
             urbanname=paste0(urbanname, ifelse(input$futureurbnutfertact == "Yes", " (With Fert. Act)", " (Without Fert. Act)")))%>%
      rename(bmptype = urbanname,
             acres = treatturfac,
             tss.red.eos = urbnm.tss.eos,
             tp.red.eos =  urbnm.tp.eos,
             tn.red.eos =  urbnm.tn.eos,
             tss.red.eot = urbnm.tss.eot,
             tp.red.eot =  urbnm.tp.eot,
             tn.red.eot =  urbnm.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # future - land cover conversion
  observeEvent(input$futurelandconversion_add_btn, {
    bmpreduction = tibble(bmpid = input$futurelandconversionid,feet=NA_real_, ftw = NA_character_, pe = NA_real_, landcoverconversions(lufrom = input$futurelandconversionconvertingfrom, luto = input$futurelandconversionconvertingto, ac = input$futurelandconversionacres, county = toupper(input$county), md8digit_cbseg = input$watershed),scenario=input$futurescenario) %>%
      mutate(number = 1)%>%
      rename(acres = ac,
             tss.red.eos = lcc.tss.eos,
             tp.red.eos =  lcc.tp.eos,
             tn.red.eos =  lcc.tn.eos,
             tss.red.eot = lcc.tss.eot,
             tp.red.eot =  lcc.tp.eot,
             tn.red.eot =  lcc.tn.eot)
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  
  
  # future - stormwater management
  observeEvent(input$futurestormwatermanagement_add_btn, {
    if(input$ag_disag_imp == 'No'){
      bmpreduction = tibble(bmpid = input$futurestormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$futurestormwatermanagementbmptype,pe = input$futurestormwatermanagementpe,ftw = input$futurestormwatermanagementftw,agimp = input$futurestormwatermanagementagimp, turf = input$futurestormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario=input$futurescenario) %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    } else if (input$ag_disag_imp == 'Yes'){
      bmpreduction = tibble(bmpid = input$futurestormwatermanagementid,feet=NA_real_,stormwatermanagement(stormwatertype = input$futurestormwatermanagementbmptype,pe = input$futurestormwatermanagementpe,ftw = input$futurestormwatermanagementftw,improad = input$futurestormwatermanagementimproad,impnonroad = input$futurestormwatermanagementimpnonroad,turf = input$futurestormwatermanagementturf,county = toupper(input$county), md8digit_cbseg=input$watershed),scenario=input$futurescenario) %>%
        mutate(number = 1)%>%
        rename(bmptype = stormwatertype, 
               tss.red.eos = uplandstorm.tss.eos,
               tp.red.eos =  uplandstorm.tp.eos,
               tn.red.eos =  uplandstorm.tn.eos,
               tss.red.eot = uplandstorm.tss.eot,
               tp.red.eot =  uplandstorm.tp.eot,
               tn.red.eot =  uplandstorm.tn.eot)
    }
    print(bmpreduction)
    t = bind_rows(bmpreduction,bmps_table())
    bmps_table(t)
  })
  
  #########################################################################
  # Delete from BMP Table
  #########################################################################
  
  observeEvent(input$bmp_delete_btn, { 
    t = bmps_table()
    print(nrow(t))
    if (!is.null(input$bmps_table_rows_selected)) {
      t <- t[-as.numeric(input$bmps_table_rows_selected),]
    }
    bmps_table(t)
  })
  
  
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  baselinelanduse_table <- reactiveVal(baselinelanduse_table)
  
  observeEvent(input$baselinelanduse_add_btn, {
    baselineLoads=baselineloads(landuse = input$baseline_landuse,acres = input$landuse_ac, county = toupper(input$county), md8digit_cbseg=input$watershed)
    print(baselineLoads)
    t = rbind(baselineLoads,baselinelanduse_table())
    baselinelanduse_table(t)
  })
  
  observeEvent(input$baselinelanduse_delete_btn, {
    t = baselinelanduse_table()
    print(nrow(t))
    if (!is.null(input$baselinelanduse_table_rows_selected)) {
      t <- t[-as.numeric(input$baselinelanduse_table_rows_selected),]
    }
    baselinelanduse_table(t)
  })
  
  output$baselinelanduse_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Please select baseline land use.")
    )
    datatable(baselinelanduse_table(), selection = 'single', options = list(dom = 't'),colnames = c("Land Use","Acres/Systems","TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  

  output$baselineloads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter land use and/or BMP data for baseline year.")
    )

    if(nrow(bmps_table())>0){
      totalbaseline = bind_cols(baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
        filter(scenario == "Baseline")%>%
        summarise(across(is.numeric, sum, na.rm = TRUE))%>%
        select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      totalbaseline = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(totalbaseline) 
    }
    datatable(totalbaseline, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  output$permitloads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for permit year.")
    )
    if(nrow(bmps_table())>0){
      totalpermit = bind_cols(baselinelanduse_table() %>%
                                  select(-c("landuse","acres")) %>%
                                  summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                  filter(scenario %in% c("Baseline","Permit Year"))%>%
                                  summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                  select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      totalpermit = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(totalpermit) 
    }
    datatable(totalpermit, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })

  output$currentloads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for current year.")
    )
    if(nrow(bmps_table())>0){
      totalcurrent = bind_cols(baselinelanduse_table() %>%
                                select(-c("landuse","acres")) %>%
                                summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                filter(scenario %in% c("Baseline","Permit Year","Current Year"))%>%
                                summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      totalcurrent = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(totalcurrent) 
    }
    datatable(totalcurrent, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  output$futureloads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for future scenarios.")
    )
    if(nrow(bmps_table())>0){
      totalfuture = bind_cols(baselinelanduse_table() %>%
                                 select(-c("landuse","acres")) %>%
                                 summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                 filter(scenario %in% c("Baseline","Permit Year","Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
                                 summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                 select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      totalfuture = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(totalfuture) 
    }
    datatable(totalfuture, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  output$milestone1loads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for the Milestone 1 scenario.")
    )
    if(nrow(bmps_table())>0){
      milestone1 = bind_cols(baselinelanduse_table() %>%
                                select(-c("landuse","acres")) %>%
                                summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                filter(scenario %in% c("Baseline","Permit Year","Current Year", "Milestone 1"))%>%
                                summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      milestone1 = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(milestone1) 
    }
    datatable(milestone1, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  output$milestone2loads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for the Milestone 2 scenario.")
    )
    if(nrow(bmps_table())>0){
      milestone2 = bind_cols(baselinelanduse_table() %>%
                               select(-c("landuse","acres")) %>%
                               summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                               filter(scenario %in% c("Baseline","Permit Year","Current Year", "Milestone 1", "Milestone 2"))%>%
                               summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                               select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
    }else{
      milestone2 = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(milestone2) 
    }
    datatable(milestone2, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  output$plannedloads_table <- renderDT({
    validate(
      need(nrow(baselinelanduse_table())>0, "Enter BMP data for the Planned scenario.")
    )
    if(nrow(bmps_table())>0){
      planned = bind_cols(baselinelanduse_table() %>%
                               select(-c("landuse","acres")) %>%
                               summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                               filter(scenario %in% c("Baseline","Permit Year","Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
                               summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                               select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
        select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))
      print(planned)
    }else{
      planned = baselinelanduse_table() %>%
        select(-c("landuse","acres")) %>%
        summarise(across(is.numeric, sum, na.rm = TRUE)) %>%
        mutate(across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))
      print(planned) 
    }
    datatable(planned, selection = 'single', options = list(dom = 't'),colnames = c("TSS (lbs - EOS)"," TP (lbs - EOS)","TN (lbs - EOS)","TSS (lbs - EOT)"," TP (lbs - EOT)","TN (lbs - EOT)"),rownames = FALSE)
  })
  
  ##########
  #renderDataTable
  output$bmps_table <- renderDataTable({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    
    assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    
    all_bmps = bmps_table()%>%
      select(c(bmpid, bmptype,feet,acres,lbs, number, ftw, pe, starts_with(impair), scenario))%>%
      select(c(bmpid, bmptype,feet,acres,lbs, number, ftw, pe, ends_with(paste0(".",assess_unit)), scenario))%>%
      mutate(across(!c(bmpid, bmptype,feet,acres,lbs, number, ftw, pe,scenario), round, 1))
    all_bmps = as.data.frame(all_bmps)
    
    if(input$permittocurrent == 0){
      all_bmps = all_bmps %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    }
    
    
    print(all_bmps)
    datatable(all_bmps, selection = 'single', colnames = c("BMP ID","BMP Type", "Feet", "Acres", "Pounds","Number","FTW","Pe (in)", paste0(input$impairmentParam, " Load Reduction (lbs - ",toupper(assess_unit),")"),"Scenario"),options = list(searching = TRUE), rownames = FALSE)
  })
  
  #===================================================
  # TMDL Progress 
  #===================================================
  
  tmdlloadsummary <- reactive({
    if(nrow(bmps_table())>0){
      
      
      impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                      ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                             ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
      

        totalbaseline = bind_cols(baselinelanduse_table() %>%
                                    select(-c("landuse","acres")) %>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                    filter(scenario == "Baseline")%>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                    select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
          mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
                 tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
                 tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
                 tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
                 tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
                 tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
                 across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>%
          select(c(tss.eos,tp.eos,tn.eos,tss.eot,tp.eot,tn.eot))

      summary_baseline = totalbaseline %>% 
        select(starts_with(impair))%>%
        rename(load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(reductionLoad.eos = (1-(input$reductionPerc/100))*load.eos,
               reductionLoad.eot = (1-(input$reductionPerc/100))*load.eot,
               reduction.eos = load.eos-reductionLoad.eos,
               reduction.eot = load.eot-reductionLoad.eot,
               year=input$baselineYear,
               scenario="Baseline Year",
               progressPerc.eos = input$reductionPerc,
               progressPerc.eot = input$reductionPerc) 
      

      summary_permit = bind_cols(baselinelanduse_table() %>%
                                   select(-c("landuse","acres")) %>%
                                   summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                   filter(scenario %in% c("Baseline","Permit Year","Permit Year/Current Year"))%>%
                                   summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                   select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>% 
        select(starts_with(impair))%>%
        rename(reduction.eos=paste0(impair,".red.eos"),
               reduction.eot=paste0(impair,".red.eot"),
               load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(progressPerc.eos = ((summary_baseline$load.eos-load.eos)/summary_baseline$load.eos)*100,
               progressPerc.eot = ((summary_baseline$load.eot-load.eot)/summary_baseline$load.eot)*100,
               year = ifelse(!is.null(input$permityear),as.numeric(input$permityear),NA_real_),
               scenario="Permit Year",
               reductionLoad.eos = NA_real_,
               reductionLoad.eot = NA_real_)
      

      summary_current = bind_cols(baselinelanduse_table() %>%
                                    select(-c("landuse","acres")) %>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                    filter(scenario %in% c("Baseline","Permit Year","Current Year","Permit Year/Current Year"))%>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                    select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>% 
        select(starts_with(impair))%>%
        rename(reduction.eos=paste0(impair,".red.eos"),
               reduction.eot=paste0(impair,".red.eot"),
               load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(progressPerc.eos = ((summary_baseline$load.eos-load.eos)/summary_baseline$load.eos)*100,
               progressPerc.eot = ((summary_baseline$load.eot-load.eot)/summary_baseline$load.eot)*100,
               year = ifelse(!is.null(input$currentyear),as.numeric(input$currentyear),NA_real_),
               scenario="Current Year",
               reductionLoad.eos = NA_real_,
               reductionLoad.eot = NA_real_)
      

      summary_milestone1 = bind_cols(baselinelanduse_table() %>%
                                       select(-c("landuse","acres")) %>%
                                       summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                       filter(scenario %in% c("Baseline","Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1"))%>%
                                       summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                       select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>% 
        select(starts_with(impair))%>%
        rename(reduction.eos=paste0(impair,".red.eos"),
               reduction.eot=paste0(impair,".red.eot"),
               load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(progressPerc.eos = ((summary_baseline$load.eos-load.eos)/summary_baseline$load.eos)*100,
               progressPerc.eot = ((summary_baseline$load.eot-load.eot)/summary_baseline$load.eot)*100,
               year=ifelse(!is.null(input$milestone1year),as.numeric(input$milestone1year),NA_real_),
               scenario="Milestone 1",
               reductionLoad.eos = NA_real_,
               reductionLoad.eot = NA_real_)
      

      summary_milestone2 = bind_cols(baselinelanduse_table() %>%
                                       select(-c("landuse","acres")) %>%
                                       summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                       filter(scenario %in% c("Baseline","Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2"))%>%
                                       summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                       select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>% 
        select(starts_with(impair))%>%
        rename(reduction.eos=paste0(impair,".red.eos"),
               reduction.eot=paste0(impair,".red.eot"),
               load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(progressPerc.eos = ((summary_baseline$load.eos-load.eos)/summary_baseline$load.eos)*100,
               progressPerc.eot = ((summary_baseline$load.eot-load.eot)/summary_baseline$load.eot)*100,
               year=ifelse(!is.null(input$milestone2year),as.numeric(input$milestone2year),NA_real_),
               scenario="Milestone 2",
               reductionLoad.eos = NA_real_,
               reductionLoad.eot = NA_real_)
      

      summary_planned = bind_cols(baselinelanduse_table() %>%
                                    select(-c("landuse","acres")) %>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE)),bmps_table() %>%
                                    filter(scenario %in% c("Baseline","Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
                                    summarise(across(is.numeric, sum, na.rm = TRUE))%>%
                                    select(tss.red.eos, tp.red.eos, tn.red.eos, tss.red.eot, tp.red.eot, tn.red.eot))%>%
        mutate(tss.eos = pull(., tss.eos) - ifelse(is.na(pull(., tss.red.eos)),0,pull(., tss.red.eos)),
               tp.eos = pull(., tp.eos) - ifelse(is.na(pull(., tp.red.eos)),0,pull(., tp.red.eos)),
               tn.eos  = pull(., tn.eos) - ifelse(is.na(pull(., tn.red.eos)),0,pull(., tn.red.eos)),
               tss.eot = pull(., tss.eot) - ifelse(is.na(pull(., tss.red.eot)),0,pull(., tss.red.eot)),
               tp.eot = pull(., tp.eot) - ifelse(is.na(pull(., tp.red.eot)),0,pull(., tp.red.eot)),
               tn.eot = pull(., tn.eot) - ifelse(is.na(pull(., tn.red.eot)),0,pull(., tn.red.eot)),
               across(c(tss.eos, tp.eos, tn.eos, tss.eot, tp.eot, tn.eot), round, 1))%>% 
        select(starts_with(impair))%>%
        rename(reduction.eos=paste0(impair,".red.eos"),
               reduction.eot=paste0(impair,".red.eot"),
               load.eos=paste0(impair,".eos"),
               load.eot=paste0(impair,".eot"))%>%
        mutate(progressPerc.eos = ((summary_baseline$load.eos-load.eos)/summary_baseline$load.eos)*100,
               progressPerc.eot = ((summary_baseline$load.eot-load.eot)/summary_baseline$load.eot)*100,
               year=ifelse(!is.null(input$plannedyear),as.numeric(input$plannedyear),NA_real_),
               scenario="Planned",
               reductionLoad.eos = NA_real_,
               reductionLoad.eot = NA_real_ )
      

      tmdlloadsummary =  bind_rows(summary_baseline, summary_permit, 
                                   summary_current, summary_milestone1, 
                                   summary_milestone2, summary_planned)%>%
        filter(!is.na(year))%>%
        mutate(across(is.numeric, round, 1))
      
      tmdlloadsummary
      

    }
  })
  
  output$red_perc <- renderText({
    validate(
      need(input$reductionPerc >0, "Please enter reduction requirement.")
    )
    assess_unit_text = ifelse(input$eos_eot == 0, "edge of stream", "edge of tide")
    paste0(input$reductionPerc,"% ",tolower(input$impairmentParam)," load reduction from the ",input$baselineYear, " baseline year at the ",assess_unit_text,".")})
  
  
  #===================================================
  # Baseline Summary Table
  #=================================================== 
  
  baselinetmdlsummary_table <- reactive({
    assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    baselinetmdlsummary_table= tmdlloadsummary()%>%
      filter(scenario == "Baseline Year")%>%
      select(ends_with(paste0(".",assess_unit)))%>%
      select(paste0("load.",assess_unit),paste0("reductionLoad.",assess_unit),paste0("reduction.",assess_unit))
    
    names(baselinetmdlsummary_table) = c(paste0("Baseline Load"),
                                          paste0("Target Load"),
                                          paste0("Required Load Reduction"))
    
    baselinetmdlsummary_table = baselinetmdlsummary_table%>%
      pivot_longer(names_to = "Type", values_to = "Load (lbs)",cols = everything())
    baselinetmdlsummary_table  
  })
  
  output$baselinetmdlsummary_table <- renderDataTable({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    datatable(baselinetmdlsummary_table(), selection = 'none', options = list(dom='t',searching = FALSE),rownames = FALSE)
  })
  
    #===================================================
    # BMP Loads Summary Table
    #=================================================== 
      
  bmploadssummary_table <- reactive({
    assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    
    if(input$permittocurrent == 0){
      bmp_tab = bmps_table() %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } else {bmp_tab = bmps_table()}
    

    bmploadssummary_table = bmp_tab %>%
      filter(scenario %in% c("Baseline","Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
      mutate(BMPGroup = ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                     filter(Sector=="Septic") %>%
                                                     pull(BMPFullName)), "Septic BMPs", 
                               ifelse(bmptype %in% c("Shoreline Restoration (Protocol Reductions)",
                                                     "Shoreline Restoration (Planning Rate)",
                                                     "Stream Restoration (Protocol Reductions)",
                                                     "Stream Restoration (Planning Rate)",unique(app_data %>%extract2("bmp") %>%
                                                                                                         filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                                         pull(BMPFullName)), c("Storm Drain Cleaning (Organic)","Storm Drain Cleaning (Inorganic)"), 
                                                     paste0(unique(app_data %>%extract2("bmp") %>%
                                                                     filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                     pull(BMPFullName))," (With Fert. Act)"),
                                                     paste0(unique(app_data %>%extract2("bmp") %>%
                                                                     filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                     pull(BMPFullName))," (Without Fert. Act)")), "Alternative BMPs",
                                      ifelse(bmptype %in% pull(crossing(lufrom = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"), 
                                                                        luto = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", 
                                                                                 "Tree Canopy over Impervious Road","Tree Canopy over Impervious NonRoad", 
                                                                                 "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest",
                                                                                 "Forest with Buffer")) %>% mutate(lcc = paste0(lufrom," to ",luto)) %>% select(lcc)), "Land Cover Conversion BMPs", 
                                             ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>% pull(BMPFullName)), "Stormwater Management BMPs",NA_real_))))
      ) %>%
      group_by(scenario, BMPGroup)%>%
    summarise(across(is.numeric, sum, na.rm = TRUE))%>%
      mutate(across(is.numeric, round, 1))%>%
      select(scenario, BMPGroup, starts_with(impair))%>%
      select(scenario, BMPGroup, ends_with(paste0(".",assess_unit)))
    names(bmploadssummary_table) <- c("Scenario", "BMP Group", paste0(input$impairmentParam," ",toupper(assess_unit)," Load Reduction (lbs)"))
    bmploadssummary_table
  })
  
  output$bmploadssummary_table <- renderDataTable({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    datatable(bmploadssummary_table(), selection = 'none', options = list(dom='t',searching = FALSE),rownames = FALSE)
  })

  #===================================================
  # BMP Implementation Table
  #===================================================  
  
  bmpimplementation_table <- reactive({
   if(input$permittocurrent == 0){
      bmp_tab = bmps_table() %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } else {bmp_tab = bmps_table()}
    
    bmpimplementation_table = bmp_tab %>%
      filter(scenario %in% c("Baseline","Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
      mutate(`BMP Type` = ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                       filter(Sector=="Septic") %>%
                                                       pull(BMPFullName)), "Septic BMPs", 
                                 ifelse(bmptype %in% c("Shoreline Restoration (Protocol Reductions)",
                                                       "Shoreline Restoration (Planning Rate)"), "Shoreline Management",
                                        ifelse(bmptype %in% c("Stream Restoration (Protocol Reductions)",
                                                              "Stream Restoration (Planning Rate)"), "Stream Restoration",
                                               ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>% filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                            pull(BMPFullName)), "Street Sweeping", 
                                                      ifelse(bmptype %in% c("Storm Drain Cleaning (Organic)","Storm Drain Cleaning (Inorganic)"), "Storm Drain Cleaning",
                                                             ifelse(bmptype %in% c(paste0(unique(app_data %>%extract2("bmp") %>%
                                                                                                   filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                              "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                                   pull(BMPFullName))," (With Fert. Act)"),
                                                                                   paste0(unique(app_data %>%extract2("bmp") %>%
                                                                                                   filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                              "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                                   pull(BMPFullName))," (Without Fert. Act)")), "Urban Nitrient Management",
                                                                    ifelse(bmptype %in% pull(crossing(lufrom = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"), 
                                                                                                      luto = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", 
                                                                                                               "Tree Canopy over Impervious Road","Tree Canopy over Impervious NonRoad", 
                                                                                                               "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest",
                                                                                                               "Forest with Buffer")) %>% mutate(lcc = paste0(lufrom," to ",luto)) %>% select(lcc)), "Land Cover Conversion BMPs", 
                                                                           ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                                                                        filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                                                   "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                                                   "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                                                   "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                                                   "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>% pull(BMPFullName)), "Stormwater Management BMPs",NA_real_))))))))
      ) %>%
      group_by(scenario, `BMP Type`)%>%
      summarise(across(c(acres,feet,number,lbs), sum, na.rm = TRUE))%>%
      mutate(across(c(acres,feet,number,lbs), round, 0))%>%
      pivot_longer(cols = c(acres,feet,number,lbs),
                   values_to = "Num",
                   names_to = "Units")%>%
      mutate(Units = ifelse(`BMP Type` == "Street Sweeping" & Units == "feet","miles",Units),
             Num = ifelse(`BMP Type` == "Street Sweeping" & Units == "miles",round(Num/5280,0),Num))%>%
      pivot_wider(names_from = scenario, values_from = Num)%>%
      filter(rowSums(across(c(-`BMP Type`, -Units)),na.rm = T) > 0) %>%
      mutate(`Total (all scenarios)` = rowSums(across(c(-`BMP Type`, -Units)),na.rm = T),
             Units= str_to_title(Units) )%>% 
      relocate(Units, .after = `Total (all scenarios)`) %>%
      filter(!(`BMP Type` != "Septic BMPs" & Units == "Number")) 
    bmpimplementation_table
  })
  
  output$bmpimplementation_table <- renderDataTable({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    datatable(bmpimplementation_table(), selection = 'none', options = list(dom='t',searching = FALSE),rownames = FALSE)
  })
  
  #===================================================
  # TMDL Summary Table
  #===================================================  
  
  tmdlsummary_table <- reactive({
  assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    tmdlsummary_table= tmdlloadsummary()%>%
      filter(scenario != "Baseline Year")%>%
      select(scenario,year,ends_with(paste0(".",assess_unit)))%>%
      select(scenario,year,paste0("load.",assess_unit), paste0("progressPerc.",assess_unit),paste0("reduction.",assess_unit))
    
    if(input$permittocurrent == 0){
      currentyear = tmdlsummary_table %>%
        filter(scenario == "Permit Year")%>%
        mutate(scenario = "Current Year")
      tmdlsummary_table = bind_rows(tmdlsummary_table,currentyear)%>%
        arrange(.,year)
    } 
    names(tmdlsummary_table) = c("Scenario","Year", paste0(input$impairmentParam, " ", toupper(assess_unit)," Load (lbs)"),paste0(input$impairmentParam, " ", toupper(assess_unit), " Reduction (%)"),paste0(input$impairmentParam, " ",toupper(assess_unit), " Load Reduction (lbs)"))
    tmdlsummary_table  
  })
  
  output$tmdlsummary_table <- renderDataTable({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    datatable(tmdlsummary_table(), selection = 'none', options = list(dom='t',searching = FALSE),rownames = FALSE)
    })
  
  #===================================================
  # Plot
  #===================================================  
  
  loadplotsummary <- reactive({
    assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    
    if(input$permittocurrent == 0){
      bmp_tab = bmps_table() %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } else {bmp_tab = bmps_table()}
    
    bmpreduction = bmp_tab %>%
      filter(scenario %in% c("Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
      mutate(group = ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                  filter(Sector=="Septic") %>%
                                                  pull(BMPFullName)), "Septic BMP Load Reduction", 
                            ifelse(bmptype %in% c("Shoreline Restoration (Protocol Reductions)",
                                                  "Shoreline Restoration (Planning Rate)",
                                                  "Stream Restoration (Protocol Reductions)",
                                                  "Stream Restoration (Planning Rate)",unique(app_data %>%extract2("bmp") %>%
                                                                                                filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                                pull(BMPFullName)), c("Storm Drain Cleaning (Organic)","Storm Drain Cleaning (Inorganic)"), 
                                                  paste0(unique(app_data %>%extract2("bmp") %>%
                                                                  filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                             "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                  pull(BMPFullName))," (With Fert. Act)"),
                                                  paste0(unique(app_data %>%extract2("bmp") %>%
                                                                  filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                             "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                  pull(BMPFullName))," (Without Fert. Act)")), "Alternative BMP Load Reduction",
                                   ifelse(bmptype %in% pull(crossing(lufrom = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"), 
                                                                     luto = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", 
                                                                              "Tree Canopy over Impervious Road","Tree Canopy over Impervious NonRoad", 
                                                                              "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest",
                                                                              "Forest with Buffer")) %>% mutate(lcc = paste0(lufrom," to ",luto)) %>% select(lcc)), "Land Cover BMP Load Reduction", 
                                          ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                                       filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                  "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                  "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                  "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                  "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>% pull(BMPFullName)), "Stormwater BMP Load Reduction",NA_character_))))
      ) %>%
      group_by(scenario, group)%>%
      summarise(across(is.numeric, sum, na.rm = TRUE))%>%
      mutate(across(is.numeric, round, 1))%>%
      select(scenario, group, starts_with(impair))%>%
      select(scenario, group, ends_with(paste0(".",assess_unit)))%>%
      mutate(Year = ifelse(scenario == "Permit Year", as.numeric(input$permityear),
                           ifelse(scenario == "Current Year", as.numeric(input$currentyear),
                                  ifelse(scenario == "Permit Year/Current Year", as.numeric(input$permityear),
                                         ifelse(scenario == "Milestone 1", as.numeric(input$milestone1year),
                                                ifelse(scenario == "Milestone 2", as.numeric(input$milestone2year),
                                                       ifelse(scenario == "Planned",as.numeric(input$plannedyear),NA_real_)))))))
    
    names(bmpreduction) <- c("Scenario", "group", "Load", "Year") 
    
    tmdl_loadssummary_table= tmdlloadsummary()%>%
      select(scenario,year,ends_with(paste0(".",assess_unit)))%>%
      select(scenario,year,paste0("load.",assess_unit))%>%
      mutate(group = "Load")
    
    if(input$permittocurrent == 0){
      tmdl_loadssummary_table = tmdl_loadssummary_table %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } 
    
    targetLoad = pull(tmdl_loadssummary_table%>%
                        filter(scenario == "Baseline Year") %>%
                        select(paste0("load.",assess_unit)))*(1-(input$reductionPerc/100))
    
    names(tmdl_loadssummary_table) = c("Scenario","Year", "Load", "group")
    
    dfPlot = bind_rows(bmpreduction,tmdl_loadssummary_table)%>%
      mutate(label = "TMDL Progress")
    
    dfPlot =  dfPlot %>% 
      select(-c(Scenario)) %>%
      full_join(crossing(dfPlot$group, dfPlot$Year)%>%
                  mutate(label = "TMDL Progress")%>%
                  rename(group=1,
                         Year=2))%>% 
      mutate_if(is.numeric,coalesce,0)%>% 
      group_by(group) %>%
      arrange(Year) %>% 
      mutate(Load = case_when(group == "Load" ~ Load, group != "Load" ~ cumsum(Load)))
    
    order=pull(unique(dfPlot %>% group_by(group) %>% arrange(Load) %>% select(group)))
    
    dfPlot = dfPlot %>%
      mutate(group = factor(group,levels=order,ordered=TRUE))
    
    ggplot(dfPlot, aes(x=Year, y=Load, fill=group)) +
      facet_wrap(~label,ncol=1) +
      geom_area(alpha=0.6 , size=0.5, colour="black") + 
      scale_fill_brewer(palette = "Paired")+ guides(fill=guide_legend("")) +
      xlab("Year") + ylab(paste0(input$impairmentParam, " Load (lbs - ",toupper(assess_unit),")"))+ 
      theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
            strip.background = element_rect(fill="black"),
            strip.text = element_text(colour = 'white',face="bold",size = 12),
            panel.background = element_rect(fill="white", color="white", size=0),
            plot.background = element_rect(fill="white", color="white", size=0),
            panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                              colour = "lightgrey"),
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                              colour = "lightgrey"),
            panel.border=element_rect(colour="black",size=1,fill=NA),
            axis.text.y=element_text(colour="black"),
            legend.background = element_rect(colour = NA), legend.spacing.x = unit(0, "pt"),legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'))+
      geom_hline(yintercept = targetLoad,linetype=2, size=1)+
      annotate("richtext", x=as.numeric(input$baselineYear)+1, y=targetLoad*1.15, col="white", 
               label="Target Load", size=4,hjust=0, fill= "black")
  })
  
  output$loadplot <- renderPlot({
    validate(
      need(nrow(bmps_table())>0, "Enter load and BMP data.")
    )
    
    assess_unit = ifelse(input$eos_eot == 0, "eos", "eot")
    impair = ifelse(input$impairmentParam == "Total Suspended Sediments", "tss", 
                    ifelse(input$impairmentParam == "Total Phosphorus", "tp", 
                           ifelse(input$impairmentParam == "Total Nitrogen", "tn")))
    
    if(input$permittocurrent == 0){
      bmp_tab = bmps_table() %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } else {bmp_tab = bmps_table()}
    
    bmpreduction = bmp_tab %>%
      filter(scenario %in% c("Permit Year","Current Year", "Permit Year/Current Year", "Milestone 1", "Milestone 2", "Planned"))%>%
      mutate(group = ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                     filter(Sector=="Septic") %>%
                                                     pull(BMPFullName)), "Septic BMP Load Reduction", 
                               ifelse(bmptype %in% c("Shoreline Restoration (Protocol Reductions)",
                                                     "Shoreline Restoration (Planning Rate)",
                                                     "Stream Restoration (Protocol Reductions)",
                                                     "Stream Restoration (Planning Rate)",unique(app_data %>%extract2("bmp") %>%
                                                                                                   filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                                   pull(BMPFullName)), c("Storm Drain Cleaning (Organic)","Storm Drain Cleaning (Inorganic)"), 
                                                     paste0(unique(app_data %>%extract2("bmp") %>%
                                                                     filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                     pull(BMPFullName))," (With Fert. Act)"),
                                                     paste0(unique(app_data %>%extract2("bmp") %>%
                                                                     filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                     pull(BMPFullName))," (Without Fert. Act)")), "Alternative BMP Load Reduction",
                                      ifelse(bmptype %in% pull(crossing(lufrom = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"), 
                                                                        luto = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", 
                                                                                 "Tree Canopy over Impervious Road","Tree Canopy over Impervious NonRoad", 
                                                                                 "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest",
                                                                                 "Forest with Buffer")) %>% mutate(lcc = paste0(lufrom," to ",luto)) %>% select(lcc)), "Land Cover BMP Load Reduction", 
                                             ifelse(bmptype %in% unique(app_data %>%extract2("bmp") %>%
                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>% pull(BMPFullName)), "Stormwater BMP Load Reduction",NA_character_))))
      ) %>%
      group_by(scenario, group)%>%
      summarise(across(is.numeric, sum, na.rm = TRUE))%>%
      mutate(across(is.numeric, round, 1))%>%
      select(scenario, group, starts_with(impair))%>%
      select(scenario, group, ends_with(paste0(".",assess_unit)))%>%
      mutate(Year = ifelse(scenario == "Permit Year", as.numeric(input$permityear),
                                  ifelse(scenario == "Current Year", as.numeric(input$currentyear),
                                         ifelse(scenario == "Permit Year/Current Year", as.numeric(input$permityear),
                                                ifelse(scenario == "Milestone 1", as.numeric(input$milestone1year),
                                                       ifelse(scenario == "Milestone 2", as.numeric(input$milestone2year),
                                                              ifelse(scenario == "Planned",as.numeric(input$plannedyear),NA_real_)))))))
    
    names(bmpreduction) <- c("Scenario", "group", "Load", "Year") 
    
    tmdl_loadssummary_table= tmdlloadsummary()%>%
      select(scenario,year,ends_with(paste0(".",assess_unit)))%>%
      select(scenario,year,paste0("load.",assess_unit))%>%
      mutate(group = "Load")
    
    if(input$permittocurrent == 0){
      tmdl_loadssummary_table = tmdl_loadssummary_table %>%
        mutate(scenario = ifelse(scenario == "Permit Year", "Permit Year/Current Year", scenario))
    } 
    
    targetLoad = pull(tmdl_loadssummary_table%>%
      filter(scenario == "Baseline Year") %>%
      select(paste0("load.",assess_unit)))*(1-(input$reductionPerc/100))
    
    names(tmdl_loadssummary_table) = c("Scenario","Year", "Load", "group")
    
    dfPlot = bind_rows(bmpreduction,tmdl_loadssummary_table)%>%
      mutate(label = "TMDL Progress")
    
    dfPlot =  dfPlot %>% 
      select(-c(Scenario)) %>%
      full_join(crossing(dfPlot$group, dfPlot$Year)%>%
                  mutate(label = "TMDL Progress")%>%
                  rename(group=1,
                         Year=2))%>% 
      mutate_if(is.numeric,coalesce,0)%>% 
      group_by(group) %>%
      arrange(Year) %>% 
      mutate(Load = case_when(group == "Load" ~ Load, group != "Load" ~ cumsum(Load)))
  
    order=pull(unique(dfPlot %>% group_by(group) %>% arrange(Load) %>% select(group)))

    dfPlot = dfPlot %>%
      mutate(group = factor(group,levels=order,ordered=TRUE))

    ggplot(dfPlot, aes(x=Year, y=Load, fill=group)) +
      facet_wrap(~label,ncol=1) +
      geom_area(alpha=0.6 , size=0.5, colour="black") + 
      scale_fill_brewer(palette = "Paired")+ guides(fill=guide_legend("")) +
      xlab("Year") + ylab(paste0(input$impairmentParam, " Load (lbs - ",toupper(assess_unit),")"))+ 
      theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust=0.5,colour="black"),
            strip.background = element_rect(fill="black"),
            strip.text = element_text(colour = 'white',face="bold",size = 12),
            panel.background = element_rect(fill="#d3e0d7", color="#d3e0d7", size=0),
            plot.background = element_rect(fill="#d3e0d7", color="#d3e0d7", size=0),
            panel.grid.major.x = element_line(size = 0.5, linetype = 'solid',
                                              colour = "lightgrey"),
            panel.grid.major.y = element_line(size = 0.5, linetype = 'solid',
                                              colour = "lightgrey"),
            panel.border=element_rect(colour="black",size=1,fill=NA),
            axis.text.y=element_text(colour="black"),
            legend.background = element_rect(colour = 'black', fill = '#d3e0d7', linetype='solid'), legend.spacing.x = unit(0, "pt"),legend.margin=margin(t=-0.5,l=0.05,b=0.05,r=0.05, unit='cm'))+
      geom_hline(yintercept = targetLoad,linetype=2, size=1)+
      annotate("richtext", x=as.numeric(input$baselineYear)+1, y=targetLoad*1.1, col="white", 
               label="Target Load", size=4,hjust=0, fill= "black")

  },  bg="#d3e0d7")
 
  
  watershedNam <- reactive({
    watershedNam = str_to_title(unique(app_data %>%extract2("dfs") %>%
             filter(MD8digit_CBSeg == input$watershed) %>%
             pull(Name)))
    watershedNam
    
  })
  
  targetloadText <- reactive({
    targetloadText = prettyNum(baselinetmdlsummary_table() %>%
                                 filter(Type == "Target Load") %>%
                                 pull(`Load (lbs)`), big.mark=",")
    targetloadText
  })
  
  loadreductionText <- reactive({
    loadreductionText= prettyNum(baselinetmdlsummary_table() %>%
                                   filter(Type == "Required Load Reduction") %>%
                                   pull(`Load (lbs)`), big.mark=",")
    loadreductionText
  })
  
  EOSEOT <- reactive({ifelse(input$eos_eot == 0, "Edge of Stream", "Edge of Tide")})
  
  #===================================================
  # MD8 Map
  #===================================================
  output$md8map <-renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE), group = "Stamen Base Map")%>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri Imagery") %>%
      addPolygons(data = md8_simp, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = wes_palette('Darjeeling1', type = c("continuous")),#"#b7f5b5", # color is function of the wbd AREA column
                  fillOpacity = 0.35,
                  popup = paste("MD8-Digit: ", str_to_title(md8_simp$mde8digt), "<br>"),
                  highlightOptions = highlightOptions(color = "white",
                                                      opacity = 1.0,
                                                      weight = 2,
                                                      bringToFront = TRUE))%>%
      
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-3) %>%
      addLayersControl(baseGroups = c("Stamen Base Map","Esri Imagery"),
                       options = layersControlOptions(collapsed = FALSE))

  })
  
  #===================================================
  # Download Report
  #===================================================
  output$downloadReport <- downloadHandler(
    # name pdf output here
    filename = function() {
      paste(paste0('MDE_TIPPToolReport_',Sys.Date()), sep = '.', PDF = 'pdf')
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      tempimage <- file.path(tempdir(), "MDELogo_Vertical_BlackWhite.png")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      file.copy("MDELogo_Vertical_BlackWhite.png", tempimage, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(planName = input$planName,
                     baselineYear = input$baselineYear,
                     watershed = watershedNam(),
                     EOSEOT = EOSEOT(),
                     impairmentParam = input$impairmentParam,
                     reductionPerc = input$reductionPerc,
                     targetloadText = targetloadText(),
                     loadreductionText = loadreductionText(),
                     baselinetmdlsummary_table = baselinetmdlsummary_table(),
                     tmdlsummary_table = tmdlsummary_table(),
                     bmploadssummary_table = bmploadssummary_table(),
                     bmpimplementation_table = bmpimplementation_table(),
                     loadplotsummary = loadplotsummary())
      
      # Knit the document, passing in the `params` list
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv(),pdf_document())
      )
    }
  )

}