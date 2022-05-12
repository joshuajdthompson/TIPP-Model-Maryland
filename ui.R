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
library(ggtext)
library(wesanderson)
library(waiter)
library(leaflet)
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

# user interface
ui = fluidPage(id = 'fP',
               useWaiter(),
               useHostess(),
               waiterShowOnLoad(
                 color = "#d3e0d7",
                 hostess_loader(
                   "loader", 
                   preset = "circle",
                   stroke_color = "#0c7444",
                   text_color = "black",
                   class = "label-center",
                   center_page = TRUE
                 )
               ),#tags$style("body {background-color:#9ec7b4;}"), 
               tags$link(rel = "stylesheet", type = "text/css", href = "css/montserrat.css"),
               tags$head(HTML("<title>TMDL Implementation Progress and Planning (TIPP) Tool</title> <link rel='icon' type='image/gif/png' href='MDELogo_Symbol.png'>"),
                         tags$style(HTML(
                           "
              .navbar{background-color: #0c7444 !important; padding-left: 20px; margin-left:-20px; padding-right: 15px; margin-right:-15px;padding-top: 20px; margin-top:-20px;}
              .navbar-default .navbar-brand:hover {color: blue;}
              .navbar { background-color: gray;}
              .navbar-default .navbar-nav > li > a {color:white;}
              .navbar-default .navbar-nav > .active > a,
              .navbar-default .navbar-nav > .active > a:focus,
              .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #3d9069;}
              .navbar-default .navbar-nav > li > a:hover {color: #fbd01e;background-color:#3d9069;text-decoration:underline;}
              .butt{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              .btn-file{background-color:#505250; color:#FFFFFF; border-color:#080808;}
              * {font-family: 'Montserrat', sans-serif;}
              "
                         ))),
               titlePanel(
                 fluidRow(style = "background-color:#0c7444; padding-top: 20px; margin-top:-20px; padding-bottom: 20px; margin-bottom:-20px",
                          column(9,h4(("TMDL Implementation Progress and Planning (TIPP) Tool"),style="font-size:26px;font-style:normal; font-weight: 400; color:#FFFFFF;"),
                                 p("This is an R Shiny Implimentation of MDE's TMDL Implementation Progress and Planning (TIPP) Tool.",style="font-size:18px;font-style:normal; font-weight: 400; color:#FFFFFF;"),
                                 p("Note: this tool is provided 'as is' without warranty of any kind, either expressed, implied, or statutory.",style="font-size:11.5px;font-style:italic;color:#FFFFFF;"),
                                 p("The user assumes the entire risk as to quality and performance of the data from this tool.",style="font-size:11.5px;font-style:italic;color:#FFFFFF;"),br(),
                                 a(actionButton(inputId = "email1", label = "   Contact MDE",icon = icon("envelope", lib = "font-awesome"),
                                                style = "background-color:#505250; color:#FFFFFF; border-color:#080808"),href="mailto:joshuajamesdavidthompson@gmail.com"),
                                 a(actionButton(inputId = "github1", label = "  Contact Developer",icon = icon("github", lib = "font-awesome"),
                                                style = "background-color:#505250; color:#FFFFFF; border-color:#080808"),href="https://github.com/joshuajdthompson",target="_blank")),
                          column(3, tags$a(img(src='MDELogo_Horizontal_WhiteText-01.png', align = "right",height = 1800*0.1, width = 4519*0.1, style="padding: 0px")))
                          #div(style="margin-bottom:10px")
                 )),
               br(),
               navbarPage("",
                          
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## #######################################  Home Page  #######################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#             
                          
                          tabPanel(icon("home"),
                                   
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(6,h4(strong("TMDL Watershed Information"),style="font-size:20px;font-style:normal; font-weight: 400; color: black"),br(),
                                                      column(12, align = "center", radioButtons(inputId = "eos_eot", label="Edge of Stream (EOS) or Edge of Tide (EOT)?",
                                                                                                choiceNames=c("EOS","EOT"),
                                                                                                choiceValues=c(0,1),
                                                                                                selected= 1,
                                                                                                inline=TRUE)
                                                      ), br(),
                                                      textInput("planName", "Name of Implementation Plan", width = NULL),br(),
                                                      column(6,style="padding-left: 15px; margin-left:-15px",
                                                             selectizeInput(inputId = "county", 
                                                                            label = "County",
                                                                            choices = unique(app_data %>%extract2("tn.lr") %>%
                                                                                               mutate(County = str_to_title(County))%>%
                                                                                               pull(County)),
                                                                            selected = NULL,
                                                                            multiple = TRUE, 
                                                                            options = list(maxItems = 1)),
                                                             selectizeInput(inputId = "watershed",
                                                                            label = "Watershed",
                                                                            choices = unique(app_data %>%extract2("tn.lr") %>%
                                                                                               pull(MD8digit_CBSeg)),
                                                                            selected = NULL,
                                                                            multiple = TRUE, 
                                                                            options = list(maxItems = 1)),
                                                             numericInput("baselineYear", "Baseline Year", width = NULL,value = NULL)),
                                                      column(6,style="padding-right: 20px; margin-right:-20px",
                                                             selectizeInput(inputId = "ag_disag_imp",
                                                                            label = "Use Disaggregated Impervious?",
                                                                            choices = c("Yes","No"),
                                                                            selected = NULL,
                                                                            multiple = TRUE, 
                                                                            options = list(maxItems = 1)),
                                                             selectizeInput(inputId = "impairmentParam", 
                                                                            label = "Impairment Parameter",
                                                                            choices = c("Total Nitrogen","Total Phosphorus","Total Suspended Sediments"),
                                                                            selected = NULL,
                                                                            multiple = TRUE, 
                                                                            options = list(maxItems = 1)),
                                                             numericInput("reductionPerc", "Reduction Percent (%)", width = NULL,value = NULL)
                                                      ),br()
                                               ),
                                               column(6,leafletOutput("md8map")%>% withSpinner(color="black")))
                                             
                                   )
                          ),
                          
                          
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## #######################################  Upload Page  #####################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#              
                          
                          tabPanel("Bulk Upload Data",icon=icon("database"),#,
                                   wellPanel(style = "background-color: #e3d9af; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             
                                             fluidRow(column(12,h4(strong("Upload BMP and Land Use Data"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"),br(),
                                                             p("Data can be entered manually in the following sections.  However, if data are already aggregated, BMPs and TMDL watershed land use can be uploaded in bulk.  Bulk uploading requires that data follow strict formatting.",
                                                               style="font-size:16px;font-style:normal; font-weight: 400; color: black;"),
                                                             p("Please download an example input that shows the required format.  The file that is uploaded must be a comma separated file, and can either be a csv spreadsheet or a text file.",
                                                               style="font-size:16px;font-style:normal; font-weight: 400; color: black;"))
                                             ),
                                             br(),
                                             fluidRow(column(4,fileInput("upload", "Bulk Upload All Baseline BMPs (CSV Format)",
                                                                         multiple = FALSE,
                                                                         accept = c("text/csv",
                                                                                    "text/comma-separated-values,text/plain",
                                                                                    ".csv"))),#,
                                                      column(3,br(),downloadButton('downloadbmpexmpl', 'Download Example Input',class = "butt")))
                                   ) # wellpanel
                                   
                          ),
                          
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################  Baseline Loads  #######################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("Baseline Load", value = "1", fluid = TRUE,#icon=icon("info"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Baseline Load with Baseline BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("baselineloads_table")%>% withSpinner(color="#e6f7ff"))),br(),
                                             fluidRow(
                                               column(9,h4(strong("User-Entered Land Use"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("baselinelanduse_table")%>% withSpinner(color="#e6f7ff")),br(),br())),# column
                                   #br(),
                                   wellPanel(style = "background-color: #e3d9af; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Enter Baseline Land Use"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"))),
                                             column(4,
                                                    selectizeInput(inputId = "baseline_landuse",
                                                                   label = "Baseline Land Use",
                                                                   choices = c("Aggregate Impervious","Impervious Road",
                                                                               "Impervious NonRoad","Turf","Septic"),
                                                                   selected = NULL,
                                                                   multiple = TRUE, 
                                                                   options = list(maxItems = 1))),
                                             column(3,
                                                    numericInput("landuse_ac",
                                                                 label = "Acres (ac) or Number of Septics",
                                                                 value = NULL, width = '100%')),br(),
                                             fluidRow(column(4,
                                                             actionButton("baselinelanduse_add_btn", "Add Land Use", class = "butt"),
                                                             actionButton("baselinelanduse_delete_btn", "Delete Land Use", class = "butt")))
                                             
                                   ), #wellpanel
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Enter Baseline BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),
                                               column(4, 
                                                      selectizeInput(inputId = "baselinebmpclass", 
                                                                     label = "Select class of BMP to add:",
                                                                     choices = c("Septic","Shoreline Restoration","Stream Restoration",
                                                                                 "Street Sweeping", "Storm Drain Cleaning", "Urban Nutrient Mananagement",
                                                                                 "Land Use Conversion","Stormwater Management"),
                                                                     selected = NULL,
                                                                     multiple = TRUE, 
                                                                     options = list(maxItems = 1))),
                                               column(4,
                                                      conditionalPanel("input.baselinebmpclass == 'Septic'",
                                                                       actionButton("baselineseptic_add_btn", "Add Septic BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Shoreline Restoration'",
                                                                       actionButton("baselineshoreline_add_btn", "Add Shoreline BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Stream Restoration'",
                                                                       actionButton("baselinestream_add_btn", "Add Stream BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Street Sweeping'",
                                                                       actionButton("baselinesweep_add_btn", "Add Street Sweeping BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Storm Drain Cleaning'",
                                                                       actionButton("baselinestormdrain_add_btn", "Add Storm Drain Cleaning BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Urban Nutrient Mananagement'",
                                                                       actionButton("baselineurbnut_add_btn", "Add Urban Nutrient BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Land Use Conversion'",
                                                                       actionButton("baselinelandconversion_add_btn", "Add Land Use BMP", class = "butt")),
                                                      conditionalPanel("input.baselinebmpclass == 'Stormwater Management'",
                                                                       actionButton("baselinestormwatermanagement_add_btn", "Add Stormwater BMP", class = "butt"))
                                               ),
                                             ),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Septic'",
                                                 column(3,
                                                        textInput("baselinesepticid", "ID of Septic(s)", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinesepticbmptype", 
                                                                       label = "Septic BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(Sector=="Septic") %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        numericInput("baselinesepticnumber",
                                                                     label = "Number of Septic Systems",
                                                                     value = NULL, width = '100%'))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Shoreline Restoration'",
                                                 column(3,
                                                        textInput("baselineshorelineid", "ID of Shoreline Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("baselineshorelinelength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselineshorelinebmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.baselineshorelinebmptype == 'Protocol Reductions'",
                                                          numericInput("baselineshorelinetnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("baselineshorelinetpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("baselineshorelinetssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Stream Restoration'",
                                                 column(3,
                                                        textInput("baselinestreamid", "ID of Stream Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("baselinestreamlength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinestreambmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.baselinestreambmptype == 'Protocol Reductions'",
                                                          numericInput("baselinestreamtnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("baselinestreamtpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("baselinestreamtssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Street Sweeping'",
                                                 column(3,
                                                        textInput("baselinesweepid", "ID(s) of Street Sweeping", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinesweepbmptype", 
                                                                       label = "Street Sweeping Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("baselinesweepmiles", "Street Sweeping Lane Miles (mi)", width = NULL,value = NULL))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Storm Drain Cleaning'",
                                                 column(3,
                                                        textInput("baselinestormdrainid", "ID(s) of Storm Drain Cleaning", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinestormdraintype", 
                                                                       label = "Material Type",
                                                                       choices = c("Organic", "Inorganic"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        numericInput("baselinestormdrainlbs", "Mass of material recovered (lbs)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinestormdrainenrich", 
                                                                       label = "Use Nutrient Enrichment Factors?",
                                                                       choices = c("Yes", "No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.baselinestormdrainenrich == 'Yes'",
                                                          numericInput("baselinestormdrainenrichtn", "Total Nitrogen Enrichment Factor", width = NULL,value = NULL),
                                                          numericInput("baselinestormdrainenrichtp", "Total Phosphorus Enrichment Factor", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Urban Nutrient Mananagement'",
                                                 column(3,
                                                        textInput("baselineurbnutid", "ID(s) of Urban Nutrient Management", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselineurbnutbmptype", 
                                                                       label = "Urban Nutrient Management Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                     "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,         
                                                        selectizeInput(inputId = "baselineurbnutfertact", 
                                                                       label = "Does the Fertilizer Act apply to this BMP?",
                                                                       choices = c("Yes","No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("baselineurbnutacres", "Total treated turf acres (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Land Use Conversion'",
                                                 column(3,
                                                        textInput("baselinelandconversionid", "ID(s) of Land Use Conversion", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "baselinelandconversionconvertingfrom", 
                                                                       label = "Converting From",
                                                                       choices = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        selectizeInput(inputId = "baselinelandconversionconvertingto", 
                                                                       label = "Converting To",
                                                                       choices = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", "Tree Canopy over Impervious Road", "Tree Canopy over Impervious NonRoad", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("baselinelandconversionacres", "Acres Converted (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.baselinebmpclass == 'Stormwater Management'",
                                                 column(3,
                                                        textInput("baselinestormwatermanagementid", "ID(s) of Stormwater Management BMP", width = NULL)),
                                                 column(4,       
                                                        selectizeInput(inputId = "baselinestormwatermanagementbmptype", 
                                                                       label = "Stormwater BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        selectizeInput(inputId = "baselinestormwatermanagementftw", 
                                                                       label = "Floating Treatment Wetland Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("NO","FTW1","FTW2","FTW3","FTW4","FTW5"))) %>%
                                                                         pull(BMPFullName),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        conditionalPanel(condition = "input.baselinestormwatermanagementbmptype == 'Stormwater Performance Standard-Runoff Reduction' || input.baselinestormwatermanagementbmptype == 'Stormwater Performance Standard-Stormwater Treatment'",
                                                                         numericInput("baselinestormwatermanagementpe", "Pe (Inches)", width = NULL,value = NULL))),
                                                 column(3,
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'No'",
                                                                         numericInput("baselinestormwatermanagementagimp", "Aggregate Impervious Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("baselinestormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL)),
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'Yes'",
                                                                         numericInput("baselinestormwatermanagementimproad", "Impervious Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("baselinestormwatermanagementimpnonroad", "Impervious Non-Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("baselinestormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL))
                                                 )
                                               )
                                             )
                                   )
                                   
                                   
                          ),#fluidpage
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################  Permit Load  ##########################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("Permit Load", value = "2", fluid = TRUE,#icon=icon("info"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Permit Load with Permit Year BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("permitloads_table")%>% withSpinner(color="#e6f7ff")),br(),br())),# well panel
                                   #br(),
                                   wellPanel(style = "background-color: #e3d9af; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(6,h4(strong("Enter Permit Load BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"))),
                                             fluidRow(
                                               column(2, selectizeInput(inputId = "permityear", 
                                                                        label = "Permit Year",
                                                                        selected = NULL,
                                                                        multiple = TRUE,
                                                                        choices = c(seq(from = 1990, to = 2025, by =1)),
                                                                        options = list(maxItems = 1))),
                                               column(3, radioButtons(inputId = "permittocurrent", label="Carry over BMPs to Current Year ",
                                                                      choiceNames=c("Yes","No"),
                                                                      choiceValues=c(0,1),
                                                                      selected= 1,
                                                                      inline=TRUE
                                               ))
                                             ),
                                             fluidRow(
                                               column(4, 
                                                      selectizeInput(inputId = "permitbmpclass", 
                                                                     label = "Select class of BMP to add:",
                                                                     choices = c("Septic","Shoreline Restoration","Stream Restoration",
                                                                                 "Street Sweeping", "Storm Drain Cleaning", "Urban Nutrient Mananagement",
                                                                                 "Land Use Conversion","Stormwater Management"),
                                                                     selected = NULL,
                                                                     multiple = TRUE, 
                                                                     options = list(maxItems = 1))),
                                               column(4,
                                                      conditionalPanel("input.permitbmpclass == 'Septic'",
                                                                       actionButton("permitseptic_add_btn", "Add Septic BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Shoreline Restoration'",
                                                                       actionButton("permitshoreline_add_btn", "Add Shoreline BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Stream Restoration'",
                                                                       actionButton("permitstream_add_btn", "Add Stream BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Street Sweeping'",
                                                                       actionButton("permitsweep_add_btn", "Add Street Sweeping BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Storm Drain Cleaning'",
                                                                       actionButton("permitstormdrain_add_btn", "Add Storm Drain Cleaning BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Urban Nutrient Mananagement'",
                                                                       actionButton("permiturbnut_add_btn", "Add Urban Nutrient BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Land Use Conversion'",
                                                                       actionButton("permitlandconversion_add_btn", "Add Land Use BMP", class = "butt")),
                                                      conditionalPanel("input.permitbmpclass == 'Stormwater Management'",
                                                                       actionButton("permitstormwatermanagement_add_btn", "Add Stormwater BMP", class = "butt"))
                                               ),
                                             ),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Septic'",
                                                 column(3,
                                                        textInput("permitsepticid", "ID of Septic(s)", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitsepticbmptype", 
                                                                       label = "Septic BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(Sector=="Septic") %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        numericInput("permitsepticnumber",
                                                                     label = "Number of Septic Systems",
                                                                     value = NULL, width = '100%'))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Shoreline Restoration'",
                                                 column(3,
                                                        textInput("permitshorelineid", "ID of Shoreline Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("permitshorelinelength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitshorelinebmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.permitshorelinebmptype == 'Protocol Reductions'",
                                                          numericInput("permitshorelinetnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("permitshorelinetpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("permitshorelinetssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Stream Restoration'",
                                                 column(3,
                                                        textInput("permitstreamid", "ID of Stream Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("permitstreamlength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitstreambmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.permitstreambmptype == 'Protocol Reductions'",
                                                          numericInput("permitstreamtnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("permitstreamtpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("permitstreamtssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Street Sweeping'",
                                                 column(3,
                                                        textInput("permitsweepid", "ID(s) of Street Sweeping", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitsweepbmptype", 
                                                                       label = "Street Sweeping Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("permitsweepmiles", "Street Sweeping Lane Miles (mi)", width = NULL,value = NULL))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Storm Drain Cleaning'",
                                                 column(3,
                                                        textInput("permitstormdrainid", "ID(s) of Storm Drain Cleaning", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitstormdraintype", 
                                                                       label = "Material Type",
                                                                       choices = c("Organic", "Inorganic"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        numericInput("permitstormdrainlbs", "Mass of material recovered (lbs)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitstormdrainenrich", 
                                                                       label = "Use Nutrient Enrichment Factors?",
                                                                       choices = c("Yes", "No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.permitstormdrainenrich == 'Yes'",
                                                          numericInput("permitstormdrainenrichtn", "Total Nitrogen Enrichment Factor", width = NULL,value = NULL),
                                                          numericInput("permitstormdrainenrichtp", "Total Phosphorus Enrichment Factor", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Urban Nutrient Mananagement'",
                                                 column(3,
                                                        textInput("permiturbnutid", "ID(s) of Urban Nutrient Management", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permiturbnutbmptype", 
                                                                       label = "Urban Nutrient Management Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                     "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,         
                                                        selectizeInput(inputId = "permiturbnutfertact", 
                                                                       label = "Does the Fertilizer Act apply to this BMP?",
                                                                       choices = c("Yes","No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("permiturbnutacres", "Total treated turf acres (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Land Use Conversion'",
                                                 column(3,
                                                        textInput("permitlandconversionid", "ID(s) of Land Use Conversion", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "permitlandconversionconvertingfrom", 
                                                                       label = "Converting From",
                                                                       choices = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        selectizeInput(inputId = "permitlandconversionconvertingto", 
                                                                       label = "Converting To",
                                                                       choices = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", "Tree Canopy over Impervious Road", "Tree Canopy over Impervious NonRoad", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("permitlandconversionacres", "Acres Converted (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.permitbmpclass == 'Stormwater Management'",
                                                 column(3,
                                                        textInput("permitstormwatermanagementid", "ID(s) of Stormwater Management BMP", width = NULL)),
                                                 column(4,       
                                                        selectizeInput(inputId = "permitstormwatermanagementbmptype", 
                                                                       label = "Stormwater BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        selectizeInput(inputId = "permitstormwatermanagementftw", 
                                                                       label = "Floating Treatment Wetland Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("NO","FTW1","FTW2","FTW3","FTW4","FTW5"))) %>%
                                                                         pull(BMPFullName),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        conditionalPanel(condition = "input.permitstormwatermanagementbmptype == 'Stormwater Performance Standard-Runoff Reduction' || input.permitstormwatermanagementbmptype == 'Stormwater Performance Standard-Stormwater Treatment'",
                                                                         numericInput("permitstormwatermanagementpe", "Pe (Inches)", width = NULL,value = NULL))),
                                                 column(3,
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'No'",
                                                                         numericInput("permitstormwatermanagementagimp", "Aggregate Impervious Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("permitstormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL)),
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'Yes'",
                                                                         numericInput("permitstormwatermanagementimproad", "Impervious Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("permitstormwatermanagementimpnonroad", "Impervious Non-Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("permitstormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL))
                                                 )
                                               )
                                             )
                                   )
                                   
                          ),
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################  Current Year Load #####################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("Current Load", value = "3", fluid = TRUE,#icon=icon("info"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Current Load with Current Year BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("currentloads_table")%>% withSpinner(color="#e6f7ff")),br(),br())),# well panel
                                   #br(),
                                   wellPanel(style = "background-color: #e3d9af; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(6,h4(strong("Enter Current Load BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"))),
                                             fluidRow(
                                               column(2, selectizeInput(inputId = "currentyear", 
                                                                        label = "Current Year",
                                                                        selected = NULL,
                                                                        multiple = TRUE,
                                                                        choices = c(seq(from = as.numeric(format(Sys.Date(),format="%Y")), to = 2025, by =1)),
                                                                        options = list(maxItems = 1)))
                                             ),
                                             fluidRow(
                                               column(4, 
                                                      selectizeInput(inputId = "currentbmpclass", 
                                                                     label = "Select class of BMP to add:",
                                                                     choices = c("Septic","Shoreline Restoration","Stream Restoration",
                                                                                 "Street Sweeping", "Storm Drain Cleaning", "Urban Nutrient Mananagement",
                                                                                 "Land Use Conversion","Stormwater Management"),
                                                                     selected = NULL,
                                                                     multiple = TRUE, 
                                                                     options = list(maxItems = 1))),
                                               column(4,
                                                      conditionalPanel("input.currentbmpclass == 'Septic'",
                                                                       actionButton("currentseptic_add_btn", "Add Septic BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Shoreline Restoration'",
                                                                       actionButton("currentshoreline_add_btn", "Add Shoreline BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Stream Restoration'",
                                                                       actionButton("currentstream_add_btn", "Add Stream BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Street Sweeping'",
                                                                       actionButton("currentsweep_add_btn", "Add Street Sweeping BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Storm Drain Cleaning'",
                                                                       actionButton("currentstormdrain_add_btn", "Add Storm Drain Cleaning BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Urban Nutrient Mananagement'",
                                                                       actionButton("currenturbnut_add_btn", "Add Urban Nutrient BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Land Use Conversion'",
                                                                       actionButton("currentlandconversion_add_btn", "Add Land Use BMP", class = "butt")),
                                                      conditionalPanel("input.currentbmpclass == 'Stormwater Management'",
                                                                       actionButton("currentstormwatermanagement_add_btn", "Add Stormwater BMP", class = "butt"))
                                               ),
                                             ),
                                             fluidRow(
                                               
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Septic'",
                                                 column(3,
                                                        textInput("currentsepticid", "ID of Septic(s)", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentsepticbmptype", 
                                                                       label = "Septic BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(Sector=="Septic") %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        numericInput("currentsepticnumber",
                                                                     label = "Number of Septic Systems",
                                                                     value = NULL, width = '100%'))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Shoreline Restoration'",
                                                 column(3,
                                                        textInput("currentshorelineid", "ID of Shoreline Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("currentshorelinelength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentshorelinebmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.currentshorelinebmptype == 'Protocol Reductions'",
                                                          numericInput("currentshorelinetnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("currentshorelinetpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("currentshorelinetssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Stream Restoration'",
                                                 column(3,
                                                        textInput("currentstreamid", "ID of Stream Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("currentstreamlength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentstreambmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.currentstreambmptype == 'Protocol Reductions'",
                                                          numericInput("currentstreamtnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("currentstreamtpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("currentstreamtssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Street Sweeping'",
                                                 column(3,
                                                        textInput("currentsweepid", "ID(s) of Street Sweeping", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentsweepbmptype", 
                                                                       label = "Street Sweeping Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("currentsweepmiles", "Street Sweeping Lane Miles (mi)", width = NULL,value = NULL))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Storm Drain Cleaning'",
                                                 column(3,
                                                        textInput("currentstormdrainid", "ID(s) of Storm Drain Cleaning", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentstormdraintype", 
                                                                       label = "Material Type",
                                                                       choices = c("Organic", "Inorganic"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        numericInput("currentstormdrainlbs", "Mass of material recovered (lbs)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentstormdrainenrich", 
                                                                       label = "Use Nutrient Enrichment Factors?",
                                                                       choices = c("Yes", "No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.currentstormdrainenrich == 'Yes'",
                                                          numericInput("currentstormdrainenrichtn", "Total Nitrogen Enrichment Factor", width = NULL,value = NULL),
                                                          numericInput("currentstormdrainenrichtp", "Total Phosphorus Enrichment Factor", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Urban Nutrient Mananagement'",
                                                 column(3,
                                                        textInput("currenturbnutid", "ID(s) of Urban Nutrient Management", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currenturbnutbmptype", 
                                                                       label = "Urban Nutrient Management Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                     "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,         
                                                        selectizeInput(inputId = "currenturbnutfertact", 
                                                                       label = "Does the Fertilizer Act apply to this BMP?",
                                                                       choices = c("Yes","No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("currenturbnutacres", "Total treated turf acres (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Land Use Conversion'",
                                                 column(3,
                                                        textInput("currentlandconversionid", "ID(s) of Land Use Conversion", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "currentlandconversionconvertingfrom", 
                                                                       label = "Converting From",
                                                                       choices = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        selectizeInput(inputId = "currentlandconversionconvertingto", 
                                                                       label = "Converting To",
                                                                       choices = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", "Tree Canopy over Impervious Road", "Tree Canopy over Impervious NonRoad", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("currentlandconversionacres", "Acres Converted (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.currentbmpclass == 'Stormwater Management'",
                                                 column(3,
                                                        textInput("currentstormwatermanagementid", "ID(s) of Stormwater Management BMP", width = NULL)),
                                                 column(4,       
                                                        selectizeInput(inputId = "currentstormwatermanagementbmptype", 
                                                                       label = "Stormwater BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        selectizeInput(inputId = "currentstormwatermanagementftw", 
                                                                       label = "Floating Treatment Wetland Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("NO","FTW1","FTW2","FTW3","FTW4","FTW5"))) %>%
                                                                         pull(BMPFullName),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        conditionalPanel(condition = "input.currentstormwatermanagementbmptype == 'Stormwater Performance Standard-Runoff Reduction' || input.currentstormwatermanagementbmptype == 'Stormwater Performance Standard-Stormwater Treatment'",
                                                                         numericInput("currentstormwatermanagementpe", "Pe (Inches)", width = NULL,value = NULL))),
                                                 column(3,
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'No'",
                                                                         numericInput("currentstormwatermanagementagimp", "Aggregate Impervious Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("currentstormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL)),
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'Yes'",
                                                                         numericInput("currentstormwatermanagementimproad", "Impervious Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("currentstormwatermanagementimpnonroad", "Impervious Non-Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("currentstormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL))
                                                 )
                                               )
                                             )
                                   )
                                   
                          ),
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################  Future Implementation Load  ######################################################### #
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("Future Scenario Load", value = "4", fluid = TRUE,#icon=icon("info"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("Future Scenario Loads with All Future BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("futureloads_table")%>% withSpinner(color="#e6f7ff")),br(),br(),br(),
                                               column(9,h4(strong("Future Scenario Loads with Milestone 1 BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("milestone1loads_table")%>% withSpinner(color="#e6f7ff")),br(),br(),br(),
                                               column(9,h4(strong("Future Scenario Loads with Milestone 2 BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("milestone2loads_table")%>% withSpinner(color="#e6f7ff")),br(),br(),br(),
                                               column(9,h4(strong("Future Scenario Loads with Planned BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,DTOutput("plannedloads_table")%>% withSpinner(color="#e6f7ff")),br(),br())),# well panel
                                   #br(),
                                   wellPanel(style = "background-color: #e3d9af; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(6,h4(strong("Define Future Scenarios"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"))),
                                             fluidRow(  
                                               column(2, selectizeInput(inputId = "milestone1year", 
                                                                        label = "Milestone 1 Year",
                                                                        selected = NULL,
                                                                        multiple = TRUE,
                                                                        choices = c(seq(from = as.numeric(format(Sys.Date(),format="%Y"))+1, to = 2050, by =1)),
                                                                        options = list(maxItems = 1))),
                                               column(2,textInput("milestone1desc", "Milestone 1 Description", width = NULL)),
                                               column(2, selectizeInput(inputId = "milestone2year", 
                                                                        label = "Milestone 2 Year",
                                                                        selected = NULL,
                                                                        multiple = TRUE,
                                                                        choices = c(seq(from = as.numeric(format(Sys.Date(),format="%Y"))+1, to = 2050, by =1)),
                                                                        options = list(maxItems = 1))),
                                               column(2,textInput("milestone2desc", "Milestone 2 Description", width = NULL)),
                                               column(2, selectizeInput(inputId = "plannedyear", 
                                                                        label = "Planned Year",
                                                                        selected = NULL,
                                                                        multiple = TRUE,
                                                                        choices = c(seq(from = as.numeric(format(Sys.Date(),format="%Y"))+1, to = 2050, by =1)),
                                                                        options = list(maxItems = 1))), 
                                               column(2,textInput("planneddesc", "Planned Description", width = NULL)),
                                             ),
                                             fluidRow(
                                               column(6,h4(strong("Enter Future Scenario Load BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;"))),
                                             
                                             fluidRow(
                                               column(4, 
                                                      selectizeInput(inputId = "futurebmpclass", 
                                                                     label = "Select class of BMP to add:",
                                                                     choices = c("Septic","Shoreline Restoration","Stream Restoration",
                                                                                 "Street Sweeping", "Storm Drain Cleaning", "Urban Nutrient Mananagement",
                                                                                 "Land Use Conversion","Stormwater Management"),
                                                                     selected = NULL,
                                                                     multiple = TRUE, 
                                                                     options = list(maxItems = 1))),
                                               column(2,
                                                      selectizeInput(inputId = "futurescenario", 
                                                                     label = "Select Future Scenario",
                                                                     selected = NULL,
                                                                     multiple = TRUE,
                                                                     choices = c("Milestone 1", "Milestone 2", "Planned"),
                                                                     options = list(maxItems = 1))
                                               ),
                                               column(4,
                                                      conditionalPanel("input.futurebmpclass == 'Septic'",
                                                                       actionButton("futureseptic_add_btn", "Add Septic BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Shoreline Restoration'",
                                                                       actionButton("futureshoreline_add_btn", "Add Shoreline BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Stream Restoration'",
                                                                       actionButton("futurestream_add_btn", "Add Stream BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Street Sweeping'",
                                                                       actionButton("futuresweep_add_btn", "Add Street Sweeping BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Storm Drain Cleaning'",
                                                                       actionButton("futurestormdrain_add_btn", "Add Storm Drain Cleaning BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Urban Nutrient Mananagement'",
                                                                       actionButton("futureurbnut_add_btn", "Add Urban Nutrient BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Land Use Conversion'",
                                                                       actionButton("futurelandconversion_add_btn", "Add Land Use BMP", class = "butt")),
                                                      conditionalPanel("input.futurebmpclass == 'Stormwater Management'",
                                                                       actionButton("futurestormwatermanagement_add_btn", "Add Stormwater BMP", class = "butt"))
                                               ),
                                             ),
                                             fluidRow(
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Septic'",
                                                 column(3,
                                                        textInput("futuresepticid", "ID of Septic(s)", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futuresepticbmptype", 
                                                                       label = "Septic BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(Sector=="Septic") %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        numericInput("futuresepticnumber",
                                                                     label = "Number of Septic Systems",
                                                                     value = NULL, width = '100%'))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Shoreline Restoration'",
                                                 column(3,
                                                        textInput("futureshorelineid", "ID of Shoreline Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("futureshorelinelength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futureshorelinebmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.futureshorelinebmptype == 'Protocol Reductions'",
                                                          numericInput("futureshorelinetnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("futureshorelinetpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("futureshorelinetssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Stream Restoration'",
                                                 column(3,
                                                        textInput("futurestreamid", "ID of Stream Restoration", width = NULL)),
                                                 column(3,       
                                                        numericInput("futurestreamlength", "Length of Restoration (ft)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futurestreambmptype", 
                                                                       label = "Planning Rate or Protocol Reductions?",
                                                                       choices = c("Default Rate", "Protocol Reductions"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.futurestreambmptype == 'Protocol Reductions'",
                                                          numericInput("futurestreamtnprotocol", "Total Nitrogen Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("futurestreamtpprotocol", "Total Phosphorus Protocol Reductions (lbs)", width = NULL,value = NULL),
                                                          numericInput("futurestreamtssprotocol", "Total Suspended Sediment Protocol Reductions (lbs)", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Street Sweeping'",
                                                 column(3,
                                                        textInput("futuresweepid", "ID(s) of Street Sweeping", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futuresweepbmptype", 
                                                                       label = "Street Sweeping Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("SCP1","SCP2","SCP3","SCP4","SCP5","SCP6","SCP7","SCP8","SCP9","SCP10","SCP11")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("futuresweepmiles", "Street Sweeping Lane Miles (mi)", width = NULL,value = NULL))
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Storm Drain Cleaning'",
                                                 column(3,
                                                        textInput("futurestormdrainid", "ID(s) of Storm Drain Cleaning", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futurestormdraintype", 
                                                                       label = "Material Type",
                                                                       choices = c("Organic", "Inorganic"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        numericInput("futurestormdrainlbs", "Mass of material recovered (lbs)", width = NULL,value = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futurestormdrainenrich", 
                                                                       label = "Use Nutrient Enrichment Factors?",
                                                                       choices = c("Yes", "No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        conditionalPanel(
                                                          condition = "input.futurestormdrainenrich == 'Yes'",
                                                          numericInput("futurestormdrainenrichtn", "Total Nitrogen Enrichment Factor", width = NULL,value = NULL),
                                                          numericInput("futurestormdrainenrichtp", "Total Phosphorus Enrichment Factor", width = NULL,value = NULL)
                                                        )
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Urban Nutrient Mananagement'",
                                                 column(3,
                                                        textInput("futureurbnutid", "ID(s) of Urban Nutrient Management", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futureurbnutbmptype", 
                                                                       label = "Urban Nutrient Management Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("UrbanNMMdCA","UrbanNMMdDIY", "UrbanNMPlan",
                                                                                                                     "UrbanNMPlanHR","UrbanNMPlanLR")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,         
                                                        selectizeInput(inputId = "futureurbnutfertact", 
                                                                       label = "Does the Fertilizer Act apply to this BMP?",
                                                                       choices = c("Yes","No"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("futureurbnutacres", "Total treated turf acres (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Land Use Conversion'",
                                                 column(3,
                                                        textInput("futurelandconversionid", "ID(s) of Land Use Conversion", width = NULL)),
                                                 column(3,       
                                                        selectizeInput(inputId = "futurelandconversionconvertingfrom", 
                                                                       label = "Converting From",
                                                                       choices = c("Aggregate Impervious", "Impervious Road", "Impervious NonRoad", "Turf", "Forest"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,
                                                        selectizeInput(inputId = "futurelandconversionconvertingto", 
                                                                       label = "Converting To",
                                                                       choices = c("Aggregate Impervious", "Turf", "Tree Canopy over Aggregate Impervious", "Tree Canopy over Impervious Road", "Tree Canopy over Impervious NonRoad", "Tree Canopy over Turf", "Mixed Open", "Mixed Open with Buffer", "Forest", "Forest with Buffer"),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1))),
                                                 column(3,       
                                                        numericInput("futurelandconversionacres", "Acres Converted (ac)", width = NULL,value = NULL)
                                                 )
                                               ),
                                               conditionalPanel(
                                                 condition = "input.futurebmpclass == 'Stormwater Management'",
                                                 column(3,
                                                        textInput("futurestormwatermanagementid", "ID(s) of Stormwater Management BMP", width = NULL)),
                                                 column(4,       
                                                        selectizeInput(inputId = "futurestormwatermanagementbmptype", 
                                                                       label = "Stormwater BMP Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("BioRetNoUdAB","BioRetUdAB","BioRetUdCD","Bioswale","Dryponds",
                                                                                                                     "ExtDryPonds","UrbFilterRR","UrbFilterST","Filter","ImperviousDisconnection",
                                                                                                                     "InfiltWithSV","Infiltration","AdvancedGI","PermPavSVNoUdAB","PermPavSVUdAB",
                                                                                                                     "PermPavSVUdCD","PermPavNoSVNoUdAB","PermPavNoSVUdAB","PermPavNoSVUdCD","RR","ST",
                                                                                                                     "VegOpChanNoUdAB","VegOpChanNoUdCD","WetPondWetland","ForestBufUrbanEff")) %>%
                                                                                          pull(BMPFullName)),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        selectizeInput(inputId = "futurestormwatermanagementftw", 
                                                                       label = "Floating Treatment Wetland Type",
                                                                       choices = unique(app_data %>%extract2("bmp") %>%
                                                                                          filter(BMPShortName %in% c("NO","FTW1","FTW2","FTW3","FTW4","FTW5"))) %>%
                                                                         pull(BMPFullName),
                                                                       selected = NULL,
                                                                       multiple = TRUE, 
                                                                       options = list(maxItems = 1)),
                                                        conditionalPanel(condition = "input.futurestormwatermanagementbmptype == 'Stormwater Performance Standard-Runoff Reduction' || input.futurestormwatermanagementbmptype == 'Stormwater Performance Standard-Stormwater Treatment'",
                                                                         numericInput("futurestormwatermanagementpe", "Pe (Inches)", width = NULL,value = NULL))),
                                                 column(3,
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'No'",
                                                                         numericInput("futurestormwatermanagementagimp", "Aggregate Impervious Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("futurestormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL)),
                                                        conditionalPanel(condition = "input.ag_disag_imp == 'Yes'",
                                                                         numericInput("futurestormwatermanagementimproad", "Impervious Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("futurestormwatermanagementimpnonroad", "Impervious Non-Road Acres in BMP Drainage Area (ac)", width = NULL,value = NULL),
                                                                         numericInput("futurestormwatermanagementturf", "Turf Acres in BMP Drainage Area (ac)", width = NULL,value = NULL))
                                                 )
                                               )
                                             )
                                   )
                                   
                          ),
                          
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################  All BMPs Added  #######################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("List of BMPs", value = "5", fluid = TRUE,icon=icon("list-ul"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(9,h4(strong("All BMPs"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               column(3,actionButton("bmp_delete_btn", "Delete BMP", class = "butt")),
                                               br(),column(12,DTOutput("bmps_table")%>% withSpinner(color="black")),br(),br())
                                   ),# well panel
                                   
                                   
                          ),
                          
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          ## ##################################   TMDL Summary   #######################################################################
                          ##===========================================================================================================================#
                          ##===========================================================================================================================#
                          tabPanel("TMDL Progress Summary", value = "6", fluid = TRUE,icon=icon("calculator"),
                                   wellPanel(style = "background-color: #d3e0d7; border: 1px solid black; padding-left: 15px; margin-left:-15px; padding-right: 15px; margin-right:-15px; padding-top: 10px; margin-top:-10px",
                                             fluidRow(
                                               column(8,h4(strong("TMDL Summary"),style="font-size:22px;font-style:normal; font-weight: 400; color: black;")),
                                               column(2,downloadButton("downloadReport", label = "Download .pdf Report",class="butt")),
                                               column(2,downloadButton("downloadbmpfiles", label = "Download BMP Files",class="butt")),
                                               br(),
                                               column(12,h4(strong("Required reduction: "),style="font-size:14px;font-style:normal; font-weight: 400; color: black;"),textOutput("red_perc")),
                                               br(),
                                               column(5,h4(strong("Baseline Summary"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               column(7,h4(strong("TMDL Progress Summary"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               column(5,h4(strong("Baseline Loads: "),style="font-size:14px;font-style:normal; font-weight: 400; color: black;"),
                                                      DTOutput("baselinetmdlsummary_table")%>% withSpinner(color="black"),br()),
                                               column(7,h4(strong("Progress Loads: "),style="font-size:14px;font-style:normal; font-weight: 400; color: black;"),
                                                      DTOutput("tmdlsummary_table")%>% withSpinner(color="black"),br()),
                                               br(),column(9,h4(strong("BMP Implementation"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               column(5,h4(strong("Load Reduction Summary: "),style="font-size:14px;font-style:normal; font-weight: 400; color: black;"),
                                                      DTOutput("bmploadssummary_table")%>% withSpinner(color="black"),br(),br()),
                                               column(7,h4(strong("BMP Summary: "),style="font-size:14px;font-style:normal; font-weight: 400; color: black;"),
                                                      DTOutput("bmpimplementation_table")%>% withSpinner(color="black"),br(),br()),
                                               br(),column(9,h4(strong("Load Plot"),style="font-size:20px;font-style:normal; font-weight: 400; color: black;")),
                                               br(),column(12,plotOutput("loadplot")%>% withSpinner(color="black")),br(),br(),br()
                                             ) 
                                             
                                   )
                          ) #tab panel        
               )
)







