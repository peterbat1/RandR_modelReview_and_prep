# A hacked version of the code from ShinyLeafletExample2
#
# Peter D. Wilson
# Evolutionary Ecology Section
# Science and Conservation Branch
# Royal Botanic Gardens, Sydney
#

# 20 July 2020: Changed handling of spatial objects to use library sf; fixed
# small bug in "on-click" event handler: clicking outside bounds for a selected
# taxon repeatedly no longer causes alternate on- & off-display of 'out of
# bounds' alert; made 'out of bounds' alert more prominent by changing background
# colour and text stylings
# 26 May 2016; 17 June 2018: Added progress notifications;
# 6 December 2018: Refined a number of UI elements
# 5-7 August 2019: Serious revision to expand the range of environmental variables for
# computing matching environment layers; changes also to calling of functions in a heavily
# re-designed RandR.modelReview R-package.
# 1-8 June 2020: Enhanced by adding spinners, allowing user to select GDM
# threshold but also to allow resetting to the default value stored in the GDM
# object, and updating of help modal dialogs.
# 25 July 2020: Made into a fuly standalone application by incorporating functions
# from the R-package RandR.modelReview
#
# The method to deal with empty dataframes so that clearMarkers() and addCircleMarkers() work
# properly comes from a reply by Joe Cheng (RStudio) to a post by Catherine Smith. See:
# https://groups.google.com/forum/#!topic/shiny-discuss/pCOdOykRPMs last accessed 26 May 2016.

# // https: also suppported.
# var Esri_WorldImagery = L.tileLayer('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}', {
#   attribution: 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community'
# });
#
# onclick observer and little popup display R-function from:
#   http://stackoverflow.com/questions/37523323/identify-position-of-a-click-on-a-raster-in-leaflet-in-r?noredirect=1&lq=1
# last accessed 25 October 2016
#
# Checkbox group multicolumn CSS code from:
#   https://stackoverflow.com/questions/42742191/align-checkboxgroupinput-vertically-and-horizontally
# last accessed 25 August 2019
#

library(shiny)
library(shinybusy)
library(shinyjs)
library(shinyBS)
library(leaflet)
library(sf)
library(raster)
library(jsonlite)
library(RSQLite)
library(gdm)
library(fasterize)
library(plotly)

# Initialise stuff
markerPalette <- c("red","orange","lightskyblue","lightgreen")

### layer ordering: local genetic region, current climate match, future climate match, domain, transition zones
layerPalette <- c("#666666", "#3366ff","#F57900", "#adff2f", "#9b30ff")

envFolder <- ""

basePath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/"

modelFolder <- paste0(basePath, "models/gdm/")

boundsFolder <- paste0(basePath, "models/domain")

zonesFolder <- paste0(basePath, "models/zones")

sqlPath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/Databasing/RandR-sqlite-db/RandR_sampling.sqlite"

basePathOcc <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_KML/"

modelList <- list.files(modelFolder, "*.Rd")

zonesList <- list.files(zonesFolder, "*.geojson")

modelListDisplay <- c("No model selected", sort(gsub(".Rd$", "", basename(modelList), fixed = TRUE)))

MAT_tol <- 1.25

MAP_tol <- 300 #150

TS_tol <- 0.5

PS_tol <- 5

aspect_tol <- 45

TWI_tol <- 2


###############################################################################
############# Support functions from package RandR.modelReview ################
###############################################################################
RandR_gdal_polygonizeR <- function(pathToRasFile,
                                   outDir)
{
  pyPath <- Sys.which('gdal_polygonize.py')
  
  old_wd <- getwd()
  
  setwd(outDir)
  
  pathTo_jsonFile <- paste0(tools::file_path_sans_ext(pathToRasFile), ".geojson")
  
  if (file.exists(pathTo_jsonFile)) file.remove(pathTo_jsonFile)
  
  system2('python3', args=(sprintf('"%1$s" "%2$s" -q -f "%3$s" "%4$s"',
                                   pyPath, pathToRasFile, "GeoJSON", pathTo_jsonFile)))
  
  setwd(old_wd)
  
  return(NULL)
}


###############################################################################
############################## Shiny app code #################################
###############################################################################
# https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
# answer by 'untill' posted 2017-05-09
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

###############################################################################
ui <- bootstrapPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(tags$style(HTML("
                            .multicol .shiny-options-group{
                            -webkit-column-count: 2; /* Chrome, Safari, Opera */
                            -moz-column-count: 2;    /* Firefox */
                            column-count: 2;
                            -moz-column-fill: balanced;
                            -column-fill: balanced;
                            }
                            .checkbox{
                            margin-top: 0px !important;
                            -webkit-margin-after: 0px !important;
                            }
                            .shiny-notification {background-color:#ff3e96;
                            color:#000000;}
                            ")),
            tags$style(HTML("hr {border-top: 1px solid #000000;
                            margin-bottom: 0px}")),
            tags$style("#shadedPanel {
                       background-color: #dcdcdc;
                       }"),
            tags$style("#checkGroup {
                       padding-top: 2px;
                       margin-top: 0px;
                       }"),
            tags$style("#show3dStuff .modal-dialog{ width:800px}"),     # Styling for large bsModal to display
            tags$style("#show3dStuff .modal-body{ min-height:700px}"),  # 3D surface; see attribution in observeEvent function
            tags$script(src="https://kit.fontawesome.com/9ae27d97d5.js")),
  leafletOutput("map", width = "78%", height = "100%"),
  absolutePanel(id = "controls", top = 1, right = 5, width = "20%",
                img(src = "Restore_and Renew_logo_green_275px_whiteBackground.png"),
                absolutePanel(top = "90px", height = "20%", left = "1%", width = "95%",
                              selectInput("gdm_model",
                                          label = h4("GDM Model:"),
                                          choices = modelListDisplay
                              ),
                              uiOutput("webtool_status"),
                              uiOutput("threshold"),
                              actionButton("reset", "Reset", style = "color : #fff; background-color: #0095FF;"),
                              actionButton("show3D", "Show 3D", style ="color : #fff; background-color : #FFA400;")
                ),
                absolutePanel(top = "360px", height = "25%", left = "1%", width = "95%", #id = "shadedPanel", 
                              hr(),
                              uiOutput("occRecords")
                ),
                absolutePanel(
                  top = "505px",
                  height = "30%",
                  left = "1%",
                  width = "95%",
                  hr(),
                  #h4("Environmental variables:"),
                  uiOutput("envVars")
                ),
                absolutePanel(top = "635px", height = "30%", left = "1%", width = "95%", # id = "shadedPanel", 
                              hr(),
                              h4("Future climate scenario:"),
                              absolutePanel(#id = "shadedPanel", #top = "590px", height = "5%", right = 2, width = "25%",
                                radioButtons("rcpRadioGrp",
                                             label=h5("RCP:"),
                                             choices = list("rcp4.5" = 1,
                                                            "rcp8.5" = 2),
                                             selected = 1
                                )),
                              absolutePanel(left = "51%",  width = "49%",#top = "590px", height = "5%", , id = "shadedPanel", 
                                            radioButtons("epochRadioGrp",
                                                         label=h5("Time:"),
                                                         choices = list("2050" = 1,
                                                                        "2070" = 2),
                                                         selected = 1))
                ),
                absolutePanel(top = "770px", height = "10%", left = "1%", width = "95%",
                              hr(),
                              p(),
                              actionButton("help","Help", icon = icon("circle-info"), width = "110px", style = "color : #fff; background-color: #0095FF;"),
                              actionButton("addModel","Add model", icon = icon("arrow-right-to-bracket"), width = "110px", style = "color : #fff; background-color: #0095FF;"),
                              actionButton("close", "Close App", width = "110px", style = "color : #fff; background-color: #880000;")
                              
                )
  ),
  bsModal("show3dStuff", "Genetic difference surface", "show3d", plotlyOutput("plotyplot", width = "100%"))
  
)


###############################################################################
server <- function(input,
                   output,
                   session)
{
  ###############################################################################
  # https://stackoverflow.com/questions/35306295/how-to-stop-running-shiny-app-by-closing-the-browser-window
  # answer by 'untill' posted 2017-05-09
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
  
  ###############################################################################
  # Reactive expression to provide taxon name for the selected model
  taxonName <- reactive({
    return(trimws(sub(" genetic model.Rd", "", gsub("_", " ", input$gdm_model, fixed = TRUE), fixed = TRUE)))
  })
  
  
  ###############################################################################
  # Reactive expression for packaging user-selected rcp and epoch values
  futureParams <- reactive({
    
    if (1 %in% input$rcpRadioGrp)
    {
      rcp <- "rcp45"
    }
    else
    {
      rcp <- "rcp85"
    }
    
    if (1 %in% input$epochRadioGrp)
    {
      epoch <- "2050"
    }
    else
    {
      epoch <- "2070"
    }
    
    return(list(rcp = rcp, epoch = epoch))
  })
  
  
  ###############################################################################
  getDefaultThreshold <- reactive({
    if (input$gdm_model != "No model selected")
    {
      load(paste0(modelFolder, input$gdm_model))
      return(md$threshold)
    }
    else
      return(0)
  })
  
  
  ###############################################################################
  # Set up map object
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      fitBounds(140.5, -38, 154, -27.5) %>%
      addLayersControl(overlayGroups = c("Environmental match", "Local Genetic Region", "Model domain", "Transition zones"),
                       options = layersControlOptions(autoZIndex = TRUE)) %>%
      addScaleBar("bottomleft")
  })
  
  
  ###############################################################################
  # Mark clicked location with a bog-standard Leaflet map marker
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      currPtLat <- click$lat
      currPtLong <- click$lon
      
      proxy <- leafletProxy("map")
      
      proxy %>%
        clearMarkers() %>%
        addMarkers(click$lng, click$lat)
    }
  })
  
  
  ###############################################################################
  #### Reactive components to load essential resources
  
  landsea_mask <- reactive({raster::raster(paste0(basePath, "resources/landsea_mask.tif"))})
  
  MAT_vals <- reactive({readRDS(paste0(basePath, "resources/MAT_vals.rds"))/100})
  MAT_vals_rcp45_2050 <- reactive({readRDS(paste0(basePath, "resources/MAT_vals_rcp45_2050.rds"))/100})
  MAT_vals_rcp45_2070 <- reactive({readRDS(paste0(basePath, "resources/MAT_vals_rcp45_2070.rds"))/100})
  MAT_vals_rcp85_2050 <- reactive({readRDS(paste0(basePath, "resources/MAT_vals_rcp85_2050.rds"))/100})
  MAT_vals_rcp85_2070 <- reactive({readRDS(paste0(basePath, "resources/MAT_vals_rcp85_2070.rds"))/100})
  
  MAP_vals <- reactive({readRDS(paste0(basePath, "resources/MAP_vals.rds"))})
  MAP_vals_rcp45_2050 <- reactive({readRDS(paste0(basePath, "resources/MAP_vals_rcp45_2050.rds"))})
  MAP_vals_rcp45_2070 <- reactive({readRDS(paste0(basePath, "resources/MAP_vals_rcp45_2070.rds"))})
  MAP_vals_rcp85_2050 <- reactive({readRDS(paste0(basePath, "resources/MAP_vals_rcp85_2050.rds"))})
  MAP_vals_rcp85_2070 <- reactive({readRDS(paste0(basePath, "resources/MAP_vals_rcp85_2070.rds"))})
  
  TS_vals <- reactive({readRDS(paste0(basePath, "resources/TS_vals.rds"))/10})
  TS_vals_rcp45_2050 <- reactive({readRDS(paste0(basePath, "resources/TS_vals_rcp45_2050.rds"))/10})
  TS_vals_rcp45_2070 <- reactive({readRDS(paste0(basePath, "resources/TS_vals_rcp45_2070.rds"))/10})
  TS_vals_rcp85_2050 <- reactive({readRDS(paste0(basePath, "resources/TS_vals_rcp85_2050.rds"))/10})
  TS_vals_rcp85_2070 <- reactive({readRDS(paste0(basePath, "resources/TS_vals_rcp85_2070.rds"))/10})
  
  PS_vals <- reactive({readRDS(paste0(basePath, "resources/PS_vals.rds"))/10})
  PS_vals_rcp45_2050 <- reactive({readRDS(paste0(basePath, "resources/PS_vals_rcp45_2050.rds"))/10})
  PS_vals_rcp45_2070 <- reactive({readRDS(paste0(basePath, "resources/PS_vals_rcp45_2070.rds"))/10})
  PS_vals_rcp85_2050 <- reactive({readRDS(paste0(basePath, "resources/PS_vals_rcp85_2050.rds"))/10})
  PS_vals_rcp85_2070 <- reactive({readRDS(paste0(basePath, "resources/PS_vals_rcp85_2070.rds"))/10})
  
  aspect_vals <- reactive({readRDS(paste0(basePath, "resources/aspect_vals.rds"))})
  TWI_vals <- reactive({readRDS(paste0(basePath, "resources/TWI_vals.rds"))})
  
  
  MAT_min <- reactive({min(MAT_vals(), na.rm = TRUE)})
  MAT_max <- reactive({max(MAT_vals(), na.rm = TRUE)})
  
  MAP_min <- reactive({min(MAP_vals(), na.rm = TRUE)})
  MAP_max <- reactive({max(MAP_vals(), na.rm = TRUE)})
  
  TS_min <- reactive({min(TS_vals(), na.rm = TRUE)})
  TS_max <- reactive({max(TS_vals(), na.rm = TRUE)})
  
  PS_min <- reactive({min(PS_vals(), na.rm = TRUE)})
  PS_max <- reactive({max(PS_vals(), na.rm = TRUE)})
  
  aspect_min <- reactive({min(aspect_vals(), na.rm = TRUE)})
  aspect_max <- reactive({max(aspect_vals(), na.rm = TRUE)})
  
  TWI_min <- reactive({min(TWI_vals(), na.rm = TRUE)})
  TWI_max <- reactive({max(TWI_vals(), na.rm = TRUE)})
  
  onWebtool <- reactive({tmp <- read.csv(paste0(basePath, "resources/onWebtool_lookup.csv"), stringsAsFactors = FALSE)
  row.names(tmp) <- tmp[, "taxon"]
  return(tmp)})
  
  ###############################################################################
  #### Compute and display the Current environmental match layer for the
  #### selected location and set of env vars
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      show_modal_spinner(spin = "flower", text = "Computing Current environment match", color = "#00BFFF")
      
      #click <- input$map_click
      
      if (is.null(input$envVarsChkBox) | !any(c("MAT", "MAP", "TS", "PS", "TWI", "Aspect") %in% input$envVarsChkBox))
        currentClimate <- NULL
      else
      {
        sessionDir <- tempdir()
        
        thres <- 0
        
        cellNum <- raster::cellFromXY(landsea_mask(), cbind(click$lng, click$lat))
        
        sum_vals <- rep(0, length(MAT_vals()))
        
        if ("MAT" %in% input$envVarsChkBox)
        {
          MAT_pt <- MAT_vals()[cellNum]
          MAT_upper <- pmax(MAT_min(), pmin(MAT_pt + MAT_tol, MAT_max()))
          MAT_lower <- pmax(MAT_min(), pmin(MAT_pt - MAT_tol, MAT_max()))
          sum_vals <- sum_vals + .bincode(MAT_vals(), c(MAT_lower, MAT_upper))
          thres <- thres + 1
        }
        
        if ("MAP" %in% input$envVarsChkBox)
        {
          MAP_pt <- MAP_vals()[cellNum]
          MAP_upper <- pmax(MAP_min(), pmin(MAP_pt + MAP_tol, MAP_max()))
          MAP_lower <- pmax(MAP_min(), pmin(MAP_pt - MAP_tol, MAP_max()))
          sum_vals <- sum_vals + .bincode(MAP_vals(), c(MAP_lower, MAP_upper))
          thres <- thres + 1
        }
        
        if ("TS" %in% input$envVarsChkBox)
        {
          TS_pt <- TS_vals()[cellNum]
          TS_upper <- pmax(TS_min(), pmin(TS_pt + TS_tol, TS_max()))
          TS_lower <- pmax(TS_min(), pmin(TS_pt - TS_tol, TS_max()))
          sum_vals <- sum_vals + .bincode(TS_vals(), c(TS_lower, TS_upper))
          thres <- thres + 1
        }
        
        if ("PS" %in% input$envVarsChkBox)
        {
          PS_pt <- PS_vals()[cellNum]
          PS_upper <- pmax(PS_min(), pmin(PS_pt + PS_tol, PS_max()))
          PS_lower <- pmax(PS_min(), pmin(PS_pt - PS_tol, PS_max()))
          sum_vals <- sum_vals + .bincode(PS_vals(), c(PS_lower, PS_upper))
          thres <- thres + 1
        }
        
        if ("Aspect" %in% input$envVarsChkBox)
        {
          aspect_pt <- aspect_vals()[cellNum]
          aspect_upper <- pmax(aspect_min(), pmin(aspect_pt + aspect_tol, aspect_max()))
          aspect_lower <- pmax(aspect_min(), pmin(aspect_pt - aspect_tol, aspect_max()))
          sum_vals <- sum_vals + .bincode(aspect_vals(), c(aspect_lower, aspect_upper))
          thres <- thres + 1
        }
        
        if ("TWI" %in% input$envVarsChkBox)
        {
          TWI_pt <- TWI_vals()[cellNum]
          TWI_upper <- pmax(TWI_min(), pmin(TWI_pt + TWI_tol, TWI_max()))
          TWI_lower <- pmax(TWI_min(), pmin(TWI_pt - TWI_tol, TWI_max()))
          sum_vals <- sum_vals + .bincode(TWI_vals(), c(TWI_lower, TWI_upper))
          thres <- thres + 1
        }
        
        #####################
        ras_sum <- landsea_mask()
        raster::values(ras_sum) <- ifelse(sum_vals == thres, 1, NA)
        pathToRasFile <- paste0(sessionDir,"/matchedSite_Current.tif")
        raster::writeRaster(ras_sum, pathToRasFile, overwrite = TRUE)
        RandR_gdal_polygonizeR(pathToRasFile, sessionDir)
        
        geoJsonName <- paste0(sessionDir, "/matchedSite_Current.geojson")
        
        currentClimate <- jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE)
      }
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("CC") %>%
        addGeoJSON(currentClimate, layerId = "CC", color = layerPalette[2], group = "Environmental match")
      remove_modal_spinner()
    }
  })
  
  
  ###############################################################################
  # Compute and display the Future Climate match areas for the selected taxon
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      show_modal_spinner(spin = "flower", text = "Computing Future environment match", color = "#FF8C00")
      futureBits <- futureParams()
      
      if (is.null(input$envVarsChkBox) | !any(c("MAT", "MAP", "TS", "PS", "TWI", "Aspect") %in% input$envVarsChkBox))
        futureClimate <- NULL
      else
      {
        sessionDir <- tempdir()
        
        thres <- 0
        
        cellNum <- raster::cellFromXY(landsea_mask(), cbind(click$lng, click$lat))
        
        sum_vals <- rep(0, length(MAT_vals()))
        
        ##########################
        if ("MAT" %in% input$envVarsChkBox)
        {
          if (futureBits$rcp == "rcp45")
          {
            if (futureBits$epoch == "2050")
              MAT_valsFuture <- MAT_vals_rcp45_2050()
            else
              MAT_valsFuture <- MAT_vals_rcp45_2070()
          }
          else
          {
            if (futureBits$epoch == "2050")
              MAT_valsFuture <- MAT_vals_rcp85_2050()
            else
              MAT_valsFuture <- MAT_vals_rcp85_2070()
          }
          
          #MAT_min <- min(MAT_valsFuture, na.rm = TRUE)
          #MAT_max <- max(MAT_valsFuture, na.rm = TRUE)
          MAT_pt <- MAT_valsFuture[cellNum]
          MAT_upper <- pmax(MAT_min(), pmin(MAT_pt + MAT_tol, MAT_max()))
          MAT_lower <- pmax(MAT_min(), pmin(MAT_pt - MAT_tol, MAT_max()))
          sum_vals <- sum_vals + .bincode(MAT_vals(), c(MAT_lower, MAT_upper))
          thres <- thres + 1
        }
        
        ##########################
        if ("MAP" %in% input$envVarsChkBox)
        {
          if (futureBits$rcp == "rcp45")
          {
            if (futureBits$epoch == "2050")
              MAP_valsFuture <- MAP_vals_rcp45_2050()
            else
              MAP_valsFuture <- MAP_vals_rcp45_2070()
          }
          else
          {
            if (futureBits$epoch == "2050")
              MAP_valsFuture <- MAP_vals_rcp85_2050()
            else
              MAP_valsFuture <- MAP_vals_rcp85_2070()
          }
          
          MAP_pt <- MAP_valsFuture[cellNum]
          MAP_upper <- pmax(MAP_min(), pmin(MAP_pt + MAP_tol, MAP_max()))
          MAP_lower <- pmax(MAP_min(), pmin(MAP_pt - MAP_tol, MAP_max()))
          sum_vals <- sum_vals + .bincode(MAP_vals(), c(MAP_lower, MAP_upper))
          thres <- thres + 1
        }
        
        ##########################
        if ("TS" %in% input$envVarsChkBox)
        {
          if (futureBits$rcp == "rcp45")
          {
            if (futureBits$epoch == "2050")
              TS_valsFuture <- TS_vals_rcp45_2050()
            else
              TS_valsFuture <- TS_vals_rcp45_2070()
          }
          else
          {
            if (futureBits$epoch == "2050")
              TS_valsFuture <- TS_vals_rcp85_2050()
            else
              TS_valsFuture <- TS_vals_rcp85_2070()
          }
          
          TS_pt <- TS_valsFuture[cellNum]
          TS_upper <- pmax(TS_min(), pmin(TS_pt + TS_tol, TS_max()))
          TS_lower <- pmax(TS_min(), pmin(TS_pt - TS_tol, TS_max()))
          sum_vals <- sum_vals + .bincode(TS_vals(), c(TS_lower, TS_upper))
          thres <- thres + 1
        }
        
        ##########################
        if ("PS" %in% input$envVarsChkBox)
        {
          if (futureBits$rcp == "rcp45")
          {
            if (futureBits$epoch == "2050")
              PS_valsFuture <- PS_vals_rcp45_2050()
            else
              PS_valsFuture <- PS_vals_rcp45_2070()
          }
          else
          {
            if (futureBits$epoch == "2050")
              PS_valsFuture <- PS_vals_rcp85_2050()
            else
              PS_valsFuture <- PS_vals_rcp85_2070()
          }
          
          PS_pt <- PS_valsFuture[cellNum]
          PS_upper <- pmax(PS_min(), pmin(PS_pt + PS_tol, PS_max()))
          PS_lower <- pmax(PS_min(), pmin(PS_pt - PS_tol, PS_max()))
          sum_vals <- sum_vals + .bincode(PS_vals(), c(PS_lower, PS_upper))
          thres <- thres + 1
        }
        
        ##########################
        if ("Aspect" %in% input$envVarsChkBox)
        {
          aspect_pt <- aspect_vals[cellNum]
          aspect_upper <- pmax(aspect_min(), pmin(aspect_pt + aspect_tol, aspect_max()))
          aspect_lower <- pmax(aspect_min(), pmin(aspect_pt - aspect_tol, aspect_max()))
          sum_vals <- sum_vals + .bincode(aspect_vals(), c(aspect_lower, aspect_upper))
          thres <- thres + 1
        }
        
        ##########################
        if ("TWI" %in% input$envVarsChkBox)
        {
          TWI_pt <- TWI_vals[cellNum]
          TWI_upper <- pmax(TWI_min(), pmin(TWI_pt + TWI_tol, TWI_max()))
          TWI_lower <- pmax(TWI_min(), pmin(TWI_pt - TWI_tol, TWI_max()))
          sum_vals <- sum_vals + .bincode(TWI_vals(), c(TWI_lower, TWI_upper))
          thres <- thres + 1
        }
        
        ##########################
        ras_sumFuture <- landsea_mask()
        raster::values(ras_sumFuture) <- ifelse(sum_vals == thres, 1, NA)
        pathToRasFile <- paste0(sessionDir, "/matchedClimate_Future.tif")
        raster::writeRaster(ras_sumFuture, pathToRasFile, overwrite = TRUE)
        RandR_gdal_polygonizeR(pathToRasFile, sessionDir)
        
        geoJsonName <- paste0(sessionDir, "/matchedClimate_Future.geojson")
        
        futureClimate <- jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE)
      }
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("FC") %>%
        addGeoJSON(futureClimate, layerId = "FC", color = layerPalette[3], group = "Environmental match")
      
      remove_modal_spinner()
    }
  })
  
  
  ###############################################################################
  # Reactives to load and compute bounds objects for GDM computation
  
  ext_ras <- reactive({raster::extent(landsea_mask())})
  base_rows <- reactive({raster::nrow(landsea_mask())})
  base_cols <- reactive({raster::ncol(landsea_mask())})
  
  
  boundsPolyName <- reactive({paste0(boundsFolder,"/", gsub(" ", "_", taxonName(), fixed = TRUE), "_domain_clipped.geojson")})
  
  bounds_poly <- reactive({
    this_poly <- sf::st_read(boundsPolyName())
    st_crs(this_poly) <- 4326
    return(this_poly)
  })
  
  bounds_ras <- reactive({fasterize::fasterize(bounds_poly(),
                                               raster::raster(ext = ext_ras(),
                                                              nrows = base_rows(),
                                                              ncols = base_cols(),
                                                              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))})
  
  
  ###############################################################################
  # Compute the local genetic region for the selected taxon and optionally apply a selected threshold value
  gdmSurface <- reactive({
    click <- input$map_click
    
    if (!is.null(click))
    {
      thisPt <- sf::st_as_sf(data.frame(x = click$lng, y = click$lat), coords = c("x", "y"), crs = 4326)
      
      sessionDir <- tempdir()
      taxon_Name <- gsub(" ", "_", taxonName(), fixed = TRUE)
      s1_point <- c(click$lng, click$lat)
      
      modelPath <- paste0(modelFolder, input$gdm_model)
      load(modelPath)
      
      hotCells <- which(raster::values(bounds_ras()) == 1)
      
      # Make prediction data.frame
      s2_ll <- raster::xyFromCell(bounds_ras(), hotCells)
      
      null_dist  <- rep(1, nrow(s2_ll))
      null_wght  <- rep(1, nrow(s2_ll))
      
      s1_ll <- cbind(rep(s1_point[1], nrow(s2_ll)), rep(s1_point[2], nrow(s2_ll)))
      
      gdm_prd <- data.frame(distance = null_dist,
                            weights = null_wght,
                            s1.xCoord = rep(s1_point[1], nrow(s2_ll)),
                            s1.yCoord = rep(s1_point[2], nrow(s2_ll)),
                            s2.xCoord = s2_ll[,1],
                            s2.yCoord = s2_ll[,2])
      
      # Add other predictors as indicated by flags in the gdm object
      if(md$Q)
      {
        qdata <- raster::stack(md$qdata)
        s1_Q  <- raster::extract(qdata, s1_ll)
        s2_Q  <- raster::extract(qdata, s2_ll)
        
        colnames(s1_Q) <- paste0("s1.Q", 1:ncol(s1_Q))
        colnames(s2_Q) <- paste0("s2.Q", 1:ncol(s2_Q))
        
        gdm_prd   <- cbind(gdm_prd, s1_Q, s2_Q)
      }
      
      if(md$E)
      {
        edata  <- raster::stack(md$edata)
        enames <- names(edata)
        s1_E  <- raster::extract(edata, s1_ll)
        s2_E  <- raster::extract(edata, s2_ll)
        
        colnames(s1_E) <- paste0("s1.", enames, 1:ncol(s1_E))
        colnames(s2_E) <- paste0("s2.", enames, 1:ncol(s2_E))
        
        gdm_prd   <- cbind(gdm_prd, s1_E, s2_E)
        
        # in case environmental variable undefined in parts of srast
        gdm_prd <- gdm_prd[ rowSums(is.na(gdm_prd)) <= 0, ]
        s2_ll   <- gdm_prd[, 5:6]
      }
      
      rawPred <- predict(md$model, gdm_prd)
      
      predRas <- landsea_mask()
      cellInd <- raster::cellFromXY(predRas, s2_ll)
      #print(cellInd)
      raster::values(predRas) <- NA
      raster::values(predRas)[cellInd] <- rawPred
      
      return(predRas)
    }
    else
      return(NULL)
  })
  
  
  ###############################################################################
  # Conditionally compute and display the local genetic region for the selected taxon
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      if (input$gdm_model != "No model selected")
      {
        thisPt <- sf::st_as_sf(data.frame(x = click$lng, y = click$lat), coords = c("x", "y"), crs = 4326)
        modelPath <- paste0(modelFolder, input$gdm_model)
        load(modelPath)
        
        if (nrow(sf::st_intersection(thisPt, bounds_poly())) > 0)
        {
          removeNotification(id = "badMsg")
          show_modal_spinner(spin = "flower", text = "Computing Local Genetic Region", color = "#A9A9A9")
          
          threshRas <- gdmSurface()
          
          if (!is.null(input$threshold))
            cellInd <- which(raster::values(threshRas) <= input$threshold)
          else
            cellInd <- which(raster::values(threshRas) <= md$threshold)
          
          raster::values(threshRas) <- NA
          raster::values(threshRas)[cellInd] <- 1
          
          #### path and file name convention need to be checked and adjusted as necessary
          sessionDir <- tempdir()
          taxon_Name <- gsub(" ", "_", taxonName(), fixed = TRUE)
          outFilename <- paste0(sessionDir, "/", taxon_Name, "_local_genetic.tif")
          
          
          raster::writeRaster(threshRas,
                              outFilename,
                              format = "GTiff",
                              overwrite = TRUE)
          
          RandR_gdal_polygonizeR(outFilename, sessionDir)
          
          geoJsonName <- paste0(sessionDir, "/", taxon_Name, "_local_genetic.geojson")
          
          gdmResult <- jsonlite::toJSON(jsonlite::fromJSON(txt = geoJsonName), auto_unbox = TRUE)
          
          leafletProxy("map") %>%
            removeGeoJSON("GDM") %>%
            addGeoJSON(gdmResult, layerId = "GDM", color = layerPalette[1], group = "Local Genetic Region") # %>%
          
          remove_modal_spinner()
          #enable("show2d")
        }
        else
        {
          gdmResult <- NULL
          showNotification("Out of bounds for selected taxon - try another location", duration = NULL, id = "badMsg", type = "error")
          #disable("show2d")
        }
      }
      else
        gdmResult <- NULL
    }
  })
  
  
  ###############################################################################
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map")
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>%
      clearControls() %>%
      addLegend(position = "bottomright",
                values = c(1, 2, 3, 4, 5),
                colors = layerPalette,
                labels = c("Local genetic region", "Current env. match","Future env. match",
                           "Model domain", "Transition zones"))
  })
  
  
  ###############################################################################
  output$threshold <- renderUI({
    sliderInput(inputId = "threshold",
                label = h4("GDM threshold:"),
                min = 0,
                max = 1,
                value = 0)
  })
  
  
  ###############################################################################
  output$envVars <- renderUI({
    tags$div(id = "checkGroup", align = "left",
             class = "multicol",
             checkboxGroupInput("envVarsChkBox",
                                label = h4("Environmental variables:"),
                                choiceNames = c("Mean Ann. Temp.", "Mean Ann. Precip.",
                                                "Temp. Seasonality", "Precip. Seasonality",
                                                "Aspect", "Topo. Wetness Index"),
                                choiceValues = c("MAT", "MAP", "TS", "PS", "Aspect", "TWI"),
                                selected = c("Mean Ann. Temp.", "Mean Ann. Precip."), width = "95%")
    )
  })
  
  
  ###############################################################################
  output$occRecords <- renderUI({
    tags$div(id = "checkGroup", 
             align = "left",
             class = "multicol",
             checkboxGroupInput("showCheckGrp",
                                label=h4("Occurrence records:"),
                                choiceValues = list("R&R Vouchers", "R&R Samples", "DArT Samples", "ALA herbarium", "Survey records"),
                                choiceNames = list(p(img(src = "images/marker-icon-red-small.png"), "R&R Vouchers"),
                                                   p(img(src = "images/marker-icon-orange-small.png"), "R&R Samples"),
                                                   p(img(src = "images/marker-icon-violet-small.png"), "DArT Samples"),
                                                   p(img(src = "images/marker-icon-green-small.png"), "ALA"),
                                                   p(img(src = "images/marker-icon-grey-small.png"), "Survey")),
                                selected = "") #c("R&R Samples", "Sent to DArT", "ALA herbarium"))),
    )
  })
  
  
  ###############################################################################
  # Set flag showing webtool status of the currently selected taxon
  output$webtool_status <- renderUI({
    if (input$gdm_model != "No model selected")
    {
      if (onWebtool()[taxonName(), "onWebtool"] == "Yes")
        tags$p(style="color:#006400;", strong("On webtool: ", onWebtool()[taxonName(), "onWebtool"]))
      else
        tags$p(style="color:#ff6347;", strong("On webtool: ", onWebtool()[taxonName(), "onWebtool"]))
    }
    else
      tags$p(strong("On webtool:   "))
  })
  
  
  ###############################################################################
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    RandRdb <- RSQLite::dbConnect(SQLite(), sqlPath)
    
    cleanedTaxonName <- gsub("D$|DQ$|DQK2$|DQK3$", "", taxonName())
    
    # Grab sample data from db here so it is independently available to DArT display option
    sampleResults <- RSQLite::dbGetQuery(RandRdb, paste0('SELECT NSWnumber, decimalLongitude, decimalLatitude, dateToDArT FROM tissueSamples WHERE "scientificName" = "', cleanedTaxonName, '"
                                                OR "fieldSpeciesName" LIKE "', paste0(cleanedTaxonName, "%"), '" OR "otherSpeciesName" LIKE "', paste0(cleanedTaxonName, "%"), '";'))
    
    if ("R&R Samples" %in% input$showCheckGrp)
    {
      if (any(sampleResults$decimalLongitude == "No_data"))
      {
        sampleResults <- sampleResults[-which(sampleResults$decimalLongitude == "No_data"),]
      }
      
      samplePts <- cbind(sampleResults[,c("NSWnumber", "decimalLongitude", "decimalLatitude")], ptType = rep(2, nrow(sampleResults)))
      colnames(samplePts) <- c("recordID", "longitude", "latitude", "ptType")
    } else
    {
      samplePts <- NULL
    }
    
    if ("R&R Vouchers" %in% input$showCheckGrp)
    {
      voucherPts <- dbGetQuery(RandRdb, paste0('SELECT NSWnumber, decimalLongitude, decimalLatitude FROM voucherSamples WHERE "scientificName" = "',input$infoTaxon,'"'))
      if (!is.null(voucherPts))
      {
        voucherPts <- cbind(voucherPts, ptType = rep(1, nrow(voucherPts)))
        colnames(voucherPts) <- c("recordID", "longitude", "latitude", "ptType")
      }
      else
      {
        voucherPts <- NULL
      }
    } else
    {
      voucherPts <- NULL
    }
    
    if (("DArT Samples" %in% input$showCheckGrp) && (nrow(sampleResults) > 0))
    {
      DArT_Ind <- which(sampleResults$dateToDArT != "No_data")
      if (length(DArT_Ind) > 0)
      {
        DArT_Pts <- sampleResults[DArT_Ind,]
        badRows <- which(DArT_Pts$decimalLongitude  == "No_data")
        if (length(badRows) > 0) DArT_Pts <- DArT_Pts[-badRows, ]
        DArT_Pts$dateToDArT = rep(3, nrow(DArT_Pts))
        colnames(DArT_Pts) <- c("recordID", "longitude", "latitude", "ptType")
      }
      else
      {
        DArT_Pts <- NULL
      }
    }
    else
    {
      DArT_Pts <- NULL
    }
    
    #cleanedTaxonName <- gsub("D$|DQ$|DQK2$|DQK3$", "", taxonName())
    thisFile <- paste0(basePathOcc, cleanedTaxonName, "/", sub(" ", "_", cleanedTaxonName), "_herbariumRecords.csv")
    
    if (("ALA herbarium" %in% input$showCheckGrp) && (file.exists(thisFile)))
    {
      ALAspecimenPts <- read.csv(thisFile,
                                 stringsAsFactors = FALSE)[ ,c("siteKey", "longitude", "latitude")]
      ALAspecimenPts <- cbind(ALAspecimenPts,ptType = rep(4, nrow(ALAspecimenPts)))
      colnames(ALAspecimenPts) <- c("recordID", "longitude", "latitude", "ptType")
    }
    else
    {
      ALAspecimenPts <- NULL
    }
    
    surveyFilename <- paste0(basePathOcc, cleanedTaxonName, "/", sub(" ", "_", cleanedTaxonName), "_surveyRecords.csv")
    
    if (("Survey records" %in% input$showCheckGrp) && (file.exists(surveyFilename)))
    {
      surveyPts <- read.csv(surveyFilename,
                            stringsAsFactors = FALSE)[ , c("siteKey", "longitude", "latitude")]
      surveyPts <- cbind(surveyPts,ptType = rep(5, nrow(surveyPts)))
      colnames(surveyPts) <- c("recordID", "longitude", "latitude", "ptType")
    }
    else
    {
      surveyPts <- NULL
    }
    
    tmp <- rbind(voucherPts, samplePts, DArT_Pts, ALAspecimenPts, surveyPts)
    
    if (!is.null(tmp))
    {
      NArows <- which(is.na(tmp$longitude))
      if (length(NArows) > 0) tmp <- tmp[-NArows,]
    }
    
    RSQLite::dbDisconnect(RandRdb)
    
    tmp$latitude <- as.numeric(tmp$latitude)
    tmp$longitude <- as.numeric(tmp$longitude)
    
    return(tmp)
  })
  
  
  ###############################################################################
  observe({
    df <- filteredData()
    sampleInd <- which(df$ptType == 2)
    ALAind <- which(df$ptType == 4)
    DArTind <- which(df$ptType == 3)
    voucherInd <- which(df$ptType == 1)
    surveyInd <- which(df$ptType == 5)
    
    map <- leafletProxy("map", data = df)
    map %>% clearMarkers()
    
    if (!is.null(df))
    {
      if (length(surveyInd) > 0)
      {
        surveyIcon <- makeIcon(
          iconUrl = paste0(basePath, "images/marker-icon-grey-small.png"),
          iconWidth = 12, iconHeight = 20,
          iconAnchorX = 6, iconAnchorY = 18
        )
        
        map %>% addMarkers(icon = surveyIcon,
                           options = markerOptions(clickable=FALSE),
                           data=df[surveyInd,])
      }
      
      if (length(ALAind) > 0)
      {
        alaIcon <- makeIcon(
          iconUrl = paste0(basePath, "images/marker-icon-green-small.png"),
          iconWidth = 12, iconHeight = 20,
          iconAnchorX = 6, iconAnchorY = 18
        )
        
        map %>% addMarkers(icon = alaIcon,
                           options = markerOptions(clickable=FALSE),
                           data=df[ALAind,])
      }
      
      if (length(sampleInd) > 0)
      {
        tissueIcon <- makeIcon(
          iconUrl = paste0(basePath, "images/marker-icon-orange-small.png"),
          iconWidth = 12, iconHeight = 20,
          iconAnchorX = 6, iconAnchorY = 18
        )
        
        map %>% addMarkers(icon = tissueIcon,
                           options = markerOptions(clickable=FALSE),
                           data = df[sampleInd,])
      }
      
      if (length(voucherInd) > 0)
      {
        voucherIcon <- makeIcon(
          iconUrl = paste0(basePath, "images/marker-icon-red-small.png"),
          iconWidth = 12, iconHeight = 20,
          iconAnchorX = 6, iconAnchorY = 18
        )
        
        map %>% addMarkers(icon = voucherIcon,
                           options = markerOptions(clickable=FALSE),
                           data = df[voucherInd,])
      }
      
      if (length(DArTind) > 0)
      {
        dartIcon <- makeIcon(
          iconUrl = paste0(basePath, "images/marker-icon-violet-small.png"),
          iconWidth = 12, iconHeight = 20,
          iconAnchorX = 6, iconAnchorY = 18
        )
        
        map %>% addMarkers(icon = dartIcon,
                           options = markerOptions(clickable=FALSE),
                           data = df[DArTind,])
      }
    }
    
    # Re-instate location marker
    click <- input$map_click
    if (!is.null(click))
    {
      currPtLat <- click$lat
      currPtLong <- click$lng
      map %>% addMarkers(click$lng, click$lat)
    }
  })
  
  
  ###############################################################################
  # Render a 3D view of the genetic difference surface for the currently active
  # model and selected location
  output$plotyplot <- renderPlotly({
    
    click <- input$map_click
    
    if (!is.null(click))
    {
      testPt <- c(click$lng, click$lat)

      gdm_ras <- gdmSurface()
      gdm_ras_cropped <-raster::crop(gdm_ras, raster::extent(sf::st_bbox(bounds_poly())))
      gdm_ras_matrix <- t(as.matrix(gdm_ras_cropped))
      
      p <- plotly::plot_ly(x = raster::yFromRow(gdm_ras_cropped, 1:nrow(gdm_ras_cropped)),
                           y = raster::xFromCol(gdm_ras_cropped, 1:ncol(gdm_ras_cropped)),
                           z = gdm_ras_matrix,
                           type = "surface",
                           colorscale = "Jet",
                           height = 700)#%>%
      p <- p %>% layout(scene = list(xaxis = list(title = "Latitude", autorange="reversed"),
                                     yaxis = list(title = "Longitude")))
      p
    }
  })
  
  
  ###############################################################################
  # Reset GDM threshold to the default value stored in the GDM object
  observeEvent(input$reset,
               {
                 updateSliderInput(session, inputId = "threshold", value = getDefaultThreshold())
               })
  
  
  
  
  ###############################################################################
  # Load the clipped bounds polygon when a species changes; also load a transition zone layer if it exists
  observe({
    if (input$gdm_model != "No model selected")
    {
      # As an spatial polygon object for testing if clicked location is inside the model domain
      #boundsPolyName <- paste0(boundsFolder,"/", gsub(" ", "_", taxonName(), fixed = TRUE), "_domain_clipped.geojson")
      boundsPoly_json <- jsonlite::toJSON(jsonlite::fromJSON(txt = boundsPolyName()), auto_unbox = TRUE)
      
      # Show the selected model default threshold in the threshold slider:
      updateSliderInput(session, inputId = "threshold", value = getDefaultThreshold())
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("Domain") %>%
        removeGeoJSON("Zones") %>%
        addGeoJSON(boundsPoly_json, layerId = "Domain", color = layerPalette[4], group = "Model domain")
      
      # Identify and load any transition zone files for the selected model
      theseFileInd <- grep(sub(" ", "_", taxonName(), fixed = TRUE), zonesList)
      
      if (length(theseFileInd) > 0)
      {
        for (i in theseFileInd)
        {
          zonePoly_json <- jsonlite::toJSON(jsonlite::fromJSON(txt = paste0(zonesFolder, "/", zonesList[i])), auto_unbox = TRUE)
          proxy %>%
            removeGeoJSON("Zones") %>%
            addGeoJSON(zonePoly_json, layerId = "Zones", color = layerPalette[5], group = "Transition zones")
        }
      }
    }
    else
    {
      #boundsPoly <<- NULL
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("Domain") %>%
        removeGeoJSON("Zones") %>%
        clearMarkers() %>%
        removeGeoJSON("GDM")
      ##### Set theshold to 0
      updateSliderInput(session, inputId = "threshold", value = 0)
    }
  })
  
  
  ###############################################################################
  # Display the bsModal showing 3D genetic difference surface
  # Based on https://stackoverflow.com/questions/45547885/modal-dialog-in-shiny-can-adjust-width-but-not-height
  # Answer by "Florian" on 2017-08-07
  
  observeEvent(input$show3D,
               {
                 toggleModal(session, modalId = "show3dStuff", toggle = "toggle")
               })
  
  
  ###############################################################################
  # Show general help dialog
  observeEvent(input$help,
               {
                 showModal(modalDialog(div(img(src="Restore_and Renew_logo_green_275px_whiteBackground.png", style="float:right;"),
                                           h2("Using the app"), style = "height:80px;"),
                                       div(p(),
                                           p(strong("GDM Model:")," Select the model to be used to compute local genetic region. Select ",em("No model selected")," to show only environmental matching layers."),
                                           p(strong("Environmental variables:")," Environmental matching is performed for using the selected variables.Domain files are placed in www/models/domain. GDM model objects are placed in www/models/gdm."),
                                           p(strong("Future climate scenario:")," Future climate is computed using the combination of RCP and Epoch selected here. Recall that what is computed is the distribution ", em("today"), " of environmental conditions predicted ",em("at the selected loccation"), " for the selected epoch."),
                                           p(strong("Display control:")," The 'stack of layers' icon in the top right corner of the map opens a checklist allowing you to turn the display of environmental or genetic layers on and off to give more a easily interpreted map."),
                                           p(strong("Requirements:")," This app requires the following ", em("R"),"-packages: ", em("shiny, leaflet, rgdal,")," and ",em("raster"),".")),
                                       size = "m"
                 ))})
  
  
  ###############################################################################
  # Show a model dialog giving information on adding a new models,
  observeEvent(input$addModel,
               {
                 showModal(modalDialog(div(img(src="Restore_and Renew_logo_green_275px_whiteBackground.png", style="float:right;"),
                                           h2("Adding a new model"), style = "height:80px;"),
                                       div(p(),
                                           p(strong("File names:")," Convention is 'Genus_specificEpithet_' followed by either 'domain.geojson' or 'genetic_model.Rd'. Model names are added to the pull-down list at app start-up by listing the file names found in the 'model' folder. If you wish to compare competing models for species, then add an identifier to the specificEpithet part of the file name. For example, 'Acacia_suaveolens' and 'Acacia_suaveolensNoElev'."),
                                           p(strong("File locations:"), " All data and models live in the 'www' folder below the folder in which you placed a copy of the app. Domain files are placed in 'www/models/domain'. GDM model objects are placed in 'www/models/gdm', and Q covariate rasters are placed in www/models/qData/eastOZ''."),
                                           p(strong("Preparatory tasks:")," To perform an 'in-bounds' check for a clicked location, a clipped version of the model bounds geoJSON file must be prepared using the R-script in the file 'clipDomainPolygon.R'"),
                                           p("If a GDM uses Q covariates, the tif files must be placed in the folder '/www/qData/eastOZ' and the function ", em("setModelCovars()")," the ", em("R"),"-package RandR.webtool.dataPrep run to setup the covariate paths in the model object."),
                                           p(strong("Requirements:")," This Shiny app requires the following ", em("R"), "-packages: ", em("shiny"), ", ", em("shinybusy"), ", ", em("shinyjs"), ", ", em("shinyBS"), ", ", em("leaflet"), ", ", em("sf"), ", ", em("raster"), ", ", em("jsonlite"), " and ", em("gdm"))),
                                       size = "m"
                 ))})
  
}  

###############################################################################
shinyApp(ui, server, options = list(launch.browser = TRUE))


