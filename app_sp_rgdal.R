# A hacked version of the code from ShinyLeafletExample2
#
# Peter D. Wilson
# Evolutionary Ecology Section
# Science and Conservation Branch
# Royal Botanic Gardens, Sydney
#
# 26 May 2016; 17 June 2018: Added progress notifications;
# 6 December 2018: Refined a number of UI elements
# 5-7 August 2019: Serious revision to expand the range of environmental variables for
# computing matching environment layers; changes also to calling of functions in a heavily
# re-designed RandR.modelReview R-package.
# 1-8 June 2020: Enhanced by adding spinners, allowing user to select GDM
# threshold but also to allow resetting to the default value stored in the GDM
# object, and updating of help modal dialogs.
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
library(leaflet)
library(rgdal)
library(raster)
library(RandR.modelReview)
library(jsonlite)
library(RSQLite)
library(shinybusy)

# Initialise stuff
markerPalette <- c("red","orange","lightskyblue","lightgreen")
#markerPalette <- brewer.pal(9,"Set1")[c(1,5,4,3)]
#layerPalette <- c("#666666","#FF83FA", "#3366ff")

### layer ordering: local genetic region, current climate match, future climate match, domain, transition zones
layerPalette <- c("#666666", "#3366ff","#F57900", "#adff2f", "#9b30ff")

envFolder <- ""

basePath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_webtool_dev/RandR_modelReview_and_prep/www/"

modelFolder <- paste0(basePath, "models/gdm/")

boundsFolder <- paste0(basePath, "models/domain")

zonesFolder <- paste0(basePath, "models/zones")

sqlPath <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/Databasing/RandR-sqlite-db/RandR_sampling.sqlite"

basePathOcc <- "/home/peterw/Data_and_Projects/RBG Projects/Restore and Renew/RandR_KML/"

modelList <- list.files(modelFolder, "*.Rd") #, full.names = TRUE)
#print(modelList)

zonesList <- list.files(zonesFolder, "*.geojson")

# taxon_Name <- unlist(lapply(strsplit(modelList, "_genetic_model",  fixed = TRUE), function(el){el[1]}))
# names(taxon_Name) <- modelList

modelListDisplay <- c("No model selected", sort(gsub(".Rd$","",basename(modelList), fixed = TRUE)))

boundsPoly <- NULL


###############################################################################










###############################################################################
ui <- bootstrapPage(
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
                            "))),
  leafletOutput("map", width = "78%", height = "100%"),
  absolutePanel(id = "controls", top = 10, right = 10, width = "20%",
                img(src = "Restore_and Renew_logo_green_275px_whiteBackground.png"),
                absolutePanel(top = "90px", height = "20%", left = "1%", width = "90%",
                              selectInput("gdm_model",
                                          label = h4("GDM Model:"),
                                          choices = modelListDisplay
                              ),
                              uiOutput("threshold"),
                              actionButton("reset", "Reset", style = "color : #fff; background-color: #0095FF;")
                ),
                absolutePanel(top = "350px", height = "30%", left = "1%", width = "90%",
                              #h4("Environmental variables:"),
                              uiOutput("envVars")
                ),
                absolutePanel(top = "450px", height = "30%", left = "1%", width = "90%",
                              uiOutput("occRecords")
                ),
                
                absolutePanel(top = "585px", height = "30%", left = "1%", width = "90%",
                              h4("Future climate scenario:"),
                              absolutePanel(#top = "590px", height = "5%", right = 2, width = "25%",
                                radioButtons("rcpRadioGrp",
                                             label=h5("RCP:"),
                                             choices = list("rcp4.5" = 1,
                                                            "rcp8.5" = 2),
                                             selected = 1
                                )),
                              absolutePanel(left = "51%", #top = "590px", height = "5%", , width = "25%",
                                            radioButtons("epochRadioGrp",
                                                         label=h5("Epoch:"),
                                                         choices = list("2050" = 1,
                                                                        "2070" = 2),
                                                         selected = 1))
                ),
                absolutePanel(top = "750px", height = "10%", left = "1%", width = "90%",
                              actionButton("help","Help", icon = icon("info-circle"), width = "110px", style = "color : #fff; background-color: #0095FF;"),
                              actionButton("addModel","Add model", icon = icon("info-circle"), width = "110px", style = "color : #fff; background-color: #0095FF;")
                )
  )
)


server <- function(input, output, session)
{
  # Reactive expression for the data subsetted to what the user selected
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
  
  
  #### A reactive component to nicely present the list of selected environmental variables
  # selectedEnvVars <- reactive({
  #   #showNotification("BLAH CHECK BOX BLAH", duration = 10, id = "busyMsg", type = "error")
  #   ans <- input$envVarsChkBox
  #   #print(ans)
  #   showNotification(paste(ans, collapse = ","), duration = 10, id = "busyMsg", type = "message")
  #   return(paste(ans, collapse = ","))
  # })
  
  getDefaultThreshold <- reactive({
    if (input$gdm_model != "No model selected")
    {
      load(paste0(modelFolder, input$gdm_model))
      return(md$threshold)
    }
    else
      return(0)
  })
  
  
  # Set up map object
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      fitBounds(140.5,-38,154,-27.5) %>%
      #setView(lng = 150.45, lat = -33.66, zoom = 6) %>%
      addLayersControl(overlayGroups = c("Environmental match", "Local Genetic Region", "Model domain", "Transition zones"),
                       options = layersControlOptions(autoZIndex = TRUE)) %>%
      addScaleBar("bottomleft")
  })
  
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
        addMarkers(click$lng,click$lat)
    }
  })
  
  #### Reactive component to compute the current match layer
  currentClimate <- reactive(
    {
      click <- input$map_click
      ###RandR_currentSiteMatch(click$lng, click$lat, input$envVarsChkBox)
      RandR_currentSiteMatch(click$lng, click$lat, input$envVarsChkBox)
    })
  
  #### Compute and display the Current environmental match layer for the selected location and set of env vars
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      show_modal_spinner(spin = "dots", text = "Computing Current environment match", color = "#00BFFF")
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("CC") %>%
        addGeoJSON(currentClimate(), layerId = "CC", color = layerPalette[2], group = "Environmental match")
      remove_modal_spinner()
    }
  })
  
  # Compute and display the Future Climate match areas for the selected taxon
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      show_modal_spinner(spin = "dots", text = "Computing Future environment match", color = "#00BFFF")
      futureBits <- futureParams()
      
      ###futureClimate <- RandR_futureSiteMatch(click$lng, click$lat, input$envVarsChkBox, futureBits$rcp, futureBits$epoch)
      futureClimate <- RandR_futureSiteMatch(click$lng, click$lat, input$envVarsChkBox, futureBits$rcp, futureBits$epoch)
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("FC") %>%
        addGeoJSON(futureClimate, layerId = "FC", color = layerPalette[3], group = "Environmental match")
      
      remove_modal_spinner()
    }
  })
  
  
  # Conditionally compute and display the local genetic region for the selected taxon
  observe({
    click <- input$map_click
    if (!is.null(click))
    {
      proxy <- leafletProxy("map")
      
      removeNotification(id = "badMsg")
      
      if (input$gdm_model != "No model selected")
      {
        thisPt <- data.frame(x = click$lng, y = click$lat)
        coordinates(thisPt) <- ~x + y
        proj4string(thisPt) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
        
        if (!is.na(over(thisPt, boundsPoly)))
        {
          show_modal_spinner(spin = "dots", text = "Computing Local Genetic Region", color = "#00BFFF")
          
          gdmResult <- RandR_GDM_prediction(click$lng,
                                            click$lat,
                                            taxonName(),
                                            paste0(modelFolder, input$gdm_model),
                                            threshold = input$threshold,
                                            verbose = FALSE)
        }
        else
        {
          gdmResult <- NULL
          showNotification("Out of bounds for selected taxon - try another location", duration = NULL, id = "badMsg", type = "error")
        }
      }
      else
        gdmResult <- NULL
      
      proxy %>%
        removeGeoJSON("GDM") %>%
        addGeoJSON(gdmResult, layerId = "GDM", color = layerPalette[1], group = "Local Genetic Region") # %>%
      
      remove_modal_spinner()
    }
  })
  
  
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
  
  
  output$threshold <- renderUI({
    sliderInput(inputId = "threshold",
                label = h4("GDM threshold:"),
                min = 0,
                max = 1,
                value = 0)
  })
  
  
  output$envVars <- renderUI({
    tags$div(align = "left",
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
  
  
  output$occRecords <- renderUI({
    tags$div(align = "left",
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
  
  
  # Reactive expression to provide taxon name for the selected model
  taxonName <- reactive({
    return(trimws(sub(" genetic model.Rd", "", gsub("_", " ", input$gdm_model, fixed = TRUE), fixed = TRUE)))
  })
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    RandRdb <- dbConnect(SQLite(), sqlPath)
    
    cleanedTaxonName <- gsub("D$|DQ$|DQK2$|DQK3$", "", taxonName())
    
    # Grab sample data from db here so it is independently available to DArT display option
    sampleResults <- dbGetQuery(RandRdb, paste0('SELECT NSWnumber, decimalLongitude, decimalLatitude, dateToDArT FROM tissueSamples WHERE "scientificName" = "', cleanedTaxonName, '"
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
    
    dbDisconnect(RandRdb)
    
    tmp$latitude <- as.numeric(tmp$latitude)
    tmp$longitude <- as.numeric(tmp$longitude)

    return(tmp)
  })
  
  
  
  observe({
    df <- filteredData()
    sampleInd <- which(df$ptType == 2)
    ALAind <- which(df$ptType == 4)
    DArTind <- which(df$ptType == 3)
    voucherInd <- which(df$ptType == 1)
    surveyInd <- which(df$ptType == 5)
    
    map <- leafletProxy("map", data = df)
    map %>% clearMarkers() # %>% clearMarkerClusters()
    
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
                           #clusterOptions = markerClusterOptions(),
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
                           #clusterOptions = markerClusterOptions(),
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
                           #clusterOptions = markerClusterOptions(),
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
                           #clusterOptions = markerClusterOptions(),
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
                           #clusterOptions = markerClusterOptions(),
                           data = df[DArTind,])
      }
    }
    
    # Re-instate location marker
    click <- input$map_click
    if (!is.null(click))
    {
      currPtLat <- click$lat
      currPtLong <- click$lon
      map %>% addMarkers(click$lng,click$lat)
    }
  })
  
  
  # Show general help dialog
  observeEvent(input$help,{showModal(modalDialog(div(img(src="Restore_and Renew_logo_green_275px_whiteBackground.png", style="float:right;"),
                                                     h2("Using the app"), style = "height:80px;"),
                                                 div(p(),
                                                     p(strong("GDM Model:")," Select the model to be used to compute local genetic region. Select ",em("No model selected")," to show only environmental matching layers."),
                                                     p(strong("Environmental variables:")," Environmental matching is performed for using the selected variables.Domain files are placed in www/models/domain. GDM model objects are placed in www/models/gdm."),
                                                     p(strong("Future climate scenario:")," Future climate is computed using the combination of RCP and Epoch selected here. Recall that what is computed is the distribution ", em("today"), " of environmental conditions predicted ",em("at the selected loccation"), " for the selected epoch."),
                                                     p(strong("Display control:")," The 'stack of layers' icon in the top right corner of the map opens a checklist allowing you to turn the display of environmental or genetic layers on and off to give more a easily interpreted map."),
                                                     p(strong("Requirements:")," This app requires the ",em("R"),"-package ",em("RandR.modelReview"),", a modified version of the ",em("R"),"-package ",em("RandR.webtool"),". It is available on the N-drive at 'Evolutionary Ecology/Restore & Renew/RandR.modelReview'. It also requires the following ", em("R"),"-packages: ", em("shiny, leaflet, rgdal,")," and ",em("raster"),".")),
                                                 size = "m"
  ))})
  
  
  # Show a model dialog giving information on adding a new models,
  observeEvent(input$addModel,{showModal(modalDialog(div(img(src="Restore_and Renew_logo_green_275px_whiteBackground.png", style="float:right;"),
                                                         h2("Adding a new model"), style = "height:80px;"),
                                                     div(p(),
                                                         p(strong("File names:")," Convention is 'Genus_specificEpithet_' followed by either 'domain.geojson' or 'genetic_model.Rd'. Model names are added to the pull-down list at app start-up by listing the file names found in the 'model' folder. If you wish to compare competing models for species, then add an identifier to the specificEpithet part of the file name. For example, 'Acacia_suaveolens' and 'Acacia_suaveolensNoElev'."),
                                                         p(strong("File locations:")," Domain files are placed in www/models/domain. GDM model objects are placed in www/models/gdm."),
                                                         p(strong("Preparatory tasks:")," To perform an 'in-bounds' check for a clicked location, a clipped version of the model bounds geoJSON file must be prepared using the R-script in the file 'clipDomainPolygon.R'"),
                                                         p("If a GDM uses Q covariates, the tif files must be placed in the folder '/www/qData/eastOZ' and the function ", em("setModelCovars()")," the ", em("R"),"-package RandR.webtool.dataPrep run to setup the covariate paths in the model object."),
                                                         p(strong("Requirements:")," This Shiny app requires the ",em("R"),"-package ",em("RandR.modelReview"),", a modified version of the ",em("R"),"-package ",em("RandR.webtool.")," It is available on the N-drive at 'Evolutionary Ecology/Restore & Renew/RandR.modelReview'. It also requires the following ", em("R"),"-packages: ", em("shiny, leaflet, rgdal, RSQLite, jsonlite")," and ",em("raster.")," To display maps it requires access to the Internet. To display occurrence records, it requires access to the N-drive and so can only be run on DPIE laptops with VPN access, or PCs within the Brown Buidling.")),
                                                     size = "m"
  ))})
  
  
  observeEvent(input$reset, {updateSliderInput(session, inputId = "threshold", value = getDefaultThreshold())})
  
  
  
  
  # Load the clipped bounds polygon when a species changes; also load a transition zone layer if it exists
  observe({
    if (input$gdm_model != "No model selected")
    {
      # As an spatial polygon object for testing if clicked location is inside the model domain
      boundsPolyName <- paste0(boundsFolder,"/", sub(" ", "_", taxonName(), fixed = TRUE), "_domain_clipped.geojson")
      boundsPoly <<- readOGR(boundsPolyName, verbose = FALSE)

      boundsPoly_json <- jsonlite::toJSON(jsonlite::fromJSON(txt = boundsPolyName), auto_unbox = TRUE)

      # Show the selected model default threshold in the threshold slider:
      updateSliderInput(session, inputId = "threshold", value = getDefaultThreshold())
      
      proxy <- leafletProxy("map")
      proxy %>%
        removeGeoJSON("Domain") %>%
        removeGeoJSON("Zones") %>%
        addGeoJSON(boundsPoly_json, layerId = "Domain", color = layerPalette[4], group = "Model domain")

      # Identify and load any transition zone files for the selected model
      
      theseFileInd <- grep(sub(" ", "_", taxonName(), fixed = TRUE), zonesList)
      #print(theseFileInd)
      
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
      boundsPoly <<- NULL
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
}


#
shinyApp(ui, server, options = list(launch.browser = TRUE))

