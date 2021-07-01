require(remotes)
#### Uncomment below line to install cft package if you don't have it ####
#remotes::install_github("earthlab/cft") 
require(cft)
require(shiny)
require(rgdal)
require(mapview)

reference <- argument_reference

server <- function(input, output) {
  output$table <- renderTable({
    tbl <- as.data.frame(matrix(NA,nrow = length(reference$labels),
                                ncol = 2))
    tbl[,1] <- names(reference$labels)
    for (i in 1:length(reference$labels)) {
      tbl[i,2] <- reference$labels[i]
    }
    colnames(tbl) <- c("label","Description")
    tbl
  })
  
  output$message <- renderText("Your files are ready for download once your
                                 shapefile is loaded above")
  
  observeEvent(input$goButton,{
    shpdf <- input$fileshp
    if(is.null(shpdf)){
      return()
    }
    previouswd <- getwd()
    uploaddirectory <- dirname(shpdf$datapath[1])
    setwd(uploaddirectory)
    for(i in 1:nrow(shpdf)){
      file.rename(shpdf$datapath[i], shpdf$name[i])
    }
    setwd(previouswd)
    
    map <- readOGR(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
    #map <- spTransform(map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
    
    output$mapview<-renderMapview({
      mapview(map)
    })
    startY <- as.numeric(format(input$yearly[1],format="%Y"))
    endY <-  as.numeric(format(input$yearly[2],format="%Y"))
    d <- cftdata(aoi = map,
                 area_name = input$areaname,
                 parameters = input$parameters,
                 years = c(startY,endY),
                 models = input$models,
                 local_dir = getwd(),
                 scenarios = input$scenarios)
    df <- cft_df(d)
    temp <- df
    df <- subset(df, date>= input$yearly[1]&date<= input$yearly[2])
    
    temp$month <- format(temp$date,format="%m")
    temp$year <- format(temp$date,format="%Y")
    
    annualdf <- aggregate(temp[-c(1:5,ncol(temp)-1,ncol(temp))],
                          list(temp$year,temp$rcp,
                               temp$model,temp$ensemble,
                               temp$area_name), mean)
    colnames(annualdf)[1] <- "year"
    colnames(annualdf)[2:5] <- colnames(temp)[c(1,3:5)]
    
    monthlydf <- aggregate(temp[-c(1:5,ncol(temp)-1,ncol(temp))],
                           list(temp$month,temp$year,temp$rcp,
                                temp$model,temp$ensemble,
                                temp$area_name), mean)
    colnames(monthlydf)[1:2] <- c("month","year")
    colnames(monthlydf)[3:6] <- colnames(temp)[c(1,3:5)]
    
    output$downloadDailyData <- downloadHandler(
      filename = function() {
        paste0(input$areaname,"_",input$parameters,
               "_",input$models,"_",input$scenarios,
               "_",input$yearly[1],
               "_",input$yearly[2],"_daily.csv")
      },
      content = function(file) {
        write.csv(df, file, row.names = FALSE)
      })
    
    output$downloadMonthlyData <- downloadHandler(
      filename = function() {
        paste0(input$areaname,"_",input$parameters,
               "_",input$models,"_",input$scenarios,
               "_",format(input$yearly[1],format="%Y"),
               "_",format(input$yearly[2],format="%Y"),
               "_monthly.csv")
      },
      content = function(file) {
        write.csv(monthlydf, file, row.names = FALSE)
      })
    output$downloadAnnualData <- downloadHandler(
      filename = function() {
        paste0(input$areaname,"_",input$parameters,
               "_",input$models,"_",input$scenarios,
               "_",format(input$yearly[1],format="%Y"),
               "_",format(input$yearly[2],format="%Y"),"_annual.csv")
      },
      content = function(file) {
        write.csv(annualdf, file, row.names = FALSE)
      })
  })
}

ui <- fluidPage(navbarPage("NC CASC Toolbox",
                     tabPanel("Data Download",
                              sidebarLayout(
                                sidebarPanel(
                                  fileInput("fileshp", "Import Shapefile",multiple = T),
                                  textInput("areaname",
                                            "Enter Name of Area of Interest:"),
                                  selectInput("models", "Choose Model(s):",
                                              choices = reference$models,
                                              multiple = T),
                                  selectInput("parameters", "Choose Parameter(s):",
                                              choices = reference$parameters,
                                              multiple = T),
                                  selectInput("scenarios", "Choose Scenario(s):",
                                              choices = reference$scenarios,
                                              multiple = T),
                                  dateRangeInput("yearly", "Enter Period of Record",
                                                 start = "2001-01-01", end = "2003-12-31",
                                                 min = "1950-01-01",max = "2099-12-31",
                                                 format = "MM dd, yyyy"),
                                  actionButton(inputId = "goButton",label = "Go!")
                                ),
                                mainPanel(mapviewOutput("mapview"),
                                          textOutput("message"),
                                          downloadButton("downloadDailyData",
                                                         "Click here to Download the Daily Spatially-Averaged Time Series as .csv"),
                                          downloadButton("downloadMonthlyData",
                                                         "Click here to Download the Mean Monthly Spatially-Averaged Time Series as .csv"),
                                          downloadButton("downloadAnnualData",
                                                         "Click here to Download the Mean Annual Spatially-Averaged Time Series as .csv")
                                ))),
                     tabPanel("Data Information",
                              fluidRow(column(12, tableOutput("table")))
                     )))

shinyApp(ui = ui, server = server)