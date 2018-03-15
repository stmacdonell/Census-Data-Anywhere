# This app was developed by Sarah Hugenberger and Adrianna ______ at the Austin
# hackathon. It sets up the Shiny web interface to gather information used by
# Scott Macdonell's R script AddCensusDemographics.R

# The Shiny app/script can be used to add demographic data to any shapefile.
# The necessary input are the following:
# --Shapefiles (zipfile with all necessary shapefile parts)
# --Census API key (necessary for accessing census data, they limite queries)
# --State where shapefile is located
# 

# Check if other libraries are needed
source("R/AddCensusDemographics.R")
library(shiny)
library(shinyjs)
library(sf)
library(maps)
library(cdlTools)
library(ggplot2)


# Define UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Add Census data to your Shapefile"),
  shinyjs::useShinyjs(),
  # disabled buttons are black, but with better knowledge of javascript this could 
  # be improved to look more like a typical grayed out button
  tags$style(".btn.disabled {
    background-color: black;
             }"),
  
  sidebarLayout(
    
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      singleton(
        tags$head(tags$script(src = "message-handler.js"))
      ),
      
      # Input: Select a file ----
      fileInput("zipShape", "Browse to select your zipped shapefiles",
                multiple = FALSE,
                accept = ".zip"),
      
      
      # Link: create Census API key
      textInput("enterAPI", "Enter your Census API Key below"),
      wellPanel(
        helpText(   a("If you don't have a Census API key, create one here.", href="https://api.census.gov/data/key_signup.html"))),
      
      
      
      
      # Horizontal line ----
      tags$hr(),
      
      # Select your state
      selectInput("state", "Choose the state where your Shapefile is located:", 
                  c("--State List--","AL","AK","AZ","AR","CA","CO","CT", "DC","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"), , FALSE), 
      
      
      
      actionButton("submit", "Submit"),
      actionButton("maps", "View Map"), #starts out disabled/black
      # Horizontal line ----
      tags$hr(),
      # Using "MapWithDemographics.zip" was the only way I was able to get
      # a zip file output. This parameter is used as the name of the downloaded
      # file. Using anything without a .zip ending meant my PC didn't recognize
      # it as a zipfile. This can't be the right way to do it.
      downloadButton("MapWithDemographics.zip", "Download Results") #starts disabled/black
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      

      # We show the user the basic shapefile they've just uploaded
      plotOutput("plot_map")
      
      
      
    )
    
  )
)


# Define server logic to read selected file ----
server <- function(input, output, session) {

  
  

  observeEvent(input$submit, {
    # This thank you alert can be replaced with a progress bar or something better
    shinyjs::alert("Thank you")
  })
  
  observe({
    # These buttons remain disabled until the processing is complete
    shinyjs::disable("maps")
    shinyjs::disable("MapWithDemographics.zip")
   
    
  })
  
  output$plot_map <- renderPlot({
    if (!is.null(input$zipShape)) {
      #Extract zip file and save list of filenames to shapeList
      shapeList <- unzip(input$zipShape$datapath)
      #Determine the position in list where the *.shp file lives (needed for st_read())
      posShp <- grep(".shp", shapeList, ignore.case = TRUE)
      #Extract the name of the *.shp file
      shpName <- shapeList[posShp]
      #strip off the leading "./" in the filename
      finalShpName <- sub("\\../*", "", shpName)
      
      #load in the .shp file with sf library function
      map <- st_read(finalShpName)
      plot(st_geometry(map), main = "Your Shapefile:")
      fmap<<-map
    }
  }
  )
  
  observeEvent(input$submit, {
    # assign all user input to variables
    api_key <- input$enterAPI
    stateAbbrev <- input$state
    fipscode <- fips(stateAbbrev, to = "FIPS")
    #***county is hardcoded to testing data***, not sure if NULL works
    county = c(17) #hardcoded for testing, 17 is for Middlesex County, MA
    
    # Call to Scott's script
    MapWithDemographics <- AddCensusDemographics(fmap,api_key,fipscode,county)
    st_write(MapWithDemographics,dsn="testing",layer="MapWithDemographics",driver = "ESRI Shapefile")

    # Enable buttons
    shinyjs::enable("maps")
    shinyjs::enable("MapWithDemographics.zip")
    
   
    
 })
  
  # In order to replace the original simple plot of the shapefile with a plot
  # using the newly assigned census data, we chose to plot % of BVAP (black 
  # voting age population). Ideally user could specify which demographic to plot
  observeEvent(input$maps, {
    output$plot_map <- renderPlot({
      newMap<<-st_read("testing/MapWithDemographics.shp")
      ggplot(newMap) + geom_sf(aes(fill=Pop18BL / Pop18)) + scale_fill_distiller(palette = "Greens", direction = 1) + theme_bw() + ggtitle("Percentage BVAP") +theme(plot.title = element_text(hjust = 0.5),legend.title=element_blank())
    })
  })
  
  output$MapWithDemographics.zip <- downloadHandler(
    filename = function() {
      paste("MapWithDemographics.zip")
      },
    content = function(location) {
      #***Hardcoded path to my local version of zip*** needs to be changed
      # Also, it's zipping the directory "testing" (which contains the shapefiles)
      # rather than zipping the files inside. I don't have time to troubleshoot
      zip(location,dir("testing", full.names = TRUE), zip = "C:/RBuildTools/3.4/bin/zip.exe")
    },
    contentType = "application/zip"
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

