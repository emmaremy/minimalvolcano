library(shiny)
library(leaflet)
library(dplyr)

#Emma Remy
#Spring 2016

# Reading in data
EL <- read.csv("./GVP_Eruption_Results_latest.csv")
Volcanoes <- read.csv("./GVP_Volcano_List_latest.csv")

# Doing a bit of data wrangling
# (not strictly necessary,
# but creates some interesting data)
EL.conf<- 
  EL %>%
  filter(Eruption.Category == "Confirmed Eruption") %>%
  mutate(Start.Date= as.Date(paste(Start.Year, Start.Month, 
                                   Start.Day, sep = "." ), 
                             format = "%Y.%m.%d" )) %>%
  mutate(End.Date= as.Date(paste(End.Year, End.Month,
                                 End.Day, sep = "." ), 
                           format = "%Y.%m.%d" )) %>%
  mutate(Duration = as.numeric(End.Date-Start.Date +1)) %>%
  filter(Duration>=0)
EL.conf <- EL.conf[,-c(23,24)]

# Combining eruption data with information about volcanoes
comb.EL <- 
  EL.conf %>%
  inner_join(Volcanoes, by="Volcano.Number") %>%
  group_by(Volcano.Number, Volcano.Name.x, Country, Region, Subregion, 
           Primary.Volcano.Type, Dominant.Rock.Type) %>%
  summarize(avg.VEI = mean(VEI), Latitude = min(Latitude), 
            Longitude = min(Longitude), 
            eruption_count = n_distinct(Eruption.Number)) 

#server side
server<-function(input, output, session){
  
  # Initial map creation
  output$map <- renderLeaflet({
    vol <- comb.EL
    leaflet(vol) %>%
      addTiles(options=tileOptions(minZoom=2, maxZoom=10)) %>%
      setView(lng=-80, lat=5, zoom=4)
  })
  
  # Calculates and creates subset of data
  # based on the bounds of the current map window
  volInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(comb.EL[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(comb.EL,
           Latitude >= latRng[1] & Latitude <= latRng[2] &
             Longitude >= lngRng[1] & Longitude <= lngRng[2])
  })
  
  # Adds markers to map
  # and ensures that they are cleared when reloading
  # (reloads whenever bounds on map window change)
  observe({
    volWithCircles <- data.frame(volInBounds())
    leafletProxy("map", data=volWithCircles) %>%
      clearMarkers() %>%
      clearControls()
    if (nrow(volWithCircles) > 1)
      leafletProxy("map", data=volWithCircles) %>%
      addCircleMarkers(radius=10, color = "red", weight = 1, 
                       opacity = .8, fillColor="red", fillOpacity=.3)
  })
  
  # Popups on click
  showVolcanoPopup <- function(volcano, lat, lng) {
    selectedVolcano <- comb.EL %>%
      filter(Latitude == lat) %>%
      filter(Longitude == lng)
    content <- paste("<b>", selectedVolcano$Volcano.Name.x, "</b></br>",
                     selectedVolcano$Primary.Volcano.Type, "<br/>
                     Number of confirmed eruptions: ", selectedVolcano$eruption_count, "<br/>
                     Major rock type:", selectedVolcano$Dominant.Rock.Type
    )
    leafletProxy("map") %>% 
      addPopups(lng, lat, popup=content)
  }
  
  # Observing clicks and generating popups
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showVolcanoPopup(event$id, event$lat, event$lng)
    })
  })
}

#UI side
ui <- bootstrapPage(
  
  # To get the app to fill the entire page
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # The map
  leafletOutput("map", width="100%", height="100%"),
  
  # Description
  absolutePanel(top = 10, right = 50,
                h3("Volcano explorer", align = "right"),
                p("Each circle represents a volcano.", br(),
                "Click, drag, and scroll to explore", br(),
                "the volcanoes of the world.",
                align = "right")
  ),
  
  # Credits
  absolutePanel(bottom=10, right = 10,
                p("Global Volcanism Program, 2013.
                  Volcanoes of the World, v. 4.4.3. Venzke,
                  E (ed.). Smithsonian Institution. Downloaded
                  30 Mar 2016.", a("http://dx.doi.org/10.5479/si.GVP.VOTW4-2013"),
                  align = "right", style = "color:gray")
                )
)        

shinyApp(ui=ui, server=server)
