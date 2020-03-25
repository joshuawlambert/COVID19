library(shiny)
library(leaflet)
library(maps)
library(htmltools)
library(ggplot2)
library(sp)

shinyApp(
  ui = fluidPage(theme = 'slate',
    h2("Choropleth Maps of COVID-19 Cases in the United States"),
    mainPanel(width = "1000px",
      tabsetPanel(
        tabPanel(title = "Cases",icon = icon("heartbeat", "fa-2x"),
                 leafletOutput("mymap.cases",height = "800px")
        ),
         tabPanel(title = "Beds",icon = icon("bed", "fa-2x"),
                  tabsetPanel(
                    tabPanel(title = "Beds(15 miles)",
                             leafletOutput("mymap.beds.15",height = "800px")
                             ),
                    tabPanel(title = "Beds(30 miles)",
                             leafletOutput("mymap.beds.30",height = "800px")
                    ),
                    tabPanel(title = "Beds(45 miles)",
                             leafletOutput("mymap.beds.45",height = "800px")
                    ),
                    tabPanel(title = "Beds(60 miles)",
                             leafletOutput("mymap.beds.60",height = "800px")
                    ),
                    tabPanel(title = "Beds(120 miles)",
                             leafletOutput("mymap.beds.120",height = "800px")
                    )
                  )
         ),
         tabPanel(title = "Beds to Cases",icon = icon("procedures", "fa-2x"),
                  tabsetPanel(
                  tabPanel(title = "Beds to Cases (15 miles)",
                           leafletOutput("mymap.bedscases.15",height = "800px")
                  ),
                  tabPanel(title = "Beds to Cases (30 miles)",
                           leafletOutput("mymap.bedscases.30",height = "800px")
                  ),
                  tabPanel(title = "Beds to Cases (45 miles)",
                           leafletOutput("mymap.bedscases.45",height = "800px")
                  ),
                  tabPanel(title = "Beds to Cases (60 miles)",
                           leafletOutput("mymap.bedscases.60",height = "800px")
                  ),
                  tabPanel(title = "Beds to Cases (120 miles)",
                           leafletOutput("mymap.bedscases.120",height = "800px")
                  )
                  )
         ),
        tabPanel(title = "About this appication",icon = icon("question-circle", "fa-2x"),
                 tags$a(href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html", "Click here for Case Data,   "),
                 h1(),  
                 tags$a(href="https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data?geometry=134.132%2C-16.829%2C-165.048%2C72.120&orderBy=STATE", "Click here for Hospital Data"),
                 uiOutput(outputId = "lastUpdate")
        )
    )
    )
    ),
  server = function(input, output) {
    load(file = "data/data.RData")
    
    output$mymap.cases<- renderLeaflet({
      bins <- round(quantile(mapCounty$cases,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
      pal <- colorBin("YlOrRd", domain = mapCounty$cases , bins = bins,reverse = FALSE)
      
      labels <- sprintf(
        "<strong>%s</strong><br/>%g cases within county",
        paste(mapCounty$names,"county"), mapCounty$cases
      ) %>% lapply(htmltools::HTML)
      
      leaflet(data = mapCounty) %>% addTiles() %>%
        addPolygons(
          fillColor = ~pal(cases),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        addLegend(pal = pal, values = ~pal(cases), opacity = 0.7, title = "Cases (Within County)",
                  position = "topleft",na.label = "No Confirmed Cases")%>% 
        setView(-96, 37.8, zoom = 5) %>%
        addPolylines(data = mapState, color = "black", opacity = 1, weight = 3)
    })
    
    output$mymap.beds.15 <- renderLeaflet({
      bins <- round(quantile(mapCounty$within15.beds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
      pal <- colorBin("YlOrRd", domain = mapCounty$within15.beds , bins = bins,reverse = TRUE)

      labels <- sprintf(
        "<strong>%s</strong><br/>%g bed to case ratio within 15 miles<br/>%g beds within 15 miles<br/>%s cases within county",
        paste(mapCounty$names,"county"),  mapCounty$bedspercases.15, mapCounty$within15.beds, mapCounty$cases
      ) %>% lapply(htmltools::HTML)

      leaflet(data = mapCounty) %>% addTiles() %>%
        addPolygons(
          fillColor = ~pal(within15.beds),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        addLegend(pal = pal, values = ~pal(within15.beds), opacity = 0.7, title = "Hospital Beds (within 15 miles)",
                  position = "topleft",na.label = "No Confirmed Cases")%>% 
        setView(-96, 37.8, zoom = 5) %>%
        addPolylines(data = mapState, color = "black", opacity = 1, weight = 3)
    })
    output$mymap.beds.30 <- renderLeaflet({})
    output$mymap.beds.45 <- renderLeaflet({})
    output$mymap.beds.60 <- renderLeaflet({})
    output$mymap.beds.120 <- renderLeaflet({})

    output$mymap.cases<- renderLeaflet({})

    output$mymap.bedscases.15 <- renderLeaflet({})
    output$mymap.bedscases.30 <- renderLeaflet({})
    output$mymap.bedscases.45 <- renderLeaflet({})
    output$mymap.bedscases.60 <- renderLeaflet({})
    output$mymap.bedscases.120 <- renderLeaflet({})

    output$lastUpdate<-renderUI({
      h3(paste("The data used for these maps were last updated:", lastUpdated,collapse = ""))
    })
  }
)
