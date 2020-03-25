library(shiny)
library(leaflet)
library(maps)
library(htmltools)
library(ggplot2)
library(sp)
library(shinybusy)

ui <- fluidPage(
  h1("Choropleth maps useful in assessing COVID-19 within the United States"),
  uiOutput(outputId = "lastUpdate"),
  tags$a(href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html", "Click here for Case Data,   "),
  tags$a(href="https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals/data?geometry=134.132%2C-16.829%2C-165.048%2C72.120&orderBy=STATE", "Click here for Hospital Data,  "),
  tags$a(href="https://khn.org/wp-content/uploads/sites/2/2020/03/KHN_ICU_bed_county_analysis.zip", "Click here for ICU Data"),
  add_busy_gif(src = "https://media.giphy.com/media/1416VN7GIFAAmI/giphy.gif", height = 200, width = 200,position = "full-page"),
  leafletOutput("mymap",height = "800")
)

server <- function(input, output, session) {
  load(file = "data/data.RData")
  
  
  #define bins, colors, and labels for maps
  ##cases
  bins.cases <- unique(round(quantile(mapCounty$cases,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0))
  pal.cases <- colorBin("YlOrRd", domain = mapCounty$cases , bins = bins.cases,reverse = FALSE,na.color = "#FFFFFF")
  labels.cases <- sprintf(
    "<strong>%s</strong><br/>%g Confirmed cases within county",
    paste(mapCounty$names,"county"),  ifelse(is.na(mapCounty$cases),yes = 0, no = mapCounty$cases)
  ) %>% lapply(htmltools::HTML)
  
  ##deaths
  bins.deaths <- unique(round(quantile(mapCounty$deaths,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0))
  pal.deaths <- colorBin("YlOrRd", domain = mapCounty$deaths , bins = bins.deaths,reverse = FALSE,na.color = "#FFFFFF")
  labels.deaths <- sprintf(
    "<strong>%s</strong><br/>%s Confirmed deaths within county",
    paste(mapCounty$names,"county"),  ifelse(is.na(mapCounty$deaths),yes = "No Confirmed Cases or ", no = mapCounty$deaths)
  ) %>% lapply(htmltools::HTML)
  
  ##hosp beds
  bins.hospbeds <- unique(round(quantile(mapCounty$beds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.hospbeds <- colorBin("YlOrRd", domain = mapCounty$beds , bins = bins.hospbeds,reverse = TRUE,na.color = "#FFFFFF")
  labels.hospbeds <- sprintf(
    "<strong>%s</strong><br/>%g Total Hospital beds within county",
    paste(mapCounty$names,"county"),  mapCounty$beds
  ) %>% lapply(htmltools::HTML)
  
  ##icu beds
  bins.icubeds <- unique(round(quantile(mapCounty$icubeds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.icubeds <- colorBin("YlOrRd", domain = mapCounty$icubeds , bins = bins.icubeds,reverse = TRUE,na.color = "#FFFFFF")
  labels.icubeds <- sprintf(
    "<strong>%s</strong><br/>%g ICU beds within county",
    paste(mapCounty$names,"county"),  mapCounty$icubeds
  ) %>% lapply(htmltools::HTML)
  
  ##beds within 30 miles
  bins.within30.beds <- unique(round(quantile(mapCounty$within30.beds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.within30.beds <- colorBin("YlOrRd", domain = mapCounty$within30.beds , bins = bins.within30.beds,reverse = TRUE,na.color = "#FFFFFF")
  labels.within30.beds <- sprintf(
    "<strong>%s</strong><br/>%g Beds(within 30 miles of county)",
    paste(mapCounty$names,"county"),  mapCounty$within30.beds
  ) %>% lapply(htmltools::HTML)
  
  ##beds within 60 miles
  bins.within60.beds <- unique(round(quantile(mapCounty$within60.beds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.within60.beds <- colorBin("YlOrRd", domain = mapCounty$within60.beds , bins = bins.within60.beds,reverse = TRUE,na.color = "#FFFFFF")
  labels.within60.beds <- sprintf(
    "<strong>%s</strong><br/>%g Beds(within 60 miles of county)",
    paste(mapCounty$names,"county"),  mapCounty$within60.beds
  ) %>% lapply(htmltools::HTML)
  
  ##icu beds within 30 miles
  bins.within30.icubeds <- unique(round(quantile(mapCounty$within30.icubeds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.within30.icubeds <- colorBin("YlOrRd", domain = mapCounty$within30.icubeds , bins = bins.within30.icubeds,reverse = TRUE,na.color = "#FFFFFF")
  labels.within30.icubeds <- sprintf(
    "<strong>%s</strong><br/>%g ICU Beds(within 30 miles of county)",
    paste(mapCounty$names,"county"),  mapCounty$within30.icubeds
  ) %>% lapply(htmltools::HTML)
  
  ##icu beds within 60 miles
  bins.within60.icubeds <- unique(round(quantile(mapCounty$within60.icubeds,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.within60.icubeds <- colorBin("YlOrRd", domain = mapCounty$within60.icubeds , bins = bins.within60.icubeds,reverse = TRUE,na.color = "#FFFFFF")
  labels.within60.icubeds <- sprintf(
    "<strong>%s</strong><br/>%g ICU Beds(within 60 miles of county)",
    paste(mapCounty$names,"county"),  mapCounty$within60.icubeds
  ) %>% lapply(htmltools::HTML)
  
  
  ##beds per cases 30 miles
  bins.bedspercases.30 <- unique(round(quantile(mapCounty$bedspercases.30,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.bedspercases.30 <- colorBin("YlOrRd", domain = mapCounty$bedspercases.30 , bins = bins.bedspercases.30,reverse = TRUE,na.color = "#FFFFFF")
  labels.bedspercases.30 <- sprintf(
    "<strong>%s</strong><br/>%g Beds(within 30 miles of county) per # of cases",
    paste(mapCounty$names,"county"),  mapCounty$bedspercases.30
  ) %>% lapply(htmltools::HTML)
  
  ##beds per cases 60 miles
  bins.bedspercases.60 <- unique(round(quantile(mapCounty$bedspercases.60,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.bedspercases.60 <- colorBin("YlOrRd", domain = mapCounty$bedspercases.60 , bins = bins.bedspercases.60,reverse = TRUE,na.color = "#FFFFFF")
  labels.bedspercases.60 <- sprintf(
    "<strong>%s</strong><br/>%g Beds(within 60 miles of county) per # of cases ",
    paste(mapCounty$names,"county"),  mapCounty$bedspercases.60
  ) %>% lapply(htmltools::HTML)
  
  ##ICU beds per cases 30 miles
  bins.icubedspercases.30 <- unique(round(quantile(mapCounty$icubedspercases.30,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.icubedspercases.30 <- colorBin("YlOrRd", domain = mapCounty$icubedspercases.30 , bins = bins.icubedspercases.30,reverse = TRUE,na.color = "#FFFFFF")
  labels.icubedspercases.30 <- sprintf(
    "<strong>%s</strong><br/>%g ICU Beds(within 30 miles of county) per # of cases",
    paste(mapCounty$names,"county"),  mapCounty$icubedspercases.30
  ) %>% lapply(htmltools::HTML)
  
  ##ICU beds per cases 60 miles
  bins.icubedspercases.60 <- unique(round(quantile(mapCounty$icubedspercases.60,probs = c(0, 0.01, 0.05, 0.10, 0.20, 0.25, 0.50, 0.75, 1),na.rm = TRUE),digits = 0))
  pal.icubedspercases.60 <- colorBin("YlOrRd", domain = mapCounty$icubedspercases.60 , bins = bins.icubedspercases.60,reverse = TRUE,na.color = "#FFFFFF")
  labels.icubedspercases.60 <- sprintf(
    "<strong>%s</strong><br/>%g ICU Beds(within 60 miles of county) per # of cases",
    paste(mapCounty$names,"county"),  mapCounty$icubedspercases.60
  ) %>% lapply(htmltools::HTML)
  
  output$mymap <- renderLeaflet({
    m=leaflet(data = mapCounty) %>% addTiles()  %>%  addProviderTiles(providers$Esri.WorldGrayCanvas)
    
    # legends not working yet
    # %>%  addLegend(pal = pal.deaths, values = ~pal.deaths(deaths), opacity = 0.7, title = "Confirmed Deaths",
    #position = "topleft",na.label = "No Confirmed Deaths", group = "Confirmed Deaths") 
    
    m2=m %>%  addPolygons(
      group = "Confirmed Cases",
      fillColor = ~pal.cases(cases),
      weight = 2,
      opacity = 1,
      color = "#666",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#000000",
        dashArray = "",
        bringToFront = TRUE),
      label = labels.cases,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
      addPolygons(
        group = "Confirmed Deaths",
        fillColor = ~pal.deaths(deaths),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.deaths,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addPolygons(
        group = "All Hospital Beds",
        fillColor = ~pal.hospbeds(beds),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.hospbeds,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      addPolygons(
        group = "Icu Beds",
        fillColor = ~pal.icubeds(icubeds),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.icubeds,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      # addPolygons(
      #   group = "All Hospital Beds(within 30 miles)",
      #   fillColor = ~pal.within30.beds(within30.beds),
      #   weight = 2,
      #   opacity = 1,
      #   color = "#666",
      #   dashArray = "3",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 5,
      #     color = "#000000",
      #     dashArray = "",
      #     bringToFront = TRUE),
      #   label = labels.within30.beds,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto")
      # ) %>%
      addPolygons(
        group = "All Hospital Beds(within 60 miles)",
        fillColor = ~pal.within60.beds(within60.beds),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.within60.beds,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      # addPolygons(
      #   group = "Icu Beds(within 30 miles)",
      #   fillColor = ~pal.within30.icubeds(within30.icubeds),
      #   weight = 2,
      #   opacity = 1,
      #   color = "#666",
      #   dashArray = "3",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 5,
      #     color = "#000000",
      #     dashArray = "",
      #     bringToFront = TRUE),
      #   label = labels.within30.icubeds,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto")
      # ) %>%
      addPolygons(
        group = "Icu Beds(within 60 miles)",
        fillColor = ~pal.within60.icubeds(within60.icubeds),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.within60.icubeds,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      # addPolygons(
      #   group = "Beds(within 30 miles of county) per # of cases",
      #   fillColor = ~pal.bedspercases.30(bedspercases.30),
      #   weight = 2,
      #   opacity = 1,
      #   color = "#666",
      #   dashArray = "3",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 5,
      #     color = "#000000",
      #     dashArray = "",
      #     bringToFront = TRUE),
      #   label = labels.bedspercases.30,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto")
      # ) %>%
      addPolygons(
        group = "Beds(within 60 miles of county) per # of cases",
        fillColor = ~pal.bedspercases.60(bedspercases.60),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.bedspercases.60,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      # addPolygons(
      #   group = "ICU Beds(within 30 miles of county) per # of cases",
      #   fillColor = ~pal.icubedspercases.30(icubedspercases.30),
      #   weight = 2,
      #   opacity = 1,
      #   color = "#666",
      #   dashArray = "3",
      #   fillOpacity = 0.7,
      #   highlight = highlightOptions(
      #     weight = 5,
      #     color = "#000000",
      #     dashArray = "",
      #     bringToFront = TRUE),
      #   label = labels.icubedspercases.30,
      #   labelOptions = labelOptions(
      #     style = list("font-weight" = "normal", padding = "3px 8px"),
      #     textsize = "15px",
      #     direction = "auto")
      # ) %>%
      addPolygons(
        group = "ICU Beds(within 60 miles of county) per # of cases",
        fillColor = ~pal.icubedspercases.60(icubedspercases.60),
        weight = 2,
        opacity = 1,
        color = "#666",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#000000",
          dashArray = "",
          bringToFront = TRUE),
        label = labels.icubedspercases.60,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) 
    
    m2 %>% setView(-96, 37.8, zoom = 5)%>%
      addLayersControl(
        baseGroups = c('Confirmed Cases','Confirmed Deaths','All Hospital Beds',"Icu Beds",'All Hospital Beds(within 60 miles)',
                       "Icu Beds(within 60 miles)", "Beds(within 60 miles of county) per # of cases", "ICU Beds(within 60 miles of county) per # of cases"),
        options = layersControlOptions(collapsed = FALSE, autoZIndex = TRUE)
      )
   
  })
  
  output$lastUpdate<-renderUI({
    h3(paste("These maps were last updated:", lastUpdated,collapse = ""))
  })
}

shinyApp(ui, server)