load(file = "data/data.RData")

#define bins, colors, and labels for maps
##cases
bins.cases <- round(quantile(mapCounty$cases,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.cases <- colorBin("YlOrRd", domain = mapCounty$cases , bins = bins.cases,reverse = FALSE)
labels.cases <- sprintf(
  "<strong>%s</strong><br/>%g Confirmed cases within county",
  paste(mapCounty$names,"county"),  ifelse(is.na(mapCounty$cases),yes = 0, no = mapCounty$cases)
) %>% lapply(htmltools::HTML)

##deaths
bins.deaths <- unique(round(quantile(mapCounty$deaths,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0))
pal.deaths <- colorBin("YlOrRd", domain = mapCounty$deaths , bins = bins.deaths,reverse = FALSE)
labels.deaths <- sprintf(
  "<strong>%s</strong><br/>%g Confirmed deaths within county",
  paste(mapCounty$names,"county"),  ifelse(is.na(mapCounty$deaths),yes = 0, no = mapCounty$deaths)
) %>% lapply(htmltools::HTML)

##hosp beds
bins.hospbeds <- round(quantile(mapCounty$beds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.hospbeds <- colorBin("YlOrRd", domain = mapCounty$beds , bins = bins.hospbeds,reverse = TRUE)
labels.hospbeds <- sprintf(
  "<strong>%s</strong><br/>%g Total Hospital beds within county",
  paste(mapCounty$names,"county"),  mapCounty$beds
) %>% lapply(htmltools::HTML)

##icu beds
bins.icubeds <- round(quantile(mapCounty$icubeds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.icubeds <- colorBin("YlOrRd", domain = mapCounty$icubeds , bins = bins.icubeds,reverse = TRUE)
labels.icubeds <- sprintf(
  "<strong>%s</strong><br/>%g ICU beds within county",
  paste(mapCounty$names,"county"),  mapCounty$icubeds
) %>% lapply(htmltools::HTML)

##beds within 30 miles
bins.within30.beds <- round(quantile(mapCounty$within30.beds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.within30.beds <- colorBin("YlOrRd", domain = mapCounty$within30.beds , bins = bins.within30.beds,reverse = TRUE)
labels.within30.beds <- sprintf(
  "<strong>%s</strong><br/>%g Beds(within 30 miles of county)",
  paste(mapCounty$names,"county"),  mapCounty$within30.beds
) %>% lapply(htmltools::HTML)

##beds within 60 miles
bins.within60.beds <- round(quantile(mapCounty$within60.beds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.within60.beds <- colorBin("YlOrRd", domain = mapCounty$within60.beds , bins = bins.within60.beds,reverse = TRUE)
labels.within60.beds <- sprintf(
  "<strong>%s</strong><br/>%g Beds(within 60 miles of county)",
  paste(mapCounty$names,"county"),  mapCounty$within60.beds
) %>% lapply(htmltools::HTML)

##icu beds within 30 miles
bins.within30.icubeds <- round(quantile(mapCounty$within30.icubeds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.within30.icubeds <- colorBin("YlOrRd", domain = mapCounty$within30.icubeds , bins = bins.within30.icubeds,reverse = TRUE)
labels.within30.icubeds <- sprintf(
  "<strong>%s</strong><br/>%g ICU Beds(within 30 miles of county)",
  paste(mapCounty$names,"county"),  mapCounty$within30.icubeds
) %>% lapply(htmltools::HTML)

##icu beds within 60 miles
bins.within60.icubeds <- round(quantile(mapCounty$within60.icubeds,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.within60.icubeds <- colorBin("YlOrRd", domain = mapCounty$within60.icubeds , bins = bins.within60.icubeds,reverse = TRUE)
labels.within60.icubeds <- sprintf(
  "<strong>%s</strong><br/>%g ICU Beds(within 60 miles of county)",
  paste(mapCounty$names,"county"),  mapCounty$within60.icubeds
) %>% lapply(htmltools::HTML)


##beds per cases 30 miles
bins.bedspercases.30 <- round(quantile(mapCounty$bedspercases.30,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.bedspercases.30 <- colorBin("YlOrRd", domain = mapCounty$bedspercases.30 , bins = bins.bedspercases.30,reverse = TRUE)
labels.bedspercases.30 <- sprintf(
  "<strong>%s</strong><br/>%g Beds(within 30 miles of county) per # of cases",
  paste(mapCounty$names,"county"),  mapCounty$bedspercases.30
) %>% lapply(htmltools::HTML)

##beds per cases 60 miles
bins.bedspercases.60 <- round(quantile(mapCounty$bedspercases.60,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.bedspercases.60 <- colorBin("YlOrRd", domain = mapCounty$bedspercases.60 , bins = bins.bedspercases.60,reverse = TRUE)
labels.bedspercases.60 <- sprintf(
  "<strong>%s</strong><br/>%g Beds(within 60 miles of county) per # of cases ",
  paste(mapCounty$names,"county"),  mapCounty$bedspercases.60
) %>% lapply(htmltools::HTML)

##ICU beds per cases 30 miles
bins.icubedspercases.30 <- round(quantile(mapCounty$icubedspercases.30,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.icubedspercases.30 <- colorBin("YlOrRd", domain = mapCounty$icubedspercases.30 , bins = bins.icubedspercases.30,reverse = TRUE)
labels.icubedspercases.30 <- sprintf(
  "<strong>%s</strong><br/>%g ICU Beds(within 30 miles of county) per # of cases",
  paste(mapCounty$names,"county"),  mapCounty$icubedspercases.30
) %>% lapply(htmltools::HTML)

##ICU beds per cases 60 miles
bins.icubedspercases.60 <- round(quantile(mapCounty$icubedspercases.60,probs = c(0.25, 0.50, 0.75, 0.80, 0.9, 0.95, 0.99, 1),na.rm = TRUE),digits = 0)
pal.icubedspercases.60 <- colorBin("YlOrRd", domain = mapCounty$icubedspercases.60 , bins = bins.icubedspercases.60,reverse = TRUE)
labels.icubedspercases.60 <- sprintf(
  "<strong>%s</strong><br/>%g ICU Beds(within 60 miles of county) per # of cases",
  paste(mapCounty$names,"county"),  mapCounty$icubedspercases.60
) %>% lapply(htmltools::HTML)



m=leaflet(data = mapCounty) %>% addTiles() %>%
  # Layers control
  addLayersControl(
    baseGroups = c('Confirmed Cases','Confirmed Deaths','All Hospital Beds',"Icu Beds",'All Hospital Beds(within 30 miles)','All Hospital Beds(within 60 miles)',
                   "Icu Beds(within 30 miles)", "Icu Beds(within 60 miles)", "Beds(within 30 miles of county) per # of cases ", "Beds(within 60 miles of county) per # of cases",
                   "ICU Beds(within 30 miles of county) per # of cases", "ICU Beds(within 60 miles of county) per # of cases"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%  addProviderTiles(providers$Esri.WorldGrayCanvas)
  
# legends not working yet
# %>%  addLegend(pal = pal.deaths, values = ~pal.deaths(deaths), opacity = 0.7, title = "Confirmed Deaths",
#position = "topleft",na.label = "No Confirmed Deaths", group = "Confirmed Deaths") 

m2=m %>%  addPolygons(
    group = "Confirmed Cases",
    fillColor = ~pal.cases(cases),
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
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
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
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
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
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      bringToFront = TRUE),
    label = labels.icubeds,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>% 
  addPolygons(
    group = "All Hospital Beds(within 30 miles)",
    fillColor = ~pal.within30.beds(within30.beds),
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
    label = labels.within30.beds,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "All Hospital Beds(within 60 miles)",
    fillColor = ~pal.within60.beds(within60.beds),
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
    label = labels.within60.beds,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "Icu Beds(within 30 miles)",
    fillColor = ~pal.within30.icubeds(within30.icubeds),
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
    label = labels.within30.icubeds,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "Icu Beds(within 60 miles)",
    fillColor = ~pal.within60.icubeds(within60.icubeds),
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
    label = labels.within60.icubeds,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "Beds(within 30 miles of county) per # of cases",
    fillColor = ~pal.bedspercases.30(bedspercases.30),
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
    label = labels.bedspercases.30,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "Beds(within 60 miles of county) per # of cases",
    fillColor = ~pal.bedspercases.60(bedspercases.60),
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
    label = labels.bedspercases.60,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "ICU Beds(within 30 miles of county) per # of cases",
    fillColor = ~pal.icubedspercases.30(icubedspercases.30),
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
    label = labels.icubedspercases.30,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) %>%
  addPolygons(
    group = "ICU Beds(within 60 miles of county) per # of cases",
    fillColor = ~pal.icubedspercases.60(icubedspercases.60),
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
    label = labels.icubedspercases.60,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  ) 

m2 %>% setView(-96, 37.8, zoom = 5)  
  
  