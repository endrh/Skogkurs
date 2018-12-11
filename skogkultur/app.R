#==================================================================================================#
# KART MED SKOGKULTUR FRA SSB                                                                       #
# Skogbrukets Kursinstitutt                                                                        #
# EHH 181105                                                                                       #
#==================================================================================================#

# laste R-pakker
library(PxWebApiData)
library(shiny)
library(plyr)
library(dplyr)
library(doBy)
library(geojsonio)
library(rgdal)
library(raster)
library(leaflet)
library(viridis)
library(shinyWidgets)
library(sf)
library(lwgeom)
library(rgeos)

x <-
  ApiData(
    "http://data.ssb.no/api/v0/no/table/03522",
    getDataByGET = FALSE,
    ContentsCode = TRUE,
    Tid = TRUE,
    Region = list(
      "filter",
      c(
        "01",
        "02-03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12",
        "14",
        "15",
        "16",
        "17",
        "18",
        "19",
        "20",
        "50"
      )
    )
  )
df <- x[[1]]
rm(x)
unique(df$region)

df$region <-
  mapvalues(df$region, "Sør-Trøndelag (-2017)", "Trøndelag")
df$region <-
  mapvalues(df$region, "Nord-Trøndelag (-2017)", "Trøndelag")
df$region <-
  mapvalues(df$region, "Finnmark - Finnmárku", "Finnmark")
df$region <- mapvalues(df$region, "Troms - Romsa", "Troms")
colnames(df) <- c("region", "variabel", "aar", "value")
df <-
  summaryBy(value ~  region + variabel + aar, data = df, FUN = sum)
colnames(df)[4] <- "value"
df$variabel <-
  mapvalues(df$variabel, "Planter (1 000 stk)", "Planter")
df$variabel <-
  mapvalues(df$variabel, "Planting (dekar)", "Planting")
df$variabel <-
  mapvalues(df$variabel, "Planting. Kostnad (1 000 kr)", "Kostnad")

no_fylker <-
  geojson_read("http://norgeskart.no/json/norge/fylker2018.geojson", what = "sp")
no_fylker <-
  spTransform(no_fylker,
              CRS(
                "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              ))
no_land <-
  readOGR("./data",
          layer = "nor_small4")
no_land <-
  spTransform(no_land,
              CRS(
                "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
              ))
norge <- intersect(no_fylker, no_land)
norge@data <- norge@data[, 1:2]
OA <- norge[norge@data$n == "Akershus" | norge@data$n == "Oslo",]
norge <-
  norge[norge@data$n != "Akershus" & norge@data$n != "Oslo",]
OA_Sf <- st_as_sf(OA)
x = st_cast(OA_Sf, "POLYGON")
sf <- as(x, 'Spatial')
sf@data$n <- "Akershus og Oslo"
sf <- gUnaryUnion(sf)
X <- data.frame(matrix(NA, 1, 2))
colnames(X) <- c("n", "id")
X[1,] <- c("Akershus og Oslo", "02-03")
sf <- SpatialPolygonsDataFrame(sf, X)
norge <- bind(norge, sf)
norge@data$n <-
  c(
    "Østfold",
    "Hedmark",
    "Oppland",
    "Buskerud",
    "Vestfold",
    "Telemark",
    "Aust-Agder",
    "Vest-Agder",
    "Rogaland",
    "Hordaland",
    "Sogn og Fjordane",
    "Møre og Romsdal",
    "Nordland",
    "Troms",
    "Finnmark",
    "Trøndelag",
    "Akershus og Oslo"
  )
norge <-
  sp::merge(norge,
            df,
            by.x = "n",
            by.y = "region",
            duplicateGeoms = TRUE)

epsg32633 <-
  leafletCRS(
    crsClass = "L.Proj.CRS",
    code = "EPSG:32633",
    proj4def = "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs",
    resolutions = 2 ^ (13:-1),
    # 8192 down to 0.5
    origin = c(0, 0)
  )

ui <- bootstrapPage(
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }",
    "html, body {width:100%;height:100%}"
  ),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(
    top = 0,
    left = 60,
    width = 120,
    radioButtons(
      inputId = "variabel",
      inline = FALSE,
      label = "",
      choices = c(
        "Antall planter" = "Planter",
        "Areal plantet" = "Planting",
        "Kostnad" = "Kostnad"
      ),
      selected = "Planter"
    ),
    htmlOutput("aar_selector")
  ),
  absolutePanel(
    bottom = 10,
    left = 10,
    tags$a(
      img(
        src = "http://www.skogkurs.no/img/png-ikoner/fase_2_frittliggende_skogkurs_horisontal_logo_MULTE.png",
        height = 52,
        width = 154
      ),
      href = "http://www.skogkurs.no/",
      target = "_blank"
    )
  )
)

server <- function(input, output, session) {
  output$aar_selector <- renderUI({
    data_available <-
      df %>% filter(variabel == input$variabel & value > 0)
    selectInput(
      inputId = "aar",
      label = "",
      choices = sort(unique(data_available$aar), decreasing = TRUE),
      selected = unique(data_available$aar)[max(unique(data_available$aar))]
    )
  })
  output$map <- renderLeaflet({
    map <- subset(norge, aar == input$aar & variabel == input$variabel)
    bins <-
      unique(round(quantile(
        map@data$value,
        probs = seq(0, 1, 0.1),
        na.rm = TRUE
      )))

    pal <- colorBin("viridis", domain = map$value, bins = bins)
    if (input$variabel == "Planter") {
      labels <-
        sprintf(
          "%s</strong><br/>%s 000 planter</sup>",
          map$n,
          format(map$value, digits = 9, big.mark = " ")
        ) %>% lapply(htmltools::HTML)
    } else if (input$variabel == "Kostnad") {
      labels <-
        sprintf(
          "%s</strong><br/>%s 000 kr</sup>",
          map$n,
          format(map$value, digits = 9, big.mark = " ")
        ) %>% lapply(htmltools::HTML)
    } else if (input$variabel == "Planting") {
      labels <-
        sprintf(
          "%s</strong><br/>%s daa</sup>",
          map$n,
          format(map$value, digits = 9, big.mark = " ")
        ) %>% lapply(htmltools::HTML)
    }
    if (input$variabel == "Planter") {
      Ltitile <- "Antall planter (i 1000)"
    } else if (input$variabel == "Kostnad") {
      Ltitile <- "Kostnad (i 1000) kr"
    } else if (input$variabel == "Planting") {
      Ltitile <- "Planting i daa"
    }
    leaflet(map) %>%
      setView(11, 65, 5) %>%
      addPolygons(
        fillColor = ~ pal(value),
        weight = 2,
        opacity = 1,
        color = "lightgrey",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = "#FFA100",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~ value,
        opacity = 0.7,
        title = Ltitile,
        labels = bins,
        labFormat = labelFormat(big.mark = " "),
        position = "bottomright"
      ) %>% addProviderTiles(providers$CartoDB.Positron)
  })
}

shinyApp(ui, server)
