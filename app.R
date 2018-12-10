#==================================================================================================#
# INTERAKTIV APP SOM VISER TØMMERPRISER FRA SSB                                                    #
# Skogbrukets Kursinstitutt                                                                        #
# EHH 181122                                                                                       #
#==================================================================================================#

library(shiny)
library(plyr)
library(dplyr)
library(readr)
library(httr)
library(rjstat)
library(ggplot2)
library(lubridate)

url <- "http://data.ssb.no/api/v0/no/table/08981"
kpi <- '{
"query": [
{
  "code": "Maaned",
  "selection": {
  "filter": "item",
  "values": ["01","02","03","04","05","06","07","08","09","10","11","12"]
  }
},
  {
  "code": "ContentsCode",
  "selection": {
  "filter": "item",
  "values": [
  "KpiIndMnd"
  ]
  }
  },
  {
  "code": "Tid",
  "selection": {
  "filter": "all",
  "values": ["*"]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
  }'
  temp <- POST(url , body = kpi, encode = "json", verbose())
  table <- fromJSONstat(content(temp, "text"))
  kpi <- table[[1]]
  colnames(kpi) <- c("maaned", "statistikkvariabel", "aar", "value")
  kpi <- subset(kpi, aar >= 2006)
  kpi <- kpi[order(kpi$aar),]
  kpi$maaned <-
    paste0(kpi$aar, "M", rep(
      c(
        "01",
        "02",
        "03",
        "04",
        "05",
        "06",
        "07",
        "08",
        "09",
        "10",
        "11",
        "12"
      ),
      nrow(kpi) / 12
    ))
  
  url <- "http://data.ssb.no/api/v0/no/table/07405"
  data <- '{
  "query": [
  {
  "code": "Treslag",
  "selection": {
  "filter": "item",
  "values": ["0000","1142","1160","1410","2142","2160","2410","3122","3400"]
  }
  },
  {
  "code": "ContentsCode",
  "selection": {
  "filter": "item",
  "values": ["Priser"]
  }
  },
  {
  "code": "Tid",
  "selection": {
  "filter": "all",
  "values": ["*"]
  }
  }
  ],
  "response": {
  "format": "json-stat"
  }
  }'
  temp <- POST(url , body = data, encode = "json", verbose())
  table <- fromJSONstat(content(temp, "text"))
  df <- table[[1]]
  colnames(df) <-
    c("sortiment", "statistikkvariabel", "maaned", "value")
  df <- merge(df, kpi, by = "maaned")
  colnames(df) <-
    c("mnd",
      "sortiment",
      "variabel",
      "val",
      "konsumgruppe",
      "variabel.y",
      "kpi")
  df$val_j <- df$val * df[c(nrow(df)), "kpi"] / df$kpi
  df <- df[, c("sortiment", "variabel", "mnd", "val", "val_j")]
  df$mnd <-
    paste0(substring(df$mnd, 1, 4), "-", substring(df$mnd, 6, 7), "-", 01)
  df$mnd <- as.Date(df$mnd, format = "%Y-%m-%d")
  df$year <- as.numeric(substring(df$mnd, 1, 4))
  df$sortiment <-
    mapvalues(
      df$sortiment,
      from = c(
        "Furu sams skurtømmer og massevirke",
        "Gran sams skurtømmer og massevirke"
      ),
      to = c("Furu i alt", "Gran i alt")
    )
  
  ui <- fluidPage(fluidRow(column(
    12,
    align = "center",
    plotOutput(outputId = "lineplot", width = '100%'),
    fluidRow(
      column(
        3,
        selectInput(
          inputId = "sortiment",
          label = strong("Sortiment"),
          width = '100%',
          choices = c(
            "I alt",
            "Gran skurtømmer",
            "Gran massevirke",
            "Gran i alt",
            "Furu skurtømmer",
            "Furu massevirke",
            "Furu i alt",
            "Lauvtre skurtømmer",
            "Lauvtre massevirke"
          ),
          selected = "I alt"
        )
      ),
      column(
        6,
        sliderInput(
          "Y",
          strong("Start År:"),
          width = '100%',
          min = 2006,
          max = as.numeric(substring(Sys.Date(), 1, 4)),
          value = as.numeric(substring(Sys.Date(), 1, 4)) - 10,
          step = 1,
          sep = ""
        )
      ),
      column(
        3,
        h6("Kilder/SSB:", align = "left"),
        tags$a(
          href = "https://www.ssb.no/jord-skog-jakt-og-fiskeri/statistikker/skogav",
          "Skogavvirkning for salg",
          target = "_blank",
          style = "color:#739ABC;font-size:12px;float:left"
        ),
        tags$br(),
        tags$a(
          href = "https://www.ssb.no/priser-og-prisindekser/statistikker/kpi",
          "Konsumprisindeks",
          target = "_blank",
          style = "color:#739ABC;font-size:12px;float:left"
        )
      )
    )
  )),
  tags$br(),
  fluidRow(
    column(12, style = "position:absolute; bottom:100; width: 100%; height: 50%; background-color:black;",
           fillRow(
             div(
               style = "display: inline-block; margin-top: 10px; float:left;",
               tags$a(
                 img(
                   src = "http://www.skogkurs.no/img/svg-ikoner/skogkurs_fase_2_horisontal_hvitt.svg",
                   height = 40,
                   width = 150
                 ),
                 href = "http://www.skogkurs.no/",
                 target = "_blank"
               ),
               div(
                 style = "display: inline-block;",
                 tags$a(
                   img(
                     src = "http://www.skogkurs.no/img/svg-ikoner/facebooki_hvitt.svg",
                     height = 40,
                     width = 40
                   ),
                   href = "https://www.facebook.com/skogkurs/",
                   target = "_blank"
                 )
               ),
               div(
                 style = "display: inline-block;",
                 tags$a(
                   img(
                     src = "http://www.skogkurs.no/img/svg-ikoner/youtube_hvitt.svg",
                     height = 40,
                     width = 40
                   ),
                   href = "https://www.youtube.com/user/skogkurs/",
                   target = "_blank"
                 )
               )
             ),
             div(
               style = "display: inline-block; background-color: black;float:right;",
               tags$b("Skogbrukets Kursinstitutt",
                      style = "color: white;"),
               tags$br(),
               div(
                 style = "display: inline-block; background-color: black;",
                 tags$a(
                   href = "https://www.google.no/maps/place/Skogbrukets+Kursinstitutt+(Skogkurs)/@60.945431,10.635752,17z/data=!3m1!4b1!4m2!3m1!1s0x466a78f8f9b75aab:0xa4a334441f6207df",
                   target = "_blank",
                   tags$i("Honnevegen 60, 2836 Biri, Norway"),
                   style = "color: white; float:left;"
                 )
               ),
               tags$br(),
               div(
                 style = "display: inline-block; height: 20%; background-color: black;",
                 tags$b("Tel.:", style = "color: white;float:left;"),
                 tags$a("+4790888200", style = "color: white;float:left;")
               ),
               tags$br(),
               div(
                 tags$b("E-post:", style = "color: white;"),
                 tags$a(href = "mailto:post@skogkurs.no", "post@skogkurs.no", style = "color: white;")
               )
             )
           ))
  ))
  
  server <- function(input, output) {
    data <- reactive({
      df %>%
        dplyr::filter(sortiment == input$sortiment, year >= input$Y)
    })
    output$lineplot <- renderPlot(execOnResize = TRUE, {
      plot(
        x = data()$mnd[!is.na(data()$val)],
        y = data()$val[!is.na(data()$val)],
        bty = "n",
        type = "l",
        lwd = 4,
        main = "Tømmerpriser",
        cex.main = 2,
        adj = 0,
        col = alpha("#739ABC", 0.6),
        ylim = c(
          min(data()$val - 50, na.rm = T),
          max(data()$val_j + 30, na.rm = T)
        ),
        xlim = c(
          floor_date(min(data()$mnd), "year"),
          round_date(max(data()$mnd) %m+% months(9), "year")
        ),
        xlab = "",
        ylab = expression(paste("NOK /", " m" ^ {
          3
        }))
      )
      lines(
        x = data()$mnd[!is.na(data()$val)],
        y = data()$val_j[!is.na(data()$val_j)],
        lwd = 4,
        col = alpha("#FFA100", 0.6)
      )
      grid(NA, NULL)
      text(
        x = max(data()$mnd),
        y = tail(data()$val[!is.na(data()$val)], n = 1),
        labels = data()$val[which.max(data()$mnd)],
        pos = 3
      )
      text(
        x = max(data()$mnd),
        y = min(data()$val - 50, na.rm = T),
        labels = format(max(data()$mnd), '%b') #'%b-%y')
    )
      axis(
        side = 1,
        at = max(data()$mnd),
        y = min(data()$val - 50),
        labels = F,
        lwd = 5,
        tck = -0.001
      )
      legend(
        "bottomleft",
        c("Faste priser", "Løpende priser"),
        xjust = 0,
        lty = c(1, 1),
        lwd = 4,
        bty = "n",
        col = c(alpha("#FFA100", 0.6), alpha("#739ABC", 0.6))
      )
    })
  }
  
  shinyApp(ui = ui, server = server)