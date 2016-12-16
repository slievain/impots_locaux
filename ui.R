library(shiny)
source("Data Products - Project.R")

bootstrapPage(
    tags$head(tags$style(type="text/css", "
                html, body {width:100%;height:100%}
                         #loadmessage {
                         position: fixed;
                         top: 0px;
                         left: 0px;
                         width: 100%;
                         padding: 5px 0px 5px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #000000;
                         background-color: #CCFF66;
                         z-index: 105;
                         }")),
    
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage")),
    
    leafletOutput("map", width = "100%", height = "100%"),
    
    absolutePanel(top = 0, left = 50,
        titlePanel("Collected Council Tax in France in 2015")
    ),
    
    absolutePanel(top = 10, right = 10, class = "info",
        h5("Use below inputs to update the map:"),
        
        selectInput("dept", "Department:", inputs, selected = "OISE", width = "200px"),
        radioButtons("recip", "Recipient(s):", recipient, width = "200px"),
        radioButtons("palette", "Display Type:", type, width = "200px")
    )
)