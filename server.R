library(shiny)
print(list.files())
source("Data Products - Project.R")

shinyServer(function(input, output) {
    output$map <- renderLeaflet({
        
        dept <- input$dept
        recip <- input$recip
        palette <- input$palette
        
        displayDept(dept, recip, palette)
    })
})