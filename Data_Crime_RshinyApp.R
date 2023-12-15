source(file = "Global.R")
source(file = "Packages.R")


ui <- fluidPage(
  
  div(
    h2("Criminal Acts in L.A", align = "center"),
    windowTitle = "Crimes"
  ),
  
  tabsetPanel(
    tabPanel("Onglet 1",
             
             
             ),
    tabPanel("Onglet 2",
             
             
             
             )
  )
  

  
  

  
  
  
  
  
  
  
  
)

server <- function(input, output){
  
  
}

shinyApp(ui=ui , server=server)
