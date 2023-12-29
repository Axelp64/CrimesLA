source(file = "Global.R")
source(file = "Packages.R")



data <- data %>% 
  mutate(delit = case_when(
    Crm.Cd %in% c("110","113") ~ "Homicide",
    Crm.Cd %in% c("121","122","815","820","821") ~ "Viol",
    Crm.Cd %in% c("210","220") ~ "Braquage",
    Crm.Cd %in% c("310","320") ~ "Cambriolage",
  ))


ui <- fluidPage(
  
  div(
    h2("Criminal Acts in L.A", align = "center"),
    windowTitle = "Crimes"
  ),
  
  tabsetPanel(
    tabPanel("All",
             
             
             ),
    tabPanel("Vehicle theft",
             
             
             
             ),
    tabPanel("Burglary",
    )
  )
  

  
  

  
  
  
  
  
  
  
  
)

server <- function(input, output){
  
  
}

shinyApp(ui=ui , server=server)
