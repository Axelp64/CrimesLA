source(file = "Global.R")
source(file = "Packages.R")






ui <- fluidPage(
  
  div(
    h2("Criminal Acts in L.A", align = "center"),
    windowTitle = "Crimes"
  ),
  
  tabsetPanel(
    tabPanel("Tous",
             
             sidebarPanel(
               
               h3("Filtres"),
               br(),
               br(),
               sliderInput("age", label="Age_Victime",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe", label = "Sexe_Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier", label = "Quartier",
                           choices = unique(data$AREA.NAME),
                           selected = unique(data$AREA.NAME))
               
               
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
               
             )
             
             
    ),
    tabPanel("Braquage",
             sidebarPanel(
               h3("Filtres"),
               br(),
               br(),
               sliderInput("SliderAge", "Victime_Age",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe", label = "Sexe_Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier", label = "Quartier",
                                  choices = unique(data$AREA.NAME),
                                  selected = unique(data$AREA.NAME))
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
             )
             
             
             
    ),
    tabPanel("Cambriolage",
             sidebarPanel(
               h3("Filtres"),
               br(),
               br(),
               sliderInput("SliderAge", "Victime_Age",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe", label = "Sexe_Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier", label = "Quartier",
                                  choices = unique(data$AREA.NAME),
                                  selected = unique(data$AREA.NAME))
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
             )
    ),
    tabPanel("Homicide",
             sidebarPanel(
               h3("Filtres"),
               br(),
               br(),
               sliderInput("SliderAge", "Victime_Age",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe", label = "Sexe_Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier", label = "Quartier",
                                  choices = unique(data$AREA.NAME),
                                  selected = unique(data$AREA.NAME))
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
             )
    ),
    tabPanel("Viol",
             sidebarPanel(
               h3("Filtres"),
               br(),
               br(),
               sliderInput("SliderAge", "Victime_Age",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe", label = "Sexe_Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier", label = "Quartier",
                                  choices = unique(data$AREA.NAME),
                                  selected = unique(data$AREA.NAME))
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
             )
    )
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
)


server <- function(input, output){
  
  
}

shinyApp(ui=ui , server=server)
