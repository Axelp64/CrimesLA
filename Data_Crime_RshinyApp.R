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
               sliderInput("age_all", label="Age_Victime",
                           min=min(data$Vict.Age, na.rm = T),
                           max=max(data$Vict.Age, na.rm = T),
                           value = c(min(data$Vict.Age, na.rm = T),
                                     max(data$Vict.Age, na.rm = T))
                           
               ),
               br(),
               br(),
               checkboxGroupInput("sexe_all", label = "Sexe Victime",
                                  choices = c("Femme"="F","Homme"="M","Autre"="X"),
                                  selected = c("F","M","X")
               ),
               br(),
               br(),
               dateRangeInput("periode_all", label = "Periode",
                              start = min(data$Date.Rptd, na.rm = T),
                              end   = max(data$Date.Rptd, na.rm = T)
                              
                              
               ),
               br(),
               br(),
               checkboxGroupInput("quartier_all", label = "Quartier",
                                  choices = unique(data$AREA.NAME),
                                  selected = unique(data$AREA.NAME))
               
               
               
             ),
             
             mainPanel(
               br(),
               h3("Visualisations"),
               
               box(title = tags$h4("Nombre de Delits :"),
                   width = 4,
                   height = 100,
                   solidHeader = T,
                   status = "info",
                   h4(strong(textOutput("kpi_all_value")))
                   
               )
               
               
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
               h3("Visualisations")
               
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
               h3("Visualisations")
               
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
               h3("Visualisations")
               
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
               h3("Visualisations")
               
             )
    )
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
)


server <- function(input, output){
  
  ## TOUS LES DELITS
  
  #Filtrage des donnees
  
  filtered_data_all <- reactive ({data %>% 
      filter(Vict.Age >= min(input$age_all) & Vict.Age <= max(input$age_all),
             Vict.Sex %in% input$sexe_all,
             Date.Rptd >= input$periode_all[1] & Date.Rptd <= input$periode_all[2],
             AREA.NAME %in% input$quartier_all)
  })
  ## KPI
  
  output$kpi_all_value <- renderText({
    nb_delits <- nrow(filtered_data_all())
    paste(nb_delits)
    })
}

shinyApp(ui=ui , server=server)
