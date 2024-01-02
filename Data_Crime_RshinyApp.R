source(file = "Global.R")
source(file = "Packages.R")






ui <- fluidPage(
  
  #Titre de l'appli
  
  div(
    h2("Crimes et delits a Los Angeles", align = "center"),
    windowTitle = "Crimes"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      #Ensemble des filtres
      div(
        h3("Filtres"), align = "center"),
      br(),
      br(),
      sliderInput("age_filter", label="Age de la victime :",
                  min=min(data$Vict.Age, na.rm = T),
                  max=max(data$Vict.Age, na.rm = T),
                  value = c(min(data$Vict.Age, na.rm = T),
                            max(data$Vict.Age, na.rm = T))
                  
      ),
      br(),
      br(),
      checkboxGroupInput("sexe_filter", label = "Sexe de la victime : ",
                         choices = c("Femme"="F","Homme"="M","Autre"="X"),
                         selected = c("F","M","X")
      ),
      br(),
      br(),
      dateRangeInput("periode_filter", label = "Selectionnez la periode : ",
                     start = min(data$Date.Rptd, na.rm = T),
                     end   = max(data$Date.Rptd, na.rm = T)
                     
                     
      ),
      br(),
      br(),
      checkboxGroupInput("quartier_filter", label = "Selectionnez les quartiers : ",
                         choices = unique(data$AREA.NAME),
                         selected = unique(data$AREA.NAME))
      
      
      
    ),
    
    mainPanel(
      tabsetPanel(
        
        #Creation des onglets du rapport
        
        tabPanel("Tous les delits/crimes",
                 
                 fluidRow(
                  #Ligne KPI
                   column(width = 2
                          ),
                   column(width = 5,
                          br(),
                          box(title = tags$h4("Nombre de delits :"),
                              width = 20,
                              height = 100,
                              solidHeader = T,
                              status = "info",
                              h4(strong(textOutput("kpi_all_value"))))
                   ),
                   column(width = 5,
                          br(),
                          box(
                            title = tags$h4("Moyenne d'age des victimes :"),
                            width = 20,
                            height = 100,
                            solidHeader = TRUE,
                            status = "info",
                            h4(strong(textOutput("kpi_age_all_value")))
                          )
                   ),
                   #Ligne Graphiques
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Evolution du nombre de delits"), align = "center")),
                          plotOutput("evo_all_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des delits"), align = "center")),
                          leafletOutput("heatmap_all")      
                   ),
                   # Ligne Info
                   
                   column(width = 12,
                          
                          br(),
                          div(
                            p("Pour avoir acces au jeu de donnees complet, suivez le lien suivant :", 
                              a(href="https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8", "DataSet")), align = "center"),
                          div(
                            p(em("Application web concue par "), em(strong("Penacq Axel"))), align = "center")
                          )
                          
                   )
                   
                 ),
        
        # Second onglet 
        
        tabPanel("Braquages",
                 
                 fluidRow(
                   #Ligne KPI
                   column(width = 2
                   ),
                   column(width = 5,
                          br(),
                          box(title = tags$h4("Nombre de delits :"),
                              width = 20,
                              height = 100,
                              solidHeader = T,
                              status = "info",
                              h4(strong(textOutput("kpi_braquage_value"))))
                   ),
                   column(width = 5,
                          br(),
                          box(
                            title = tags$h4("Moyenne d'age des victimes :"),
                            width = 20,
                            height = 100,
                            solidHeader = TRUE,
                            status = "info",
                            h4(strong(textOutput("kpi_age_braquage_value")))
                          )
                   ),
                   #Ligne Graphiques
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Evolution du nombre de delits"), align = "center")),
                          plotOutput("evo_braquage_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des delits"), align = "center")),
                          leafletOutput("heatmap_braquage")      
                   ),
                   # Ligne Info
                   
                   column(width = 12,
                          
                          br(),
                          div(
                            p("Pour avoir acces au jeu de donnees complet, suivez le lien suivant :", 
                              a(href="https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8", "DataSet")), align = "center"),
                          div(
                            p(em("Application web concue par "), em(strong("Penacq Axel"))), align = "center")
                   )
                   
                 )
                 
        ),
        
        # troisieme onglet 
        
        tabPanel("Cambriolages"
                 
        ),
        
        # Quatrieme onglet 
        
        tabPanel("Homicides"
                 
        ),
        
        # Dernier onglet 
        
        tabPanel("Viols"
                 
        )
        
        
        
        
        
        
        
        
        
        
        
        
        
      )
      
    )
    
  )
)
















server <- function(input, output){
  
  ## TOUS LES DELITS
  
  #Filtrage des donnees
  
  filtered_data_all <- reactive ({data %>% 
      filter(Vict.Age >= min(input$age_filter) & Vict.Age <= max(input$age_filter),
             Vict.Sex %in% input$sexe_filter,
             Date.Rptd >= input$periode_filter[1] & Date.Rptd <= input$periode_filter[2],
             AREA.NAME %in% input$quartier_filter)
  })
  ## KPIs
  
  output$kpi_all_value <- renderText({
    nb_delits <- nrow(filtered_data_all())
    paste( nb_delits, " actes")
  })
  
  output$kpi_age_all_value <- renderText({
    mean_age <- mean(filtered_data_all()$Vict.Age, na.rm = TRUE)
    paste(round(mean_age, 1), " ans")
  })
  
  ## Evolution courbe 
  
  
  output$evo_all_delits <- renderPlot({
    ggplot(filtered_data_all(), aes(x = Date.Rptd)) +
      geom_histogram(binwidth = 30, fill = "grey", color = "black", alpha = 0.7) +
      labs(x = "Date",
           y = "Nombre de delits") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Heatmap
  
  center_lat_a <- reactive({median(filtered_data_all()$LAT)})
  center_lon_a <- reactive({median(filtered_data_all()$LON)})
  
  output$heatmap_all <- renderLeaflet({
    leaflet(data = filtered_data_all()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_a(), lng = center_lon_a(), zoom = 9)
  })
  
  
  
  
  ## Braquages
  
  #Filtrage des donnees
  
  filtered_data_braquage <- reactive ({data %>% 
      filter(data$delit == "Braquage",
             Vict.Age >= min(input$age_filter) & Vict.Age <= max(input$age_filter),
             Vict.Sex %in% input$sexe_filter,
             Date.Rptd >= input$periode_filter[1] & Date.Rptd <= input$periode_filter[2],
             AREA.NAME %in% input$quartier_filter)
  })
  ## KPIs
  
  output$kpi_braquage_value <- renderText({
    nb_delits <- nrow(filtered_data_braquage())
    paste( nb_delits, " actes")
  })
  
  output$kpi_age_braquage_value <- renderText({
    mean_age <- mean(filtered_data_braquage()$Vict.Age, na.rm = TRUE)
    paste(round(mean_age, 1), " ans")
  })
  
  ## Evolution courbe 
  
  
  output$evo_braquage_delits <- renderPlot({
    ggplot(filtered_data_braquage(), aes(x = Date.Rptd)) +
      geom_histogram(binwidth = 30, fill = "grey", color = "black", alpha = 0.7) +
      labs(x = "Date",
           y = "Nombre de delits") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Heatmap
  
  center_lat_b <- reactive({median(filtered_data_braquage()$LAT)})
  center_lon_b <- reactive({median(filtered_data_braquage()$LON)})
  
  output$heatmap_braquage <- renderLeaflet({
    leaflet(data = filtered_data_braquage()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_b(), lng = center_lon_b(), zoom = 9)
  })
  
}

shinyApp(ui=ui , server=server)
