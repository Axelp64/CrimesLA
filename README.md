# Web App - Crimes LA 

## Application Web - Data Visualisation 

### Présentation 
Cette Application Web offre une visualisation sur un ensemble de crimes et délits commis entre 2021 et 2023 dans la ville de Los Angeles. 
Plus précisement, vous aurez la possibilité d'y retrouver différntes statistiques et visuels sur les homicides, Viols, Braquages et Cambriolages qui y ont été commis.

Voici un aperçu du l'application web :

![Aperçu](https://i.imgur.com/c23eKhO.png)

### Utilisation de l'application 

Si vous souhaitez utiliser cette data visualisation voici comment procéder :

1. Téléchargez l'ensemble de mon dossier GitHub "CrimesLA" sur votre ordinateur personnel.
2. Rendez-vous dans le dossier téléchargé, créez un dossier "DATA" et installez les données les données via le lien suivant [Data](https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8) et placez dans votre dossier DATA.
3. Ouvrez le dossier téléchargé et lancez le fichier nommez *Data_Crime_RshinyApp.R* 
4. Une fois arrivé sur R, rendez vous sur la partie inférieure droite de la fênetre, dans l'onglet *files*. Remplacez le Répertoir directeur de votre R par le dossier "CrimesLA"
5. Ouvrez le fcihier *Gloabal.R* et remplacez, sur la première ligne, le chemin vers les données par le votre.
6. *Étape facultative* : Ouvrez maintenant le fichier *Packages.R*. Si vous n'avez pas déjà téléchargé les library utilisées, installez les et enregistrez le fichier.
7. Sur le fichier *Data_Crime_RshinyApp.R*, cliquez sur "Run App".
8. Le tour est joué ! Vous pouvez maintenant profiter de l'ensemble des fonctionnalités de l'application web :)

### Contenu et fonctionnalité

**Filtres**
- Age de la victime
- Sexe de la victime 
- Période 
- Quartiers

**Onglets**
- Tous les Délits / Crimes
- Braquages
- Cambriolages
- Homicides
- Viols
- Aperçu des données

**KPIs et visualisations**
- Nombres de délits / crimes
- Moyenne d'âge des victimes
- Graphique en barre offrant une vision sur l'évolution du nombre d'actes
- HeatMap permettant de visualiser les quartiers dans lesquelles le plus d'actes sont commis




### Code R 

`code`

```r
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
                          box(title = tags$h4("Nombre de braquages :"),
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
                            h4(em("Evolution du nombre de bracages"), align = "center")),
                          plotOutput("evo_braquage_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des braquages"), align = "center")),
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
        
        tabPanel("Cambriolages",
                 fluidRow(
                   #Ligne KPI
                   column(width = 2
                   ),
                   column(width = 5,
                          br(),
                          box(title = tags$h4("Nombre de cambriolages :"),
                              width = 20,
                              height = 100,
                              solidHeader = T,
                              status = "info",
                              h4(strong(textOutput("kpi_cambri_value"))))
                   ),
                   column(width = 5,
                          br(),
                          box(
                            title = tags$h4("Moyenne d'age des victimes :"),
                            width = 20,
                            height = 100,
                            solidHeader = TRUE,
                            status = "info",
                            h4(strong(textOutput("kpi_age_cambri_value")))
                          )
                   ),
                   #Ligne Graphiques
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Evolution du nombre de cambriolages"), align = "center")),
                          plotOutput("evo_cambri_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des cambriolages"), align = "center")),
                          leafletOutput("heatmap_cambri")      
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
        
        # Quatrieme onglet 
        
        tabPanel("Homicides",
                 fluidRow(
                   #Ligne KPI
                   column(width = 2
                   ),
                   column(width = 5,
                          br(),
                          box(title = tags$h4("Nombre d'homicides :"),
                              width = 20,
                              height = 100,
                              solidHeader = T,
                              status = "info",
                              h4(strong(textOutput("kpi_homi_value"))))
                   ),
                   column(width = 5,
                          br(),
                          box(
                            title = tags$h4("Moyenne d'age des victimes :"),
                            width = 20,
                            height = 100,
                            solidHeader = TRUE,
                            status = "info",
                            h4(strong(textOutput("kpi_age_homi_value")))
                          )
                   ),
                   #Ligne Graphiques
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Evolution du nombre d'e delits'homicides"), align = "center")),
                          plotOutput("evo_homi_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des homicides"), align = "center")),
                          leafletOutput("heatmap_homi")      
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
        
        # Dernier onglet 
        
        tabPanel("Viols",
                 fluidRow(
                   #Ligne KPI
                   column(width = 2
                   ),
                   column(width = 5,
                          br(),
                          box(title = tags$h4("Nombre de viols :"),
                              width = 20,
                              height = 100,
                              solidHeader = T,
                              status = "info",
                              h4(strong(textOutput("kpi_viol_value"))))
                   ),
                   column(width = 5,
                          br(),
                          box(
                            title = tags$h4("Moyenne d'age des victimes :"),
                            width = 20,
                            height = 100,
                            solidHeader = TRUE,
                            status = "info",
                            h4(strong(textOutput("kpi_age_viol_value")))
                          )
                   ),
                   #Ligne Graphiques
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Evolution du nombre de viols"), align = "center")),
                          plotOutput("evo_viol_delits")
                   ),
                   column(width = 6,
                          br(),
                          div(
                            h4(em("Localisation des viols"), align = "center")),
                          leafletOutput("heatmap_viol")      
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
        tabPanel("Apercu des donnees",
                       column(width = 12,
                        br(),
                        div(
                          h4(em("Data"), align = "center")),
                        tableOutput("apercu_all")))        
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
  
  # Apercu des donnees (10 premieres lignes )
  
  output$apercu_all <- renderTable({
    head(filtered_data_all(), 10)
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
  
  
  
  
  ## Cambriolages
  
  #Filtrage des donnees
  
  filtered_data_cambri <- reactive ({data %>% 
      filter(data$delit == "Cambriolage",
             Vict.Age >= min(input$age_filter) & Vict.Age <= max(input$age_filter),
             Vict.Sex %in% input$sexe_filter,
             Date.Rptd >= input$periode_filter[1] & Date.Rptd <= input$periode_filter[2],
             AREA.NAME %in% input$quartier_filter)
  })
  ## KPIs
  
  output$kpi_cambri_value <- renderText({
    nb_delits <- nrow(filtered_data_cambri())
    paste( nb_delits, " actes")
  })
  
  output$kpi_age_cambri_value <- renderText({
    mean_age <- mean(filtered_data_cambri()$Vict.Age, na.rm = TRUE)
    paste(round(mean_age, 1), " ans")
  })
  
  ## Evolution courbe 
  
  
  output$evo_cambri_delits <- renderPlot({
    ggplot(filtered_data_cambri(), aes(x = Date.Rptd)) +
      geom_histogram(binwidth = 30, fill = "grey", color = "black", alpha = 0.7) +
      labs(x = "Date",
           y = "Nombre de delits") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Heatmap
  
  center_lat_c <- reactive({median(filtered_data_cambri()$LAT)})
  center_lon_c <- reactive({median(filtered_data_cambri()$LON)})
  
  output$heatmap_cambri <- renderLeaflet({
    leaflet(data = filtered_data_cambri()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_c(), lng = center_lon_c(), zoom = 9)
  })
  
  
  
  
  ## Homicides
  
  #Filtrage des donnees
  
  filtered_data_homi <- reactive ({data %>% 
      filter(data$delit == "Homicide",
             Vict.Age >= min(input$age_filter) & Vict.Age <= max(input$age_filter),
             Vict.Sex %in% input$sexe_filter,
             Date.Rptd >= input$periode_filter[1] & Date.Rptd <= input$periode_filter[2],
             AREA.NAME %in% input$quartier_filter)
  })
  ## KPIs
  
  output$kpi_homi_value <- renderText({
    nb_delits <- nrow(filtered_data_homi())
    paste( nb_delits, " actes")
  })
  
  output$kpi_age_homi_value <- renderText({
    mean_age <- mean(filtered_data_homi()$Vict.Age, na.rm = TRUE)
    paste(round(mean_age, 1), " ans")
  })
  
  ## Evolution courbe 
  
  
  output$evo_homi_delits <- renderPlot({
    ggplot(filtered_data_homi(), aes(x = Date.Rptd)) +
      geom_histogram(binwidth = 30, fill = "grey", color = "black", alpha = 0.7) +
      labs(x = "Date",
           y = "Nombre de delits") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Heatmap
  
  center_lat_h <- reactive({median(filtered_data_homi()$LAT)})
  center_lon_h <- reactive({median(filtered_data_homi()$LON)})
  
  output$heatmap_homi <- renderLeaflet({
    leaflet(data = filtered_data_homi()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_h(), lng = center_lon_h(), zoom = 9)
  })
  
  
  
  ## Viols
  
  #Filtrage des donnees
  
  filtered_data_viol <- reactive ({data %>% 
      filter(data$delit == "Viol",
             Vict.Age >= min(input$age_filter) & Vict.Age <= max(input$age_filter),
             Vict.Sex %in% input$sexe_filter,
             Date.Rptd >= input$periode_filter[1] & Date.Rptd <= input$periode_filter[2],
             AREA.NAME %in% input$quartier_filter)
  })
  ## KPIs
  
  output$kpi_viol_value <- renderText({
    nb_delits <- nrow(filtered_data_viol())
    paste( nb_delits, " actes")
  })
  
  output$kpi_age_viol_value <- renderText({
    mean_age <- mean(filtered_data_viol()$Vict.Age, na.rm = TRUE)
    paste(round(mean_age, 1), " ans")
  })
  
  ## Evolution courbe 
  
  
  output$evo_viol_delits <- renderPlot({
    ggplot(filtered_data_viol(), aes(x = Date.Rptd)) +
      geom_histogram(binwidth = 30, fill = "grey", color = "black", alpha = 0.7) +
      labs(x = "Date",
           y = "Nombre de delits") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  ## Heatmap
  
  center_lat_v <- reactive({median(filtered_data_viol()$LAT)})
  center_lon_v <- reactive({median(filtered_data_viol()$LON)})
  
  output$heatmap_viol <- renderLeaflet({
    leaflet(data = filtered_data_viol()) %>%
      addTiles()%>% 
      addTiles() %>%
      addHeatmap(
        lat = ~LAT,
        lng = ~LON,
        blur = 20,
        max = 0.2,
        radius = 10
      )    %>%
      setView(lat = center_lat_v(), lng = center_lon_v(), zoom = 9)
  })
  
}

shinyApp(ui=ui , server=server)

```
