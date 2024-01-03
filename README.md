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
5. *Étape facultative* : Ouvrez maintenant le fichier *Packages.R*. Si vous n'avez pas déjà téléchargé les library utilisées, installez les et enregistrez le fichier.
6. Sur le fichier *Data_Crime_RshinyApp.R*, cliquez sur "Run App".
7. Le tour est joué ! Vous pouvez maintenant profiter de l'ensemble des fonctionnalités de l'application web :)

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




### Code R créé 

`code`

```r
2+2
```
