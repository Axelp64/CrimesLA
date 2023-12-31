data <- read.csv("DATA/Crime_Data_from_2020_to_Present_20231110.csv")
source(file = "Packages.R")

## reformatage de la date au format yyyy-mm-dd
data$Date.Rptd <- as.Date(data$Date.Rptd, format = "%m/%d/%Y %I:%M:%S %p")
data$Date.Rptd <- as.Date(data$Date.Rptd)
class(data$Date.Rptd)

data <- data %>% 
  mutate(delit = case_when(
    Crm.Cd %in% c("110","113") ~ "Homicide",
    Crm.Cd %in% c("121","122","815","820","821") ~ "Viol",
    Crm.Cd %in% c("210","220") ~ "Braquage",
    Crm.Cd %in% c("310","320") ~ "Cambriolage",
  ))

data <- subset(data, !is.na(delit))

data <- subset(data, Vict.Age >= 0)

data <- mutate(data, Vict.Sex = ifelse(Vict.Sex == "H", "M", Vict.Sex))

table(data$Vict.Sex)




