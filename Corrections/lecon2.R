vec <- c(23, 53, 4, 5, 6)

moy <- mean(vec)

moy
print(paste("La moyenne est :", moy))
print(max(vec))

library(readr)
donnees_meteo <- read_csv(file.path("donnees", "donnees_meteo.csv"))

head(donnees_meteo)
donnees_meteo[1:10, ]

str(donnees_meteo)

donnees_meteo[4:6, c("temperature","ville")]
donnees_meteo$ville

mean(donnees_meteo$temperature) # NA

mean(donnees_meteo$temperature, na.rm = TRUE) # 18.05
plot(donnees_meteo$date, donnees_meteo$temperature)


donnees <- read.csv(file.path("donnees", "donnees_meteo.csv"))
plot(donnees$date, donnees$temperature)
donnees$date <- as.Date(donnees$date)
plot(donnees$date, donnees$temperature,
     type = "l",
     xlab = "Date")


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
pib <- read_excel("econ-gen-pib-composante.xlsx",    skip = 3, n_max = 15)
pib <- pib %>% drop_na()
pib_t <- t(pib)
colnames(pib_t) <- pib_t[1, ]
pib_t <- pib_t[-1, ]
head(pib_t)
pib_t <- as_tibble(pib_t, rownames = "annees") %>% 
  rowwise() %>% 
  mutate(annees = str_sub(annees, 1, 4)) %>% 
  mutate(across(everything(), as.numeric))
write_csv(pib_t, "PIB.csv")
