# Préalable
library(questionr)
library(tidyverse)

data("hdv2003")
d <- hdv2003

# Q1

nrow(d) # Nombres d'observations
ncol(d) # Nombres de variables

# Q2
glimpse(d)
# 3 variables quanti : id, age, poids
# 3 variables quali : sexe, nivetud, occup, etc

# Q3

levels(d$relig)

# Q4

glimpse(d)

# Oui, dans, par exemple, nivetud, ou qualif

# Exercice 2

# Dans un script commenté :

#    13 Affichez uniquement les variables age, qualif et sport.
#     Combien y a-t-il d’enquêtés de plus de 60 ans ?
#     Affichez les hommes qui déclarent pratiquer un sport, triés du plus jeune au plus âgé.
#     Créez une variable tv.annuelle (heures de télévision par an) et conservez-la dans d.
#     Quel est l’enquêté le plus âgé du jeu de données ? Quelle est sa qualification ?
#     Combien d’enquêtés ont entre 25 et 35 ans ?

# 1

d |> select(age, qualif, sport)

# 2

d |> filter(age > 60) |> nrow() # 488
# ou
d |> filter(age >= 60) |> nrow() # 516

# 3

d |> filter(sport == "Oui", sexe == "Homme") |> arrange(age)

# 4

d <- d |>  mutate(tv.annuelle = heures.tv * 365)

# 5

d |> filter(age == max(d$age)) |> select(poids)

# 6

d |> filter(age >=25, age < 35) |> nrow() # 321
