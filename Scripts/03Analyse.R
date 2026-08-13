# Exercice 1 

# Faites le tri à plat de la variable relig. Quelle est la modalité la plus fréquente ?
# Exprimez ce tri à plat en pourcentages.
# Quelle proportion des enquêtés pratique la pêche ou la chasse (peche.chasse) ?
# Combien de valeurs manquantes contient la variable trav.satisf ? Que représentent-elles, à votre avis ?

# Q1

d |> count(relig) # Appartenance sans pratique

# Q2

d |> 
  count(relig) |>
  mutate(pourcentage = round(n / sum(n)*100, 1))

# Q3

d |> 
  count(peche.chasse) |> 
  mutate(pourcentage = round(n / sum(n)*100, 1))
# 11,2%

# Q4

d |> 
  summarise(sum(is.na(trav.satisf)))
# 952 : les gens qui n'ont pas de travail (chômeurs et retraités) 

# Exercice 2

# L’âge moyen varie-t-il selon le niveau d’études (nivetud) ?
# Les personnes qui déclarent lire des bandes dessinées sont-elles plus jeunes que les autres ?
# Comparez le nombre moyen d’heures de télévision selon la catégorie socio-professionnelle. 
# Y a-t-il des groupes trop petits pour que la comparaison soit sérieuse ?

# 1
d |> 
  group_by(nivetud) |> 
  summarise(
    age = mean(age),
    effectif = n()
  )

# 2

d |> 
  group_by(lecture.bd) |> 
  summarise(
    age = mean(age),
    effectif = n()
  )

# 3

d |> 
  group_by(qualif) |> 
  summarise(
    heures.tv = mean(heures.tv),
    effectif = n()
  )  

# Exercice 3

# Produisez un court script commenté qui répond à :

# 1. Quel est l'âge moyen des enquêtés, par catégorie socio-professionnelle ?
# 2. Le nombre d'heures de télévision varie-t-il selon le niveau d'études ?
# 3. Les hommes et les femmes déclarent-ils pratiquer un sport dans les mêmes proportions ?

# 1 

d |> 
  group_by(qualif) |> 
  summarise(
    age = mean(age),
    effectif = n()
  )

# 2 

d |> 
  group_by(nivetud) |> 
  summarise(
    heures.tv = mean(heures.tv, na.rm = TRUE),
    effectif = n()
  )  

# 3

d |> 
  count(sexe, sport) |> 
  group_by(sexe) |> 
  mutate(pourcentage = round(100 * n/sum(n), 1)) |> 
  ungroup() |> 
  select(-n) |> 
  pivot_wider(names_from = sexe, values_from = pourcentage)
