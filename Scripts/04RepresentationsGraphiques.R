# Exercice

# Représentez la distribution de l’âge selon le sexe, avec des boîtes à moustaches.
# Faites un diagramme en barres du niveau d’études (nivetud), lisible.
# Le nombre d’heures de télévision diffère-t-il selon que l’on pratique un sport ? Représentez-le, puis comparez avec le tableau correspondant de la séance dernière.
# Reprenez le nuage de points âge / heures de télévision, découpez-le par catégorie socio-professionnelle avec facet_wrap(), et donnez-lui un titre et des axes nommés.

# Q1 

ggplot(d) + 
  geom_boxplot(aes(x = sexe, y=age)) +
  labs(
    x = "Sexe", 
    y = "Age",
    title = "Boîtes à moustache de l'âge suivant le sexe"
  )

# Q2

ggplot(d) + 
  geom_bar(aes(y=nivetud)) +
  labs(
    x = "Effectif", 
    y = "Niveau d'étude",
  )

# Q3

ggplot(d) + 
  geom_boxplot(aes(x = sport, y=heures.tv)) +
  labs(
    x = "Pratique du sport", 
    y = "Heures de télélvision par jour",
  )

# Q4

ggplot(d) + 
  geom_jitter(aes(x = age, y = heures.tv), alpha = 0.3) +
  labs(
    title = "Télévision et âge, en fonction de la CSP",
    subtitle = "Enquête Histoires de vie, INSEE, 2003",
    x = "Âge (années)",
    y = "Heures de télévision par jour",
  ) +
  facet_wrap(~ qualif)
