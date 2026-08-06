classifier_moyenne <- function(x, na.rm = FALSE){
  # print(paste("Ce vecteur contient", sum(is.na(x)), "valeurs manquantes."))
  if(mean(x, na.rm=na.rm) >= 0){
    return("Positif")
  } else{
    return("Négatif")
  }
}

ventes <- data.frame(
  mois = c("Jan", "Fév", "Mar", "Avr", "Mai", "Jun"),
  montant = c(1500, 2000, NA, 1800, 2200, 1900),
  vendeur = c("Alice", "Bob", "alice", "Bob", "Alice", "bob")
)

ventes$vendeur <- tolower(ventes$vendeur)

etudiants <- data.frame(
  nom = c("Dupont", "Martin", "Durand", "", "Petit"),
  age = c(20, "vingt-deux", 19, 21, 18),
  note_math = c(15, 12, NA, 16, 14),
  note_info = c(13, NA, 15, 12, 16),
  filiere = c("Info", "info", "Maths", "INFO", "maths")
)

etudiants[2, "age"] <- 22
etudiants$age <- as.numeric(etudiants$age)
etudiants$filiere <- tolower(etudiants$filiere)
etudiants[4, "nom"] <- NA


etudiants$moyenne <- (etudiants$note_math + etudiants$note_info)/2
etudiants_info <- etudiants[etudiants$filiere == "info",]
mean(etudiants_info$moyenne, na.rm=TRUE)
etudiants_math <- etudiants[etudiants$filiere == "maths",]
mean(etudiants_math$moyenne, na.rm=TRUE)

library(ggplot2)

ggplot(data = etudiants, aes(y=note_math, x=note_info)) +
  geom_count()

ggplot(data=ventes, aes(x= mois, y=montant)) +
  geom_col(fill='blue') +
  labs(title ='Ventes par mois',
       x = "Mois",
       y = "Montant (€)",
       fill = "Vendeur") +
  theme_minimal() #+
  # scale_fill_discrete(palette = scales::pal_viridis())
