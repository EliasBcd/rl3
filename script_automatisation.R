# Script d'automatisation pour générer des rapports
# Ce script montre comment générer automatiquement plusieurs rapports

library(rmarkdown)

# Fonction pour générer un rapport
generer_rapport <- function(fichier_donnees, output_dir = "rapports/") {
  # Extraire le nom du fichier sans extension
  nom_base <- tools::file_path_sans_ext(basename(fichier_donnees))

  # Créer le dossier de sortie si nécessaire
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Générer le rapport
  output_file <- file.path(output_dir, paste0("rapport_", nom_base, ".html"))

  rmarkdown::render(
    "template_analyse.Rmd",
    params = list(
      dataset = fichier_donnees,
      titre = paste("Analyse de", nom_base)
    ),
    output_file = output_file
  )

  message("Rapport généré : ", output_file)
  return(output_file)
}

# Fonction pour analyser un dossier
analyser_dossier <- function(chemin, pattern = "\\.csv$") {
  # Lister les fichiers
  fichiers <- list.files(path = chemin, pattern = pattern,
                         full.names = TRUE)

  if (length(fichiers) == 0) {
    stop("Aucun fichier trouvé !")
  }

  # Analyser chaque fichier
  resultats <- lapply(fichiers, function(f) {
    tryCatch({
      donnees <- read.csv(f)
      data.frame(
        fichier = basename(f),
        n_lignes = nrow(donnees),
        n_colonnes = ncol(donnees),
        colonnes = paste(names(donnees), collapse = ", ")
      )
    }, error = function(e) {
      message("Erreur avec ", f, ": ", e$message)
      data.frame(
        fichier = basename(f),
        n_lignes = NA,
        n_colonnes = NA,
        colonnes = "ERREUR"
      )
    })
  })

  # Combiner et retourner
  do.call(rbind, resultats)
}

# Exemple d'utilisation : générer des rapports pour tous les fichiers de ventes
if (interactive()) {
  # Lister les fichiers de ventes
  fichiers <- list.files("donnees/", pattern = "ventes_.*\\.csv$", full.names = TRUE)

  # Générer un rapport pour chaque fichier
  rapports <- lapply(fichiers, generer_rapport)

  cat("\n✓ Tous les rapports ont été générés avec succès !\n")
  cat("Nombre de rapports:", length(rapports), "\n")
}
