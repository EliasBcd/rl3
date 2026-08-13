# Cours R — L3 économie-finance (M3P), Paris 8

## Contexte

Cours d'initiation à R pour des étudiants de L3 économie-finance, **sans aucune
notion de programmation**. Cinq séances de trois heures, en salle machine,
environ 20 étudiants.

Enseignant : Elias Bouacida.

Objectif : que les étudiants sachent charger un jeu de données, le décrire et
le représenter. Le cours prépare aux usages en économétrie, séries temporelles
et projet tutoré.

## Historique du dépôt — à connaître avant toute modification

- La version **2025** (tag git `2025`, ancien dossier `M3P/`, retiré du
  répertoire de travail) intégrait des LLM comme source d'explication
  première, dès la première séance. **Cette approche a échoué** : les
  étudiants attribuaient les erreurs à l'outil (« c'est la sortie de
  ChatGPT ») sans réfléchir à ce qu'ils faisaient, et n'ont pas acquis
  d'autonomie. Ne pas y revenir.
- La version **2024.1** (tag git, commit `2c176d0`) est la base retenue pour la
  refonte. Elle a été écrite sans LLM et n'a donc rien à retirer.
- Le travail en cours consiste à **élaguer et adapter** 2024.1, pas à
  reconstruire.

## Décisions arrêtées

1. **Un seul jeu de données du début à la fin** : `hdv2003` (paquet
   `questionr`). La version 2024 basculait sur `rp2018` en séance 4 sans
   prévenir — corrigé.
2. **Tidyverse à partir de la séance 2**, annoncé explicitement en séance 1.
   La version 2024 mélangeait base R et tidyverse sans transition.
3. **Pas de LLM dans le déroulé du cours.** Une mise en garde en séance 1
   (on ne peut juger une réponse que si on sait faire sans), une phrase de
   clôture en séance 5. Rien d'autre.
4. **Priorité à l'analyse et à la représentation de données** plutôt qu'à la
   programmation. Conditions, boucles et écriture de fonctions sortent du
   présentiel et restent disponibles en swirl pour les volontaires.
5. **Le temps machine sert à pratiquer, pas à exposer.**
6. **Rendu final en script `.R` commenté**, avec sections `# Titre ----`.
   Pas de Quarto attendu des étudiants (mentionné en option, non noté).
7. **Le manuel de Barnier n'est plus un travail obligatoire** (changement
   d'août 2026). Les étudiants le lisaient peu, et certains passages sur le
   tidyverse ont vieilli. Il reste cité comme ressource indicative : une
   diapositive de ressources en séance 1, un rappel dans la clôture de
   séance 5. Entre deux séances, seules les leçons swirl sont attendues.
8. **Les exercices de mise en pratique passent en tête de la séance
   suivante** (changement d'août 2026), au lieu d'être donnés à faire à la
   maison. Un exercice fait en salle est un exercice fait, et le rappel est
   plus efficace après une semaine qu'immédiatement. Voir la section
   « Reprise » ci-dessous.

## Structure des séances

| Fichier | Séance | Contenu |
|---|---|---|
| `00Introduction.qmd` | 1 | Installation R/RStudio, présentation du cours |
| `01PremiersPas.qmd` | 1 | Objets, types, vecteurs, fonctions, erreurs, scripts, paquets, installation de swirl |
| `02Donnees.qmd` | 2 | Reprise séance 1, passage au tidyverse, tibble, `glimpse`, pipe, `select`/`filter`/`arrange`/`mutate` |
| `03Analyse.qmd` | 3 | Reprise séance 2, `summarise`, valeurs manquantes, erreurs silencieuses, `count`, `group_by`, croisements |
| `04RepresentationsGraphiques.qmd` | 4 | Reprise séance 3, ggplot2 sur `hdv2003` |
| `05Projet.qmd` | 5 | Reprise séance 4, projet en autonomie |
| `Swirl.qmd` | — | Page unique d'instructions swirl, référencée depuis chaque séance |
| `index.qmd` | — | Syllabus |

La navigation (`_quarto.yml`) et `index.qmd` reflètent déjà cette structure.
Chaque séance se termine par des leçons swirl (seul travail attendu entre deux
séances) et s'ouvre par une reprise de la séance précédente (voir
« Structure des reprises » ci-dessous). `01PremiersPas.qmd` n'a pas de
prédécesseur et n'a donc pas de reprise.

## Fils conducteurs à préserver

- **La lecture des messages d'erreur** (séance 1) puis **les erreurs
  silencieuses** (séance 3) remplacent fonctionnellement l'assistant IA. Sans
  elles, un débutant bloqué n'a nulle part où aller. Ne pas les couper pour
  gagner du temps.
- **`n()` systématique dans les `summarise()`** : c'est ce qui rend visible le
  problème des effectifs faibles.
- **Prédire avant d'exécuter** : réflexe demandé dès la séance 1 et rappelé
  ensuite.
- Chaque séance annonce la suivante et rappelle la précédente.

## Structure des reprises (depuis août 2026)

Chaque séance, sauf la première, s'ouvre par une section « Reprise » insérée
juste après le bloc `setup` et avant le contenu propre à la séance : une
diapositive de consigne (`#| eval: false` pour ne pas afficher la sortie),
suivie d'une diapositive de correction. Visée : 30 minutes, correction
comprise. La correction se fait collectivement au tableau — c'est là qu'est
la valeur pédagogique, et elle sert de diagnostic sur ce qui n'est pas passé.

| Fichier | Reprend | Format |
|---|---|---|
| `02Donnees.qmd` | Séance 1 | Prédiction de sortie sur papier (assignation, réassignation, vecteur, coercition silencieuse `c("Chihuahua", 5)`, `mean()` avec `NA`, comparaison logique) |
| `03Analyse.qmd` | Séance 2 | Un tableau de six lignes inventé (`prenom`/`age`/`categorie`/`heures_tv`) ; les étudiants dessinent sur papier le résultat de deux enchaînements dplyr, sans coder |
| `04RepresentationsGraphiques.qmd` | Séance 3 | Script sur `hdv2003` qui tourne sans erreur ni avertissement mais donne un résultat faux : combine le piège du `mutate()` sans `<-` (le recodage n'a jamais lieu) et celui de l'effectif affiché par `n()` qui masque le `na.rm = TRUE` |
| `05Projet.qmd` | Séance 4 | Nuage de points `age`/`heures.tv` sans transparence (2000 points superposés) ; correction avec `geom_jitter()` + `alpha` |

Contrainte propre à l'exercice de `04RepresentationsGraphiques.qmd` : le
script erroné doit s'exécuter **sans aucun message**, ni erreur ni
avertissement — un avertissement le rendrait trivial à repérer.

## Ce qui a été coupé de 2024.1 — ne pas réintroduire sans raison

Trigonométrie, constantes mathématiques et logarithmes (fusionnés en une diapo
de référence), les listes, `ControlFlow.qmd`, `TiragesAleatoires.qmd`,
`AdvancedR.qmd`, `Markdown.qmd`, `Diamants.qmd`.

Le matériel 2024.1 contenait nettement plus de 15 heures de contenu.
**Le tri est le travail principal.** En cas de doute, couper.

## swirl

Cours interactif en français, dépôt `EliasBcd/InitiationR`, 21 leçons.
Installation depuis R : `install_course_github("EliasBcd", "InitiationR")`.

Les étudiants déposent le fichier `.txt` produit à la fin de chaque leçon sur
Moodle : c'est le suivi de progression.

Leçons utilisées : `Manipulations_simples`, `Types`, `Vecteurs`, `Messages_erreurs`, `Fonctions`,
`Logique` (séance 1), `Tableaux_de_donnes`, `Manipuler_les_donnees`
(séance 2), `Valeurs_manquantes`, `Analyse_de_données` (séance 3),
`Representations_graphiques` (séance 4).

Vérifié fonctionnel sur R 4.6.1 / swirl 2.4.5.

## Conventions techniques

- Quarto, sortie `html` + `revealjs`, `embed-resources: true`.
- Français, `lang: fr`.
- Pipe natif `|>`, pas `%>%`.
- Le tableau de travail s'appelle `d` (convention reprise du manuel de
  Barnier, resté une référence même s'il n'est plus un travail obligatoire).
- CSS minimal : ne pas ajouter de mise en forme élaborée.
- Images dans `resources/`.

## Reste à faire



