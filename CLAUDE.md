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

- La version **2025** (dossier `M3P/`) intégrait des LLM comme source
  d'explication première, dès la première séance. **Cette approche a échoué** :
  les étudiants attribuaient les erreurs à l'outil (« c'est la sortie de
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
5. **Le temps machine sert à pratiquer, pas à exposer.** La théorie est lue à
   la maison dans le manuel de Barnier, *Introduction à R et au tidyverse*.
6. **Rendu final en script `.R` commenté**, avec sections `# Titre ----`.
   Pas de Quarto attendu des étudiants (mentionné en option, non noté).

## Structure des séances

| Fichier | Séance | Contenu |
|---|---|---|
| `02Lecon1.qmd` | 1 | Objets, types, vecteurs, fonctions, erreurs, scripts, paquets |
| `03Donnees.qmd` | 2 | Tibble, `glimpse`, pipe, `select`/`filter`/`arrange`/`mutate` |
| `04Analyse.qmd` | 3 | `summarise`, `count`, `group_by`, croisements, erreurs silencieuses |
| `05Graphiques.qmd` | 4 | ggplot2 sur `hdv2003` |
| `06Projet.qmd` | 5 | Projet en autonomie |

Chaque séance se termine par des leçons swirl et une lecture dans Barnier.

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

Leçons utilisées : `Manipulations_simples`, `Types`, `Vecteurs` (séance 1),
`Dataframes` (séance 2), `Valeurs_manquantes`, `Analyse_de_données` (séance 3),
`Representations_graphiques` (séance 4).

**À vérifier avant la rentrée** : le paquet `swirl` n'a pas été testé depuis
octobre 2024, il est sensible aux montées de version de R.

## Conventions techniques

- Quarto, sortie `html` + `revealjs`, `embed-resources: true`.
- Français, `lang: fr`.
- Pipe natif `|>`, pas `%>%`.
- Le tableau de travail s'appelle `d`, comme chez Barnier.
- CSS minimal : ne pas ajouter de mise en forme élaborée.
- Images dans `resources/`.

## Reste à faire

- Mettre à jour `index.qmd` et `_quarto.yml` pour la nouvelle numérotation.
- Retirer de la navigation les fichiers écartés.
- Compiler les cinq séances et vérifier les sorties (jamais rendues à ce jour).
- Vérifier `swirl` sur la version de R des salles machine.
- Décider du barème.
