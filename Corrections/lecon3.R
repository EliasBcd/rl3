notes <- c(12, 5, 20)


ifelse(note >=16, "Excellent", "Pas excellent")

for (note in notes){
  if (note >=16){
    print('Excellent')
  } else if(note >= 10) {
    print("Réussite")
  } else {
    print('Echec')
  }
}