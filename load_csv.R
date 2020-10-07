# load players dataset 

load_players <- function() {
  players <- read.csv('./players_19.csv')
  vars_selection <- c("age", "overall", "potential", "weak_foot", "skill_moves", "international_reputation")
  dataset <- players[vars_selection]
  return(dataset)
}