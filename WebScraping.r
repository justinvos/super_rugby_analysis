require("rvest")
require("dplyr")


scrape.match <- function(match.id) {
  
  results.html <- read_html(paste("http://www.espn.co.uk/rugby/matchstats?league=242041&gameId=", match.id ,sep=""))
  
  teams.nodes <- html_nodes(results.html, ".team-name .short-name")
  start = regexpr(">", teams.nodes) + 1
  end = regexpr("</", teams.nodes) - 1
  team.names <- substr(teams.nodes, start, end)

  
  results.nodes <- html_nodes(results.html, ".compareTable tr")
  scores <- html_nodes(results.nodes, ".score")
  
  start = regexpr(">", scores) + 1
  end = regexpr("</", scores) - 1
  scores <- substr(scores, start, end)
  for(i in 1:length(scores)) {
    scores[i] <- clean.score(scores[i])
  }
  
  
  labels <- c("tries", "conversions", "penalties", "kick_success", "kicks_from_hand", "passes", "runs", "possession", "territory", "clean_breaks", "defenders_beaten", "offloads", "rucks_won", "mauls_won", "turnovers_conceded", "red_cards", "yellow_card", "total_free_kicks_conceded")
  
  stat.label <- labels[1:(2*length(labels)+1) %/% 2]
  home.away.prefixes <- rep(c("home", "away"), times = length(stat.label) %/% 2)
  stat.label <- paste(home.away.prefixes, stat.label, sep="_")
  
  labels <- c("match_id", "home_team", "away_team", stat.label)
  
  df <- data.frame()
  df <- rbind(df, c(match.id, team.names, scores), stringsAsFactors=FALSE)
  names(df) <- labels
  
  return(df)
}

clean.score <- function(score) {
  if(grepl("\\(", score)) {
    start <- regexpr("\\(", score) + 1
    end <- regexpr("\\)", score) - 1
    return(clean.score(substr(score, start, end)))
  } else if(grepl("/", score)) {
    pos = regexpr("/", score)
    first.half <- substr(score, 0, pos - 2)
    second.half <- substr(score, pos + 2, nchar(score))
    return((clean.score(first.half) + clean.score(second.half)) / 2)
  } else if(grepl("%", score)) {
    end <- regexpr("%", score) - 1
    score <- substr(score, 0, end)
    return(as.numeric(score) / 100)
  } else {
    return(as.numeric(score))
  }
}




matches.df <- scrape.match(290769)

match.ids <- 290770:290833

for(match.id in match.ids) {
  matches.df <- rbind(matches.df, scrape.match(match.id))
}

matches.df

