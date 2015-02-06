library(plyr)
library(dplyr)

# Create separate tables for games played and bases stolen per player
players <- group_by(Batting, playerID)
Stolen.bases <- summarise(players, sbs = sum(SB))
Games <- summarise(players, gs = sum(G))

# Filter out for players with only 200+ games
over200 <- subset(Games, gs > 200)
over200.steals <- subset(Stolen.bases, playerID %in% over200$playerID)

# Divide the steals over a per game basis and pull top-5
final <- transform(over200.steals,  per.game = sbs / over200$gs)
final <- arrange(final, desc(per.game))
head(final, 5)

