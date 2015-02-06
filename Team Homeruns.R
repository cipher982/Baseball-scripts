require(dplyr)

# Find total homeruns and HR averages for each MLB team

by_team <- group_by(Batting, teamID)
Homeruns <-   summarise(by_team, homeruns = sum(HR), average = mean(HR))
Homeruns.total <-   arrange(Homeruns, desc(homeruns))
Homeruns.avg   <-   arrange(Homeruns, desc(average))

Homeruns.team <- outer_join(Homeruns.total, Homeruns.avg, by=teamID)
