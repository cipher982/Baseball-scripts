install.packages("devtools")
library(devtools)
install_github ("geoffjentry/twitteR")
library(twitteR)

setup_twitter_oauth ("Lm9Hq6lxNaLhu5yNL10Uxg", "8C9u5jaGEWgHdo8Pkobo4NEP371xjlbfJ4zBv4PU", "194374752-Jw7tqcOd6ELvUrLqQNcxxgKbtQ5fe2iJszQvT8YC", "q2rsS9ysDyUwjQl0PbTUTdK3GdmbD5eeQ41SUeQDWECmX")  # authenticate


userTimeline('drose101',n=10) # tweets from a user

homeTimeline (n=15) # get tweets from home timeline

mentions (n=15) # get your tweets that were retweeted

favs <- favorites("r_programming", n =10) # tweets a user has favorited















