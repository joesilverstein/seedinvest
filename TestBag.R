bag_o_words(dfComplete$Use.of.Funds)
by(DATA$state, DATA$person, bag_o_words)
lapply(dfComplete$Use.of.Funds,  bag_o_words)

breaker(DATA$state)
by(DATA$state, DATA$person, breaker)
lapply(DATA$state,  breaker)

word.split(c(NA, DATA$state))