# Pre-load packages
library(ggplot2)
# Problem 1: Price Histograms
data(diamonds)
ggplot(data=diamonds, aes(x=price)) + geom_histogram(aes(fill = cut)) +
  facet_wrap(~color) + scale_fill_brewer(palette="Spectral") + scale_x_log10()
# Problem 2: Price vs Table Colored by Cut
ggplot(data = diamonds, aes(x = table, y = price)) + geom_point(aes(color = cut)) +
         scale_color_brewer(type = 'qual') 
#  Problem 3: Price vs Volume and Diamond Clarity
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
volume99 <- quantile(diamonds$volume, .99)
ggplot(data = subset(diamonds, diamonds$volume < volume99), aes(x = volume , y = log10(price))) + geom_point(aes(color = clarity)) +
  scale_color_brewer(type = 'div')
# Problem 4
pf <- read.delim('pseudo_facebook.tsv')
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count
# Problem 6: prop_intiated vs tenure
pf$year_joined <- pf$year_joined <- floor(2014 - pf$tenure / 365)
pf$year_joined.bucket <- cut(pf$year_joined, breaks = c(2004,2009,2011,2012,2014))
ggplot(data = pf, aes(x = tenure , y = prop_initiated)) + geom_line(aes(color = year_joined.bucket), stat = 'summary',  fun.y = median) +
  scale_color_brewer(type = 'div') + geom_smooth(method ="lm")
# Problem 9: Largest Group mean
pf$prop_initiated <- pf$friendships_initiated/pf$friend_count
pf1214<- subset(pf, pf$year_joined > 2012)
mean(pf1214$prop_initiated)
# Problem 10: Price/Carat binned, facetted, colored
ggplot(data = diamonds, aes(x = cut, y = price/carat))+geom_jitter(aes(color = color))+
  facet_wrap(~clarity)
