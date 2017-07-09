#load diamonds dataset
library(ggplot2)
data(diamonds)
summary(diamonds)
qplot(x = price, data = diamonds)
table(diamonds$price, diamonds)
dim(subset(diamonds,diamonds$price >= 15000))
qplot(x = price, data = diamonds, binwidth = 20)+ 
  scale_x_continuous(limits = c(0,2000), breaks = seq(300,2000,100))
# break diamond prices by cut
qplot(x = price, data = diamonds)+facet_wrap(~cut)
# which cut has the highest priced diamond/lowest/median
by(diamonds$price, diamonds$cut, median)
# scales and multiple histograms
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")
# histogram of price per carat and facet by cut
qplot(x = price/carat, data = diamonds, binwidth = 0.05) + facet_wrap(~cut)+
  scale_x_log10()
# box plot and numerical summary for price (faceted by color)
qplot(x = color, y= price, data = diamonds, geom= 'boxplot') 
by(diamonds$price, diamonds$color, summary)
# price per carat of diamonds across different colors using boxplots
qplot(x = color, y= price/carat, data = diamonds, geom = 'boxplot')
# carat using frequency polygon 
qplot(x = carat, data = diamonds, binwidth = 0.5, geom = 'freqpoly')
length(subset(diamonds$carat, diamonds$carat == 5))
# investigate gapminder data
bill <- read.csv('billion_age.csv', header = T, row.names = 1, check.names = F)
View(bill)
library(tidyr)
library(dplyr)
cleanbill <- t(bill[colSums(!is.na(bill)) > 0]) # remove NA columns
View(cleanbill) # look at new data
qplot(x = 'United States', data = cleanbill, geom = 'line') 
