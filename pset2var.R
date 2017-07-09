#Pre-load packages
library(ggplot2)
#Problem 1
data(diamonds)
qplot(diamonds$x,diamonds$price,diamonds)+geom_point()
#Problem 3
cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)
#Problem 4
qplot(diamonds$depth,diamonds$price,diamonds)
# Problem 7
cor.test(diamonds$price, diamonds$depth)
# Problem 8
one_carat <- quantile(diamonds$carat, .99)
one_price <- quantile(diamonds$price, .99)
one_data <- subset(diamonds, diamonds$carat < one_carat & diamonds$price < one_price )
qplot(diamonds$carat,diamonds$price, one_data )
# Problem 9
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
qplot(diamonds$volume, diamonds$price, diamonds)
library(plyr)
library(dplyr)
count(diamonds$volume == 0)
# Problem 11
cleandiamonds <- subset(diamonds,  diamonds$volume > 0 & diamonds$volume <= 800)
cor.test(cleandiamonds$price, cleandiamonds$volume)
# Problem 12
ggplot(aes(x = cleandiamonds$volume, y = cleandiamonds$price), data = cleandiamonds)+
  geom_smooth(alpha = 1/20)+abline(lm(cleandiamonds$volume~cleandiamonds$price))
# Problem 14
detach("package:plyr", unload=TRUE) 
dclarity <- group_by(diamonds,clarity)
diamondsByClarity <- summarise(dclarity, 
                               mean_price = mean(price), 
                               median_price = median(price),
                               min_price = min(price),
                               max_price = max(price),
                               n = n()) 
diamondsByClarity <- arrange(diamondsByClarity,clarity)
# Problem 15- prep
data(diamonds)
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))
library(gridExtra)
clarity_bar <-barplot(diamonds_mp_by_clarity$mean_price, names.arg = diamonds_mp_by_clarity$clarity, ylab = "Price", xlab = "Clarity")
color_bar <-barplot(diamonds_mp_by_color$mean_price, names.arg = diamonds_mp_by_color$color, ylab = "Price", xlab = "Clarity")
