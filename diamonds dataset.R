#histogram of price
qplot(x=price, data=diamonds, bins=15)



qplot(x = price , data = diamonds) +  facet_wrap(~cut)


p <- ggplot(diamonds, aes(price, depth)) + geom_point()

p + facet_grid(cut~.) + scale_x_continuous(limits = c(300, 350))

qplot(x = price , data = diamonds) +
  facet_wrap(~cut)



qplot(x = price, data = diamonds) + facet_grid(~cut, scales="free_y")



diamonds$ppc1=diamonds$price/ diamonds$carat
qplot(x=ppc1, data=diamonds) +
  facet_wrap(~cut,  scales = 'free_y') +
  scale_x_log10() 


qplot(x = price/carat, data = diamonds,
      xlab = 'Price per Carat',
      ylab = 'Number of diamonds in sample',
      color = I('black'), fill = I('#099DD9')) +
  facet_wrap(~cut, scales = 'free_y') +
  scale_x_log10()



ggplot(data = subset(diamonds, !is.na(price)), aes(x = carat, y = price)) + 
  geom_point(alpha=1/100)+
  xlim(0, quantile(diamonds$carat, 0.99)) +
  ylim(0, quantile(diamonds$price, 0.99))

ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(alpha=1/100)+
  scale_x_continuous(breaks=seq(0, 80, 2)) 




diamonds_sub <- subset(diamonds,diamonds$price <= quantile(diamonds$price,0.99) & diamonds$carat <= quantile(diamonds$carat,0.99))

ggplot(aes(x = carat, y = price), data = diamonds_sub) +
  geom_point() +
  scale_y_continuous(breaks = seq(0,19000,2000)) +
  scale_x_continuous(breaks = seq(0,5.2,0.5))
 



#volume
diamonds$diamond_volume <- diamonds$x * diamonds$y * diamonds$z
head(diamonds,20)


ggplot(aes(x = diamond_volume, y = price), data = diamonds) +
  geom_point()

diamonds_subset = subset(diamonds, diamond_volume >0 & diamond_volume <= 800)
cor.test(x = diamonds_subset$diamond_volume, y = diamonds_subset$price)
ggplot(aes(x = diamond_volume, y = price), data = diamonds_subset) +
  geom_point(alpha=1/20)+
  geom_smooth(method='lm', color='red')



#diamonds by clarity
library(dplyr)
diamondsByClarity <- group_by(diamonds,clarity)
diamonds.by_clarity <- summarise(diamondsByClarity,
                                 mean_price = mean(price),
                                 median_price = median(as.numeric(price)),
                                 min_price = min(price),
                                 max_price = max(price),
                                 n = n())

head(diamonds.by_clarity)


#barcharts
diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

#more on bar charts with >geom_bar()
p1 <- ggplot(aes(x = clarity, y = mean_price), data = diamonds_mp_by_clarity) +
  geom_bar(stat = "identity")

p2 <- ggplot(aes(x = color, y = mean_price), data = diamonds_mp_by_color) +
  geom_bar(stat = "identity")

grid.arrange(p1,p2, ncol = 1)