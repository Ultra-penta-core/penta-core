library(ggplot2)
p <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = F)
p + labs(title = 'Fuel efficiency .vs. engine size',
         subtitle = 'Two seaters are exceptional', 
         caption = 'Data from fueleconomy.gov',
         x = 'Engine displacement (L)',
         y = 'Highway fuel economy (mpg)',
         color = 'Car Type')


str(mpg)
df <- mpg[ ,c(2, 3, 9, 11)]

library(tidyverse) # 함수 연산자 (파이프라인 연산자 %>%)

df %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

worst_by_class <- df %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

worst_by_class

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) ==  1)
p + geom_text(aes(label = model), data = best_in_class)
p + geom_label(aes(label = model), data = best_in_class,
               nudge_y = 2, alpha = 0.2)

label <- tibble(displ = Inf, hwy = Inf,
                label = '동해물과 백두산이 \n 마르고 닳도록')
p + geom_text(data = label, aes(label = label),
              vjust = 'top',
              hjust = 'right')

# 범례 위치
p + theme(legend.position = 'top')
p + theme(legend.position = 'right')
p + theme(legend.position = 'bottom')
p + theme(legend.position = 'left')

str(mpg)
df <- mpg[ ,c(2, 3, 9, 11)]

library(tidyverse) # 함수 연산자 (파이프라인 연산자 %>%)

df %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

worst_by_class <- df %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) == 1)

worst_by_class

best_in_class <- mpg %>%
  group_by(class) %>%
  filter(row_number(desc(hwy)) ==  1)
p + geom_text(aes(label = model), data = best_in_class)
p + geom_label(aes(label = model), data = best_in_class,
               nudge_y = 2, alpha = 0.2)

label <- tibble(displ = Inf, hwy = Inf,
                label = '동해물과 백두산이 \n 마르고 닳도록')
p + geom_text(data = label, aes(label = label),
              vjust = 'top',
              hjust = 'right')

# 범례 위치
p + theme(legend.position = 'top')
p + theme(legend.position = 'right')
p + theme(legend.position = 'bottom')
p + theme(legend.position = 'left')

p + theme(legend.position = 'bottom') +
  guides(color = guide_legend(nrow = 1, 
                              override.aes = list(size = 4)))
# 여러 테마 
p + theme_classic()
p + theme_bw()
p + theme_light()
p + theme_dark()
p + theme_void()
p + theme_linedraw()
p + theme_minimal()

# 그래프 파일로 저장
ggsave('myfigure.png', width = 8, height = 6)
ggsave('myfigure.png', width = 1920, height = 1080,
       units = 'px')
ggsave('myplot.pdf')



## sankey
install.packages("networkD3")
# Library
library(networkD3)
library(dplyr)

# Make a connection data frame
links <- data.frame(
  source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)
links
str(links)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)
nodes
str(nodes)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", colourScale=my_color)
p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/sankeyColor1.html"))

sankeyNetwork # 코드 정보


# GDP 기대수명
install.packages("gapminder")
library(gapminder)
str(gapminder)
ggplot(gapminder, aes(gdpPercap, lifeExp,
                      color = continent, size = pop)) +
  geom_point(alpha = 0.5) +
  scale_x_log10(labels = scales::dollar) +
  labs(title = 'Life Expectancy by GDP per Capita',
       x = 'GDP per capita', y = 'Life Expectancy') +
  theme(plot.title = element_text(size = 24,
                                  face = 'bold',
                                  color = 'tomato'))
# scale_x_log10 x축을 log10 단위로 변경 
ggsave('LifeExpGDPperCap.pdf')
ggsave('LifeExpGDPperCap.png', width = 8, height = 6)

# 점의 크기를 인수수(pop)에 대비시키시오.
str(gapminder)

# 연습문제 
df <- read.csv('flights.csv')
df
# year month passengers
ggplot(df, aes(year, passengers,
               color = month, 
               size = passengers)) +
  scale_x_continuous(breaks = seq(1949,1960,1))+
  scale_y_continuous(breaks = seq(0,700,100))+
  geom_point(alpha = 0.8) +
  geom_smooth(color = 'yellow')+
  labs(title = '1949 ~ 1960 Passengers by Month',
       x = 'Year', y = 'Passengers') +
  theme(plot.title = element_text(size = 18,
                                  face = 'bold',
                                  color = 'red'))

ggsave('PassengersByMonth.pdf')
ggsave('PassengersByMonth.png', width = 8, height = 6)