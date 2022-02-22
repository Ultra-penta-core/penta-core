anscombe
str(anscombe)

anscombe[, c("x1","y1")]


df<-anscombe

par(mfrow=c(2,2))
plot(df$x1, df$y1, pch=19, col="tomato")
abline(lm(y1~x1, data=df), col="blue")

plot(df$x2, df$y2, pch=19, col="tomato")
abline(lm(y2~x2, data=df), col="blue")

plot(df$x3, df$y3, pch=19, col="tomato")
abline(lm(y3~x3, data=df), col="blue")

plot(df$x4, df$y4, pch=19, col="tomato")
abline(lm(y4~x4, data=df), col="blue")

par(mfrow=c(1,1))
#################################################

#install.packages("tidyverse")

library(tidyverse)

str(mpg)


p <- ggplot(data = mpg, mapping = aes(x=displ, y=hwy))
# p는 배경화면이다.
p + geom_point(mapping = aes(color=class, shape = class))
# aes는 그래프의 미적(?)인 부분으로 x축, y축, 칼러 등 그래프가 안에 어떻게 생겼는지를 제외한 모습을 지정할 수 있다. 

p + geom_point(mapping =aes(color = "blue"))
#위에거 안먹힘,, ㅋㅋ 그래서
p + geom_point(color = "blue")
# 걍 바로 color = blue 지정해주면 먹힌다.

colors()
#컬러 commend 확인인

p + geom_point(color = "orange") + facet_wrap(~ class, nrow = 2)

#facet_wrap() 함수 : 함수 안의~ 오른쪽에 기재되는 변수별 level 순서대로 sub그래프를 그려준다. sub그래프들은 1차원(한쪽 방향:왼쪽에서 오른쪽)으로만 그려진다. 

table(mpg$drv)
?mpg
#mpg information
table(mpg$cyl)
par(mfrow=c(1,1))
barplot(table(mpg$cyl), col = "steelblue")

p + geom_point(color = "orange") + facet_grid(drv~cyl)
# facet_grid() 함수 : 함수 안의 ~ 좌/우 변수를 각각 행/열로 나누어 2차원으로 sub그래프들을 그려준다. 

#geom : geometric object -> plot 종류 한번 확인해도 좋을듯, 수십개 있으니까 찾아써라 ~

p + geom_point(color = "blue") + geom_smooth(color="cyan")
# p는 아까 말했지만 도화지라고 생각하고, 그 위에 geom_point, geom_smooth를 그려준다. 
# * geom_smooth = 추세선 도화지(p) 위에 writing#


p + geom_point(mapping = aes(color = class)) + geom_smooth(color = "tomato")

library(ggplot2)
str(diamonds)
p <- ggplot(data= diamonds)

p + geom_bar(mapping = aes(x = cut), fill="steelblue")
p + stat_count(mapping = aes(x = cut), fill="steelblue")

# 여기서 geom_bar나 stat_count의 plot의 결과는 똑같다.
# 왜? -> geom_bar나 stat_count는 똑같은 함수래요.

p + geom_bar(mapping = aes(x=cut, fill = clarity), position = "fill")
# position = "fill" 옵션을 주면 -> 누적막대그래프 형태로 보인다

p + geom_bar(mapping = aes(x=cut, fill = clarity), position = "dodge")
# position = "dodge" -> 세로로 눕혀서
p + geom_bar(mapping = aes(x=cut, fill = clarity), position = "jitter")

#jitter -> 원래 자기위치에서 살짝 비틀어서 보여주는 것 (흩뿌리기형태- 산점도)
ggplot(data = mpg) + geom_point(aes(x=displ, y= hwy, color = class))
# 위 코드는 흔들기 전
ggplot(data = mpg) + geom_point(aes(x=displ, y= hwy, color = class), position = "jitter")

# jitter을 넣으니 산점도의 detail이 더 커진다.(더 많이 보임)(흔든다는 표현 쓰심 ㅋㅋ 이걸로 이해하면 될 듯)



# 이상치(outlier) 까지 보여주는 boxplot을 그려보자 #

# outlier(이상치)는 그래프 안에서의 점점점...이 이상치이다. # 

p <- ggplot(data = mpg, mapping = aes(x = class, y = hwy))

p + geom_boxplot(fill = "lightyellow") + coord_flip()

#coord_flip() -> 눕히는거,,


p <- ggplot(diamonds, aes(x = cut, fill = cut))
# x는 cut, fill = cut 

p + geom_bar(show.legend = F, width = 1)
# barplot 그리기, width = n은 0.5로 바꿔보면 무슨 뜻인지 알 수있다 -> bar간 간격..ㅎ


p + geom_bar(show.legend = F, width = 1) + coord_polar() + labs(x = NULL, y = NULL)
# coord_polar() 는 극좌표계를 바꿔준다 labs(라벨)의 x,y 는 null로 없애준다.

# * 극좌표계 -> 공간 위치가 원점으로부터의 거리와 방향에 의해서 표현된 좌표계를 말한다.

world <- map_data("world")
ggplot(world, aes(long, lat, group = group)) + geom_polygon(fill = "orange", color = "tomato")

?world

