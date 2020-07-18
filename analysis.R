library(fpp2)

if(!require(ggthemes)) {
  install.packages('ggthemes')
  library(ggthemes)
}

library(RColorBrewer)

edu_svc <- read.csv('C:/R/data/교육서비스업취업자.csv', stringsAsFactors = F)
tot.edu_svc <- read.csv('C:/R/data/교육서비스업취업자(전체).csv', stringsAsFactors = F)

ts.edu_svc <- ts(edu_svc[, -1], start = c(2013, 1), frequency = 12)

ts.tot.edu_svc <- ts(tot.edu_svc[, -1], start = c(2013, 1), frequency = 12)

autoplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1) + 
  ggtitle('전체 취업자수') +
  geom_point(color ='grey50') +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  scale_color_manual(values = c('#40b8d0'), breaks = c('취업자수')) +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))


autoplot(ts.edu_svc, series = '취업자수', size = 1) + 
  ggtitle('교육서비스업 취업자수') +
  geom_point(color ='grey50') +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  scale_color_manual(values = c('#40b8d0'), breaks = c('취업자수')) +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))


ggseasonplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('월별 전체 취업자수') +
  geom_point() + 
  xlab('월') + ylab('취업자수(천명)') +
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))


ggseasonplot(ts.edu_svc, series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('월별 교육서비스업 취업자수') +
  geom_point() + 
  scale_y_continuous(breaks = seq(1650, 2000, 50)) + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))

ggsubseriesplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('월별 전체 취업자수 추이') +
  geom_point() + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))

ggsubseriesplot(ts.edu_svc, series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('월별 교육서비스업 취업자수 추이') +
  geom_point() + 
  scale_y_continuous(breaks = seq(1650, 2000, 50)) + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30))

?ggsubseriesplot
t <- as.data.frame(cbind(rep(2013:2020, each = 12)[-(91:96)], as.vector(month.abb[cycle(ts.edu_svc)]), as.numeric(ts.edu_svc)))

nrow(t)
as.numeric(as.character(t[,3]))
t[,3] <- as.numeric(as.character(t[,3]))

t[,2] <- factor(t[,2], levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = T)
colnames(t) <- c('year', 'month', 'data')
t

library(tidyverse)
t %>% filter(month == 'Jun') %>% summarise(median(data))
t %>% filter(month == 'Jun') %>% summarise(mean(data))

t <- t %>% group_by(month) %>%
  mutate(med = median(data))

t <- t %>% group_by(month) %>%
  mutate(mean = mean(data))

t
library(ggrepel)
ggplot(t, aes(x = year, y = data)) + geom_line(aes(group = month)) + facet_wrap(~month)+ 
  scale_x_discrete(breaks = seq(2013, 2020, 1), labels = c('13', '14', '15', '16', '17', '18', '19', '20')) +
  geom_point(color = 'grey70') + 
  ggtitle('월별 교육서비스업 취업자수 추이') +
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30)) +
  geom_hline(aes(yintercept = med, group = month), colour = 'red') +
  geom_hline(aes(yintercept = mean, group = month), colour = 'blue')
  
mean(t[t$year == '2018', 3])

t <- t %>% group_by(year) %>%
  mutate(med.year = median(data))

t <- t %>% group_by(year) %>%
  mutate(mean.year = mean(data))
t %>% group_by(year) %>% distinct(mean.year)
rm(text.facet)
text.facet.med <- t %>% group_by(year) %>% distinct(med.year) %>% select(year, med.year)
text.facet.mean <- t %>% group_by(year) %>% distinct(mean.year) %>% select(year, mean.year)
text.facet.mean$mean.year <- round(text.facet.mean$mean.year) 
  
ggplot(t, aes(x = month, y = data)) + geom_line(aes(group = year)) + facet_wrap(~year, nrow = 3, ncol = 3) + 
  ggtitle('연도별 교육서비스업 취업자수 추이') +
  scale_x_discrete(labels = c('1','2','3','4','5','6','7','8','9','10','11','12')) +
  geom_point(color = 'grey70') + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'none', 
        legend.title = element_blank(),
        axis.title.y = element_text(size = rel(1.5), angle = 90),
        axis.title.x = element_text(size = rel(1.5)),
        plot.title = element_text(size = 30)) +
  geom_hline(aes(yintercept = med.year, group = year), colour = 'red') +
  geom_hline(aes(yintercept = mean.year, group = year), colour = 'blue') + 
  geom_text_repel(data = text.facet.med, aes(label = paste('중앙 : ', med.year)), x = -Inf, y = Inf, color = 'red') +
  geom_text_repel(data = text.facet.mean, aes(label = paste('평균 : ', mean.year)), x = 7.5, y = Inf, color = 'blue')

+
  geom_text_repel(x=1, y=1800, label = t$med.year)

?annotate

t1 <- as.data.frame(cbind(rep(2013:2020, each = 12)[-(91:96)], as.vector(month.abb[cycle(ts.tot.edu_svc[,1])]), as.vector(ts.tot.edu_svc[,1])))
t1[,3] <- as.numeric(as.character(t1[,3]))
t1[,2] <- factor(t1[,2], levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = T)
colnames(t1) <- c('year', 'month', 'data')

ggplot(t1, aes(x = year, y = data)) + geom_line(aes(group = month)) + facet_wrap(~month) + 
  scale_x_discrete(breaks = seq(2013, 2020, 1), labels = c('13', '14', '15', '16', '17', '18', '19', '20')) +
  geom_point(color = 'grey70') + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())

ggplot(t1, aes(x = month, y = data)) + geom_line(aes(group = year)) + facet_wrap(~year, nrow = 3, ncol = 3) + 
  scale_x_discrete(labels = c('1','2','3','4','5','6','7','8','9','10','11','12')) +
  geom_point(color = 'grey70') + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'black'),
        panel.grid.minor = element_line(color = 'black'),
        panel.background = element_blank(),
        legend.position = 'bottom',
        legend.title = element_blank()) + 
  theme_economist()

diff(ts.tot.edu_svc[,1], 1)
diff(ts.tot.edu_svc[,1], 12)

colnames(ts.tot.edu_svc) <- c('total', 'education')
rate <- round((ts.edu_svc/ts.tot.edu_svc[,1])*100, 3)

diff(rate, 1)
diff(rate, 12)

USAccDeaths
install.packages('tsibble')
library(tsibble)
library(ggplot2)
install.packages('ggplot2')
install.packages('scales')
library(scales)
as_tsibble(USAccDeaths)
str(as_tsibble(USAccDeaths))
as_tsibble(USAccDeaths) %>% ggplot(aes(x = index, y = value)) + geom_line()
