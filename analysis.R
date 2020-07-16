library(fpp2)

if(!require(ggrepel)) {
  install.packages('ggrepel')
  library(ggrepel)
}

if(!require(ggthemes)) {
  install.packages('ggthemes')
  library(ggthemes)
}

library(ggrepel)
library(RColorBrewer)

install.packages('tinytex')
tinytex::install_tinytex()

install.packages("knitr")

edu_svc <- read.csv('D:/R/data/교육서비스업취업자.csv', stringsAsFactors = F)
tot.edu_svc <- read.csv('D:/R/data/교육서비스업취업자(전체).csv', stringsAsFactors = F)

ts.edu_svc <- ts(edu_svc[, -1], start = c(2013, 1), frequency = 12)

ts.tot.edu_svc <- ts(tot.edu_svc[, -1], start = c(2013, 1), frequency = 12)

autoplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1) + 
  ggtitle('전체 취업자수') +
  geom_point(color ='#40b8d0') +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  scale_color_manual(values = c('#40b8d0'), breaks = c('취업자수')) +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())


autoplot(ts.edu_svc, series = '취업자수', size = 1) + 
  ggtitle('교육서비스업 취업자수') +
  geom_point(color ='#40b8d0') +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + 
  xlab('연도') + ylab('취업자수(천명)') + 
  theme_economist() +
  scale_color_manual(values = c('#40b8d0'), breaks = c('취업자수')) +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())


ggseasonplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('전체 취업자수') +
  geom_point() + 
  xlab('월') + ylab('취업자수(천명)') +
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())


ggseasonplot(ts.edu_svc, series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('교육서비스업 취업자수') +
  geom_point() + 
  scale_y_continuous(breaks = seq(1650, 2000, 50)) + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())

ggsubseriesplot(ts.tot.edu_svc[,1], series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('전체 취업자수') +
  geom_point() + 
  scale_y_continuous(breaks = seq(1650, 2000, 50)) + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())

ggsubseriesplot(ts.edu_svc, series = '취업자수', size = 1, year.labels = T) + 
  scale_color_brewer(palette = "Dark2") +
  ggtitle('교육서비스업 취업자수') +
  geom_point() + 
  scale_y_continuous(breaks = seq(1650, 2000, 50)) + 
  xlab('월') + ylab('취업자수(천명)') + 
  theme_economist() +
  theme(axis.text.x = element_text(color = 'black', size = 10),
        axis.text.y = element_text(color = 'black', size = 10), 
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position = 'bottom', 
        legend.title = element_blank())

t <- as.data.frame(cbind(rep(2013:2020, each = 12)[-(91:96)], as.vector(month.abb[cycle(ts.edu_svc)]), as.vector(ts.edu_svc)))
t[,3] <- as.numeric(t[,3])
t[,2] <- factor(t[,2], levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = T)
colnames(t) <- c('year', 'month', 'data')

ggplot(t, aes(x = year, y = data)) + geom_line(aes(group = month)) + facet_wrap(~month) + 
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

ggplot(t, aes(x = month, y = data)) + geom_line(aes(group = year)) + facet_wrap(~year, nrow = 3, ncol = 3) + 
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
  
diff(ts.edu_svc, 1)
diff(ts.edu_svc, 12)


t1 <- as.data.frame(cbind(rep(2013:2020, each = 12)[-(91:96)], as.vector(month.abb[cycle(ts.tot.edu_svc[,1])]), as.vector(ts.tot.edu_svc[,1])))
t1[,3] <- as.numeric(t1[,3])
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

