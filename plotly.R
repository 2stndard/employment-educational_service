if(!require(fpp2)) {
  install.packages('fpp2')
  library(fpp2)
}


if(!require(ggthemes)) {
  install.packages('ggthemes')
  library(ggthemes)
}

if(!require(RColorBrewer)) {
  install.packages('RColorBrewer')
  library(RColorBrewer)
}

if(!require(scales)) {
  install.packages('scales')
  library(scales)
}

if(!require(plotly)) {
  install.packages('plotly')
  library(plotly)
}

if(!require(tidyverse)) {
  install.packages('tidyverse')
  library(tidyverse)
}

if(!require(lubridate)) {
  install.packages('lubridate')
  library(lubridate)
}

if(!require(zoo)) {
  install.packages('zoo')
  library(zoo)
}

#############   data import and setting
tot.edu_svc <- read.csv('D:/R/data/교육서비스업취업자(전체).csv', stringsAsFactors = F)
colnames(tot.edu_svc) <- c('year', 'tot', 'edu')

edu_svc <- tot.edu_svc[, c(1, 3)]

ts.edu_svc <- ts(edu_svc[, -1], start = c(2013, 1), frequency = 12)

ts.tot.edu_svc <- ts(tot.edu_svc[, -1], start = c(2013, 1), frequency = 12)

date <- as.Date(ts.tot.edu_svc[,1])

############   time series plot : 전체 취업자수

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


############   plotly plot

ts.tot.edu_svc %>%
  as.data.frame() %>%
  plot_ly(x = ~date, y = ~tot, color = I('red'), showlegend = F) %>%
  add_trace(type = 'scatter', mode = 'lines', color = I('#40B8D0'), name = '전체') %>%
  add_trace(type = 'scatter', mode = 'markers', color = I('#7F7F7F'), name = '취업자수', 
            marker = list(size = 5)
            ) %>% 
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black')), 
                      showgrid = F,
                      color = 'black', 
                      tickmode = 'array',
                      rangemode = 'tozero',
                      showline = T, 
                      ticks = 'inside'
                      ), 
         yaxis = list(title = '취업자수(천명)', 
                      visible = TRUE, 
                      color = 'black', 
                      nticks = 5, 
                      tickvals = list(25000, 26000, 27000),
                      ticktext = list('25,000', '26,000', '27,000'),
                      tickmode = 'array', 
                      gridcolor = toRGB('white')
                      ), 
         title = list(text = '전체 취업자수', 
                      font = list(size = 30, 
                                  color = toRGB('black')
                                  ), 
                      x = 0, 
                      y = 0.97, 
                      xref = 'paper'
                      ),
         paper_bgcolor = '#d5e4eb',
         plot_bgcolor =  '#d5e4eb'
         )


############   time series plot : 교육서비스업 취업자수

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


############   plotly plot

ts.tot.edu_svc %>%
  as.data.frame() %>%
  plot_ly(x = ~date, y = ~edu, color = I('red'), showlegend = F) %>%
  add_trace(type = 'scatter', mode = 'lines', color = I('#40B8D0'), name = '전체') %>%
  add_trace(type = 'scatter', mode = 'markers', color = I('#7F7F7F'), name = '취업자수', 
            marker = list(size = 5)
  ) %>% 
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black')), 
                      showgrid = T,
                      color = 'black', 
                      tickmode = 'array',
                      rangemode = 'tozero',
                      showline = T, 
                      ticks = 'inside',
                      gridcolor = toRGB('white')
  ), 
        yaxis = list(title = '취업자수(명)', 
                     visible = TRUE, 
                     color = 'black', 
                     nticks = 5, 
                     tickvals = list(1700, 1800, 1900),
                     ticktext = list('1,700', '1,800', '1,900'),
                     tickmode = 'array', 
                     gridcolor = toRGB('white')
        ), 
        title = list(text = '교육서비스업 취업자수', 
                     font = list(size = 30, 
                                 color = toRGB('black')
                     ), 
                     x = 0, 
                     y = 0.97, 
                     xref = 'paper'
        ),
        paper_bgcolor = '#d5e4eb',
        plot_bgcolor =  '#d5e4eb', 
        margin = list(t=50)
  )


############   time series plot : 전체 취업자수 대비 교육서비스업 취업자 비중

autoplot(round(ts.edu_svc/ts.tot.edu_svc[,1], 3)*100, series = '취업자수', size = 1) + 
  ggtitle('전체 취업자수 대비 교육서비스업 취업자 비중') +
  geom_point(color ='grey50') +
  scale_x_continuous(breaks = seq(2013, 2020, 1)) + 
  xlab('연도') + ylab('비중(%)') + 
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


############   plotly plot

ts.tot.edu_svc %>%
  as.data.frame() %>%
  plot_ly(x = ~date, y = ~round(edu/tot, 3)*100, color = I('red'), showlegend = F) %>%
  add_trace(type = 'scatter', mode = 'lines', color = I('#40B8D0'), name = '전체') %>%
  add_trace(type = 'scatter', mode = 'markers', color = I('#7F7F7F'), name = '취업자수', 
            marker = list(size = 5)
  ) %>% 
  layout(xaxis = list(title = list(text ='연도', 
                                   standoff = 5, 
                                   font = list(color = 'black')), 
                      showgrid = T,
                      color = 'black', 
                      tickmode = 'array',
                      rangemode = 'tozero',
                      showline = T, 
                      ticks = 'inside',
                      gridcolor = toRGB('white')
                      ), 
          yaxis = list(title = '취업자비율(%)', 
                       visible = TRUE, 
                       color = 'black', 
                       nticks = 5, 
                       tickvals = list(6.6, 6.8, 7.0, 7.2),
#                       ticktext = list('1,700', '1,800', '1,900'),
                       tickmode = 'array', 
                       gridcolor = toRGB('white')
          ), 
          title = list(text = '교육서비스업 취업자수', 
                       font = list(size = 30, 
                                   color = toRGB('black')
                       ), 
                       x = 0, 
                       y = 0.97, 
                       xref = 'paper'
          ),
          paper_bgcolor = '#d5e4eb',
          plot_bgcolor =  '#d5e4eb', 
          margin = list(t=50)
  )


############   time series plot : seasonal plot
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

############   plotly plot

ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year) %>%
  do (
    p = plot_ly(data = ., x = ~month, y = ~tot, showlegend = F) %>%
      add_trace(type = 'scatter', mode = 'lines', title = list(text = ~year))
  ) %>% subplot(nrows = 3, shareY = T, shareX = T)



ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = factor(year(date)), month = factor(month(date))) %>%
  plot_ly(x = ~month, y = ~tot, showlegend = T) %>%
  add_trace(type = 'scatter', mode = 'lines+markers', name = ~year, color = ~year) %>%
  layout(xaxis = list(title = list(text ='월', 
                                   standoff = 5, 
                                   font = list(color = 'black')), 
                      tickmode = 'array', 
                      tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                      ticktext = list('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월'),
                      showgrid = T,
                      color = 'black', 
                      tickmode = 'array',
                      rangemode = 'tozero',
                      showline = T, 
                      ticks = 'inside',
                      gridcolor = toRGB('white')
  ), 
  yaxis = list(title = '취업자수(명)', 
               visible = TRUE, 
               color = 'black', 
               nticks = 5, 
               tickvals = list(25000, 26000, 27000),
               ticktext = list('25,000', '26,000', '27,000'),
               tickmode = 'array', 
               gridcolor = toRGB('white')
  ), 
  title = list(text = '월별 교육서비스업 취업자수', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '연도'
    ), 
    y = 0.5
  ), 
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

############   time series plot : seasonal plot 교육서비스업

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
        plot.title = element_text(size = 30)) + 
  geom_hline(yintercept = mean(ts.edu_svc), color = 'red', linetype = 'dotted', size = 1.2) + 
  annotate('text', label = '평균', x = 1, y = 1840)


############   plotly plot

ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = year(date), month = month(date)) %>%
  group_by(year) %>%
  do (
    p = plot_ly(data = ., x = ~month, y = ~edu, showlegend = F) %>%
    add_trace(type = 'scatter', mode = 'lines', title = list(text = ~year))
  ) %>% subplot(nrows = 3, shareY = T, shareX = T)



ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = factor(year(date)), month = factor(month(date))) %>%
  plot_ly(x = ~month, y = ~edu, color = ~year, showlegend = T) %>%
  add_trace(type = 'scatter', mode = 'lines+markers', name = ~year) %>%
  add_trace(type = 'scatter', x = ~month, y = ~rep(mean(edu)), mode = 'lines', color = I('red'), name = '평균' ) %>%
  add_data(ts.tot.edu_svc %>%
             as.data.frame() %>%
             mutate(year = factor(year(date)), month = factor(month(date))) %>%
             arrange(year, desc(month)) %>%
             group_by(year) %>%
             slice(1:1)) %>%
  add_trace(type = 'scatter', x = ~month, y = ~edu, mode = 'text', text = ~year, showlegend = F, textposition = "right") %>%
  layout(xaxis = list(title = list(text ='월', 
                                   standoff = 5, 
                                   font = list(color = 'black')), 
                      tickmode = 'array', 
                      tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                      ticktext = list('1월','2월','3월','4월','5월','6월','7월','8월','9월','10월','11월','12월'),
                      showgrid = T,
                      color = 'black', 
                      tickmode = 'array',
                      showline = T, 
                      ticks = 'inside',
                      gridcolor = toRGB('white')
  ), 
  yaxis = list(title = '취업자수(명)', 
               showline = T, 
               visible = TRUE, 
               color = 'black', 
               nticks = 5, 
               tickvals = list(1700, 1800, 1900, 2000),
               ticktext = list('1,700', '1,800', '1,900'),
               tickmode = 'array', 
               gridcolor = toRGB('white')
  ), 
  title = list(text = '월별 교육서비스업 취업자수', 
               font = list(size = 30, 
                           color = toRGB('black')
               ), 
               x = 0, 
               y = 0.97, 
               xref = 'paper'
  ),
  legend = list(
    title = list(
      text = '연도'
      ), 
    y = 0.5
    ),
  paper_bgcolor = '#d5e4eb',
  plot_bgcolor =  '#d5e4eb', 
  margin = list(t=50)
  )

############   time series plot : seasonal plot 교육서비스업

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

############   plotly plot
ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = year(date)) %>%
  mutate(month = factor(month(date)) %>% fct_recode('Jan' = '1',  'Feb' = '2', 'Mar' = '3', 'Apr' = '4', 'May' = '5', 'Jun' = '6', 'Jul' = '7', 'Aug' = '8', 'Sep' = '9', 'Oct' = '10', 'Nov' = '11', 'Dec' = '12') ) %>%
  group_by(month) %>%
  mutate(tot.med = median(tot), tot.mean = mean(tot), edu.med = median(edu), edu.mean = mean(edu)) %>%
  select(year, month, tot, edu, tot.med, tot.mean, edu.med, edu.mean) %>%
  group_by(month) %>%
  do (
    p = plot_ly(data = ., x = ~year, y = ~tot, name = ~month, showlegend = F) %>%
      add_trace(type = 'scatter', mode = 'lines+markers', color = I('black')) %>%
      add_trace(type = 'scatter', mode = 'lines', x = ~year, y = ~tot.mean, color = I('red'), name = '평균') %>%
      add_trace(type = 'scatter', mode = 'lines', x = ~year, y = ~tot.med, color = I('blue'), name = '중간') %>%
#      add_trace(type = 'scatter', mode = 'text', text = ~(scales::number_format(big.mark = ',', accuracy = 1)(edu)), textposition = 'top center', textfont = list(size = 10)) %>%
      layout(xaxis = list(title = list(text =''), 
                          tickmode = 'auto', 
                          showgrid = T,
                          color = 'black', 
                          tickmode = 'array',
                          showline = T, 
                          showticklabels = F,
                          ticks = 'none',
                          gridcolor = toRGB('white')
      ), 
      yaxis = list(title = NA, 
                   showline = T, 
                   visible = TRUE, 
                   color = 'black', 
                   nticks = 5, 
                   tickvals = list(24000, 25000, 26000, 27000),
                   ticktext = list('2,400', '2,500', '2,600', '2,700'),
                   tickmode = 'array', 
                   gridcolor = toRGB('white'), 
                   zeroline = T
      ), 
      annotations = list(
        list(
          x = 0.5, 
          y = -0.05, 
          font = list(size = 16), 
          text = ~unique(month), 
          xref = "paper", 
          yref = "paper", 
          xanchor = "center", 
          yanchor = "bottom", 
          showarrow = FALSE
        )
      ),
      paper_bgcolor = '#d5e4eb',
      plot_bgcolor =  '#d5e4eb'
      )
  ) %>% 
  subplot(nrows = 1, shareX = T, shareY = T, titleX = T, margin = 0) %>%
  layout(title=list(text='월별 전체 취업자수', font = list(size = 20, color = 'blue')), 
         margin = list(t = 50, l = 70, r = 25, b = 50)) %>%
  layout(annotations = list(
    list(x = -0.08 , y = 0.5, text = "취업자수(명)",
         font = list(size = 18),
         textangle = 270,
         showarrow = F, xref='paper', yref='paper', size=15)
  )
  ) %>%
  layout(
    annotations = list(
      list(x = 0.5 , y = -0.08, text = "월",
           font = list(size = 18),
           showarrow = F, xref='paper', yref='paper', size=15)
    )
  )





############   time series plot : seasonal plot 교육서비스업

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


############   plotly plot
ts.tot.edu_svc %>%
  as.data.frame() %>%
  mutate(year = year(date)) %>%
  mutate(month = factor(month(date)) %>% fct_recode('Jan' = '1',  'Feb' = '2', 'Mar' = '3', 'Apr' = '4', 'May' = '5', 'Jun' = '6', 'Jul' = '7', 'Aug' = '8', 'Sep' = '9', 'Oct' = '10', 'Nov' = '11', 'Dec' = '12') ) %>%
  group_by(month) %>%
  mutate(tot.med = median(tot), tot.mean = mean(tot), edu.med = median(edu), edu.mean = mean(edu)) %>%
  select(year, month, tot, edu, tot.med, tot.mean, edu.med, edu.mean) %>%
  group_by(month) %>%
  do (
    p = plot_ly(data = ., x = ~year, y = ~edu, name = ~month, showlegend = F) %>%
    add_trace(type = 'scatter', mode = 'lines+markers', color = I('black')) %>%
    add_trace(type = 'scatter', mode = 'lines', x = ~year, y = ~edu.mean, color = I('red')) %>%
    add_trace(type = 'scatter', mode = 'lines', x = ~year, y = ~edu.med, color = I('blue')) %>%
    #    add_trace(type = 'scatter', mode = 'text', text = ~(scales::number_format(big.mark = ',', accuracy = 1)(edu)), textposition = 'top center', textfont = list(size = 10)) %>%
    layout(xaxis = list(title = list(text =''), 
                        tickmode = 'auto', 
                        showgrid = T,
                        color = 'black', 
                        tickmode = 'array',
                        showline = T, 
                        ticks = 'inside',
                        gridcolor = toRGB('white')
      ), 
      yaxis = list(title = NA, 
                   showline = T, 
                   visible = TRUE, 
                   color = 'black', 
                   nticks = 5, 
                   tickvals = list(1700, 1800, 1900, 2000),
                   ticktext = list('1,700', '1,800', '1,900'),
                   tickmode = 'array', 
                   gridcolor = toRGB('white'), 
                   zeroline = T
      ), 
      annotations = list(
        list(
          x = 0.5, 
          y = 1.0, 
          font = list(size = 16), 
          text = ~unique(month), 
          xref = "paper", 
          yref = "paper", 
          xanchor = "center", 
          yanchor = "bottom", 
          showarrow = FALSE
        )
      ),
      paper_bgcolor = '#d5e4eb',
      plot_bgcolor =  '#d5e4eb'
    )
  ) %>% 
  subplot(nrows = 1, shareX = T, shareY = T, titleX = T, margin = 0) %>%
  layout(title=list(text='월별 교육서비스업 취업자수', font = list(size = 20, color = 'blue')), 
         margin = list(t = 70, l = 70, r = 25, b = 25)) %>%
  layout(annotations = list(
    list(x = -0.08 , y = 0.5, text = "취업자수(명)",
         font = list(size = 18),
         textangle = 270,
         showarrow = F, xref='paper', yref='paper', size=15)
    )
  ) %>%
  layout( 
    annotations = list(
      list(x = 0.5 , y = -0.08, text = "연도",
           font = list(size = 18),
           showarrow = F, xref='paper', yref='paper', size=15)
      )
  )
  
  
  
  