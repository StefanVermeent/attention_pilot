library(bayestestR)  
library(ggforce)
library(ggtext)
library(bruceR)


ssp_plot_data1 <- distribution_normal(n = 1000, mean = 0, sd = 1.5)

ssp_plot_data1 %>% 
  density() %>%  
  as.data.frame() %>% 
  ggplot(aes(x=x, y=y)) +
  geom_line(linewidth = 1) +
  geom_segment(aes(x = -4, xend = -4, y = 0, yend = 0.008), linetype = "longdash") +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 0.008), linetype = "longdash") +
  geom_segment(aes(x = -2, xend = -2, y = 0, yend = 0.11), linetype = "longdash") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 0.11), linetype = "longdash") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.26), linetype = "longdash") +
  scale_x_continuous(breaks = c(-4,-2,0,2,4), labels=c("\u2190<br>|<br>*p*","\u2190<br>|<br>*p*","\u2192<br>|<br>*p*","\u2190<br>|<br>*p*","\u2190<br>|<br>*p*")) +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    axis.text.x = element_markdown(color = 'black', size = 25),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    y = ""
  ) 
  


ssp_plot_data2 <- distribution_normal(n = 1000, mean = 0, sd = 1)

ssp_plot_data2 %>% 
  density() %>%  
  as.data.frame() %>% 
  ggplot(aes(x=x, y=y)) +
  geom_line(linewidth = 1) +
  geom_segment(aes(x = -2, xend = -2, y = 0, yend = 0.06), linetype = "longdash") +
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 0.06), linetype = "longdash") +
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 0.39), linetype = "longdash") +
  scale_x_continuous(breaks = c(-4,-2,0,2,4), labels=c("\u2190<br>|<br>*p*","\u2190<br>|<br>*p*","\u2192<br>|<br>*p*","\u2190<br>|<br>*p*","\u2190<br>|<br>*p*"), limits = c(-4,4)) +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    axis.ticks = element_blank(),
    axis.title.x = element_markdown(),
    axis.text.x = element_markdown(color = 'black', size = 25),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.line.y = element_blank()
  ) +
  
  labs(
    y = ""
  ) 

