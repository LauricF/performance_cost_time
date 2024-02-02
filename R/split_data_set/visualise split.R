# explore data set split

library("scales")
p1 <- ggplot(data_demographics_ogtt, aes(x = date_at_first_drawing)) +
  geom_histogram() +
  scale_x_date(breaks = date_breaks("24 months"),
                   labels = date_format("%Y")) +
  ylab("observations (n)") +
  xlab("years") +
  geom_vline(xintercept = as.numeric(as.Date("2012-02-20")), 
             color = "red", 
             lwd = 1) +
  theme_bw()
  p1

p2 <- ggplot(data_demographics_ogtt, aes(x = date_at_first_drawing)) +
  scale_x_date(breaks = date_breaks("24 months"),
               labels = date_format("%Y")) +
  geom_histogram(aes(y=cumsum(after_stat(count))))+
  ylab("observations (n)") +
  xlab("years") +
  geom_vline(xintercept = as.numeric(as.Date("2012-02-20")), 
             color = "red", 
             lwd = 1) +
  theme_bw()

plot_grid(p1, p2, labels = c('Histogram', 'Cumulative number'), label_x = 0.05, label_y = 0.95)

ggsave(paste0(path_figures,"cuttingbydate.JPEG"), width = 10, height = 7, dpi = 300)
