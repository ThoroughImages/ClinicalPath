library(ggplot2)
library(reshape2)


filepath <- "/path/to/data"
filename <- "data.csv"
setwd(filepath)
dataset <- read.csv(paste(filepath, filename, sep = ""), fileEncoding = 'GBK')
df <- melt(dataset, id = c("factor", "doctor_number"))
var_names <-
  c("AUC", "Sensitivity", "Specificity", "The average time")

df_SS <- df[df$factor == 'Sensitivity' | df$factor == 'Specificity', ]
df_SS$factor <- as.factor(df_SS$factor)

p = ggplot(data = df_SS, aes(x = factor, y = value, fill = variable)) +
  geom_boxplot(size = 0.1, color = "#4C4948",
               width = 0.3, legend = F) +
  scale_fill_manual(values = c("white", "white"))
pp = p + stat_summary(
  mapping = aes(group = interaction(factor, variable)),
  fun = "mean",
  colour = c("#77AC30", "#77AC30", "#D95319", "#D95319"),
  geom = "point",
  shape = 22,
  size = 3.5,
  stroke = 1.5,
  fill = "white",
  position = position_dodge(0.3),
  show.legend = F
)
pp_point = pp + geom_point(aes(colour = variable, shape = variable),
                           size = 3,
                           position = position_dodge(0.3))
ppp = pp_point +
  #scale_fill_discrete(breaks = c("Without_assistance", "With_assistance")) +
  scale_color_manual(values = c(Without_assistance = "#77AC30", With_assistance =
                                  "#D95319")) +
  scale_shape_manual(values = c(Without_assistance = 16, With_assistance =
                                  16)) +
  scale_y_continuous(limits = c(0.4, 1)) +
  theme_bw() +
  theme(legend.text = element_text(
    colour = "#4C4948",
    size = 16,
    family = "Helvetica"
  )) +
  theme(legend.title = element_blank()) +
  theme(legend.box.background = element_rect(colour = "#4C4948", size =
                                               0.5)) +
  theme(legend.position = c(0.8, 0.1)) +
  labs(y = "Performance\n", x = '') +
  theme(
    axis.title.x = element_text(
      size = 18,
      color = "#4C4948",
      hjust = 0.5,
      family = "Helvetica"
    ),
    axis.title.y = element_text(
      size = 18,
      color = "#4C4948",
      hjust = 0.5,
      family = "Helvetica"
    )
  ) +
  theme(plot.title = element_text(
    size = 18,
    hjust = 0.5,
    vjust = 0.5,
    family = "Helvetica"
  )) +
  theme(axis.text.x = element_text(
    colour = "#4C4948",
    size = 18,
    family = "Helvetica"
  )) +
  theme(axis.text.y = element_text(
    colour = "#4C4948",
    size = 18,
    family = "Helvetica"
  ))

ggsave(
  file = 'performance.eps',
  ppp,
  width = 9.37,
  height = 7.04,
  units = "in",
  device = 'eps'
)