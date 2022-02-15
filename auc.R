library(ggplot2)
library(reshape2)


filepath <- "/path/to/data"
filename <- "data.csv"
setwd(filepath)
dataset <- read.csv(paste(filepath, filename, sep = ""), fileEncoding = 'GBK')
df <- melt(dataset, id = c("factor", "doctor_number"))
var_names <-
  c("AUC", "Sensitivity", "Specificity", "The average time")

df_auc <- df[df$factor == 'AUC', ]
p = ggplot(data = df_auc, aes(x = variable, y = value)) +
  geom_boxplot(color = "#4C4948", width = 0.15)
pp = p + stat_summary(
  mapping = aes(group = variable),
  fun = "mean",
  colour = c("#77AC30", "#D95319"),
  geom = "point",
  shape = 22,
  size = 3.5,
  fill = "white",
  stroke = 1.5,
  position = position_dodge(0.5)
)
pp_point = pp + geom_point(aes(colour = variable, shape = variable), size =
                             3)
ppp = pp_point +
  scale_fill_discrete(breaks = c("Without_assistance", "With_assistance")) +
  scale_color_manual(values = c(Without_assistance = "#77AC30", With_assistance =
                                  "#D95319")) +
  scale_shape_manual(values = c(Without_assistance = 16, With_assistance =
                                  16)) +
  scale_y_continuous(limits = c(0.7, 1)) +
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
  labs(y = "AUC\n", x = '') +
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
  file = "AUC.eps",
  ppp,
  width = 9.37,
  height = 7.04,
  units = "in",
  device = "eps"
)