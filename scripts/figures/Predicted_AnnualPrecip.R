#plot model prediction of precip on top of data
ggplot(data, aes(x = factor(All_ants), y = total_abundance)) +
  ggtitle("Predicted Plant Sp. Abundance from Annual Precipitation") +
  labs(x = "Presence/Absence of All Ants", y = "Total Species Abundnace") +
  geom_jitter(width = 0.1) + 
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "red") + 
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  annotate("text", x = 1.5, y = 3500, label = "Slope = -0.3170", col = "red")
