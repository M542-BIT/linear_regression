congruent_time <- c(12.079, 16.791, 9.564, 8.630, 14.669, 12.238, 14.692, 8.987, 9.401, 14.480, 22.328, 15.298, 15.073, 16.929, 18.200, 12.130, 18.495, 10.639, 11.344, 12.369, 12.944, 14.233, 19.710, 16.004)
incongruent_time <- c(19.278, 18.741, 21.214, 15.687, 22.803, 20.878, 24.572, 17.394, 20.762, 26.282, 24.524, 18.644, 17.510, 20.330, 35.255, 22.158, 25.139, 20.429, 17.425, 34.288, 23.894, 17.960, 22.058, 21.157)
stroop_data <- data.frame(congruent_time, incongruent_time)

library(pwr)
pwr.t.test(n = NULL, d = 0.5, sig.level = 0.05, power = 0.8, type = "paired")

stroop_data$condition <- rep(c("congruent", "incongruent"), each = 12)
model <- lm(congruent_time ~ condition, data = stroop_data)
summary(model)


library(ggplot2)

ggplot(stroop_data, aes(x = congruent_time)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black") +
  labs(x = "Reaction Time (seconds)", y = "Frequency") +
  theme_classic()


ggplot(stroop_data, aes(x = congruent_time, y = incongruent_time)) +
  geom_point() +
  labs(x = "Congruent Reaction Time", y = "Incongruent Reaction Time") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(fill = "white", color = "black", size = 1)) +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE, color = "red")



