df1 <- read.csv("plotly_example.csv")

theme_opts <- list(theme(panel.grid.major = element_line(color = "grey", size = 0.1), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), 
                         axis.line = element_line(color = "black"),
                         plot.background = element_blank(),
                         legend.position="right",
                         plot.title = element_text(size = 20)))

p <- ggplot(df1, aes(x, y, col = factor(treatment), 
                     text = paste('X:', round(x, 2),
                                  '<br>Y: ', round(y, 2),
                                  '<br>Tau: ', round(difference, 2)))) + 
  geom_point() + 
  scale_color_manual(values = c("0" = "blue", "1" = "red"), guide = guide_legend(title = "Treatment")) +
  xlab(label = "Pretest") + 
  ylab(label = "Outcome") + 
  theme_opts + 
  stat_function(data = df1[(df1$treatment == 1), ], aes(x = x), col = "red", fun = f1) +
  stat_function(fun = f4, linetype="dashed", col = "red") + 
  stat_function(data = df1[(df1$treatment == 0), ], aes(x = x), col = "blue", fun = f2) +
  stat_function(fun = f3, linetype="dashed", col = "blue") +
  ggtitle(label = "Interactive Plot")

p1 <- p %>% ggplotly(tooltip = "text") %>%
  add_fun(function(p) {
    p %>% filter(treatment == 0) %>%
      add_segments(x = ~x, xend = ~x, y = ~(90 + exp(x*beta1)), yend = ~y, 
                   showlegend = F, color = I("blue"), alpha = 0.3, size = I(1))
  }) %>%
  add_fun(function(p) {
    p %>% filter(treatment == 1) %>%
      add_segments(x = ~x, xend = ~x, y = ~y, yend = ~(72 + beta0*x^(1/2)), 
                   showlegend = F, color = I("red"), alpha = 0.3, size = I(1))
  }) %>%
  rangeslider()