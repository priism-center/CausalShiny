df0 <- read.csv("plotly_example.csv")

df2 <- df0 %>% select(2:7)
df2$group <- ifelse((df2$x > 30 & df2$treatment == 0) | (df2$x < 30 & df2$treatment == 1), 3, 1)
df3 <- df2 %>% filter((x < 20 & treatment == 0) | (x > 40 & treatment == 1)) %>% mutate(group = 5, treatment = 1 - treatment)
df3$y <- ifelse(df3$treatment, df3$y1, df3$y0)
df1 <- rbind.data.frame(df2, df3)

df1 <- df1 %>% mutate(rnum = 1:172)
# relax the groups to make them 5
df1[sample(df1[which(df1$group == 3), ]$rnum, 15), ]$group <- 2
df1[sample(df1[which(df1$group == 5), ]$rnum, 26), ]$group <- 4
df1 <- df1 %>% arrange(group)


theme_opts <- list(theme(panel.grid.major = element_line(color = "grey", size = 0.1), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), 
                         axis.line = element_line(color = "black"),
                         plot.background = element_blank(),
                         legend.position="none",
                         plot.title = element_text(size = 12)))

beta1 <- .06
beta0 <- 3
#tmp <- c(74.0943636, 8.3200778, 0.5136005)

f1 <- function(x) {
  90 + exp(x*beta1)
}

f2 <- function(x) {
  72+beta0*x^(1/2)
}

plotly_overlap <- function(group_thr) {
  #df0 <- df1 %>% filter(group <= group_thr + 1)
  thresh <- round(group_thr*73/100 + 89, 0)
  df0 <- df1 %>% slice(1:thresh)
  
  tmp <- as.numeric(lm(y ~ x + treatment, data = df0)$coefficients)
  
  f3 <- function(x) {
    tmp[1] + tmp[2]*x
  }
  
  f4 <- function(x) {
    tmp[1] + tmp[3] + tmp[2]*x
  }
  
  p <- ggplot(df0, aes(x, y, col = factor(treatment), 
                       text = paste('X:', round(x, 2),
                                    '<br>Y: ', round(y, 2),
                                    '<br>Tau: ', round(difference, 2)))) + 
    geom_point() + 
    scale_color_manual(values = c("0" = "blue", "1" = "red"), guide = guide_legend(title = "Treatment")) +
    xlab(label = "Pretest") + 
    ylab(label = "Outcome") + 
    theme_opts + 
    stat_function(data = df0[(df0$treatment == 1), ], aes(x = x), col = "red", fun = f1, size = 0.1) +
    stat_function(fun = f4, linetype="dashed", col = "red") + 
    stat_function(data = df0[(df0$treatment == 0), ], aes(x = x), col = "blue", fun = f2, size = 0.1) +
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
  
  p2 <- ggplot() + 
    theme_opts +
    geom_histogram(data = filter(df0, treatment == 1), aes(x, y = ..count..), 
                   alpha = 0.5, position = "identity", col = "red", fill = "red", binwidth = 3) + 
    geom_histogram(data = filter(df0, treatment == 0), aes(x, y = -..count..), 
                   alpha = 0.5, position = "identity", col = "blue", fill = "blue", binwidth = 3)
  
  p3 <- p2 %>% ggplotly() %>% layout(autosize = F, width = 550, height = 700) %>%
    subplot(p1, nrows = 2, shareX = T) 
  
  print(p3)
}

p3 <- plotly_overlap(2)