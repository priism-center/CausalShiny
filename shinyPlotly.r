#require(tidyverse)
#require(plotly)

mc1 <- list(color = toRGB("blue", 0.7))
mc2 <- list(color = toRGB("blue", 0.3))
mt1 <- list(color = toRGB("red", 0.7))
mt2 <- list(color = toRGB("red", 0.3))
invcol <- list(color = toRGB("white", 0))

plotly_vis_cont <- function(dat, confounder, nbins) {
  #xplotly <- fit$data.rsp@x
  #yplotly <- fit$data.rsp@x
  covariate <- dat[[confounder]]
  yplotly <- dat[ ,1]
  treat <- dat[ ,2]
  
  df <- cbind.data.frame(covariate, yplotly, treat)
  #df_dis <- df %>% group_by(covariate, treat) %>% mutate()
  
  p1 <- df %>% plot_ly(type = 'scatter', mode = 'markers') %>% 
    add_fun(function(p) {
      p %>% filter(treat == 0) %>%
        add_trace(x = ~covariate, y = ~yplotly, showlegend = F, marker = mc2)
    }) %>% 
    add_fun(function(p) {
      p %>% filter(treat == 1) %>%
        add_trace(x = ~covariate, y = ~yplotly, showlegend = F, marker = mt2)
    }) %>% 
    layout(dragmode = "select")  
  
  p2 <- df %>% plot_ly(type = 'histogram', histnorm = "probability density") %>% 
    add_fun(function(p) {
      p %>% filter(treat == 0) %>%
        add_histogram(x = ~covariate, showlegend = F, autobinx = F, 
                      xbins = nbins, marker = mc2)
    }) %>% 
    add_fun(function(p) {
      p %>% filter(treat == 1) %>%
        add_histogram(x = ~covariate, showlegend = F, autobinx = F, 
                      xbins = nbins, marker = mt2)
    })  %>% layout(barmode = "overlay")
  
  p3 <- p1  %>% subplot(p2, nrows = 2, shareX = T) %>% layout(autosize = F, 
    width = 500, height = 600, yaxis1 = list(title = 'Response(Y)')) %>% rangeslider()
  
  p3
}

plotly_vis_dis <- function(dat, confounder) {
  covariate <- dat[[confounder]]
  yplotly <- dat[ ,1]
  treat <- dat[ ,2]
  
  df <- cbind.data.frame(covariate, yplotly, treat)
  
  p1 <- df %>% plot_ly(type = 'box') %>% 
    add_fun(function(p) {
      p %>% filter(treat == 0) %>%
        add_boxplot(x = ~covariate, y = ~yplotly, jitter = 0.1, pointpos = 1.35, boxpoints = 'all', 
                    showlegend = F, marker = mc2, line = mc2)
    }) %>% 
    add_fun(function(p) {
      p %>% filter(treat == 1) %>%
        add_boxplot(x = ~covariate, y = ~yplotly, jitter = -0.1, pointpos = -1.35, boxpoints = 'all', 
                    showlegend = F, marker = mt2, line = mt2)
    }) %>% layout(boxmode = 'group', yaxis = list(title = 'Response(Y)'))
  
  p2 <- df %>% plot_ly(type = 'histogram', histnorm = "probability density") %>% 
    add_fun(function(p) {
      p %>% filter(treat == 0) %>%
        add_histogram(x = ~covariate, showlegend = F, marker = mc2)
    }) %>% 
    add_fun(function(p) {
      p %>% filter(treat == 1) %>%
        add_histogram(x = ~covariate, showlegend = F, marker = mt2)
    }) %>% layout(barmode = 'group')
  
  p3 <- p1 %>% subplot(p2, nrows = 2, shareX = T) %>% layout(autosize = F, 
          width = 500, height = 800, yaxis1 = list(title = 'Response(Y)'))
  
  p3
}

#xtest <- rep(c(1, 2, 3), 40)
#ytest <- rnorm(120, 10, 1)
#ztest <- rep(c(0, 1), 60)
#df_test <- cbind.data.frame(xtest, ytest, ztest)
#p1 <- df_test %>% plot_ly(x = ~xtest, y = ~ytest, type = 'scatter', mode = 'markers', 
#                          marker = invcol, showlegend = F) %>% 
#  add_fun(function(p) {
#    p %>% filter(ztest == 0) %>%
#      add_boxplot(y = ~ytest, jitter = 0.1, pointpos = 1.35, boxpoints = 'all', 
#                  showlegend = F, marker = mc2, line = mc2)
#  }) %>% 
#  add_fun(function(p) {
#    p %>% filter(ztest == 1) %>%
#      add_boxplot(y = ~ytest, jitter = -0.1, pointpos = -1.35, boxpoints = 'all', 
#                  showlegend = F, marker = mt2, line = mt2)
#  }) %>% 
#  layout(boxmode = 'group')
#p2 <- df_test %>% plot_ly(type = 'bar') %>% 
#  add_fun(function(p) {
#    p %>% filter(ztest == 0) %>%
#      add_histogram(x = ~xtest, showlegend = F, marker = mc2)
#  }) %>% 
#  add_fun(function(p) {
#    p %>% filter(ztest == 1) %>%
#      add_histogram(x = ~xtest, showlegend = F, marker = mt2)
#  })  %>% layout(barmode = 'group')
#p3 <- p1 %>% subplot(p2, nrows = 2)

#cpsdat <- read.csv("/Users/George/Desktop/A3SR/Others/Causal_Inference_Shiny/experiment/cps.csv", header = T)
#cpstest <- cpsdat %>% select(re78, treat, age, educ, black, married, nodegree, re74)

