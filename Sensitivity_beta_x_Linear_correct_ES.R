  library(ggplot2)
  library(tidyverse)
  #library(metR)
  library(plotly)
  #setwd("~/Dropbox (LSE Statistics)/Elena/R code/Code_checking/")
  setwd("C:/Users/ElenaStanghellini/Dropbox/Elena/R code")
  
  #Code for beta_x contour
  # x axis is p(U=0|Y=0,X=0) = p_ux0
  # y axis is p(U=0|Y=0, X=1)
  #A = OR(Y,U) (beta_u + beta_xu) overall contribution of the mediator
  # for plausible values of x and y
  
  # let's rewrite eq a - beta_x _ log(f(A,x,y)) = 0 as
  # For a range of fixed A (which we can think about what is plausible)
  # We plot beta_x against x and y and focus on beta_x=0
  
  gamma_x = function(p_ux1, p_ux0){
    ret1 <- log(p_ux1/(1-p_ux1)) - log(p_ux0/(1-p_ux0))
    ret1
  }
  
  x= seq(0+0.05,1-0.05,0.05) #a prob so between 0,1
  y = seq(0+0.05,1-0.05,0.05) #a prob so between 0,1
  #rr_ux= seq(0.1,5,0.25) #an RR so +ve -- could parametrise as rr_ux * p_ux0 instead of p_ux1 if 
  # factors are more intuitive e.g if prevalence of U|Y=0,X=1 is twice that of U|Y=0,X=0
  
  gamma_x.sim <- array(dim=c(length(x),length(y)))
  for(i in 1:length(x)){
    for(j in 1:length(y)){
        gamma_x.sim[i,j] <- gamma_x(x[i],y[j])
      }
  }
#### possible values of alpha (a)##########  
  a = 0.378
  a_up=0.503
  a_lo=0.235
###### in what follows a_lo is used###########  
beta_u=c(0.001,0.2,0.5,0.8,1,1.5,2,4,6) #bxu=0
  L=exp(beta_u)
  
  cont.list<-list()
  
  for(i in 1:length(L)){
  test.mat.gamma <- matrix(gamma_x.sim[,],ncol=length(x),nrow=length(y))
  cont.plotly <- plot_ly(x=unique(x),y=unique(y),z=test.mat.gamma, type="contour",colorscale="Greys",
                         contours = list(showlabels = TRUE))%>%
    layout(shapes = list(list(type = "line",
                              line = list(dash = "dash",color="white"),
                              x0 = 0,
                              y0 = 0, 
                              x1 =1 ,
                              y1=1),
      list(type = "line",
                             x0 = 0,
                             y0 = (exp(a_lo)-1)/(L[i]-1), 
                             x1 = 1,
    y1=((exp(a_lo)-1)/(L[i]-1)) + (exp(a_lo)))),
    xaxis = list(range = c(0, 1), title="p0"),
    yaxis = list(range = c(0, 1), title="p1"),
    annotations=list(text=paste("beta_u=",round(beta_u[i],1),sep=""),
                     showarrow = F,x=0.5,y=0.2, 
                     font = list(color = 'white',
                                 family = 'sans serif',
                                 size = 16))) %>%
    hide_colorbar()
  cont.list[[i]]<-cont.plotly}
  
  
  fig1 <- subplot(cont.list, nrows=3, titleX = T, titleY = T)
  fig1
  
  aol <- function(a_v, L_m, x_v,y_v){
    y_int = (exp(a_v)-1)/(L_m-1)
    x_int = (1-((exp(a_v)-1)/(L_m-1)))/exp(a_v)
    #  print(y_int)
    #  print(x_int)
    ret <- (1-y_int)/x_int
    #lim_aol <- (1-(exp(a_v)-1)/(100-1))*(1-((exp(a_v)-1)/(100-1)))/exp(a_v)/2
    #ret <- ret/lim_aol
    ret
    #ret <- c(ret, lim_aol)
  }
  
  aol.test<-aol(a_lo,3,x,y)
  aol.test
