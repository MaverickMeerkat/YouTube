library(gganimate)
library(plotly)

###########
# General #
###########
# Uniform Kernel
f1 = function(u) as.numeric((u<=0.5) & (u>=-0.5))
# Normal Kernel
f2 = function(u) dnorm(u)
# Epanechnikov kernel
f3 = function(u) 0.75*(1-u^2)*(u<=1)*(u>=-1)

Kernels = c(f1, f2, f3)
KTitles = c("Uniform", "Normal", "Epanechnikov")

# Window-Size
D = seq(0.01, 0.21, 0.02) # list of Window-Sizes

# Set the graph X-axis
t = seq(0.05, 0.95, 0.01)

#############################
# Kernel Density Estimation #
#############################
n = 1000
x = c(rnorm(n, 0.2, 0.1), rnorm(n, 0.6, 0.1)) 
hist(x, breaks=20)

KDE = function(x, t, d, kernel) {
  # x - data points
  # t - plotting points
  # d - window size
  # kernel - kernel
  nt = length(t)
  pdf = rep(0, nt)
  for (i in 1:nt){
    # Apply KDE
    u = (t[i]-x)/d  
    pdfi = mean(sapply(u, kernel))/d
    pdf[i] = pdfi
  }
  return(pdf)
}

f_plotly = function(df, type, ktype, D, xlim, ylim, points=NULL) {
  steps = list()
  fig = plot_ly() %>%
    layout(title = paste0(type, ", kernel: ", ktype),
           xaxis = list(range=xlim),
           yaxis = list(range=ylim))
  # if (!missing(points)) {
  #   fig = fig %>% 
  #     add_markers(data=points, x=~x, y=~y, visible=visible)
  # }
  for (i in 1:11) {
    if (i==1){
      visible = T
    } else {
      visible = F
    }
    fig = add_lines(fig, x=df$z1[df$frame==i],  y=df$z2[df$frame==i], visible=visible, 
                     name = D[i], type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                     line=list(color='00CED1'), showlegend = FALSE)
    
    step = list(args = list('visible', rep(FALSE, 11)),
                 method = 'restyle', label={D[i]})
    step$args[[2]][i] = TRUE  
    steps[[i]] = step 
  }  
  # add slider control to plot
  fig = fig %>%
    layout(sliders = list(list(active = 0,
                               currentvalue = list(prefix = "Window Size: "),
                               steps = steps)))
  fig
}

f_KDE = function(x, t, D, kernel, ktitle, xlim, ylim) {
  # Populate the Animation-Data-Frame, z1=t, z2=KDE estimate, frame=frame (window size)
  nt = length(t)
  mat = matrix(nrow=nt*10, ncol=3)
  df = data.frame(z1=mat[,1], z2=mat[,2], frame=mat[,3])
  for (frame in 1:length(D)) {
    # Get window size for the "frame"
    d = D[frame]
    pdf = KDE(x, t, d, kernel)
    df[((frame-1)*nt+1):(frame*nt),1] = t
    df[((frame-1)*nt+1):(frame*nt),2] = pdf
    df[((frame-1)*nt+1):(frame*nt),3] = frame
  }
  f_plotly(df, "KDE", ktitle, D, xlim, ylim)
}

k = 3
f_KDE(x, t, D, Kernels[[k]], KTitles[k], xlim=c(0,1), ylim=c(0,2.6))

#############################
# Non Parametric Regression #
#############################
y = sin(10*x)+0.1*rnorm(2*n)
plot(x,y)
points = data.frame(x=x, y=y)

# Nadarya-Watson (Moving Average - with Kernel Function)
NWReg = function(x, y, t, d, kernel) {
  # x,y - data points
  # t - plotting points
  # d - window size
  # kernel - kernel
  nt = length(t)
  y.h = rep(0, nt)
  for (i in 1:nt){
    # Apply NW Kernel
    u = (t[i]-x)/d
    w = sapply(u, kernel)
    y.hi = sum(w*y)/(sum(w)+1e-8)
    y.h[i] = y.hi
  }
  return(y.h) # return y.hat, i.e. regression
}

# Priestly-Chao (Generalization of Piece-Wise Linear)
PCReg = function(x, y, t, d, kernel) {
  # x,y - data points
  # t - plotting points
  # d - window size
  # kernel - kernel
  ind = order(x)
  xs = x[ind]
  ys = y[ind]
  nx = length(xs)
  nt = length(t)
  y.h = rep(0, nt)
  for (i in 1:nt){
    # Apply PC Kernel
    u = (xs-t[i])/d
    w = sapply(u[2:nx], kernel)
    dw = xs[2:nx]-xs[1:(nx-1)]
    y.hi = sum(w*dw*ys[2:nx])/d
    y.h[i] = y.hi
  }
  return(y.h) # return y.hat, i.e. regression
}

f_KReg = function(x, y, t, D, reg, kernel, ktitle, xlim, ylim, points) {
  # Populate the Animation-Data-Frame, z1=t, z2=KDE estimate, frame=frame (window size)
  nt = length(t)
  mat = matrix(nrow=nt*10, ncol=3)
  df = data.frame(z1=mat[,1], z2=mat[,2], frame=mat[,3])
  for (frame in 1:length(D)) {
    # Get window size for the "frame"
    d = D[frame]
    pdf = reg(x, y, t, d, kernel)
    df[((frame-1)*nt+1):(frame*nt),1] = t
    df[((frame-1)*nt+1):(frame*nt),2] = pdf
    df[((frame-1)*nt+1):(frame*nt),3] = frame
  }
  f_plotly(df, "NW Regression", ktitle, D, xlim, ylim, points)
}

k = 3
f_KReg(x, y, t, D, NWReg, Kernels[[k]], KTitles[k], xlim=c(0,1), ylim=c(-1.5,1.5), points)

# David Refaeli 2021