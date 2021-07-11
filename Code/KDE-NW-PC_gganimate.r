library(gganimate)
library(plotly)

###########
# General #
###########
# Set the Kenrel function
# Uniform Kernel
f = function(u) as.numeric((u<=0.5) & (u>=-0.5))
# Normal Kernel
f2 = function(u) dnorm(u)
# Epanechnikov kernel
f3 = function(u) 0.75*(1-u^2)*(u<=1)*(u>=-1)

Kernels = c(f, f2, f3)
KTitles = c("Uniform", "Normal", "Epanechnikov")
k = 1 # knob to choose Kernel

# Window-Size
D = seq(0.01, 0.21, 0.02) # list of Window-Sizes

# Set the graph X-axis
t = seq(0.05, 0.95, 0.01)
nt = length(t)

#############################
# Kernel Density Estimation #
#############################
n = 1000
# x = runif(n) # uniform density, a bit boring
x = c(rnorm(n, 0.2, 0.1), rnorm(n, 0.6, 0.1)) 
hist(x, breaks=20)
# Sub-Sample 1/10 for plotting
xx = sort(x)
smpl = xx[seq(1,2000, 15)]
density = data.frame(x=smpl, y=rep(0,length(smpl)))

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

# Populate the Animation-Data-Frame, z1=t, z2=KDE estimate, frame=frame (window size)
mat = matrix(nrow=nt*10, ncol=3)
df = data.frame(z1=mat[,1], z2=mat[,2], frame=mat[,3])
for (frame in 1:length(D)) {
  # Get window size for the "frame"
  d = D[frame]
  pdf = KDE(x, t, d, Kernels[[k]])
  df[((frame-1)*nt+1):(frame*nt),1] = t
  df[((frame-1)*nt+1):(frame*nt),2] = pdf
  df[((frame-1)*nt+1):(frame*nt),3] = frame
}

plot(t, df[1:nt,2], type='l')  # sanity check

title <- data.frame(x = 1:11, y = D) # use for title

# Create (gganimate) Animation
# create plot 
p = ggplot() + # x-y data to use in animations
  geom_point(mapping=aes(x=x, y=y), data=density, color='red', shape=4) + 
  geom_line(mapping=aes(x=z1, y=z2), data=df) +
  ylim(0,2.75) + xlim(0,1) + # limits
  labs(title = "Window Size:{title$y[as.integer(closest_state)]}, Kernel: {KTitles[k]}") # title
# create animation by changing the "frame" (window size) and adding transition
anim <- p + transition_states(frame, transition_length=1, state_length=1) 
anim
# Save Animation
# anim_save('KDE_Epanechnikov_kernel.gif', animation = last_animation())

# Create Plotly interactive graph
# create steps and plot all traces
f_plotly = function(df, type, ktype, title) {
  steps <- list()
  fig <- plot_ly() %>%
    layout(title = paste0(type, ", kernel: ", ktype), yaxis = list(range=c(0,2.6)))
  for (i in 1:11) {
    if (i==1){
      visible = T
    } else {
      visible = F
    }
    fig <- add_lines(fig, x=df$z1[df$frame==i],  y=df$z2[df$frame==i], visible=visible, 
                     name = title[i,2], type = 'scatter', mode = 'lines', hoverinfo = 'name', 
                     line=list(color='00CED1'), showlegend = FALSE)
    step <- list(args = list('visible', rep(FALSE, 11)),
                 method = 'restyle', label={title[i,2]})
    step$args[[2]][i] = TRUE  
    steps[[i]] = step 
  }  
  # add slider control to plot
  fig <- fig %>%
    layout(sliders = list(list(active = 0,
                               currentvalue = list(prefix = "Window: "),
                               steps = steps)))
  fig
}

f_plotly(df, "KDE", KTitles[k], title)

#############################
# Non Parametric Regression #
#############################
y = sin(10*x)+0.1*rnorm(2*n)
length(x)-length(y)
plot(x,y)
regression = data.frame(x=x, y=y)

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
    y.hi = sum(w*y)/(sum(w))
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

t = seq(-0.1, 0.9, 0.01)
nt = length(t)
# Populate the Animation-Data-Frame, z1=t, z2=NW estimate, frame=frame (window size)
df2 = data.frame(z1=mat[,1], z2=mat[,2], frame=mat[,3])
for (frame in 1:length(D)) {
  # Get window size for the "frame"
  d = D[frame]
  y.hat = NWReg(x, y, t, d, Kernels[[k]])
  # y.hat = PCReg(x, y, t, d, Kernels[[k]])
  df2[((frame-1)*nt+1):(frame*nt),1] = t
  df2[((frame-1)*nt+1):(frame*nt),2] = y.hat
  df2[((frame-1)*nt+1):(frame*nt),3] = frame
}

lines(t, df2[1:nt,2], col="red") # sanity check

# Animation
p = ggplot() + 
  geom_point(mapping=aes(x=x, y=y), data=regression) +
  geom_line(mapping=aes(x=z1, y=z2), data=df2, col="red") +
  ylim(-1.3,1.3) + xlim(-0.2,1) + 
  labs(title = "Window Size:{title$y[as.integer(closest_state)]}, Kernel: {KTitles[k]}")
anim <- p + transition_states(frame, transition_length=1, state_length=1) 
anim
# Save Animation
anim_save('Reg_NW_Uniform_kernel.gif', animation = last_animation())





# D.R.