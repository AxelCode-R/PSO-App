library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)


mrunif <- function(nr, nc, lower, upper) {
  return(matrix(runif(nr*nc,0,1),nrow=nr,ncol=nc)*(upper-lower)+lower)
}


fn <- function(pos){
  -20 * exp(-0.2 * sqrt(0.5 *((pos[1]-1)^2 + (pos[2]-1)^2))) -
    exp(0.5*(cos(2*pi*pos[1]) + cos(2*pi*pos[2]))) +
    exp(1) + 20
}


par <- rep(NA, 2)
lower <- -10
upper <- 10
control <- list(
  s = 5, # swarm size
  c.p = 0.5, # inherit best
  c.g = 0.5, # global best
  maxiter = 100, # iterations
  w0 = 0.8, # starting inertia weight
  wN = 0.4 # ending inertia weight
)

X <- mrunif(
  nr = length(par), nc=control$s, lower=lower, upper=upper
)
if(all(!is.na(par))){
  X[, 1] <- par
}
X_fit <- apply(X, 2, fn)
V <- mrunif(
  nr = length(par), nc=control$s,
  lower=-(upper-lower), upper=(upper-lower)
)/10

P <- X
P_fit <- X_fit
p_g <- P[, which.min(P_fit)]
p_g_fit <- min(P_fit)


save_X <- data.frame("iter"=0, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X))))
for(i in 1:control$maxiter){
  # move particles
  V <-
    (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
    control$c.p * runif(length(par)) * (P-X) +
    control$c.g * runif(length(par)) * (p_g-X)
  X <- X + V

  # set velocity to zeros if not in valid space
  V[X > upper] <- 0
  V[X < lower] <- 0

  # move into valid space
  X[X > upper] <- upper
  X[X < lower] <- lower

  # evaluate objective function
  X_fit <- apply(X, 2, fn)

  # save new previews best
  P[, P_fit > X_fit] <- X[, P_fit > X_fit]
  P_fit[P_fit > X_fit] <- X_fit[P_fit > X_fit]

  # save new global best
  if(any(P_fit < p_g_fit)){
    p_g <- P[, which.min(P_fit)]
    p_g_fit <- min(P_fit)
  }


  save_X <- rbind(save_X, data.frame("iter"=i, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X)))))
}



grid <- setNames(expand.grid(seq(-10, 10, 0.1), seq(-10, 10, 0.1)), c("axis_1", "axis_2"))
grid$fitness <- apply(grid, 1, fn)
grid <- grid %>% spread(., key = axis_2, value = fitness) %>% column_to_rownames("axis_1") %>% as.matrix()




fig <- plot_ly()
fig <- fig %>% add_surface(
  type = 'surface',
  contours = list(
    z = list(show = TRUE, start = round(min(grid)), end = round(max(grid)), size = round((max(grid)-min(grid))/10), color="grey")
  ),
  showscale = FALSE,
  opacity=0.6,
  x = rownames(grid),
  y = colnames(grid),
  z = grid)
fig <- fig %>%
  add_trace(
    data=save_X,
    x=~axis_1,
    y=~axis_2,
    z=~fitness,
    color = ~id,
    frame = ~iter,
    mode ='markers',
    type = 'scatter3d',
    showlegend=F,
    marker = list(color = 'red', size=4, showscale = F)
  )
fig




