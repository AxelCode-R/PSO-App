library(plotly)
library(dplyr)
library(tidyr)
library(tidyverse)
library(htmlwidgets)
library(shiny)


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
  maxiter = 20, # iterations
  w0 = 1.2, # starting inertia weight
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
)/5

P <- X
P_fit <- X_fit
p_g <- P[, which.min(P_fit)]
p_g_fit <- min(P_fit)


save_X <- data.frame("iter"=0, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X))))
save_V <- NULL
for(i in 1:control$maxiter){
  Vw <- (control$w0-(control$w0-control$wN)*i/control$maxiter) * V
  Vp <- control$c.p * runif(length(par)) * (P-X)
  Vg <- control$c.g * runif(length(par)) * (p_g-X)

  save_V <- rbind(save_V,
    cbind(
      data.frame(
        "iter" = i-1,
        "id" = 1:ncol(X)
      ),
      setNames(data.frame(t( Vw )), paste0("Vw_", 1:2)),
      setNames(data.frame(t( Vp )), paste0("Vp_", 1:2)),
      setNames(data.frame(t( Vg )), paste0("Vg_", 1:2))
    )
  )

  # move particles
  V <- Vw + Vp + Vg
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



X_anchors <- rbind(
  save_X,
  save_X %>% mutate(fitness=fitness+3)
) %>%
  group_by(id)

X_velocitys <- rbind(
  save_X %>% mutate(step=1),
  save_X %>% mutate(iter = iter-1) %>% mutate(step=2)
) %>% arrange(iter, id) %>% filter(iter >= min(save_X$iter), iter < max(save_X$iter))
X_velocitys[X_velocitys$step==2,]$fitness <- X_velocitys[X_velocitys$step==1,]$fitness
X_velocitys <- X_velocitys %>%
  group_by(id)

#
# Vw <- rbind(
#   save_V %>% mutate(step=1),
#   save_V %>% mutate(iter = iter-1) %>% mutate(step=2)
# ) %>% arrange(iter, id) %>% filter(iter >= min(save_V$iter), iter < max(save_V$iter))
# Vw[Vw$step==2,]$fitness <- Vw[Vw$step==1,]$fitness
# Vw <- Vw %>%
#   group_by(id)

Vw <- rbind(
  save_X,
  save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vw_1, axis_2 = axis_2 + Vw_2) %>% select(colnames(save_X))
) %>%
  filter(iter < max(iter)) %>%
  group_by(id)

Vp <- rbind(
  save_X,
  save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vp_1, axis_2 = axis_2 + Vp_2) %>% select(colnames(save_X))
) %>%
  filter(iter < max(iter)) %>%
  group_by(id)

Vg <- rbind(
  save_X,
  save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vg_1, axis_2 = axis_2 + Vg_2) %>% select(colnames(save_X))
) %>%
  filter(iter < max(iter)) %>%
  group_by(id)


fix_scale <- data.frame(
  "x"=rep(c(-20, -20, 20, 20), 2),
  "y"=rep(c(-20, 20, -20, 20), 2),
  "z"=c(rep(0, 4), rep(25, 4))
)

fig <- plot_ly() %>%
  add_surface(
    type = 'surface',
    contours = list(
      z = list(show = TRUE, start = round(min(grid)), end = round(max(grid)), size = round((max(grid)-min(grid))/10), color="grey")
    ),
    showscale = FALSE,
    opacity=0.6,
    x = rownames(grid),
    y = colnames(grid),
    z = grid
  ) %>%
  add_trace(
    data=save_X,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    color = ~id,
    frame = ~iter,
    mode ='markers',
    type = 'scatter3d',
    showlegend=F,
    marker = list(color = 'red', size=6, showscale = F)
  ) %>%
  add_trace(
    data = X_anchors,
    x=~axis_1,
    y=~axis_2,
    z=~fitness,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=F,
    line = list(color = 'black', size=4, showscale = F)
  )  %>%
  add_trace(
    data = X_velocitys,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=T,
    name = "V",
    line = list(color = 'red', size=8, showscale = F)
  ) %>%
  add_trace(
    data = Vw,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=T,
    name = "V w",
    line = list(color = 'blue', size=4, showscale = F)
  ) %>%
  add_trace(
    data = Vp,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=T,
    name = "V p",
    line = list(color = 'green', size=4, showscale = F)
  ) %>%
  add_trace(
    data = Vg,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=T,
    name = "V g",
    line = list(color = 'yellow', size=4, showscale = F)
  ) %>%
  add_trace(
    x=rep(c(-20, -20, 20, 20), 2),
    y=rep(c(-20, 20, -20, 20), 2),
    z=c(rep(0, 4), rep(22, 4)),
    #frame = sapply(unique(save_X$iter), function(x){rep(x,4)}) %>% as.vector(),
    mode ='markers',
    type = 'scatter3d',
    showlegend=F,
    marker = list(color = 'transparent', size=0, showscale = F),
    hoverinfo="none"
  ) %>%
  animation_opts(
    redraw = T,
    frame = 1000
  ) %>%
  layout(scene = list(
    xaxis=list(range=c(-20,20)),
    yaxis=list(range=c(-20,20)),
    zaxis=list(range=c(0,22))#,
    #camera = list(eye=list(x=1,y=1,z=2))
  ))

fig








fig2 <- plotly_build(fig)

iters <- unique(save_X$iter)
ids <- unique(save_X$id)
for(i in iters[iters!=max(iters)]){
  anno <- list()
  for(id in ids){
    row <- save_X[save_X$iter==i & save_X$id==id, ]
    row_next <- save_X[save_X$iter==i & save_X$id==id+1, ]
    anno[[id]] <- list(
      type="text",
      x=row$axis_1,
      y=row$axis_2,
      z=row$fitness+3,
      frame=i,
      xref="paper",
      yref="paper",
      zref="paper",
      showarrow = TRUE,
      arrowhead = 4,
      arrowsize = .5,
      ax = row_next$axis_1,
      ay = row_next$axis_2,
      az = row$fitness+3,
      text="v"
    )
  }
  fig2$x$frames[[i+1]]$layout <- list(annotations = list(anno))
}










# save code:
fig <- plot_ly() %>%
  add_surface(
    type = 'surface',
    contours = list(
      z = list(show = TRUE, start = round(min(grid)), end = round(max(grid)), size = round((max(grid)-min(grid))/10), color="grey")
    ),
    showscale = FALSE,
    opacity=0.6,
    x = rownames(grid),
    y = colnames(grid),
    z = grid
  ) %>%
  add_trace(
    data=save_X,
    x=~axis_1,
    y=~axis_2,
    z=~fitness+3,
    color = ~id,
    #split = ~id,
    frame = ~iter,
    mode ='markers',
    type = 'scatter3d',
    showlegend=F,
    marker = list(color = 'red', size=4, showscale = F)
  ) %>%
  add_trace(
    data = X_anchors,
    x=~axis_1,
    y=~axis_2,
    z=~fitness,
    frame = ~iter,
    mode ='lines',
    type = 'scatter3d',
    showlegend=F,
    line = list(color = 'black', size=4, showscale = F)
  ) %>%
  animation_opts(
    redraw = T,
    frame = 1000#,
    # transition = 1000,
    # easing = "linear"
  )# %>%
# add_annotations(
#   x=1,
#   y=1,
#   z=15,
#   frame=0,
#   xref="x",
#   yref="y",
#   showarrow = TRUE,
#   arrowhead = 4,
#   arrowsize = .5,
#   ax = 20,
#   ay = -40,
#   text="hi"
# )
#fig




## annotation in each frame: https://stackoverflow.com/questions/72176553/plotly-r-how-to-add-dynamic-annotation-which-belongs-to-frame



fig2 <- plotly_build(fig)

iters <- unique(save_X$iter)
ids <- unique(save_X$id)
for(i in iters){
  anno <- list()
  seg <- list()
  for(id in ids){
    row <- save_X[save_X$iter==i & save_X$id==id, ]

    # anno[[id]] <- list(
    #   type="text",
    #   x=2,
    #   y=2,
    #   z=10,
    #   frame=0,
    #   xref="x",
    #   yref="y",
    #   showarrow = TRUE,
    #   arrowhead = 4,
    #   arrowsize = .5,
    #   ax = 20,
    #   ay = 10,
    #   text="hi"
    # )
    seg[[id]] <- list(
      x = row$axis_1,
      y = row$axis_2,
      z = row$fitness,
      xend = row$axis_1,
      yend = row$axis_2,
      zend = row$fitness + 3,
      size = I(5)
    )
  }
  fig2$x$frames[[i+1]]$layout <- list(annotations = list(anno), segments = list(seg))
}



fig2$x$frames[[1]]$layout <- list(annotations = list(list(
  type="text",
  x=2,
  y=2,
  z=10,
  frame=0,
  xref="x",
  yref="y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 20,
  ay = 10,
  text="hi"
)))

fig2$x$frames[[2]]$layout <- list(annotations = list(list(
  type="text",
  x=3,
  y=3,
  z=13,
  frame=0,
  xref="x",
  yref="y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 20,
  ay = 10,
  text="hi"
)))

fig2$x$frames[[3]]$layout <- list(annotations = list(list(
  type="text",
  x=4,
  y=4,
  z=15,
  frame=0,
  xref="x",
  yref="y",
  showarrow = TRUE,
  arrowhead = 4,
  arrowsize = .5,
  ax = 20,
  ay = 10,
  text="hi"
)))


# # delay einbauen in shiny: https://stackoverflow.com/questions/56855505/how-to-change-plotly-animation-speed-within-a-javascript-code-in-r
#
# ui <- fluidPage(
#   actionButton("anim", "Animate"),
#   div(style="height:100%, width:100%;", plotlyOutput("plot", width = "auto", height="auto"))
# )
# server <- function(input, output){
#   output[["plot"]] <- renderPlotly({
#     fig <- plot_ly() %>%
#       add_surface(
#         type = 'surface',
#         contours = list(
#           z = list(show = TRUE, start = round(min(grid)), end = round(max(grid)), size = round((max(grid)-min(grid))/10), color="grey")
#         ),
#         showscale = FALSE,
#         opacity=0.6,
#         x = rownames(grid),
#         y = colnames(grid),
#         z = grid
#       ) %>%
#       add_trace(
#         data=save_X,
#         x=~axis_1,
#         y=~axis_2,
#         z=~fitness,
#         color = ~id,
#         frame = ~iter,
#         mode ='markers',
#         type = 'scatter3d',
#         showlegend=F,
#         marker = list(color = 'red', size=4, showscale = F)
#       ) %>%
#       animation_button(visible = FALSE) %>%
#       onRender("
#           function(el,x){
#             $('#anim').on('click', function(){
#               Plotly.animate(el,
#                 null,
#                 {
#                   transition: {
#                     duration: 1000,
#                     easing: 'cubic-in-out'
#                   },
#                   frame: {
#                     duration: 1000
#                   }
#                 }
#               );
#             });
#           }")
#     fig
#   })
# }
#
# shinyApp(ui, server)
