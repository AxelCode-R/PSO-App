pso_2d_server <- function(input, output, session){
  r <- reactiveValues("fn"=NULL, "range"=NULL, "resolution"=NULL, "max_iter"=NULL, "grid"=NULL, "grid_plot"=NULL, "save_X"=NULL)

  observeEvent(input$pso_2d_save_settings,{
    req(input$pso_2d_fun)

    disable("pso_2d_grid_preview")
    eval(parse(text = paste('fn <- function(x) {return(' , gsub("x2", "x[2]",gsub("x1", "x[1]", input$pso_2d_fun)) , ')}', sep='')))
    r$fn <- fn
    r$range <- data.frame(
      lower = c(input$pso_2d_range_x1[1], input$pso_2d_range_x2[1]),
      upper = c(input$pso_2d_range_x1[2], input$pso_2d_range_x2[2])
    )
    r$resolution <- input$pso_2d_resolution
    r$max_iter <- input$pso_2d_iter
    #browser()

    grid <- setNames(expand.grid(seq(r$range$lower[1], r$range$upper[1], length.out = r$resolution), seq(r$range$lower[2], r$range$upper[2], length.out = r$resolution)), c("x1", "x2"))
    grid$fitness <- apply(grid, 1, r$fn)
    r$grid <- grid %>% spread(., key = x2, value = fitness) %>% column_to_rownames("x1") %>% as.matrix()

    dim_z <- max(r$grid)-min(r$grid)

    r$grid_plot <- plot_ly() %>%
      add_surface(
        type = 'surface',
        contours = list(
          z = list(show = TRUE, start = min(r$grid)-0.03*dim_z, end = round(max(r$grid))+0.03*dim_z, size = round(dim_z/10), color="grey", opacity = 0.7)
        ),
        showscale = FALSE,
        opacity=0.7,
        x = rownames(r$grid),
        y = colnames(r$grid),
        z = r$grid,
        inherit = F
      ) %>%
      layout(scene = list(xaxis=list(title="x1"), yaxis=list(title="x2"))) %>%
      config(displayModeBar = FALSE)

    enable("pso_2d_grid_preview")
  })


  output$pso_2d_grid_plot <- renderPlotly({
    req(r$grid_plot)
    r$grid_plot
  })

  observeEvent(input$pso_2d_grid_preview, {
    showModal(modalDialog(
      title = NULL,
      addSpinner(plotlyOutput("pso_2d_grid_plot")),
      easyClose = T,
      size = "xl",
      footer = NULL
    ))
  })


  observeEvent(input$pso_2d_start_pso, {
    req(r$grid_plot)

    fn <- r$fn

    lower <- r$range$lower
    upper <- r$range$upper

    par <- rep(NA, 2)
    control <- list(
      s = 10, # swarm size
      c.p = 0.5, # inherit best
      c.g = 0.5, # global best
      maxiter = r$max_iter, # iterations
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

    lower_mat <- matrix(rep(lower, ncol(X)), ncol=ncol(X))
    upper_mat <- matrix(rep(upper, ncol(X)), ncol=ncol(X))

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
      X[X > upper_mat] <- upper_mat[X > upper_mat]
      X[X < lower_mat] <- lower_mat[X < lower_mat]

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

    r$save_X <- save_X
  })


  output$pso_2d_pso_plot <- renderPlotly({
    req(r$save_X)
    req(r$grid)

    save_X <- r$save_X
    grid <- r$grid
    save(save_X, grid, file="test.rdata")
    browser()

    plot_ly(z = ~grid, type = "contour", y = rownames(grid), x = colnames(grid), showscale = F) %>%
      layout(
        yaxis = list(showticklabels=FALSE, tickvals=""),
        xaxis = list(showticklabels=FALSE, tickvals=""),
        margin=list(r=0, b=0, l=0, t=0, pad=0)
      ) %>%
      html_save(., zoom = 1, vheight = NULL, vwidth = NULL)


    library('plotly')
    library('htmlwidgets')
    library('RCurl')

    image_file <- "img/p.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    save_X$z <- c(min(grid), rep(max(grid), nrow(save_X)-1))
    plot_ly() %>%
      # add_trace(
      #   z = ~grid, type = "contour", x = rownames(grid), y = colnames(grid), frame=0:20, opacity=0.3
      # ) %>%
      add_trace(
        data=save_X,
        x=~axis_2,
        y=~axis_1,
        color = ~z,
        frame = ~iter,
        mode ='markers',
        type = 'scatter',
        showlegend=F,
        marker = list(color = 'red', size=10, opacity = 1, showscale = F)
      ) %>%
      animation_opts(redraw=F, easing="quad") %>%
      layout(
        xaxis = list(gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
        yaxis = list(gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
        images = list(
          list(
            # Add images
            source =  paste('data:image/png;base64', txt, sep=','),
            xref = "x",
            yref = "y",
            y = min(as.numeric(rownames(grid))),
            x = min(as.numeric(colnames(grid))),
            sizey = max(as.numeric(rownames(grid)))-min(as.numeric(rownames(grid))),
            sizex = max(as.numeric(colnames(grid)))-min(as.numeric(colnames(grid))),
            sizing = "stretch",
            xanchor="left",
            yanchor="bottom",
            opacity = 0.4,
            layer = "below"
          )
        )
      )


  })

}
