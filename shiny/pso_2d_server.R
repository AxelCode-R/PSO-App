pso_2d_server <- function(input, output, session){
  r <- reactiveValues("fn"=NULL, "range"=NULL, "resolution"=NULL, "max_iter"=NULL, "grid"=NULL, "grid_plot"=NULL, "save_X"=NULL)

  observeEvent(input$pso_2d_save_settings,{
    req(input$pso_2d_fun)

    #updateProgressBar(session = session, id = "pso_2d_settings", value = 20)
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

    #updateProgressBar(session = session, id = "pso_2d_settings", value = 40)

    grid <- setNames(expand.grid(seq(r$range$lower[1], r$range$upper[1], length.out = r$resolution), seq(r$range$lower[2], r$range$upper[2], length.out = r$resolution)), c("x1", "x2"))
    #updateProgressBar(session = session, id = "pso_2d_settings", value = 60)
    grid$fitness <- apply(grid, 1, r$fn)
    #updateProgressBar(session = session, id = "pso_2d_settings", value = 80)
    r$grid <- grid %>% spread(., key = x2, value = fitness) %>% column_to_rownames("x1") %>% as.matrix()

    #updateProgressBar(session = session, id = "pso_2d_settings", value = 100)
    enable("pso_2d_grid_preview")
  })



  output$pso_2d_grid_plot <- renderPlotly({
    req(r$grid)

    dim_z <- max(r$grid)-min(r$grid)

    plot_ly() %>%
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
  }) %>%
    bindEvent(input$pso_2d_grid_preview)


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
    req(r$grid)

    #updateProgressBar(session = session, id = "pso_2d_settings", value = 20)
    isolate({
      fn <- r$fn

      lower <- r$range$lower
      upper <- r$range$upper

      par <- rep(NA, 2)
      control <- list(
        s = input$pso_2d_s, # swarm size
        c.p = input$pso_2d_coef_p, # inherit best
        c.g = input$pso_2d_coef_g, # global best
        maxiter = r$max_iter, # iterations
        w0 = input$pso_2d_inertia_weight_w0, # starting inertia weight
        wN = input$pso_2d_inertia_weight_wN # ending inertia weight
      )
    })


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

    lower_mat <- matrix(rep(lower, ncol(X)), ncol=ncol(X))
    upper_mat <- matrix(rep(upper, ncol(X)), ncol=ncol(X))

    #browser()

    save_X <- data.frame("iter"=0, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X))))
    save_V <- NULL
    save_V_raw <- NULL
    for(i in 1:control$maxiter){
      Vw_raw <- V
      Vp_raw <- (P-X)
      Vg_raw <- (p_g-X)
      Vw <- (control$w0-(control$w0-control$wN)*i/control$maxiter) * Vw_raw
      Vp <- control$c.p * t(runif(ncol(X)) * t(Vp_raw))
      Vg <- control$c.g * t(runif(ncol(X)) * t(Vg_raw))

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
      save_V_raw <- rbind(save_V_raw,
                      cbind(
                        data.frame(
                          "iter" = i-1,
                          "id" = 1:ncol(X)
                        ),
                        setNames(data.frame(t( Vw_raw )), paste0("Vw_raw_", 1:2)),
                        setNames(data.frame(t( Vp_raw )), paste0("Vp_raw_", 1:2)),
                        setNames(data.frame(t( Vg_raw )), paste0("Vg_raw_", 1:2))
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
    r$save_V <- save_V
    r$save_V_raw <- save_V_raw
  }) #%>%
    #bindEvent(input$pso_2d_start_pso)


  output$pso_2d_pso_plot_line <- renderPlotly({
    req(r$save_X)

    save_X <- r$save_X

    temp <- save_X %>% group_by(iter) %>% filter(fitness==min(fitness)) %>% ungroup() %>% select(iter, fitness) %>% .[!duplicated(.$iter),]
    temp_min <- temp[temp$iter==0,]$fitness
    for(i in 1:max(temp$iter)){
      if(temp_min < temp[temp$iter==i,]$fitness){
        temp[temp$iter==i,]$fitness <- temp_min
      }else{
        temp_min <- temp[temp$iter==i,]$fitness
      }
    }
    plot_ly(
      data = temp,
      x = ~iter,
      y = ~fitness,
      type = "scatter",
      mode = "lines",
      line = list(width=4)
    ) %>%
      layout(xaxis = list(tickformat = "digits")) %>%
      config(displayModeBar = FALSE)
  })



  output$pso_2d_pso_plot <- renderPlotly({
    req(r$save_X)
    req(r$save_V)
    req(r$save_V_raw)
    req(r$grid)

    save_X <- r$save_X
    save_V <- r$save_V
    save_V_raw <- r$save_V_raw
    grid <- r$grid

    # save(save_X, save_V, save_V_raw, grid, file="test.rdata")
    # browser()


    plot_ly(z = ~grid, type = "contour", y = rownames(grid), x = colnames(grid), showscale = F) %>%
      layout(
        yaxis = list(showticklabels=FALSE, tickvals=""),
        xaxis = list(showticklabels=FALSE, tickvals=""),
        margin=list(r=0, b=0, l=0, t=0, pad=0)
      ) %>%
      html_save(., zoom = 1, vheight = NULL, vwidth = NULL)

    image_file <- "img/p.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    save_X$z <- c(min(grid), rep(max(grid), nrow(save_X)-1))

    plot_ly() %>%
      add_trace(
        data=save_X,
        x=~axis_2,
        y=~axis_1,
        color = ~z,
        frame = ~iter,
        text = ~fitness,
        name = "",
        mode ='markers',
        type = 'scatter',
        showlegend=F,
        marker = list(color = 'red', size=14, opacity = 1, showscale = F)
      ) %>%
      animation_opts(redraw=F, easing="cubic-in-out", transition = 500, frame = 700) %>%
      layout(
        xaxis = list(title = "x1", gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
        yaxis = list(title = "x2", gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
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
            opacity = 0.8,
            layer = "below"
          )
        )
      ) %>%
      config(displayModeBar = FALSE)



  }) %>%
    bindEvent(input$pso_2d_render_anim)




  output$pso_2d_pso_plot2 <- renderPlotly({
    req(r$save_X)
    req(r$save_V)
    req(r$grid)

    save_X <- r$save_X
    save_V <- r$save_V
    grid <- r$grid

    #save(save_X, save_V, grid, file="test.rdata")
    #browser()

    save_X_dub <- rbind(
      save_X %>% mutate(iter=iter*2),
      save_X %>% mutate(iter=iter*2+1)
    ) %>%
      mutate(iter=iter/2) %>%
      arrange(iter)

    save_X_empty <- save_X %>% mutate(iter=(iter*2+1)/2) %>% select(iter, id, axis_1, axis_2)

    X_velocitys <- rbind(
      save_X %>% mutate(step=1),
      save_X %>% mutate(iter = iter-1) %>% mutate(step=2)
    ) %>% arrange(iter, id) %>% filter(iter >= min(save_X$iter), iter < max(save_X$iter))
    X_velocitys[X_velocitys$step==2,]$fitness <- X_velocitys[X_velocitys$step==1,]$fitness
    X_velocitys <- X_velocitys %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)

    Vw <- rbind(
      save_X,
      save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vw_1, axis_2 = axis_2 + Vw_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)

    Vp <- rbind(
      save_X,
      save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vp_1, axis_2 = axis_2 + Vp_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)

    Vg <- rbind(
      save_X,
      save_X %>% left_join(., save_V, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vg_1, axis_2 = axis_2 + Vg_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)


    Vw_raw <- rbind(
      save_X,
      save_X %>% left_join(., save_V_raw, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vw_raw_1, axis_2 = axis_2 + Vw_raw_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)

    Vp_raw <- rbind(
      save_X,
      save_X %>% left_join(., save_V_raw, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vp_raw_1, axis_2 = axis_2 + Vp_raw_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)

    Vg_raw <- rbind(
      save_X,
      save_X %>% left_join(., save_V_raw, by=c("iter", "id")) %>% mutate(axis_1 = axis_1 + Vg_raw_1, axis_2 = axis_2 + Vg_raw_2) %>% select(colnames(save_X))
    ) %>%
      filter(iter < max(iter)) %>%
      group_by(id) %>%
      bind_rows(., save_X_empty) %>%
      arrange(id, iter)




    plot_ly(z = ~grid, type = "contour", y = rownames(grid), x = colnames(grid), showscale = F) %>%
      layout(
        yaxis = list(showticklabels=FALSE, tickvals=""),
        xaxis = list(showticklabels=FALSE, tickvals=""),
        margin=list(r=0, b=0, l=0, t=0, pad=0)
      ) %>%
      html_save(., zoom = 1, vheight = NULL, vwidth = NULL)

    image_file <- "img/p.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    save_X_dub$z <- c(min(grid), rep(max(grid), nrow(save_X)-1))

    plot_ly() %>%
      add_trace(
        data=save_X_dub,
        x=~axis_2,
        y=~axis_1,
        color = ~z,
        frame = ~iter,
        text = ~fitness,
        name = "",
        mode ='markers',
        type = 'scatter',
        showlegend=F,
        marker = list(color = 'red', size=12, opacity = 1, showscale = F)
      ) %>%
    add_trace(
      data = X_velocitys,
      y=~axis_1,
      x=~axis_2,
      #z=~fitness+fit_offset,
      frame = ~iter,
      mode ='lines',
      type = 'scatter',
      showlegend=T,
      name = "V",
      line = list(color = 'red', width=3, showscale = F)
    ) %>%
      add_trace(
        data = Vw,
        y=~axis_1,
        x=~axis_2,
        #z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V w",
        line = list(color = 'blue', width=2, showscale = F)
      ) %>%
      add_trace(
        data = Vp,
        y=~axis_1,
        x=~axis_2,
        #z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V p",
        line = list(color = 'green', width=2, showscale = F)
      ) %>%
      add_trace(
        data = Vg,
        y=~axis_1,
        x=~axis_2,
        # z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V g",
        line = list(color = 'yellow', width=2, showscale = F)
      ) %>%
      add_trace(
        data = Vw_raw,
        y=~axis_1,
        x=~axis_2,
        #z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V w raw",
        line = list(color = 'blue', width=1, showscale = F, dash = 'dot')
      ) %>%
      add_trace(
        data = Vp_raw,
        y=~axis_1,
        x=~axis_2,
        #z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V p raw",
        line = list(color = 'green', width=1, showscale = F, dash = 'dot')
      ) %>%
      add_trace(
        data = Vg_raw,
        y=~axis_1,
        x=~axis_2,
        # z=~fitness+fit_offset,
        frame = ~iter,
        mode ='lines',
        type = 'scatter',
        showlegend=T,
        name = "V g raw",
        line = list(color = 'yellow', width=1, showscale = F, dash = 'dot')
      ) %>%
      animation_opts(redraw=T, easing="cubic-in-out", transition = 500, frame = 700, mode="next") %>%
      layout(
        xaxis = list(title = "x1", gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
        yaxis = list(title = "x2", gridcolor = '#0000', zerolinewidth = 0, zerolinecolor = '#0000'),
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
            opacity = 0.8,
            layer = "below"
          )
        )
      ) %>%
      config(displayModeBar = FALSE)



  }) %>%
    bindEvent(input$pso_2d_render_anim)
}
