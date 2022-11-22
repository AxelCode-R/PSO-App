pso_2d_server <- function(input, output, session){
  r <- reactiveValues("fn"=NULL, "range"=NULL, "resolution"=NULL, "max_iter"=NULL, "grid"=NULL, "grid_plot"=NULL, "save_X"=NULL)

  observe({
    updateProgressBar(session = session, id = "pso_2d_settings1", value = 0)
    updateProgressBar(session = session, id = "pso_2d_settings2", value = 0)
    updateProgressBar(session = session, id = "pso_2d_settings3", value = 0)
    updateProgressBar(session = session, id = "pso_2d_settings4", value = 0)
  }) %>%
    bindEvent(input$pso_2d_fun, input$pso_2d_range_x1, input$pso_2d_range_x2, input$pso_2d_resolution)



  observeEvent(input$pso_2d_fun_drop_wave,{
    updateTextAreaInput(session, "pso_2d_fun_fit", value="-(1+cos(12*sqrt(x1^2+x2^2)))/(0.5*(x1^2+x2^2)+2)")
    updateTextAreaInput(session, "pso_2d_fun_const", value="0")
    updateNumericRangeInput(session, "pso_2d_range_x1", value=c(-5.12, 5.12))
    updateNumericRangeInput(session, "pso_2d_range_x2", value=c(-5.12, 5.12))
  })

  observeEvent(input$pso_2d_fun_ackley,{
    updateTextAreaInput(session, "pso_2d_fun_fit", value="-20*exp(-0.2*sqrt(0.5*(x1^2+x2^2)))-
                        exp(0.5*(cos(2*pi*x1)+cos(2*pi*x2)))+
                        exp(1)+20")
    updateTextAreaInput(session, "pso_2d_fun_const", value="0")
    updateNumericRangeInput(session, "pso_2d_range_x1", value=c(-32.768, 32.768))
    updateNumericRangeInput(session, "pso_2d_range_x2", value=c(-32.768, 32.768))
  })

  observeEvent(input$pso_2d_fun_rosenbrock,{
    updateTextAreaInput(session, "pso_2d_fun_fit",
                        value="(1-x1)^2+100*(x2-x1^2)^2")
    updateTextAreaInput(session, "pso_2d_fun_const", value="if(((x1-1)^3-x2+1)>0){((x1-1)^3-x2+1)*100}else{0} +
                        if((x1+x2-2)>0){(x1+x2-2)*100}else{0}")
    updateNumericRangeInput(session, "pso_2d_range_x1", value=c(-1.5, 1.5))
    updateNumericRangeInput(session, "pso_2d_range_x2", value=c(-0.5, 2.5))
  })


  observeEvent(input$pso_2d_fun_gole,{
    updateTextAreaInput(session, "pso_2d_fun_fit",
                        value="4*x1^2-2.1*x1^4+1/3*x1^6+x1*x2-4*x2^2+4*x2^4")
    updateTextAreaInput(session, "pso_2d_fun_const", value="if((-sin(4*pi*x1)+2*sin(2*pi*x2)^2)>1.5){(-sin(4*pi*x1)+2*sin(2*pi*x2)^2)}else{0}")
    updateNumericRangeInput(session, "pso_2d_range_x1", value=c(-1, 0.75))
    updateNumericRangeInput(session, "pso_2d_range_x2", value=c(-1, 1))
  })

  observeEvent(input$pso_2d_fun_no_convergence,{
    updateTextAreaInput(session, "pso_2d_fun_fit",
                        value="(x1^2+x2^2)/(x1^4+x2^4+1)")
    updateTextAreaInput(session, "pso_2d_fun_const", value="0")
    updateNumericRangeInput(session, "pso_2d_range_x1", value=c(-10, 10))
    updateNumericRangeInput(session, "pso_2d_range_x2", value=c(-10, 10))
  })

  observeEvent(input$pso_2d_save_settings,{
    req(input$pso_2d_fun_fit)
    req(input$pso_2d_fun_const)


    updateProgressBar(session = session, id = "pso_2d_settings1", value = 20)
    disable("pso_2d_grid_preview")
    worked1 <- tryCatch(
      {
        eval(parse(text = paste('fn_fit <- function(x) {return(' , gsub("x2", "x[2]",gsub("x1", "x[1]", input$pso_2d_fun_fit)) , ')}', sep='')))
        TRUE
      },
      error = function(e) showNotification(session, "some error accured by reading the function!")
    )
    worked2 <- tryCatch(
      {
        eval(parse(text = paste('fn_const <- function(x) {return(' , gsub("x2", "x[2]",gsub("x1", "x[1]", input$pso_2d_fun_const)) , ')}', sep='')))
        TRUE
      },
      error = function(e) showNotification(session, "some error accured by reading the function!")
    )
    worked3 <- tryCatch(
      {
        eval(parse(text = paste('fn <- function(x) {return(' , gsub("x2", "x[2]",gsub("x1", "x[1]", input$pso_2d_fun_fit)), "+1000*", gsub("x2", "x[2]",gsub("x1", "x[1]", input$pso_2d_fun_const)), ')}', sep='')))
        TRUE
      },
      error = function(e) showNotification(session, "some error accured by reading the function!")
    )
    if(worked1 && worked2 && worked3){
      r$fn_fit <- fn_fit
      r$fn_const <- fn_const
      r$fn <- fn
      r$range <- data.frame(
        lower = c(input$pso_2d_range_x1[1], input$pso_2d_range_x2[1]),
        upper = c(input$pso_2d_range_x1[2], input$pso_2d_range_x2[2])
      )
      r$resolution <- input$pso_2d_resolution


      updateProgressBar(session = session, id = "pso_2d_settings1", value = 40)

      grid <- setNames(expand.grid(seq(r$range$lower[1], r$range$upper[1], length.out = r$resolution), seq(r$range$lower[2], r$range$upper[2], length.out = r$resolution)), c("x1", "x2"))
      updateProgressBar(session = session, id = "pso_2d_settings1", value = 60)
      grid$fitness <- apply(grid, 1, function(x){if(r$fn_const(x)==0){r$fn_fit(x)}else{NA}})
      updateProgressBar(session = session, id = "pso_2d_settings1", value = 80)
      r$grid <- grid %>% spread(., key = x1, value = fitness) %>% column_to_rownames("x2") %>% as.matrix()
    }

    updateProgressBar(session = session, id = "pso_2d_settings1", value = 100)
    enable("pso_2d_grid_preview")
  })



  output$pso_2d_grid_plot <- renderPlotly({
    req(r$grid)


    #dim_z <- max(r$grid)-min(r$grid)
    #grid <- r$grid

    # plot_ly() %>%
    #   add_surface(
    #     data = grid,
    #     type = 'surface',
    #     # contours = list(
    #     #   z = list(show = TRUE, start = min(r$grid)-0.03*dim_z, end = round(max(r$grid))+0.03*dim_z, size = round(dim_z/10), color="grey", opacity = 0.2)
    #     # ),
    #     showscale = FALSE,
    #     opacity=0.7,
    #     x = ~x1,
    #     y = ~x2,
    #     z = ~fitness,
    #     inherit = F
    #   ) %>%
    #   layout(scene = list(
    #     xaxis=list(title="x1"),
    #     yaxis=list(title="x2")
    #   )) %>%
    #   config(displayModeBar = FALSE)


    # x <- as.numeric(rownames(r$grid))
    # y <- as.numeric(colnames(r$grid))
    # z <- as.matrix(r$grid) #if(r$reverse){t(r$grid)}else{r$grid}
    # colnames(z) <- NULL
    # rownames(z) <- NULL
    # plot_ly() %>%
    #   add_surface(
    #     type = 'surface',
    #     # contours = list(
    #     #   z = list(show = TRUE, start = min(r$grid)-0.03*dim_z, end = round(max(r$grid))+0.03*dim_z, size = round(dim_z/10), color="grey", opacity = 0.2)
    #     # ),
    #     showscale = FALSE,
    #     opacity=0.7,
    #     x = ~x,
    #     y = ~y,
    #     z = ~z,
    #     inherit = T
    #   ) %>%
    #   layout(scene = list(
    #     xaxis=list(title="x1", range=c(min(as.numeric(rownames(r$grid))), max(as.numeric(rownames(r$grid)))), autorange="reversed" ),
    #     yaxis=list(title="x2", range=c(min(as.numeric(colnames(r$grid))), max(as.numeric(colnames(r$grid)))), autorange="reversed" )
    #   )) %>%
    #   config(displayModeBar = FALSE)



    # x <- as.numeric(rownames(r$grid))
    # y <- as.numeric(colnames(r$grid))
    # z <- as.matrix(r$grid) #if(r$reverse){t(r$grid)}else{r$grid}
    # colnames(z) <- NULL
    # rownames(z) <- NULL
    plot_ly() %>%
      add_surface(
        type = 'surface',
        # contours = list(
        #   z = list(show = T, usecolormap=T, project=list(z=TRUE))
        # ),
        showscale = FALSE,
        opacity=0.7,
        x = as.numeric(colnames(r$grid)),
        y = as.numeric(rownames(r$grid)),
        z = r$grid,
        inherit = T
      ) %>%
      layout(scene = list(
        xaxis=list(title="x1"),
        yaxis=list(title="x2")
      )) %>%
      config(displayModeBar = FALSE)
  }) %>%
    bindEvent(input$pso_2d_grid_preview)


  observeEvent(input$pso_2d_grid_preview, {
    showModal(modalDialog(
      title = NULL,
      #awesomeCheckbox("pso_2d_grid_plot_reverse", "Fix Axis Reversed Bug", value = FALSE),
      addSpinner(plotlyOutput("pso_2d_grid_plot")),
      easyClose = T,
      size = "xl",
      footer = NULL
    ))
  })

  # observeEvent(input$pso_2d_grid_plot_reverse,{
  #   isolate({
  #     r$reverse <- input$pso_2d_grid_plot_reverse
  #   })
  # })


  observe({
    updateProgressBar(session = session, id = "pso_2d_settings2", value = 0)
    updateProgressBar(session = session, id = "pso_2d_settings3", value = 0)
    updateProgressBar(session = session, id = "pso_2d_settings4", value = 0)
  }) %>%
    bindEvent(input$pso_2d_iter, input$pso_2d_s, input$pso_2d_inertia_weight_w0, input$pso_2d_inertia_weight_wN,
              input$pso_2d_coef_p, input$pso_2d_coef_g, input$pso_2d_variant, input$pso_2d_k)



  observeEvent(input$pso_2d_variant,{
    if(input$pso_2d_variant == "global best (standard)"){
      hide("pso_2d_k")
      show("pso_2d_inertia_weight_w0")
      show("pso_2d_inertia_weight_wN")
      show("pso_2d_coef_p")
      show("pso_2d_coef_g")
    }
    if(input$pso_2d_variant == "local"){
      show("pso_2d_k")
      show("pso_2d_inertia_weight_w0")
      show("pso_2d_inertia_weight_wN")
      show("pso_2d_coef_p")
      show("pso_2d_coef_g")
    }
    if(input$pso_2d_variant == "self-adaptive velocity"){
      hide("pso_2d_k")
      hide("pso_2d_inertia_weight_w0")
      hide("pso_2d_inertia_weight_wN")
      hide("pso_2d_coef_p")
      hide("pso_2d_coef_g")
    }
  })


  observeEvent(input$pso_2d_start_pso, {
    req(r$grid)


    if(input$pso_2d_variant == "global best (standard)"){
      isolate({
        fn <- r$fn

        lower <- r$range$lower
        upper <- r$range$upper

        par <- rep(NA, 2)
        control <- list(
          s = input$pso_2d_s, # swarm size
          c.p = input$pso_2d_coef_p, # inherit best
          c.g = input$pso_2d_coef_g, # global best
          maxiter = input$pso_2d_iter, # iterations
          w0 = input$pso_2d_inertia_weight_w0, # starting inertia weight
          wN = input$pso_2d_inertia_weight_wN # ending inertia weight
        )
      })

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 20)
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


      updateProgressBar(session = session, id = "pso_2d_settings2", value = 40)

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

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 80)

      #save_X$fitness <- apply(save_X %>% select(axis_1, axis_2), 1, function(x){if(r$fn_const(x)==0){r$fn_fit(x)}else{NA}})
      r$save_X <- save_X
      r$save_V <- save_V
      r$save_V_raw <- save_V_raw

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 100)
    }



    if(input$pso_2d_variant == "local"){
      isolate({
        fn <- r$fn

        lower <- r$range$lower
        upper <- r$range$upper

        par <- rep(NA, 2)
        control <- list(
          s = input$pso_2d_s, # swarm size
          c.p = input$pso_2d_coef_p, # inherit best
          c.g = input$pso_2d_coef_g, # global best
          maxiter = input$pso_2d_iter, # iterations
          w0 = input$pso_2d_inertia_weight_w0, # starting inertia weight
          wN = input$pso_2d_inertia_weight_wN, # ending inertia weight
          k=2
        )
      })

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 20)
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

      neighbors <- sapply(1:control$s, function(x){ ((x+round(-control$k/2)):(x+round(control$k/2))-1) %% control$s + 1 }) %>% unique()
      updateProgressBar(session = session, id = "pso_2d_settings2", value = 40)



      save_X <- data.frame("iter"=0, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X))))
      save_V <- NULL
      save_V_raw <- NULL
      for(i in 1:control$maxiter){
        P_g <- matrix(1, nrow=length(par), ncol=control$s)
        for(z in 1:ncol(P_g)){
          P_g[, z] <- P[, neighbors[which.min(P_fit[neighbors[, z]]), z]]
        }

        Vw_raw <- V
        Vp_raw <- (P-X)
        Vg_raw <- (P_g-X)
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

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 80)

      #save_X$fitness <- apply(save_X %>% select(axis_1, axis_2), 1, function(x){if(r$fn_const(x)==0){r$fn_fit(x)}else{NA}})
      r$save_X <- save_X
      r$save_V <- save_V
      r$save_V_raw <- save_V_raw

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 100)
    }



    if(input$pso_2d_variant == "self-adaptive velocity"){

      isolate({
        fn <- r$fn
        fn_const <- r$fn_const

        lower <- r$range$lower
        upper <- r$range$upper

        par <- rep(NA, 2)
        control <- list(
          s = input$pso_2d_s, # swarm size
          maxiter = input$pso_2d_iter, # iterations
          Sp = 0.8,
          Cp = 0.7
        )
      })


      updateProgressBar(session = session, id = "pso_2d_settings2", value = 20)

      # init data-structure
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

      ac_params <- data.frame("w"=rep(0.5, control$s), "c.p"=rep(2, control$s), "c.g"=rep(2, control$s))

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 40)

      lower_mat <- matrix(rep(lower, ncol(X)), ncol=ncol(X))
      upper_mat <- matrix(rep(upper, ncol(X)), ncol=ncol(X))

      #browser()

      save_X <- data.frame("iter"=0, "id"= 1:ncol(X), "fitness"=X_fit, setNames(data.frame(t(X)), paste0("axis_",1:nrow(X))))
      save_V <- NULL
      save_V_raw <- NULL
      for(i in 1:control$maxiter){

        mu1 <- 0.1*(1-(i/control$maxiter)^2)+0.3
        sig1 <- 0.1
        mu2 <- 0.4*(1-(i/control$maxiter)^2)+0.2
        sig2 <- 0.4

        Vw_raw <- V
        Vp_raw <- (P-X)
        Vg_raw <- (p_g-X)
        Vw <- V
        Vp <- V
        Vg <- V

        for(p in 1:control$s){
          if(runif(1) > control$Sp){
            Vw[,p] <- ac_params[p,]$w * V[,p]
            Vp[,p] <- ac_params[p,]$c.p * runif(1) * (P[,p]-X[,p])
            Vg[,p] <- ac_params[p,]$c.g * runif(1) * (p_g-X[,p])
          }else{
            if(runif(1) < 0.5){
              Vw[,p] <- ac_params[p,]$w * V[,p]
              Vp[,p] <- ac_params[p,]$c.p * rcauchy(1, mu1, sig1) * (P[,p]-X[,p])
              Vg[,p] <- ac_params[p,]$c.g * rcauchy(1, mu1, sig1) * (p_g-X[,p])
            }else{
              Vw[,p] <- ac_params[p,]$w * V[,p]
              Vp[,p] <- ac_params[p,]$c.p * rcauchy(1, mu2, sig2) * (P[,p]-X[,p])
              Vg[,p] <- ac_params[p,]$c.g * rcauchy(1, mu2, sig2) * (p_g-X[,p])
            }
          }
        }
        V <- Vw + Vp + Vg
        X <- X + V

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



        upper_breaks <- X > upper_mat
        ub_ind <- which(upper_breaks==T, arr.ind = T)
        if(nrow(ub_ind)>0){
          for(k in 1:nrow(ub_ind)){
            if(runif(1) > control$Cp){
              X[ub_ind[k,1],ub_ind[k,2]] <- runif(1, lower_mat[ub_ind[k,1], ub_ind[k,2]], upper_mat[ub_ind[k,1], ub_ind[k,2]])
            }else{
              X[ub_ind[k,1],ub_ind[k,2]] <- upper_mat[ub_ind[k,1], ub_ind[k,2]]
            }
          }
        }

        lower_breaks <- X < lower_mat
        lb_ind <- which(lower_breaks==T, arr.ind = T)
        if(nrow(lb_ind)>0){
          for(k in 1:nrow(lb_ind)){
            if(runif(1) > control$Cp){
              X[lb_ind[k,1],lb_ind[k,2]] <- runif(1, lower_mat[lb_ind[k,1], lb_ind[k,2]], upper_mat[lb_ind[k,1], lb_ind[k,2]])
            }else{
              X[lb_ind[k,1],lb_ind[k,2]] <- lower_mat[lb_ind[k,1], lb_ind[k,2]]
            }
          }
        }


        # evaluate objective function
        X_fit <- apply(X, 2, fn)
        X_const <- apply(X, 2, fn_const)
        X_fit_temp <- X_fit

        if(!all(X_const!=0)){
          max_fit <- max(X_fit_temp[X_const==0])
          X_fit_temp[X_const!=0] <- max_fit
          WG <- abs(X_fit_temp-max_fit)/sum(abs(X_fit_temp-max_fit))
        }else{
          max_fit <- max(X_fit_temp)
          WG <- abs(X_fit_temp-max_fit)/sum(abs(X_fit_temp-max_fit))
        }
        ac_params$w <- rcauchy(control$s, sum(WG*ac_params$w), 0.2)
        ac_params$c.p <- rcauchy(control$s, sum(WG*ac_params$c.p), 0.3)
        ac_params$c.g <- rcauchy(control$s, sum(WG*ac_params$c.g), 0.3)

        ac_params$w[ac_params$w > 1] <- runif(1)
        ac_params$w[ac_params$w < 0] <- runif(1)/10

        ac_params$c.p[ac_params$c.p > 4] <- runif(1)*4
        ac_params$c.p[ac_params$c.p < 0] <- runif(1)

        ac_params$c.g[ac_params$c.g > 4] <- runif(1)*4
        ac_params$c.g[ac_params$c.g < 0] <- runif(1)


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

      updateProgressBar(session = session, id = "pso_2d_settings2", value = 80)

      r$save_X <- save_X
      r$save_V <- save_V
      r$save_V_raw <- save_V_raw
    }
  })






  output$pso_2d_pso_plot_line <- renderPlotly({
    req(r$save_X)

    save_X <- r$save_X



    temp <- save_X %>% group_by(iter) %>% filter(fitness==min(fitness, na.rm = T)) %>% ungroup() %>% select(iter, fitness) %>% .[!duplicated(.$iter),]
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

    updateProgressBar(session = session, id = "pso_2d_settings3", value = 20)

    save_X <- r$save_X
    save_V <- r$save_V
    save_V_raw <- r$save_V_raw
    grid <- r$grid

    # save(save_X, save_V, save_V_raw, grid, file="test.rdata")



    # plot_ly(z = ~grid, type = "contour", y = as.numeric(rownames(grid)), x = as.numeric(colnames(grid)), showscale = F) %>%
    #   layout(
    #     yaxis = list(showticklabels=FALSE, tickvals="", range=c(min(as.numeric(rownames(grid))), max(as.numeric(rownames(grid)))) ),
    #     xaxis = list(showticklabels=FALSE, tickvals="", range=c(min(as.numeric(colnames(grid))), max(as.numeric(colnames(grid)))) ),
    #     margin=list(r=0, b=0, l=0, t=0, pad=0)
    #   ) %>%
    #   html_save(., zoom = 1, vheight = NULL, vwidth = NULL)

    plot_ly(z = grid, type = "contour", y = as.numeric(rownames(grid)), x = as.numeric(colnames(grid)), showscale = F) %>%
      layout(
        yaxis = list(showticklabels=FALSE, tickvals="", zeroline = FALSE),
        xaxis = list(showticklabels=FALSE, tickvals="", zeroline = FALSE),
        margin=list(r=0, b=0, l=0, t=0, pad=0)
      ) %>%
      config(displayModeBar = FALSE) %>%
      html_save(., zoom = 1, vheight = NULL, vwidth = NULL, selector = ".cartesianlayer")

    image_file <- "img/p.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    save_X$z <- c(min(grid, na.rm = T), rep(max(grid, na.rm = T), nrow(save_X)-1))

    updateProgressBar(session = session, id = "pso_2d_settings3", value = 60)

    fig <- plot_ly() %>%
      add_trace(
        data=save_X,
        x=~axis_1,
        y=~axis_2,
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
        xaxis = list(title = "x1", gridcolor = '#0000', zerolinewidth = 0, zeroline = FALSE, zerolinecolor = '#0000', range=c(min(as.numeric(colnames(grid))), max(as.numeric(colnames(grid))))),
        yaxis = list(title = "x2", gridcolor = '#0000', zerolinewidth = 0, zeroline = FALSE, zerolinecolor = '#0000', range=c(min(as.numeric(rownames(grid))), max(as.numeric(rownames(grid))))),
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

    updateProgressBar(session = session, id = "pso_2d_settings3", value = 100)
    fig

  }) %>%
    bindEvent(input$pso_2d_render_anim)




  output$pso_2d_pso_plot_details <- renderPlotly({
    req(r$save_X)
    req(r$save_V)
    req(r$save_V_raw)
    req(r$grid)

    updateProgressBar(session = session, id = "pso_2d_settings4", value = 10)

    save_X <- r$save_X
    save_V <- r$save_V
    save_V_raw <- r$save_V_raw
    grid <- r$grid

    #save(save_X, save_V, grid, file="test.rdata")


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


    updateProgressBar(session = session, id = "pso_2d_settings4", value = 40)


    plot_ly(z = ~grid, type = "contour", y = as.numeric(rownames(grid)), x = as.numeric(colnames(grid)), showscale = F) %>%
      layout(
        yaxis = list(showticklabels=FALSE, tickvals="", zeroline = FALSE, range=c(min(as.numeric(rownames(grid))), max(as.numeric(rownames(grid))))),
        xaxis = list(showticklabels=FALSE, tickvals="", zeroline = FALSE, range=c(min(as.numeric(colnames(grid))), max(as.numeric(colnames(grid))))),
        margin=list(r=0, b=0, l=0, t=0, pad=0)
      ) %>%
      config(displayModeBar = FALSE) %>%
      html_save(., zoom = 1, vheight = NULL, vwidth = NULL, selector = ".cartesianlayer")

    image_file <- "img/p.png"
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

    save_X_dub$z <- c(min(grid, na.rm = T), rep(max(grid, na.rm = T), nrow(save_X)-1))

    updateProgressBar(session = session, id = "pso_2d_settings4", value = 60)

    fig <- plot_ly() %>%
      add_trace(
        data=save_X_dub,
        x=~axis_1,
        y=~axis_2,
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
      x=~axis_1,
      y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        x=~axis_1,
        y=~axis_2,
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
        xaxis = list(title = "x1", gridcolor = '#0000', zerolinewidth = 0, zeroline = FALSE, zerolinecolor = '#0000', range=c(min(as.numeric(colnames(grid))), max(as.numeric(colnames(grid))))),
        yaxis = list(title = "x2", gridcolor = '#0000', zerolinewidth = 0, zeroline = FALSE, zerolinecolor = '#0000', range=c(min(as.numeric(rownames(grid))), max(as.numeric(rownames(grid))))),
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

    updateProgressBar(session = session, id = "pso_2d_settings4", value = 100)

    fig
  }) %>%
    bindEvent(input$pso_2d_render_anim_details)
}
