


pool_data <- get_yf(
  tickers = c("IBM", "GOOG", "ADBE", "AAPL", "MSFT"),
  from = as.Date("2010-01-01"),
  to = Sys.Date()
)

last_prices <- pool_data$prices %>% last(1) %>% as.vector()

nav <- 5000

mu <- ret_to_geomeanret(pool_data$returns)
cov <- as.matrix(nearPD(cov_(pool_data$returns, mu))$mat)


standard_pso <- function(
    par,
    fn,
    lower,
    upper,
    control = list()
){

  # use default control values if not set
  control_ = list(
    s = 10, # swarm size
    c.p = 0.5, # inherit best
    c.g = 0.5, # global best
    maxiter = 200, # iterations
    w0 = 1.2, # starting inertia weight
    wN = 0, # ending inertia weight
    save_traces = F, # save more information
    save_fit = F
  )
  control <- c(control, control_[!names(control_) %in% names(control)])

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


  trace_data <- NULL
  fit_data <- NULL
  for(i in 1:control$maxiter){

    # move particles
    V <-
      (control$w0-(control$w0-control$wN)*i/control$maxiter) * V +
      control$c.p * t(runif(ncol(X)) * t(P-X)) +
      control$c.g * t(runif(ncol(X)) * t(p_g-X))
    X <- X + V

    # set velocity to zeros if not in valid space
    V[X > upper] <- 0#-V[X > upper]
    V[X < lower] <- 0#-V[X < lower]

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

    if(control$save_traces){
      trace_data <- rbind(trace_data, data.frame("iter"=i, t(X)))
    }
    if(control$save_fit){
      fit_data <- rbind(fit_data, data.frame("iter"=i, "mean"=mean(P_fit), "best"=p_g_fit))
    }
  }

  res <- list(
    "solution" = p_g,
    "fitness" = p_g_fit
  )
  if(control$save_traces){
    res$trace_data <- trace_data
  }
  if(control$save_fit){
    res$fit_data <- fit_data
  }
  return(res)
}




res_SPSO <- standard_pso(
  par = rep(1/length(mu), length(mu)),
  fn = function(x){
    x <- round(x*nav/last_prices)*last_prices/nav
    port_sd <- sqrt(t(x) %*% cov %*% x)
    port_mu <- (t(mu) %*% x)
    if(port_sd>0){
      -port_mu/port_sd + 100*(max(0.01, abs(sum(x)-0.99))-0.01)
    }else{
      10^6
    }
  },
  lower = 0,
  upper = 0.4,
  control = list(
    s = 50, # swarm size
    c.p = 0.5, # inherit best
    c.g = 0.5, # global best
    maxiter = 400, # iterations
    w0 = 1.2, # starting inertia weight
    wN = 0, # ending inertia weight
    save_traces = F, # save more information
    save_fit = T
  )
)
res_SPSO$solution <- round(res_SPSO$solution*nav/last_prices)*last_prices/nav
names(res_SPSO$solution) <- names(mu)
res_SPSO$shares <- res_SPSO$solution*nav/last_prices
res_SPSO$shares_budget <- res_SPSO$solution*nav

plotly_line_chart_xts(ret_to_cumret(cbind.xts(pool_data$returns, "Portfolio"=pool_data$returns %*% res_SPSO$solution)))
