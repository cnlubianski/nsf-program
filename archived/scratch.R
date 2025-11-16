# install.packages(c("sandwich","lmtest","quantreg","dplyr","purrr","tidyr","ggplot2"))
library(sandwich)
library(lmtest)
library(quantreg)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)

# Helper: cluster-robust vcov
vcov_robust <- function(fit, cluster = NULL, type = "HC1") {
  if (!is.null(cluster)) {
    sandwich::vcovCL(fit, cluster = cluster)
  } else {
    sandwich::vcovHC(fit, type = type)
  }
}

# Helper: covariance of regressor means (stochastic regressors)
cov_of_mean <- function(X) {
  S <- stats::cov(X) / nrow(X)
  if ("(Intercept)" %in% colnames(S)) {
    S["(Intercept)", ] <- 0
    S[, "(Intercept)"] <- 0
  }
  S
}

# Pad a vcov to full set of columns (zeros for dropped/collinear predictors)
pad_vcov <- function(Vsmall, names_small, names_full) {
  Vfull <- matrix(0, length(names_full), length(names_full),
                  dimnames = list(names_full, names_full))
  Vfull[names_small, names_small] <- Vsmall
  Vfull
}

# Core delta-method Oaxaca at the mean (A-referenced)
oaxaca_mean_delta <- function(formula, data, group_var, group_A, group_B,
                              cluster_var = NULL, robust = TRUE, alpha = 0.05) {
  data <- data |> drop_na()
  
  # Design
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  X <- model.matrix(formula, data = mf)
  cn <- colnames(X)
  
  # Group indicators
  gA <- data[[group_var]] == group_A
  gB <- data[[group_var]] == group_B
  
  XA <- X[gA, , drop = FALSE]
  XB <- X[gB, , drop = FALSE]
  yA <- y[gA]; yB <- y[gB]
  
  ## --- NEW: group means and CIs ---
  if (!is.null(cluster_var)) {
    clA <- cluster_var[gA]
    clB <- cluster_var[gB]
    mA <- tapply(yA, clA, mean)
    mB <- tapply(yB, clB, mean)
    var_ybarA <- var(mA) / length(mA)
    var_ybarB <- var(mB) / length(mB)
  } else {
    var_ybarA <- var(yA) / length(yA)
    var_ybarB <- var(yB) / length(yB)
  }
  ybarA <- mean(yA)
  ybarB <- mean(yB)
  ybar_diff <- ybarA - ybarB
  var_ybar_diff <- var_ybarA + var_ybarB
  
  z <- qnorm(1 - alpha/2)
  ci <- function(est, se) c(lower = est - z*se, upper = est + z*se)
  
  mean_rows <- data.frame(
    component = c("mean_y_A", "mean_y_B", "mean_diff"),
    estimate  = c(ybarA, ybarB, ybar_diff),
    se        = c(sqrt(var_ybarA), sqrt(var_ybarB), sqrt(var_ybar_diff)),
    ci_lower  = c(ci(ybarA, sqrt(var_ybarA))[1],
                  ci(ybarB, sqrt(var_ybarB))[1],
                  ci(ybar_diff, sqrt(var_ybar_diff))[1]),
    ci_upper  = c(ci(ybarA, sqrt(var_ybarA))[2],
                  ci(ybarB, sqrt(var_ybarB))[2],
                  ci(ybar_diff, sqrt(var_ybar_diff))[2])
  )
  ## --- END NEW ---
  
  # Fit group models
  fitA <- lm(yA ~ XA - 1)
  fitB <- lm(yB ~ XB - 1)
  
  VbA_small <- vcov(fitA)
  VbB_small <- vcov(fitB)
  
  VbA <- pad_vcov(VbA_small, colnames(XA), cn); VbA[is.na(VbA)] <- 0
  VbB <- pad_vcov(VbB_small, colnames(XB), cn); VbB[is.na(VbB)] <- 0
  
  # Coefficients aligned to full design
  bA <- rep(0, length(cn)); names(bA) <- cn
  bB <- rep(0, length(cn)); names(bB) <- cn
  coefA <- coef(fitA); coefA[is.na(coefA)] <- 0
  coefB <- coef(fitB); coefB[is.na(coefB)] <- 0
  bA[colnames(XA)] <- coefA
  bB[colnames(XB)] <- coefB
  
  # Means of X and their covariances
  xbarA <- colMeans(XA); xbarB <- colMeans(XB)
  dX <- xbarA - xbarB
  VxA <- cov_of_mean(XA); VxB <- cov_of_mean(XB)
  
  # Decomposition point estimates
  E_hat <- as.numeric(t(dX) %*% bA)
  U_hat <- as.numeric(t(xbarB) %*% (bA - bB))
  D_hat <- E_hat + U_hat
  
  # Variances (approximate; ignore trace term)
  VE <- as.numeric(t(dX) %*% VbA %*% dX) + as.numeric(t(bA) %*% (VxA + VxB) %*% bA)
  VU <- as.numeric(t(xbarB) %*% (VbA + VbB) %*% xbarB) +
    as.numeric(t(bA - bB) %*% VxB %*% (bA - bB))
  VD <- VE + VU
  
  seE <- sqrt(VE); seU <- sqrt(VU); seD <- sqrt(VD)
  
  decomp_rows <- data.frame(
    component = c("explained","unexplained","total"),
    estimate  = c(E_hat, U_hat, D_hat),
    se        = c(seE, seU, seD),
    ci_lower  = c(ci(E_hat,seE)[1], ci(U_hat,seU)[1], ci(D_hat,seD)[1]),
    ci_upper  = c(ci(E_hat,seE)[2], ci(U_hat,seU)[2], ci(D_hat,seD)[2])
  )
  
  list(
    estimates = rbind(mean_rows, decomp_rows),
    pieces = list(bA = bA, bB = bB, xbarA = xbarA, xbarB = xbarB,
                  VbA = VbA, VbB = VbB, VxA = VxA, VxB = VxB)
  )
}

rhs <- ~ year + quarter + labor_force + avg_quarter_emp +
  tot_quarterly_wages + avg_weekly_wage + employment_loc_quotient

out <- oaxaca_mean_delta(
  formula = formula(paste0("cycle ", paste(rhs, collapse = " "))),
  data       = regression_data |> drop_na(),
  group_var  = "metro",
  group_A    = 1,
  group_B    = 0,
  cluster_var = NULL, #regression_data |> drop_na() |> pull(county),
  robust     = TRUE,
  alpha      = 0.05
)



# Build rif_y, then call oaxaca_mean_delta on rif_y ~ X
rif_oaxaca_delta_profile <- function(taus, rhs_formula, data, group_var, group_A, group_B,
                                     cluster_var = NULL, robust = TRUE, alpha = 0.05) {
  # rhs_formula like: ~ year + quarter + labor_force + avg_quarter_emp + ...
  # We'll programmatically prepend the LHS.
  build_mm <- function(formula_y, data) {
    mf <- model.frame(formula_y, data)
    list(y = model.response(mf), X = model.matrix(formula_y, data))
  }
  
  map_dfr(taus, function(tau) {
    # Estimate quantile and density (unconditional)
    qhat <- quantreg::rq(cycle ~ 1, tau = tau, data = data)$coef
    dens <- density(data$cycle)
    f_q <- approx(dens$x, dens$y, xout = qhat)$y
    
    # Compute RIF outcome
    data$rif_y <- as.numeric(qhat + (tau - (data$cycle <= qhat)) / f_q)
    
    # Compose full formula rif_y ~ RHS
    full_formula <- formula(paste0("rif_y ", paste(rhs, collapse = " ")))
    
    res <- oaxaca_mean_delta(full_formula, data, group_var, group_A, group_B,
                             cluster_var = cluster_var, robust = robust, alpha = alpha)
    
    out <- res$estimates %>%
      mutate(tau = tau, component = factor(component,
                  levels = c("mean_y_A", "mean_y_B", "mean_diff",
                             "explained", "unexplained", "total")))
    out
  })
}

# 2) RIF–Oaxaca across quantiles (e.g., 10th, 25th, 50th, 75th, 90th)
taus <- c(0.05, 0.10, 0.2, 0.25, 0.40, 0.50, 0.60, 0.75, 0.8, 0.90, 0.95)

rif_prof <- rif_oaxaca_delta_profile(
  taus       = taus,
  rhs_formula = rhs,
  data       = regression_data |> drop_na(),
  group_var  = "metro",
  group_A    = 1,
  group_B    = 0,
  cluster_var = NULL, #regression_data |> drop_na() |> pull(county),
  robust     = TRUE,
  alpha      = 0.05
)
