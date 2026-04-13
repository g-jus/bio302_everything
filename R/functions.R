## Import data function.
herring_read <- function() {
  data <- read.csv("https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv", na.strings = ".")
  return(data)
}

## Clean herring data.
cleaning_herring <- function(data) {
  data |>
    filter(
      .data$weight > 0
    ) |>
    tidyr::drop_na("length", "age", "weight")
}

fit_vbgm <- function(data, age_cap = "age", length_cap = "length") {
  ## Find starting values for VBGM parameters.
  vb_start <- findGrowthStarts(
    as.formula(paste(length_cap, "~", age_cap)), 
    data = data
  )
  
  ## Define Von Bertalanffy Growth Model equation.
  vbgm <- as.formula(
    paste(length_cap, "~ Linf * (1 - exp(-K * (", age_cap, "- t0)))")
  )
  
  ## Fit VBGM model using NLS.
  fitted_vbgm <- nls(vbgm, data = data, start = vb_start)
  
  return(fitted_vbgm)
}

result_vbgm <- function(data) {
  ## Results from model.
  mod_tidy <- tidy(data, conf.int = TRUE)

  ## Table of VBGM results.
  vbgm_tbl <- mod_tidy |>
    gt() |>
    fmt_number(c(estimate, statistic, std.error, conf.low, conf.high),
               decimals = 2) |>
    cols_label(
      term = "Parameter",
      estimate = "Estimate",
      std.error = "Std. Error",
      statistic = "t-value",
      conf.low = "95% CI (Lower)",
      conf.high = "95% CI (Upper)"
    ) |>
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = -term)
    ) 
  
  return(vbgm_tbl)
}

plot_vbgm <- function(data, model, age_cap = "age", length_cap = "length") {
  # Extract parameter values
  coef_vals <- coef(model)
  Linf <- round(coef_vals["Linf"], 2)
  K    <- round(coef_vals["K"], 3)
  t0   <- round(coef_vals["t0"], 2)
  
  # Create annotation text
  vb_text <- paste0("L∞ = ", Linf, 
                    ", K = ", K, 
                    ", t₀ = ", t0)
  
  text_df <- data.frame(x = 8, y = 50, label = vb_text)
  
  # Create prediction grid
  age_seq <- seq(min(data[[age_cap]], na.rm = TRUE), 
                 max(data[[age_cap]], na.rm = TRUE), 
                 length.out = 100)
  pred_vbgm <- data.frame(age = age_seq)
  names(pred_vbgm) <- age
  pred_vbgm$length_pred <- predict(model, newdata = pred_vbgm)
  
  # Create plot
  vbgm_plot <- ggplot(data, aes(x = .data[[age_cap]], y = .data[[length_cap]])) +
    geom_jitter(
      size = 2,
      color = "black",
      height = 0,
      width = 0.1,
      alpha = 0.5
    ) +
    geom_line(data = pred_vbgm, 
              aes(x = .data[[age_cap]], 
                  y = length
              ),
              color = "steelblue", 
              linewidth = 2
    ) +
    geom_text(data = text_df,
              aes(x = x, 
                  y = y, 
                  label = label
              ),
              size = 5, 
              color = "steelblue", 
              hjust = 0.1
    ) +
    labs(x = "Age (years)", 
         y = "Total Length (cm)"
    ) +
    scale_x_continuous(limits = c(-0.2, 12), breaks = seq(0, 12, by = 2)) +
    scale_y_continuous(limits = c(0, 55), breaks = seq(0, 55, by = 10)) +
    theme_bw() +
    theme(axis.title = element_text(size = 14, face = "bold"),
          axis.text = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(margin = margin(t = 20)),
          axis.title.y = element_text(margin = margin(r = 20))
    )
  
  return(vbgm_plot)
}