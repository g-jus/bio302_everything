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

fit_vbgm <- function(data, length, age) {
  ## Find starting values for VBGM parameters.
  vb_start <- findGrowthStarts(
    as.formula(paste(length, "~", age)), 
    data = data
  )
  
  ## Define Von Bertalanffy Growth Model equation.
  vbgm <- as.formula(
    paste(length, "~ Linf * (1 - exp(-K * (", age, "- t0)))")
  )
  
  ## Fit VBGM model using NLS.
  fit <- nls(vbgm, data = data, start = vb_start)
  
  return(fit)
}

result_vbgm <- function(data) {
  ## Results from model.
  mod_tidy <- tidy(data, conf.int = TRUE)

  ## Table of VBGM results.
  VBGM_tbl <- mod_tidy |>
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
  
  return(VBGM_tbl)
}