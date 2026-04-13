## Import data function.
herring_read <- function() {
  data <- utils::read.csv("https://ftp.nmdc.no/nmdc/IMR/Herring/HerringData.csv", na.strings = ".")
  return(data)
}

## Clean herring data.
cleaning_herring <- function(data) {
  data |>
    dplyr::filter(
      .data$weight > 0
    ) |>
    tidyr::drop_na("length", "age", "weight")
}