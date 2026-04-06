

if (FALSE) {
  
  d = data(package = 'survival') |>
    packageIQR::dataFrom()
  
  d |>
    sapply(FUN = nrow) |>
    sort() |>
    head(n = 30L)
  
  imotor = survival::imotor |>
    within.data.frame(expr = {
      breakdown = survival::Surv(time = time, event = status)
      time = status = NULL
    })
  
  unique_or_identity = \(x) {
    u = unique(x)
    if (length(u) == 1L) return(u)
    return(x)
  }
  
  imotor |> 
    aggregate( # ?stats:::aggregate.formula
      x = . ~ temp, 
      FUN = unique_or_identity, 
      simplify = FALSE
    ) # bad!!

  imotor |>
    aggregate.data.frame(
      by = list(imotor$temp),
      FUN = unique_or_identity, 
      simplify = FALSE
    )
  
  # how did my
  # groupedHyperframe:::aggregate2hyper.data.frame
  # solve this problem??
  
}
