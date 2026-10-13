# Testy pierwiastka jednostkowego: ADF (urca) i KPSS (tseries)
# ADF:  H0 = jest pierwiastek jednostkowy (szereg NIESTACJONARNY)
# KPSS: H0 = szereg jest stacjonarny (odwrotnie niż w ADF!)
# Wymaga: tidyverse, urca, tseries

testuj_szereg <- function(x, nazwa, typ = c("drift", "trend"), lag_max = 12) {
  typ <- match.arg(typ)
  x <- x[!is.na(x)]
  # lagi różnic dobierane kryterium AIC, maksymalnie lag_max
  adf <- urca::ur.df(x, type = typ, lags = lag_max, selectlags = "AIC")
  tau  <- unname(adf@teststat[1, 1])                  # statystyka dla gamma (test na pierwiastek)
  kryt <- unname(adf@cval[1, "5pct"])
  lagi <- sum(grepl("z.diff.lag", rownames(adf@testreg$coefficients)))
  kp <- suppressWarnings(tseries::kpss.test(x, null = if (typ == "trend") "Trend" else "Level"))
  tibble(
    szereg = nazwa, specyfikacja = typ,
    adf_tau = round(tau, 2), adf_kryt5 = round(kryt, 2),
    adf_stacjonarny = tau < kryt,                      # TRUE = odrzucamy pierwiastek jednostkowy
    kpss_stat = round(unname(kp$statistic), 3), kpss_p = round(kp$p.value, 3),
    kpss_stacjonarny = kp$p.value >= 0.05,             # TRUE = nie odrzucamy stacjonarności
    lagi_adf = lagi
  )
}
