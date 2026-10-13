# ETAP 4: specyfikacja VAR - liczba opóźnień, kolejność (Cholesky), estymacja
library(vars)       # przed tidyverse (maskowanie select)
library(tidyverse)
source("R/var_pomocnicze.R")

dane <- readRDS("data/processed/dane_var.rds")
y <- dane |> dplyr::select(all_of(KOLEJNOSC))
cat("Zmienne (w kolejności Choleskiego):", paste(KOLEJNOSC, collapse = " -> "), "\n")
cat("Próba:", format(min(dane$date)), "-", format(max(dane$date)), ", T =", nrow(y), "\n\n")

# ---- 1. Dobór opóźnień -------------------------------------------------------
vs <- VARselect(y, lag.max = 12, type = "const", season = 12)
kryt <- as_tibble(t(vs$criteria), rownames = "p") |> mutate(p = as.integer(p))
cat("Kryteria informacyjne - wybrane p:\n"); print(vs$selection)
print(kryt |> filter(p <= 8), n = 8)
write_csv(kryt, "output/tables/e4_varselect.csv")

# ---- 2. Diagnostyka reszt dla kandydatów p = 1..6 (podstawa wyboru) ----------
kand <- map(1:6, \(p) {
  m <- estymuj_var(dane, p = p)
  tibble(p = p,
         portmanteau_p = serial.test(m, lags.pt = 24, type = "PT.asymptotic")$serial$p.value,
         bg12_p        = serial.test(m, lags.bg = 12, type = "BG")$serial$p.value,
         bg6_p         = serial.test(m, lags.bg = 6, type = "BG")$serial$p.value,
         max_pierwiastek = max(roots(m)),
         liczba_parametrow_na_rownanie = length(coef(m$varresult[[1]])))
}) |> list_rbind()
cat("\nDiagnostyka kandydatów (p-value; autokorelacja: chcemy > 0.05):\n"); print(kand, width = Inf)
write_csv(kand, "output/tables/e4_kandydaci_p.csv")

# ---- 3. Wybór: p = 3 --------------------------------------------------------
# AIC i FPE wskazują 3; HQ/SC wskazują 1, ale przy p = 1 reszty są autokorelowane (Portmanteau p ~ 0.01).
# p = 3 to najmniejsze p z AIC-optimum i przechodzącym Portmanteau. p = 6 sprawdzamy w etapie 7.
P_BAZOWE <- 3
model <- estymuj_var(dane, p = P_BAZOWE)
cat("\n=== Model bazowy: VAR(", P_BAZOWE, ") ===\n", sep = "")
print(summary(model, equation = "d_stopa"))   # pełne podsumowania zapisujemy do pliku
sink("output/tables/e4_var_podsumowanie.txt"); print(summary(model)); sink()
saveRDS(model, "data/processed/model_var_bazowy.rds")

# Współczynniki równania inflacji: czy opóźnienia stopy są istotne?
cat("\nRównanie inflacji m/m:\n")
print(round(summary(model)$varresult$inflacja_mm$coefficients[1:(3 * P_BAZOWE + 1), ], 3))

# ---- 4. Macierz kowariancji reszt i korelacje - uzasadnienie kolejności ------
S <- summary(model)$covres
cat("\nKorelacje reszt (jeśli duże, kolejność ma znaczenie):\n"); print(round(cov2cor(S), 2))
cat("\nOdch. standardowe reszt:", paste(colnames(S), round(sqrt(diag(S)), 3), collapse = " | "), "\n")
