# ETAP 3: stacjonarność - ADF/KPSS, decyzja o różnicowaniu, demonstracja regresji pozornej
library(tidyverse)
library(patchwork)
source("R/testy_stacjonarnosci.R")
source("R/motyw.R")
set.seed(2026)

d <- readRDS("data/processed/dane_miesieczne.rds")

# ---- 1. Szeregi w POZIOMACH (logarytmy dla kursu i produkcji, x100) ----------
poziomy <- d |> transmute(date, stopa = stopa_kon, inflacja, inflacja_mm,
                          leurpln = 100 * log(eurpln), lprodukcja = 100 * log(produkcja))

# ---- 2. Testy na poziomach i na pierwszych różnicach -------------------------
zmienne <- c("stopa", "inflacja", "leurpln", "lprodukcja")
wyniki <- map(zmienne, \(z) {
  bind_rows(
    testuj_szereg(poziomy[[z]], z, "drift"),
    testuj_szereg(poziomy[[z]], z, "trend"),
    testuj_szereg(diff(poziomy[[z]]), paste0("d_", z), "drift")
  )
}) |> list_rbind()
print(wyniki, n = Inf, width = Inf)
write_csv(wyniki, "output/tables/e3_testy_stacjonarnosci.csv")

# ---- 2b. Inflacja m/m: ma sezonowość, która zniekształca ADF, więc testujemy też po jej usunięciu ----
sezon_mm <- resid(lm(inflacja_mm ~ factor(lubridate::month(date)), data = poziomy))
wyniki_mm <- bind_rows(
  testuj_szereg(poziomy$inflacja_mm, "inflacja_mm (surowa)", "drift"),
  testuj_szereg(sezon_mm, "inflacja_mm (bez sezonowości)", "drift"),
  testuj_szereg(sezon_mm[poziomy$date <= "2019-12-01"], "inflacja_mm bez sez., do 2019", "drift"),
  testuj_szereg(sezon_mm[poziomy$date >= "2010-01-01"], "inflacja_mm bez sez., od 2010", "drift")
)
cat("\nInflacja m/m:\n"); print(wyniki_mm, n = Inf, width = Inf)
write_csv(wyniki_mm, "output/tables/e3_testy_inflacja_mm.csv")

# ---- 3. Decyzja ---------------------------------------------------------------
# * stopa, kurs (log), produkcja (log): pierwsze różnice (poziomy I(1), różnice I(0)).
# * inflacja m/m: zostaje w poziomie (I(0) po uwzględnieniu sezonowości; w VAR dummies miesięczne).
#   Dlaczego nie różnicujemy r/r: Δ(r/r) = π(t) - π(t-12) ma autokorelację -0.36 na lagu 12 (efekt bazy).
# * d_inflacja (Δ r/r) zostaje w tabeli tylko do kontroli odporności (etap 7).
# stopa i inflacja: pp; kurs i produkcja: zmiana log x100 (~ %).
dane_var <- poziomy |>
  mutate(across(c(stopa, inflacja, leurpln, lprodukcja), \(x) x - lag(x))) |>
  rename(d_stopa = stopa, d_inflacja = inflacja, d_eurpln = leurpln, d_produkcja = lprodukcja) |>
  drop_na()
cat("\nDane do VAR:", format(min(dane_var$date)), "-", format(max(dane_var$date)),
    ",", nrow(dane_var), "obserwacji\n")
print(summary(dane_var |> select(-date)))
saveRDS(dane_var, "data/processed/dane_var.rds")
write_csv(dane_var, "data/processed/dane_var.csv")

# ---- 4. Czy poziomy nie są skointegrowane? (test Johansena, ślad) ------------
# Jeśli są, różnicowanie gubi relację długookresową, a VAR trzeba by zastąpić VECM.
joh <- urca::ca.jo(poziomy |> select(stopa, inflacja, leurpln), type = "trace",
                   ecdet = "const", K = 3, spec = "transitory")
cat("\nJohansen (trace), stopa + inflacja + log EUR/PLN, K = 3:\n")
print(round(rbind(test = joh@teststat, kryt5 = joh@cval[, "5pct"]), 2))
joh_wyn <- tibble(H0 = c("r <= 2", "r <= 1", "r = 0"), stat = joh@teststat, kryt5 = joh@cval[, "5pct"],
                  odrzuca = stat > kryt5)
write_csv(joh_wyn, "output/tables/e3_johansen.csv")

# Wrażliwość: liczba relacji kointegrujących r (sekwencyjnie od r = 0) dla różnych K i prób.
# Dla 3 zmiennych r = 3 znaczyłoby, że wszystkie są stacjonarne (sprzeczne z ADF) -> test przesadnie odrzuca.
rzad_kointegracji <- function(dd, K, zm) {
  j <- urca::ca.jo(dd[zm], type = "trace", ecdet = "const", K = K, spec = "transitory")
  st <- rev(j@teststat); cv <- rev(j@cval[, "5pct"]); r <- 0
  while (r < length(st) && st[r + 1] > cv[r + 1]) r <- r + 1
  r
}
pp <- poziomy |> rename(leur = leurpln)
proby <- list(
  "pełna próba"       = pp,
  "do 2019"           = filter(pp, date <= "2019-12-01"),
  "od 2010"           = filter(pp, date >= "2010-01-01"),
  "bez 2020-2022"     = filter(pp, date < "2020-03-01" | date > "2022-12-01")
)
joh_wrazl <- imap(proby, \(dd, nazwa) {
  tibble(proba = nazwa, K = 2:8, r = map_int(2:8, \(K) rzad_kointegracji(dd, K, c("stopa", "inflacja", "leur"))))
}) |> list_rbind()
cat("\nWrażliwość Johansena (liczba relacji r wg testu śladu, 5%), K = 2..8:\n")
print(joh_wrazl |> pivot_wider(names_from = K, values_from = r, names_prefix = "K="))
write_csv(joh_wrazl, "output/tables/e3_johansen_wrazliwosc.csv")

# ---- 5. Regresja pozorna: dwa NIEZALEŻNE błądzenia losowe -------------------
n <- nrow(poziomy); N <- 2000
symul <- map(1:N, \(i) {
  y <- cumsum(rnorm(n)); x <- cumsum(rnorm(n))
  s <- summary(lm(y ~ x))
  dw <- sum(diff(resid(lm(y ~ x)))^2) / sum(resid(lm(y ~ x))^2)
  tibble(t = s$coefficients[2, 3], r2 = s$r.squared, dw = dw)
}) |> list_rbind()
cat("\nSymulacja:", N, "par NIEZALEŻNYCH błądzeń losowych (n =", n, ")\n")
cat("  odsetek 'istotnych' wsp. (|t| > 1.96):", round(mean(abs(symul$t) > 1.96), 2), "(powinno być 0.05)\n")
cat("  średnie R2:", round(mean(symul$r2), 2), ", mediana DW:", round(median(symul$dw), 2), "\n")
r_poz <- lm(inflacja ~ stopa, data = poziomy)
r_roz <- lm(d_inflacja ~ d_stopa, data = dane_var)
dw <- \(m) round(sum(diff(resid(m))^2) / sum(resid(m)^2), 2)
cat("Regresja inflacja ~ stopa na POZIOMACH: R2 =", round(summary(r_poz)$r.squared, 2),
    ", DW =", dw(r_poz), "\n")
cat("Regresja d_inflacja ~ d_stopa na RÓŻNICACH: R2 =", round(summary(r_roz)$r.squared, 3),
    ", DW =", dw(r_roz), "\n")
write_csv(tibble(odsetek_istotnych = mean(abs(symul$t) > 1.96), srednie_r2 = mean(symul$r2),
                 mediana_dw = median(symul$dw), n_symulacji = N, n_obs = n,
                 r2_poziomy = summary(r_poz)$r.squared, dw_poziomy = dw(r_poz),
                 r2_roznice = summary(r_roz)$r.squared, dw_roznice = dw(r_roz)),
          "output/tables/e3_regresja_pozorna.csv")

# ---- 6. Wykres szeregów po różnicowaniu --------------------------------------
zd <- zdarzenia |> filter(date >= min(dane_var$date), date <= max(dane_var$date))
ggsave("output/figures/e3_roznice.png",
  dane_var |>
    select(date, d_stopa, inflacja_mm, d_eurpln, d_produkcja) |>
    pivot_longer(-date) |>
    mutate(name = recode(name, inflacja_mm = "Inflacja m/m (%, bez różnicowania)", d_stopa = "Δ stopa NBP (pp)", d_inflacja = "Δ inflacja r/r (pp)",
                         d_eurpln = "Δ log EUR/PLN (×100)", d_produkcja = "Δ log produkcji (×100)")) |>
    ggplot(aes(date, value)) + geom_line(linewidth = 0.5) +
    geom_vline(data = zd, aes(xintercept = date), linetype = "dashed", color = "grey50") +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    labs(title = "Szeregi użyte w VAR (po transformacji)", x = NULL, y = NULL, caption = zrodlo) +
    motyw_projektu(),
  width = 9, height = 9, dpi = 150)
