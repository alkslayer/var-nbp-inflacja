# ETAP 5: diagnostyka modelu bazowego - stabilność, autokorelacja, normalność, heteroskedastyczność
library(vars)
library(tidyverse)
library(patchwork)
source("R/var_pomocnicze.R"); source("R/motyw.R")

model <- readRDS("data/processed/model_var_bazowy.rds")
dane  <- readRDS("data/processed/dane_var.rds")
daty  <- dane$date[-(1:model$p)]
u     <- resid(model)

# ---- 1. Stabilność: moduły pierwiastków (muszą być < 1) ----------------------
korzenie <- roots(model, modulus = FALSE)
cat("Moduły pierwiastków:", round(sort(Mod(korzenie), decreasing = TRUE), 3), "\n")
cat("Stabilny:", all(Mod(korzenie) < 1), "\n")

png("output/figures/e5_pierwiastki.png", 900, 500, res = 130)
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
th <- seq(0, 2 * pi, length.out = 200)
plot(cos(th), sin(th), type = "l", asp = 1, xlab = "Re", ylab = "Im",
     main = "Pierwiastki charakterystyczne (odwrócone)")
abline(h = 0, v = 0, col = "grey80"); points(Re(korzenie), Im(korzenie), pch = 19, col = "#c0392b")
stab <- stability(model, type = "OLS-CUSUM")
plot(stab$stability$d_stopa, main = "OLS-CUSUM: równanie stopy", ylab = "Empiryczny proces", xlab = "Czas (0-1)")
dev.off()
cusum <- map_dfr(names(stab$stability), \(n) {
  s <- stab$stability[[n]]; tibble(rownanie = n, max_proces = max(abs(s$process)),
                                   krytyczna5 = 1.358, przekroczenie = max(abs(s$process)) > 1.358) })
cat("\nCUSUM (przekroczenie progu 5% = niestabilność parametrów):\n"); print(cusum)

# ---- 2. Autokorelacja reszt -------------------------------------------------
pt24 <- serial.test(model, lags.pt = 24, type = "PT.asymptotic")$serial
pv <- \(x) as.numeric(x$p.value)   # p-value jako zwykła liczba (bywa macierzą 1x1)
pt12 <- serial.test(model, lags.pt = 12, type = "PT.asymptotic")$serial
bg6  <- serial.test(model, lags.bg = 6,  type = "BG")$serial
bg12 <- serial.test(model, lags.bg = 12, type = "BG")$serial
# BG w małej próbie z dużą liczbą lagów bywa liberalne; sprawdzamy też Edgeworth/ACF per równanie
cat("\nAutokorelacja: Portmanteau(12) p =", round(pt12$p.value, 3), "| Portmanteau(24) p =", round(pt24$p.value, 3),
    "| BG(6) p =", round(bg6$p.value, 4), "| BG(12) p =", round(bg12$p.value, 4), "\n")
acf_tab <- map_dfr(colnames(u), \(n) {
  a <- acf(u[, n], lag.max = 24, plot = FALSE)$acf[-1]
  tibble(rownanie = n, lag = 1:24, acf = a, prog = 1.96 / sqrt(nrow(u)))
})
cat("Opóźnienia z |ACF| > progu 5% (rownanie: lag):\n")
print(acf_tab |> filter(abs(acf) > prog) |> mutate(acf = round(acf, 2)), n = 20)

# ---- 3. Normalność (Jarque-Bera, skośność, kurtoza) --------------------------
nor <- normality.test(model, multivariate.only = FALSE)
jb <- map_dfr(names(nor$jb.uni), \(n) tibble(rownanie = n, JB = nor$jb.uni[[n]]$statistic,
                                              p = nor$jb.uni[[n]]$p.value))
jb$kurtoza <- apply(u, 2, \(x) mean((x - mean(x))^4) / var(x)^2 * (1 - 1/nrow(u))^2)
jb$skosnosc <- apply(u, 2, \(x) mean((x - mean(x))^3) / sd(x)^3)
cat("\nNormalność (JB wielowymiarowy p =", signif(nor$jb.mul$JB$p.value, 3), "):\n"); print(jb)

# ---- 4. Heteroskedastyczność (ARCH-LM wielowymiarowy) -------------------------
arch5  <- arch.test(model, lags.multi = 5,  multivariate.only = TRUE)$arch.mul
arch12 <- arch.test(model, lags.multi = 12, multivariate.only = TRUE)$arch.mul
cat("\nARCH-LM (wielowym.): lag 5 p =", signif(arch5$p.value, 3), "| lag 12 p =", signif(arch12$p.value, 3), "\n")

# gdzie są wariancje? odchylenie reszt w podokresach
podokresy <- tibble(date = daty, inflacja_mm = u[, 1], d_stopa = u[, 2], d_eurpln = u[, 3]) |>
  mutate(okres = case_when(date < "2020-03-01" ~ "2002-02 do 2020-02",
                           date <= "2022-12-01" ~ "2020-03 do 2022-12 (COVID + szok)",
                           TRUE ~ "od 2023-01")) |>
  pivot_longer(-c(date, okres), names_to = "rownanie") |>
  summarise(odch_reszt = sd(value), n = n(), .by = c(okres, rownanie)) |>
  pivot_wider(names_from = okres, values_from = c(odch_reszt, n))
cat("\nOdch. standardowe reszt wg okresu:\n"); print(podokresy, width = Inf)

# obserwacje odstające (|reszta| > 3 sd)
odst <- tibble(date = daty, inflacja_mm = u[, 1], d_stopa = u[, 2], d_eurpln = u[, 3]) |>
  pivot_longer(-date, names_to = "rownanie") |>
  mutate(z = value / sd(value), .by = rownanie) |>
  filter(abs(z) > 3) |> arrange(date)
cat("\nObserwacje odstające (|z| > 3):\n"); print(odst, n = 30)

# ---- 5. Tabela zbiorcza + wykresy reszt --------------------------------------
zbiorcza <- tribble(
  ~test, ~H0, ~p_value, ~wynik, ~co_robic_gdy_nie_przechodzi,
  "Pierwiastki", "stabilność (moduły < 1)", NA_real_, ifelse(all(Mod(korzenie) < 1), "OK", "NIE"), "zmienić transformację/różnicowanie, sprawdzić liczbę opóźnień",
  "Portmanteau(24)", "brak autokorelacji reszt", pv(pt24), ifelse(pv(pt24) > .05, "OK", "NIE"), "zwiększyć p, dodać dummies/zmienną",
  "Breusch-Godfrey(6)", "brak autokorelacji reszt", pv(bg6), ifelse(pv(bg6) > .05, "OK", "NIE"), "zwiększyć p (sprawdzić p = 6 w etapie 7)",
  "Jarque-Bera (wielowym.)", "normalność reszt", as.numeric(nor$jb.mul$JB$p.value), ifelse(as.numeric(nor$jb.mul$JB$p.value) > .05, "OK", "NIE"), "bootstrap reszt zamiast przedziałów normalnych; dummies na odstające",
  "ARCH-LM(5)", "homoskedastyczność", as.numeric(arch5$p.value), ifelse(as.numeric(arch5$p.value) > .05, "OK", "NIE"), "okres bez 2020-2022; bootstrap; interpretować przedziały ostrożnie"
)
print(zbiorcza |> select(-co_robic_gdy_nie_przechodzi), width = Inf)
write_csv(zbiorcza, "output/tables/e5_diagnostyka_zbiorcza.csv")

zd <- zdarzenia |> filter(date >= min(daty))
p_res <- tibble(date = daty, inflacja_mm = u[, 1], d_stopa = u[, 2], d_eurpln = u[, 3]) |>
  pivot_longer(-date) |>
  ggplot(aes(date, value)) + geom_line(linewidth = 0.4) +
  geom_vline(data = zd, aes(xintercept = date), linetype = "dashed", color = "grey50") +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  labs(title = "Reszty modelu VAR(3)", x = NULL, y = NULL) + motyw_projektu()
p_acf <- ggplot(acf_tab, aes(lag, acf)) + geom_col(width = 0.4) +
  geom_hline(aes(yintercept = prog), linetype = "dashed", color = "#c0392b") +
  geom_hline(aes(yintercept = -prog), linetype = "dashed", color = "#c0392b") +
  facet_wrap(~rownanie, ncol = 1) + labs(title = "ACF reszt", x = "Opóźnienie", y = NULL) + motyw_projektu()
ggsave("output/figures/e5_reszty.png", p_res | p_acf, width = 11, height = 7, dpi = 150)
