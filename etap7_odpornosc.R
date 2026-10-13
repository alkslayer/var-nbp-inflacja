# ETAP 7: odporność - ta sama analiza dla innych prób, opóźnień, kolejności i specyfikacji
# Cel: sprawdzić, czy wnioski z modelu bazowego (etap 6) są stabilne.
library(vars)
library(tidyverse)
library(patchwork)
source("R/var_pomocnicze.R"); source("R/motyw.R")

mies <- readRDS("data/processed/dane_miesieczne.rds") |> arrange(date)
dv <- readRDS("data/processed/dane_var.rds") |>
  left_join(mies |> transmute(date, d_brent = 100 * (log(brent) - lag(log(brent))),
                               stopa = stopa_kon, leurpln = 100 * log(eurpln)), by = "date")
stopifnot(!anyNA(dv$d_brent), !anyNA(dv$stopa))

# impulsowe dummies na trzy obserwacje odstające inflacji m/m (|z| > 3 w etapie 5)
odst <- as.Date(c("2022-02-01", "2022-03-01", "2023-01-01"))
dum <- sapply(odst, \(d) as.numeric(dv$date == d)); colnames(dum) <- paste0("odst_", format(odst, "%Y%m"))

B3 <- c("inflacja_mm", "d_stopa", "d_eurpln")
specyfikacje <- list(
  list(nazwa = "1. Bazowy: VAR(3), pełna próba",         zm = B3, p = 3),
  list(nazwa = "2. Więcej opóźnień: VAR(6)",             zm = B3, p = 6),
  list(nazwa = "3. Próba od 2010",                       zm = B3, p = 3, od = "2010-01-01"),
  list(nazwa = "4. Próba do 2019 (przed COVID)",         zm = B3, p = 3, do = "2019-12-01"),
  list(nazwa = "5. Dummies na odstające (2022-02/03, 2023-01)", zm = B3, p = 3, dummy = TRUE),
  list(nazwa = "6. Odwrócona kolejność: stopa -> inflacja -> kurs", zm = c("d_stopa", "inflacja_mm", "d_eurpln"), p = 3),
  list(nazwa = "7. Kurs przed stopą: inflacja -> kurs -> stopa", zm = c("inflacja_mm", "d_eurpln", "d_stopa"), p = 3),
  list(nazwa = "8. + produkcja przemysłowa (4 zm.)",      zm = c("d_produkcja", B3), p = 3),
  list(nazwa = "9. + ropa Brent i produkcja (5 zm.)",     zm = c("d_brent", "d_produkcja", B3), p = 3),
  list(nazwa = "10. VAR na poziomach (stopa, kurs w poziomach), p = 4", zm = c("inflacja_mm", "stopa", "leurpln"), p = 4)
)

uruchom <- function(sp, B = 500) {
  d <- dv
  if (!is.null(sp$od)) d <- filter(d, date >= sp$od)
  if (!is.null(sp$do)) d <- filter(d, date <= sp$do)
  exo <- if (isTRUE(sp$dummy)) dum[dv$date %in% d$date, , drop = FALSE] else NULL
  m  <- estymuj_var(d, sp$zm, sp$p, exogen = exo)
  bt <- irf_boot(m, h = 36, B = B, seed = 11)
  ceny <- przedzialy(bt, "ceny_poziom"); rr <- przedzialy(bt, "infl_rr"); kurs <- przedzialy(bt, "kurs_poziom")
  stopa_n <- if ("d_stopa" %in% sp$zm) "d_stopa" else "stopa"
  g1 <- test_pary(m, stopa_n, "inflacja_mm"); g2 <- test_pary(m, "inflacja_mm", stopa_n)
  # przy dummies na pojedyncze obserwacje (dźwignia = 1) macierz HC1 jest osobliwa -> używamy testu klasycznego
  if (isTRUE(sp$dummy)) { g1$p_HC1 <- g1$p_klasyczny; g2$p_HC1 <- g2$p_klasyczny }
  fe <- fevd_tablica(bt$punkt)["inflacja_mm", stopa_n, 13] * 100
  list(
    wiersz = tibble(
      specyfikacja = sp$nazwa, T = m$obs, p = sp$p,
      portmanteau_p = as.numeric(serial.test(m, lags.pt = 24, type = "PT.asymptotic")$serial$p.value),
      stabilny = all(Mod(roots(m, modulus = FALSE)) < 1),
      ceny_h12 = ceny$punkt[13], ceny_h12_lo90 = ceny$lo90[13], ceny_h12_hi90 = ceny$hi90[13],
      ceny_h24 = ceny$punkt[25], ceny_h24_lo90 = ceny$lo90[25], ceny_h24_hi90 = ceny$hi90[25],
      infl_rr_szczyt = max(rr$punkt), infl_rr_szczyt_h = rr$h[which.max(rr$punkt)],
      kurs_h12 = kurs$punkt[13], kurs_h12_lo90 = kurs$lo90[13], kurs_h12_hi90 = kurs$hi90[13],
      granger_stopa_infl_pHC1 = g1$p_HC1, granger_infl_stopa_pHC1 = g2$p_HC1,
      fevd_infl_przez_stope_h12 = fe),
    ceny = ceny |> mutate(specyfikacja = sp$nazwa), kurs = kurs |> mutate(specyfikacja = sp$nazwa),
    rr = rr |> mutate(specyfikacja = sp$nazwa))
}

wyniki <- map(specyfikacje, \(sp) { cat("Spec:", sp$nazwa, "\n"); uruchom(sp) })
tab <- map(wyniki, "wiersz") |> list_rbind()
write_csv(tab, "output/tables/e7_odpornosc.csv")
options(width = 200)
cat("\n=== Porównanie specyfikacji (reakcja na +25 pb stopy) ===\n")
print(tab |> transmute(specyfikacja = str_trunc(specyfikacja, 42), T, p, PT = round(portmanteau_p, 2),
                       ceny12 = round(ceny_h12, 2), lo = round(ceny_h12_lo90, 2), hi = round(ceny_h12_hi90, 2),
                       rr_max = round(infl_rr_szczyt, 2), kurs12 = round(kurs_h12, 2),
                       kl = round(kurs_h12_lo90, 2), kh = round(kurs_h12_hi90, 2),
                       G_s_i = round(granger_stopa_infl_pHC1, 3), G_i_s = round(granger_infl_stopa_pHC1, 3),
                       fevd = round(fevd_infl_przez_stope_h12, 1)), n = Inf, width = Inf)
cat("\nPoziom cen po 12 mies. dodatni we wszystkich specyfikacjach (punkt):", all(tab$ceny_h12 > 0), "\n")
cat("Specyfikacje z przedziałem 90% poziomu cen po 12 mies. NIE obejmującym 0:",
    sum(tab$ceny_h12_lo90 > 0 | tab$ceny_h12_hi90 < 0), "z", nrow(tab), "\n")
cat("Specyfikacje z ujemną reakcją kursu po 12 mies. (punkt):", sum(tab$kurs_h12 < 0), "z", nrow(tab), "\n")

# ---- wykresy: reakcja poziomu cen i kursu we wszystkich specyfikacjach -------
graf <- function(klucz, tytul, kolor) {
  map(wyniki, klucz) |> list_rbind() |>
    mutate(specyfikacja = factor(specyfikacja, levels = map_chr(specyfikacje, "nazwa"))) |>
    ggplot(aes(h)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90), fill = kolor, alpha = 0.2) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_line(aes(y = punkt), color = kolor, linewidth = 0.7) +
    facet_wrap(~specyfikacja, ncol = 2, labeller = label_wrap_gen(45)) +
    labs(title = tytul, x = "Miesiące po szoku +25 pb", y = NULL, caption = zrodlo) +
    motyw_projektu() + theme(strip.text = element_text(size = 8))
}
ggsave("output/figures/e7_ceny_odpornosc.png",
       graf("ceny", "Reakcja poziomu cen (%) na szok stopy +25 pb: 10 specyfikacji (pasmo 90%)", "#8e44ad"),
       width = 10, height = 11, dpi = 150)
ggsave("output/figures/e7_kurs_odpornosc.png",
       graf("kurs", "Reakcja EUR/PLN (%) na szok stopy +25 pb: 10 specyfikacji (pasmo 90%)", "#27ae60"),
       width = 10, height = 11, dpi = 150)
