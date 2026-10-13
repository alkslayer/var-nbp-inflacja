# ETAP 6: przyczynowość Grangera, IRF (bootstrap reszt), FEVD
library(vars)
library(tidyverse)
library(patchwork)
source("R/var_pomocnicze.R"); source("R/motyw.R")

model <- readRDS("data/processed/model_var_bazowy.rds")
H <- 36   # horyzont w miesiącach

# ---- 1. Przyczynowość Grangera (F + bootstrap) -------------------------------
pary <- bind_rows(
  test_pary(model, "d_stopa", "inflacja_mm"), test_pary(model, "inflacja_mm", "d_stopa"),
  test_pary(model, "d_eurpln", "inflacja_mm"), test_pary(model, "inflacja_mm", "d_eurpln"),
  test_pary(model, "d_stopa", "d_eurpln"), test_pary(model, "d_eurpln", "d_stopa")
)
cat("Przyczynowość Grangera (testy parami, równanie VAR(3)):\n"); print(pary, width = Inf)
write_csv(pary, "output/tables/e6_granger.csv")
g_blok <- causality(model, cause = "d_stopa", boot = TRUE, boot.runs = 2000)
cat("\nStopa -> (inflacja, kurs) łącznie, bootstrap: F =", round(unname(g_blok$Granger$statistic), 2),
    " p =", round(as.numeric(g_blok$Granger$p.value), 4), "\n")

# ---- 2. IRF z bootstrapem ----------------------------------------------------
t0 <- Sys.time()
bt <- irf_boot(model, h = H, B = 1000)
cat("\nBootstrap:", bt$B, "replikacji,", round(difftime(Sys.time(), t0, units = "secs")), "s\n")
saveRDS(bt, "data/processed/irf_bootstrap_bazowy.rds")

wielkosci <- c(stopa_poziom = "Poziom stopy NBP (pp)", ceny_poziom = "Poziom cen HICP (%)",
               infl_rr = "Inflacja r/r (pp)", kurs_poziom = "Poziom EUR/PLN (%, + = deprecjacja)",
               infl_mm = "Inflacja m/m (pp)")
irf_tab <- imap(wielkosci, \(opis, nazwa) przedzialy(bt, nazwa) |> mutate(wielkosc = nazwa, opis = opis)) |>
  list_rbind()
write_csv(irf_tab, "output/tables/e6_irf_bazowy.csv")

ile_wspolne <- irf_tab |> filter(h %in% c(0, 3, 6, 12, 18, 24, 36), wielkosc %in% c("infl_rr", "ceny_poziom", "kurs_poziom")) |>
  mutate(across(c(punkt, lo90, hi90), \(x) round(x, 3))) |> select(wielkosc, h, punkt, lo90, hi90)
cat("\nReakcje na szok +25 pb stopy (punkt, przedział 90%):\n"); print(ile_wspolne, n = 30)

# kiedy reakcja inflacji r/r jest istotna (przedział 90% nie obejmuje 0)?
infl <- irf_tab |> filter(wielkosc == "infl_rr")
cat("\nInflacja r/r: min reakcji =", round(min(infl$punkt), 3), "pp przy h =", infl$h[which.min(infl$punkt)],
    "| max =", round(max(infl$punkt), 3), "pp przy h =", infl$h[which.max(infl$punkt)], "\n")
cat("Miesiące z istotnie UJEMNĄ reakcją (90%):", paste(infl$h[infl$hi90 < 0], collapse = ", "), "\n")
cat("Miesiące z istotnie DODATNIĄ reakcją (90%):", paste(infl$h[infl$lo90 > 0], collapse = ", "), "\n")
ceny <- irf_tab |> filter(wielkosc == "ceny_poziom")
cat("Poziom cen: min =", round(min(ceny$punkt), 3), "% przy h =", ceny$h[which.min(ceny$punkt)],
    "| istotnie ujemny (90%) w:", paste(ceny$h[ceny$hi90 < 0], collapse = ", "), "\n")
cat("Poziom cen: istotnie DODATNI (90%) w:", paste(ceny$h[ceny$lo90 > 0], collapse = ", "), "\n")

wykres_irf <- function(w, tytul, kolor) {
  irf_tab |> filter(wielkosc == w) |>
    ggplot(aes(h)) +
    geom_ribbon(aes(ymin = lo90, ymax = hi90), fill = kolor, alpha = 0.15) +
    geom_ribbon(aes(ymin = lo68, ymax = hi68), fill = kolor, alpha = 0.30) +
    geom_hline(yintercept = 0, color = "grey40") +
    geom_line(aes(y = punkt), color = kolor, linewidth = 0.9) +
    labs(title = tytul, x = "Miesiące po szoku", y = NULL) + motyw_projektu()
}
panel <- (wykres_irf("stopa_poziom", "Stopa NBP (pp)", "#1b6ca8") + wykres_irf("infl_rr", "Inflacja r/r (pp)", "#c0392b")) /
         (wykres_irf("ceny_poziom", "Poziom cen HICP (%)", "#8e44ad") + wykres_irf("kurs_poziom", "EUR/PLN (%; + = słabszy złoty)", "#27ae60")) +
  plot_annotation(title = "Reakcja na nieoczekiwany wzrost stopy NBP o 25 pb",
                  subtitle = "Linia: estymacja punktowa; pasma: 68% i 90% (bootstrap reszt, 1000 replikacji). Identyfikacja Choleskiego: inflacja -> stopa -> kurs.",
                  caption = zrodlo, theme = theme(plot.title = element_text(face = "bold")))
ggsave("output/figures/e6_irf.png", panel, width = 10, height = 7.5, dpi = 150)

# ---- 3. FEVD -----------------------------------------------------------------
hor <- c(1, 6, 12, 24, 36)
fe_pkt <- fevd_tablica(bt$punkt)
fe_b <- map(bt$proby, \(x) fevd_tablica(x))
fevd_tab <- map(c("inflacja_mm", "d_eurpln", "d_stopa"), \(k) {
  map(c("inflacja_mm", "d_stopa", "d_eurpln"), \(j) {
    wekt <- sapply(fe_b, \(a) a[k, j, ])                 # (h+1) x B
    tibble(zmienna = k, szok = j, h = hor,
           udzial = 100 * fe_pkt[k, j, hor + 1],
           lo90 = 100 * apply(wekt[hor + 1, , drop = FALSE], 1, quantile, 0.05),
           hi90 = 100 * apply(wekt[hor + 1, , drop = FALSE], 1, quantile, 0.95))
  }) |> list_rbind()
}) |> list_rbind()
write_csv(fevd_tab, "output/tables/e6_fevd.csv")
cat("\nFEVD (udział szoku w wariancji błędu prognozy, %; przedział 90%):\n")
print(fevd_tab |> filter(zmienna == "inflacja_mm") |> mutate(across(c(udzial, lo90, hi90), \(x) round(x, 1))))
cat("\nFEVD kursu EUR/PLN:\n")
print(fevd_tab |> filter(zmienna == "d_eurpln", h %in% c(12, 36)) |> mutate(across(c(udzial, lo90, hi90), \(x) round(x, 1))))
cat("\nFEVD stopy:\n")
print(fevd_tab |> filter(zmienna == "d_stopa", h %in% c(12, 36)) |> mutate(across(c(udzial, lo90, hi90), \(x) round(x, 1))))

p_fe <- fevd_tab |> filter(zmienna == "inflacja_mm") |>
  mutate(szok = recode(szok, inflacja_mm = "szok inflacyjny", d_stopa = "szok stopy NBP", d_eurpln = "szok kursowy")) |>
  ggplot(aes(factor(h), udzial, fill = szok)) + geom_col() +
  scale_fill_manual(values = c("#c0392b", "#1b6ca8", "#27ae60")) +
  labs(title = "Co wyjaśnia zmienność inflacji m/m?", subtitle = "Dekompozycja wariancji błędu prognozy (FEVD), %",
       x = "Horyzont (miesiące)", y = "%", fill = NULL, caption = zrodlo) + motyw_projektu() +
  theme(legend.position = "top")
ggsave("output/figures/e6_fevd.png", p_fe, width = 8, height = 4.5, dpi = 150)
