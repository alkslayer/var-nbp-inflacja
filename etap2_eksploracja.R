# ETAP 2: eksploracja - wykresy, statystyki opisowe, zdarzenia
library(tidyverse)
library(patchwork)
source("R/motyw.R")

dane <- readRDS("data/processed/dane_miesieczne.rds") |>
  transmute(date, stopa = stopa_kon, inflacja, eurpln, produkcja)

# ---- statystyki opisowe ------------------------------------------------------
opis <- dane |>
  pivot_longer(-date, names_to = "zmienna") |>
  summarise(n = n(), srednia = mean(value), odch = sd(value), min = min(value),
            mediana = median(value), max = max(value),
            data_max = date[which.max(value)], .by = zmienna)
print(opis, width = Inf)
write_csv(opis, "output/tables/statystyki_opisowe.csv")

cat("\nKorelacje na POZIOMACH:\n"); print(round(cor(select(dane, -date)), 2))
cat("\nOkresy stopy = 0.10:", format(range(dane$date[dane$stopa == 0.1])), "\n")
cat("Liczba zmian stopy w próbie:", sum(diff(dane$stopa) != 0), "\n")
cat("Inflacja > cel 2.5 +/- 1 pp (poza pasmem 1.5-3.5) w", 
    sum(dane$inflacja > 3.5 | dane$inflacja < 1.5), "z", nrow(dane), "miesięcy\n")

# ---- wykresy -----------------------------------------------------------------
zd <- zdarzenia |> filter(date >= min(dane$date), date <= max(dane$date))
dodaj_zdarzenia <- function() {
  list(geom_vline(data = zd, aes(xintercept = date), linetype = "dashed", color = "grey50"))
}
etykiety_zd <- function(y) {
  geom_text(data = zd, aes(x = date, y = y, label = opis), angle = 90, vjust = -0.4,
            hjust = 1, size = 2.8, color = "grey35", inherit.aes = FALSE)
}

p1 <- ggplot(dane, aes(date, stopa)) + geom_step(color = "#1b6ca8", linewidth = 0.8) +
  dodaj_zdarzenia() +
  labs(title = "Stopa referencyjna NBP", subtitle = "Stan na koniec miesiąca, %", x = NULL, y = NULL) +
  motyw_projektu()
p2 <- ggplot(dane, aes(date, inflacja)) + geom_line(color = "#c0392b", linewidth = 0.8) +
  geom_hline(yintercept = 2.5, linetype = "dotted") +
  dodaj_zdarzenia() +
  annotate("text", x = min(dane$date), y = 2.5, label = "cel NBP 2,5%", vjust = -0.6, hjust = 0, size = 3) +
  labs(title = "Inflacja HICP (r/r)", subtitle = "%", x = NULL, y = NULL) + motyw_projektu()
p3 <- ggplot(dane, aes(date, eurpln)) + geom_line(color = "#27ae60", linewidth = 0.8) +
  dodaj_zdarzenia() +
  labs(title = "Kurs EUR/PLN", subtitle = "Średnia miesięczna, PLN za 1 EUR", x = NULL, y = NULL) + motyw_projektu()
p4 <- ggplot(dane, aes(date, produkcja)) + geom_line(color = "#8e44ad", linewidth = 0.8) +
  dodaj_zdarzenia() +
  labs(title = "Produkcja przemysłowa", subtitle = "Indeks 2021 = 100, po wyrównaniu sezonowym", x = NULL, y = NULL) + motyw_projektu()

panel <- (p1 / p2 / p3 / p4) + plot_annotation(caption = zrodlo, theme = theme(plot.caption = element_text(color = "grey40")))
ggsave("output/figures/e2_szeregi.png", panel, width = 9, height = 11, dpi = 150)

# stopa vs inflacja na jednym wykresie (z opóźnieniem do wzrokowej oceny)
p5 <- dane |> select(date, stopa, inflacja) |>
  pivot_longer(-date) |>
  mutate(name = recode(name, stopa = "Stopa referencyjna NBP", inflacja = "Inflacja HICP r/r")) |>
  ggplot(aes(date, value, color = name)) + geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("#c0392b", "#1b6ca8")) + dodaj_zdarzenia() +
  labs(title = "Stopa NBP i inflacja", subtitle = "%, miesięcznie", x = NULL, y = NULL,
       color = NULL, caption = zrodlo) +
  motyw_projektu() + theme(legend.position = "top")
ggsave("output/figures/e2_stopa_inflacja.png", p5, width = 9, height = 4.5, dpi = 150)

# korelacja krzyżowa NAIWNA na poziomach (ilustracja pozornej zależności)
cc <- ccf(dane$stopa, dane$inflacja, lag.max = 24, plot = FALSE)
cat("\nCCF poziomów (stopa[t+k], inflacja[t]): max |r| =", round(max(abs(cc$acf)), 2),
    "przy k =", cc$lag[which.max(abs(cc$acf))], "\n")
