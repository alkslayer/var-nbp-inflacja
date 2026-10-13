# Wspólny wygląd wykresów i lista zdarzeń (używane w etapach 2, 6, raporcie)
library(ggplot2)

motyw_projektu <- function() {
  theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          panel.grid.minor = element_blank(),
          plot.caption = element_text(color = "grey40"))
}

zdarzenia <- data.frame(
  date  = as.Date(c("2020-03-01", "2022-02-01", "2026-01-01")),
  opis  = c("COVID-19 (03.2020)", "Inflacyjny szok 2022 (wojna w Ukrainie)",
            "Eurostat: ECOICOP v2 (01.2026)")
)

zrodlo <- "Źródło: Eurostat, NBP (archiwum stóp i API kursów), obliczenia własne."
