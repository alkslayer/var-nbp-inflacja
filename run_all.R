# Uruchamia cały projekt od zera: dane -> analiza -> raport.
# Użycie (z katalogu projektu):  Rscript run_all.R
# Wymaga Quarto (np. wbudowane w RStudio) - ścieżka w zmiennej QUARTO_PATH.
etapy <- c("etap1_dane.R", "etap2_eksploracja.R", "etap3_stacjonarnosc.R", "etap4_specyfikacja.R",
           "etap5_diagnostyka.R", "etap6_wnioski.R", "etap7_odpornosc.R")
for (s in etapy) {
  cat("\n========", s, "========\n")
  st <- system2("Rscript", s)
  if (st != 0) stop("Etap nie powiódł się: ", s)
}
if (Sys.getenv("QUARTO_PATH") == "") {
  rstudio_q <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
  if (file.exists(rstudio_q)) Sys.setenv(QUARTO_PATH = rstudio_q)
}
quarto::quarto_render("report/raport.qmd", output_format = "html")
quarto::quarto_render("report/raport.qmd", output_format = "typst")
dir.create("docs", showWarnings = FALSE)
file.copy("report/raport.html", "docs/index.html", overwrite = TRUE)   # kopia dla GitHub Pages
cat("\nGotowe: report/raport.html, report/raport.pdf\n")
