# Jak stopy procentowe NBP wpływają na inflację w Polsce (VAR w R)

Analiza wektorowej autoregresji (VAR) na danych miesięcznych 2002–2026: stopa referencyjna NBP, inflacja HICP, kurs EUR/PLN
(kontrole: produkcja przemysłowa, ropa Brent). Funkcje reakcji na impuls z bootstrapem, dekompozycja wariancji,
przyczynowość Grangera i 10 testów odporności.

**Raport:** `report/raport.html` (interaktywny) i `report/raport.pdf`.

## Główny wynik
Prosty VAR z identyfikacją Choleskiego **nie pokazuje, by niespodziewana podwyżka stopy NBP obniżała inflację**: poziom cen rośnie
(zagadka cenowa, *price puzzle*), co jest efektem reakcji NBP na inflację, której model nie widzi. Znak jest stabilny we wszystkich
10 specyfikacjach, wielkość efektu nie (zależy od próby; do 2019 r. bliska zera). Szczegóły i ograniczenia: raport.

## Dane (stan na 13.10.2026)
| Seria | Źródło |
|---|---|
| Stopa referencyjna NBP | NBP, archiwum zmian (`static.nbp.pl/dane/stopy/stopy_procentowe_archiwum.xml`), stan na koniec miesiąca; kontrola z BIS (`WS_CBPOL`) |
| Inflacja HICP r/r i m/m | Eurostat, `prc_hicp_minr`, `coicop18 = TOTAL`, `RCH_A` / `RCH_M` (ECOICOP v2) |
| EUR/PLN | API NBP, tabela A, średnia miesięczna |
| Produkcja przemysłowa | Eurostat, `sts_inpr_m` (B–D, SCA, I21) |
| Ropa Brent | FRED, `DCOILBRENTEU`, średnia miesięczna |

Wspólny zakres: 01.2002 – 08.2026 (296 miesięcy, bez luk). Weryfikacja źródeł (zakres dat, braki, jednostki) jest w `etap1_dane.R`.

## Struktura
```
etap1_dane.R ... etap7_odpornosc.R   skrypty etapów (kolejno)
R/                                    funkcje: fetch_nbp, stopa_nbp, testy_stacjonarnosci, var_pomocnicze, motyw
data/raw/                             surowe pobrania (XML NBP, CSV BIS/FRED, RDS Eurostat/NBP)
data/processed/                       tabela miesięczna, dane do VAR, modele, bootstrap
output/figures, output/tables         wykresy i tabele wynikowe
report/raport.qmd                     raport Quarto -> HTML + PDF (Typst)
run_all.R                             uruchamia wszystko od zera (~1 min)
```

## Uruchomienie
```
Rscript run_all.R
```
Wymagane pakiety R: tidyverse, eurostat, httr2, xml2, vars, urca, tseries, patchwork, car, sandwich, lmtest, quarto.
Quarto CLI (np. wbudowane w RStudio; `run_all.R` ustawia `QUARTO_PATH`). PDF powstaje przez Typst, LaTeX niepotrzebny.

## Uwaga metodologiczna
Inflacja w modelu to zmiana **m/m** (z dummies miesięcznymi), nie Δ(r/r): różnica rocznej stopy ma mechaniczną
autokorelację rzędu 12, która psuje diagnostykę reszt (opis w raporcie, rozdz. 3).
