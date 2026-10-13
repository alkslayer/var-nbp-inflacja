# ETAP 1: pobranie szeregów, kontrola jakości, tabela miesięczna
# Uruchamiać z katalogu projektu:  Rscript etap1_dane.R
library(tidyverse)
library(lubridate)
library(eurostat)
library(httr2)

source("R/fetch_nbp.R")
source("R/stopa_nbp.R")

dzis <- Sys.Date()
cat("Data pobrania:", format(dzis), "\n\n")

# ---- 1. Inflacja HICP r/r (Eurostat, klasyfikacja ECOICOP v2) ----------------
infl_raw <- get_eurostat(
  "prc_hicp_minr",
  filters     = list(geo = "PL", coicop18 = "TOTAL", unit = "RCH_A"),
  time_format = "date"
)
saveRDS(infl_raw, "data/raw/inflacja_eurostat_raw.rds")

stopifnot(n_distinct(infl_raw$time) == nrow(infl_raw))        # 1 wiersz = 1 miesiąc
stopifnot(all(infl_raw$unit == "RCH_A"), all(infl_raw$geo == "PL"))

inflacja <- infl_raw |>
  transmute(date = time, inflacja = values) |>
  arrange(date)

# ---- 1b. Inflacja HICP m/m (ta sama tabela, unit = RCH_M; NIEWYRÓWNANA sezonowo) ---
# Po co: zmiana inflacji r/r ma mechaniczną autokorelację rzędu 12 (efekt bazy), co psuje
# diagnostykę VAR (etap 4-5). Inflacja m/m tego nie ma, ale ma sezonowość -> dummies w VAR.
infl_m_raw <- get_eurostat(
  "prc_hicp_minr",
  filters     = list(geo = "PL", coicop18 = "TOTAL", unit = "RCH_M"),
  time_format = "date"
)
saveRDS(infl_m_raw, "data/raw/inflacja_mm_eurostat_raw.rds")
stopifnot(n_distinct(infl_m_raw$time) == nrow(infl_m_raw), all(infl_m_raw$unit == "RCH_M"))
inflacja_mm <- infl_m_raw |> transmute(date = time, inflacja_mm = values) |> arrange(date)
cat("Inflacja m/m:", format(min(inflacja_mm$date[!is.na(inflacja_mm$inflacja_mm)])), "-",
    format(max(inflacja_mm$date[!is.na(inflacja_mm$inflacja_mm)])), "\n")

# ---- 2. Produkcja przemysłowa (Eurostat, SA+kalendarz, 2021=100) -------------
ip_raw <- get_eurostat(
  "sts_inpr_m",
  filters     = list(geo = "PL", nace_r2 = "B-D", s_adj = "SCA", unit = "I21"),
  time_format = "date"
)
saveRDS(ip_raw, "data/raw/produkcja_eurostat_raw.rds")
stopifnot(n_distinct(ip_raw$time) == nrow(ip_raw))

produkcja <- ip_raw |>
  transmute(date = time, produkcja = values) |>
  arrange(date)

# ---- 3. Kurs EUR/PLN (NBP, tabela A, kursy średnie dzienne) ------------------
eur_raw <- fetch_nbp("eur", "2002-01-02", dzis)
saveRDS(eur_raw, "data/raw/eurpln_nbp_dzienne_raw.rds")

cat("EUR/PLN dzienne:", format(min(eur_raw$date)), "-", format(max(eur_raw$date)),
    ",", nrow(eur_raw), "notowań\n")
stopifnot(!anyDuplicated(eur_raw$date), all(eur_raw$kurs > 2, eur_raw$kurs < 6))

eurpln <- eur_raw |>
  mutate(date = floor_date(date, "month")) |>
  summarise(eurpln = mean(kurs), notowania = n(), .by = date) |>
  arrange(date)

# ---- 3b. Ropa Brent (FRED/EIA, USD za baryłkę, dziennie) -> średnia miesięczna ----
# Po co: kontrola "zagadki cenowej" (bank centralny reaguje na szoki podażowe, których VAR bez ropy nie widzi).
brent_url  <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DCOILBRENTEU"
brent_plik <- "data/raw/brent_fred_dzienne.csv"
download.file(brent_url, brent_plik, quiet = TRUE, mode = "wb")
brent_raw <- read_csv(brent_plik, show_col_types = FALSE, na = c("", "NA", "."),
                      col_types = cols(observation_date = col_date(), DCOILBRENTEU = col_double()))
cat("Brent dzienne:", format(min(brent_raw$observation_date)), "-", format(max(brent_raw$observation_date)),
    ", NA:", sum(is.na(brent_raw$DCOILBRENTEU)), ", zakres USD:",
    paste(round(range(brent_raw$DCOILBRENTEU, na.rm = TRUE), 1), collapse = "-"), "\n")
brent <- brent_raw |>
  filter(!is.na(DCOILBRENTEU)) |>
  mutate(date = floor_date(observation_date, "month")) |>
  summarise(brent = mean(DCOILBRENTEU), brent_dni = n(), .by = date) |>
  arrange(date)

# ---- 4. Stopa referencyjna NBP (archiwum XML) --------------------------------
url_stopy <- "https://static.nbp.pl/dane/stopy/stopy_procentowe_archiwum.xml"
plik_xml  <- "data/raw/stopy_procentowe_archiwum.xml"
download.file(url_stopy, plik_xml, quiet = TRUE, mode = "wb")

zmiany <- wczytaj_stopy_xml(plik_xml)
cat("\nStopa ref.: liczba zmian =", nrow(zmiany), ", od", format(min(zmiany$od)),
    "do", format(max(zmiany$od)), "\n")
stopifnot(!anyNA(zmiany$stopa), all(zmiany$stopa > 0, zmiany$stopa < 30))

# Ostatnia zmiana w archiwum to marzec 2026; zakładamy brak zmian później.
# To założenie weryfikujemy w sekcji 5 danymi BIS.
koniec_dni <- ceiling_date(min(floor_date(dzis, "month") - 1, as.Date("2026-09-30")), "month") - 1
stopa_m <- zmiany |>
  rozwin_do_dni(do = koniec_dni) |>
  agreguj_stopy_miesiecznie()

# ---- 5. Kontrola krzyżowa stopy z BIS (WS_CBPOL, M.PL) ----------------------
bis_url <- "https://stats.bis.org/api/v2/data/dataflow/BIS/WS_CBPOL/1.0/M.PL?format=csv"
bis_plik <- "data/raw/bis_cbpol_PL.csv"
download.file(bis_url, bis_plik, quiet = TRUE, mode = "wb")
bis <- read_csv(bis_plik, show_col_types = FALSE) |>
  transmute(date = ym(TIME_PERIOD), stopa_bis = OBS_VALUE)

por <- stopa_m |> inner_join(bis, by = "date")
cat("\nBIS: zakres", format(min(bis$date)), "-", format(max(bis$date)), "\n")
cat("Porównanie z BIS (", nrow(por), "miesięcy ):\n")
cat("  średnia ważona dniami == BIS:", sum(abs(por$stopa_sr  - por$stopa_bis) < 0.005), "\n")
cat("  stan na koniec miesiąca == BIS:", sum(abs(por$stopa_kon - por$stopa_bis) < 0.005), "\n")
cat("  średnia ważona vs BIS, miesiące z różnicą > 0.05 pp:",
    sum(abs(por$stopa_sr - por$stopa_bis) > 0.05), "\n")
cat("  koniec miesiąca vs BIS, miesiące z różnicą > 0.05 pp:\n")
print(por |> filter(abs(stopa_kon - stopa_bis) > 0.05), n = 20)

# ---- 6. Złączenie: najpierw full_join, żeby zobaczyć braki -------------------
szerokie <- stopa_m |>
  select(date, stopa_sr, stopa_kon) |>
  full_join(inflacja,  by = "date") |>
  full_join(inflacja_mm, by = "date") |>
  full_join(eurpln,    by = "date") |>
  full_join(produkcja, by = "date") |>
  full_join(brent,     by = "date") |>
  arrange(date)

cat("\nZakresy dostępności (pierwszy / ostatni miesiąc z wartością):\n")
szerokie |>
  select(stopa_sr, inflacja, inflacja_mm, eurpln, produkcja, brent, date) |>
  pivot_longer(-date, names_to = "szereg") |>
  filter(!is.na(value)) |>
  summarise(od = min(date), do = max(date), n = n(), .by = szereg) |>
  print()

# Wspólny zakres 4 zmiennych; ostatni miesiąc EUR/PLN może być niepełny
ostatni_pelny <- floor_date(dzis, "month") - months(1)
dane <- szerokie |>
  filter(!is.na(stopa_sr), !is.na(inflacja), !is.na(inflacja_mm), !is.na(eurpln), date <= ostatni_pelny)

# luki w ciągłości dat?
stopifnot(all(diff(dane$date) %in% c(28:31)))
stopifnot(!anyNA(dane$stopa_sr), !anyNA(dane$inflacja), !anyNA(dane$eurpln))

cat("\nWSPÓLNY ZAKRES (stopa, inflacja, EUR/PLN):", format(min(dane$date)), "-",
    format(max(dane$date)), ",", nrow(dane), "miesięcy\n")
cat("Braki w zakresie: produkcja =", sum(is.na(dane$produkcja)), ", brent =", sum(is.na(dane$brent)), "\n")
print(dane |> filter(date >= "2020-01-01", date <= "2020-06-01" | date >= "2022-05-01") |> select(date, brent) |> slice(c(1:6, 30:n())), n = 40)
cat("Miesiące z niepełnym EUR/PLN (<18 notowań):\n")
print(dane |> filter(notowania < 18) |> select(date, notowania))

# Spójność inflacji m/m i r/r: skumulowane 12 miesięcy m/m powinno dać r/r (z dokładnością zaokrągleń)
spojnosc <- dane |>
  mutate(rr_z_mm = (zoo::rollapplyr(1 + inflacja_mm / 100, 12, prod, fill = NA) - 1) * 100,
         roznica = rr_z_mm - inflacja)
cat("\nSpójność m/m vs r/r: mediana |różnicy| =", round(median(abs(spojnosc$roznica), na.rm = TRUE), 2),
    "pp, maksimum =", round(max(abs(spojnosc$roznica), na.rm = TRUE), 2), "pp\n")

# Jednostki - podsumowanie
print(summary(dane |> select(-date, -notowania)))

saveRDS(dane, "data/processed/dane_miesieczne.rds")
write_csv(dane, "data/processed/dane_miesieczne.csv")
cat("\nZapisano data/processed/dane_miesieczne.{rds,csv}\n")
