# Stopa referencyjna NBP: z archiwum zmian (XML) do szeregu miesięcznego.
# Archiwum zawiera tylko DATY ZMIAN ("obowiazuje_od"), więc najpierw
# rozwijamy je do szeregu dziennego, a potem agregujemy do miesięcy.
# Wymaga: tidyverse, xml2

# 1. Wczytanie archiwum XML -> tibble(od, stopa)
wczytaj_stopy_xml <- function(plik) {
  doc <- xml2::read_xml(plik)
  pozycje <- xml2::xml_find_all(doc, "//pozycje")

  tibble(
    od = as.Date(xml2::xml_attr(pozycje, "obowiazuje_od")),
    # dla każdej daty zmiany bierzemy dziecko id="ref" (stopa referencyjna)
    stopa = map_chr(pozycje, \(p) {
      xml2::xml_attr(xml2::xml_find_first(p, "./pozycja[@id='ref']"), "oprocentowanie")
    })
  ) |>
    mutate(stopa = as.numeric(sub(",", ".", stopa, fixed = TRUE))) |>   # "24,00" -> 24
    arrange(od)
}

# 2. Zmiany -> szereg dzienny (stopa obowiązuje od dnia zmiany do dnia przed kolejną)
rozwin_do_dni <- function(zmiany, do) {
  tibble(day = seq(min(zmiany$od), as.Date(do), by = "day")) |>
    left_join(zmiany, by = c("day" = "od")) |>
    fill(stopa, .direction = "down")
}

# 3. Dzienny -> miesięczny: średnia ważona dniami oraz stan na koniec miesiąca
agreguj_stopy_miesiecznie <- function(dzienne) {
  dzienne |>
    mutate(date = floor_date(day, "month")) |>
    summarise(
      stopa_sr  = mean(stopa),          # średnia z dni = średnia ważona czasem
      stopa_kon = last(stopa),          # stan na ostatni dzień miesiąca
      dni       = n(),
      .by = date
    )
}
