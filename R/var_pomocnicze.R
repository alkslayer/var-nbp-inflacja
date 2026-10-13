# Funkcje pomocnicze do VAR: estymacja o ustalonej specyfikacji, IRF/FEVD z bootstrapem reszt.
# Wymaga: vars (ładować PRZED tidyverse, bo vars/MASS maskuje dplyr::select), tidyverse

KOLEJNOSC <- c("inflacja_mm", "d_stopa", "d_eurpln")   # kolejność w identyfikacji Choleskiego
SKALA_SZOKU_PP <- 0.25                                  # szok monetarny = 25 pb (typowy krok RPP)

# Estymacja: stała + dummies miesięczne (inflacja m/m jest niewyrównana sezonowo)
# exogen: opcjonalna macierz zmiennych egzogenicznych (np. dummies na odstające obserwacje)
estymuj_var <- function(dane, zmienne = KOLEJNOSC, p = 3, exogen = NULL) {
  y <- as.data.frame(dane)[, zmienne]
  m <- vars::VAR(y, p = p, type = "const", season = 12, exogen = exogen)
  attr(m, "exogen") <- exogen          # zapamiętane na potrzeby bootstrapu
  m
}

# Pojedyncza ścieżka IRF ze współczynników modelu: zwraca tablicę [zmienna, szok, horyzont 0..h]
# (ortogonalizacja Choleskiego: P = dolny trójkąt z rozkładu Sigma_u)
irf_tablica <- function(model, h) {
  Phi <- vars::Phi(model, nstep = h)                    # K x K x (h+1), reakcje na szok z reszt
  P   <- t(chol(summary(model)$covres))                 # dolnotrójkątna
  K   <- dim(Phi)[1]
  out <- array(NA_real_, c(K, K, h + 1), dimnames = list(colnames(P), colnames(P), 0:h))
  for (s in 0:h) out[, , s + 1] <- Phi[, , s + 1] %*% P
  out
}

# Rozkład wariancji błędu prognozy (FEVD) z tej samej tablicy: udział szoku j w wariancji zmiennej k
fevd_tablica <- function(irf, h = dim(irf)[3] - 1) {
  K <- dim(irf)[1]
  out <- array(NA_real_, c(K, K, h + 1), dimnames = dimnames(irf))
  for (s in 0:h) {
    sq <- apply(irf[, , 1:(s + 1), drop = FALSE]^2, c(1, 2), sum)  # suma kwadratów reakcji do horyzontu s
    out[, , s + 1] <- sq / rowSums(sq)
  }
  out
}

# Wielkości, które raportujemy (wszystko dla szoku w d_stopa, przeskalowanego do 25 pb)
# - stopa_poziom: reakcja POZIOMU stopy (skumulowane d_stopa), pp
# - ceny_poziom : reakcja poziomu cen (skumulowana inflacja m/m), %
# - infl_rr     : reakcja inflacji r/r = zmiana poziomu cen w ostatnich 12 mies. (pp)
# - infl_mm     : reakcja inflacji m/m, pp
# - kurs_poziom : reakcja poziomu log EUR/PLN (skumulowane d_eurpln), % (dodatnia = deprecjacja PLN)
# Obsługuje też VAR NA POZIOMACH (zmienne "stopa", "leurpln" zamiast "d_stopa", "d_eurpln"):
# wtedy reakcje stopy i kursu są już poziomami i nie wymagają kumulowania.
transformuj_irf <- function(irf, szok = NULL, skala = SKALA_SZOKU_PP) {
  nz <- dimnames(irf)[[1]]
  roznice <- "d_stopa" %in% nz
  if (is.null(szok)) szok <- if (roznice) "d_stopa" else "stopa"
  kurs <- if (roznice) "d_eurpln" else "leurpln"
  kum <- if (roznice) cumsum else identity
  s <- skala / irf[szok, szok, 1]                       # 25 pb / reakcja stopy w t=0
  r <- irf[, szok, ] * s                                # K x (h+1)
  ceny <- cumsum(r["inflacja_mm", ])
  list(
    stopa_poziom = kum(r[szok, ]),
    infl_mm      = r["inflacja_mm", ],
    ceny_poziom  = ceny,
    infl_rr      = ceny - c(rep(0, 12), head(ceny, -12)),
    kurs_poziom  = kum(r[kurs, ])
  )
}

# Bootstrap rekurencyjny reszt (z korektą stopni swobody). Zwraca listę:
# punkt (IRF z modelu), proby (lista tablic IRF z prób bootstrapowych)
irf_boot <- function(model, h = 36, B = 1000, seed = 2026) {
  set.seed(seed)
  K <- model$K; p <- model$p; T <- model$obs
  y0 <- as.matrix(model$y)
  u  <- resid(model)
  exo <- attr(model, "exogen")
  det_all <- setdiff(colnames(model$datamat), c(colnames(y0), unlist(lapply(1:p, \(l) paste0(colnames(y0), ".l", l)))))
  u  <- scale(u, center = TRUE, scale = FALSE) * sqrt(T / (T - K * p - length(det_all)))
  B_list <- vars::Acoef(model)                          # lista macierzy K x K dla lagów 1..p
  det_names <- setdiff(colnames(model$datamat), c(colnames(y0), unlist(lapply(1:p, \(l) paste0(colnames(y0), ".l", l)))))
  Bdet <- t(sapply(model$varresult, \(r) coef(r)[det_names]))     # K x (stała + dummies)
  Ddet <- as.matrix(model$datamat[, det_names, drop = FALSE])     # T x (...) deterministyczne regresory
  punkt <- irf_tablica(model, h)
  proby <- vector("list", B)
  for (b in seq_len(B)) {
    ub <- u[sample.int(T, T, replace = TRUE), , drop = FALSE]
    ys <- matrix(NA_real_, T + p, K, dimnames = list(NULL, colnames(y0)))
    ys[1:p, ] <- y0[1:p, ]                              # warunki początkowe z danych
    for (t in 1:T) {
      mu <- Bdet %*% Ddet[t, ]
      for (l in 1:p) mu <- mu + B_list[[l]] %*% ys[p + t - l, ]
      ys[p + t, ] <- mu + ub[t, ]
    }
    mb <- vars::VAR(as.data.frame(ys), p = p, type = "const", season = 12, exogen = exo)
    proby[[b]] <- irf_tablica(mb, h)
  }
  list(punkt = punkt, proby = proby, h = h, B = B)
}

# Przedziały percentylowe dla transformowanej wielkości (nazwa z transformuj_irf)
przedzialy <- function(bt, wielkosc, poziomy = c(0.05, 0.16, 0.84, 0.95)) {
  pkt <- transformuj_irf(bt$punkt)[[wielkosc]]
  mat <- sapply(bt$proby, \(x) transformuj_irf(x)[[wielkosc]])   # (h+1) x B
  qs  <- t(apply(mat, 1, quantile, probs = poziomy, na.rm = TRUE))
  tibble::tibble(h = 0:bt$h, punkt = pkt, lo90 = qs[, 1], lo68 = qs[, 2], hi68 = qs[, 3], hi90 = qs[, 4])
}

# Test Grangera dla pary (blok opóźnień zmiennej-przyczyny w równaniu skutku): F klasyczny i odporny HC1
test_pary <- function(model, przyczyna, skutek) {
  p <- model$p
  # równanie VAR odtwarzamy jako zwykłe lm() na macierzy danych modelu (dla odpornych błędów HC1)
  dm <- as.data.frame(model$datamat)
  regresory <- setdiff(colnames(dm), colnames(model$y))
  eq <- lm(reformulate(c("0", regresory), response = skutek), data = dm)
  nazwy <- paste0(przyczyna, ".l", 1:p)
  w <- car::linearHypothesis(eq, nazwy, test = "F",
                             vcov. = sandwich::vcovHC(eq, type = "HC1"))   # odporny na heteroskedastyczność (HC1; HC3 jest niestabilny przy dummies na pojedyncze obserwacje)
  w0 <- car::linearHypothesis(eq, nazwy, test = "F")
  tibble(przyczyna = przyczyna, skutek = skutek, F_klasyczny = w0$F[2], p_klasyczny = w0$`Pr(>F)`[2],
         F_HC1 = w$F[2], p_HC1 = w$`Pr(>F)`[2])
}
