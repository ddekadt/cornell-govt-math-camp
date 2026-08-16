# =====================================================================
# Math Camp 2026 -- Day 2
# Calculus - Linear algebra - Data wrangling
#
# Runs top to bottom with no external files: Section 0 writes its own
# messy raw data into a folder, and the rest of the script cleans it.
#
# Requires: R >= 4.1 (for the |> pipe), tidyverse with dplyr >= 1.1
#           (for join_by(), relationship =, and .default in case_when)
#
# Run it a chunk at a time. Read the output after every step.
# =====================================================================

library(tidyverse)

hd <- function(x) cat("\n\n==================", x, "==================\n")


# =====================================================================
# PART I -- CALCULUS                                     (slides 4-15)
# =====================================================================

# --- Slide 5: the derivative as a limit ------------------------------
# f'(x) = lim (f(x+h) - f(x)) / h.  Watch the difference quotient
# converge as h shrinks. f(x) = x^2 at x = 3, so the answer is 6.

hd("Slide 5: the limit definition")

f <- function(x) x^2
h <- c(1, 0.1, 0.01, 0.001, 0.0001)
print(tibble(h = h, slope = (f(3 + h) - f(3)) / h))


# --- Slides 6-9: the rules, symbolically -----------------------------
# D() differentiates an expression. Compare each answer to the slide.

hd("Slides 6-9: symbolic derivatives")

D(expression(x^3), "x")                  # power rule
D(expression(5*x^2 - 2*x + 7), "x")      # term by term
D(expression(x^2 * exp(x)), "x")         # product rule
D(expression(x / (x + 1)), "x")          # quotient rule
D(expression(log(3 * x^2)), "x")         # chain rule, simplifies to 2/x
D(expression(exp(-x^2 / 2)), "x")        # the normal kernel


# --- Slides 11-12: FOC and SOC ---------------------------------------
# f(x) = -x^2 + 4x + 1.  FOC: f'(x) = 0.  SOC: sign of f''.

hd("Slides 11-12: optimize -x^2 + 4x + 1")

expr  <- expression(-x^2 + 4*x + 1)
d1    <- D(expr, "x")
d2    <- D(d1, "x")
print(d1)
print(d2)

# solve f'(x) = 0 numerically over an interval
foc <- uniroot(function(x) eval(d1, list(x = x)), interval = c(-10, 10))
x_star <- foc$root
cat("critical point x* =", x_star, "\n")
cat("f''(x*)          =", eval(d2, list(x = x_star)), "-> negative, so a maximum\n")

# same answer straight from an optimizer
print(optimize(function(x) -x^2 + 4*x + 1, interval = c(-10, 10), maximum = TRUE))

# a function where the SOC actually has work to do: x^3 - 3x
hd("Slide 12: two critical points, two kinds")
g1 <- D(expression(x^3 - 3*x), "x")      # 3x^2 - 3, zero at x = -1 and x = 1
g2 <- D(g1, "x")                          # 6x
for (cp in c(-1, 1)) {
  cat("x* =", cp, " f''(x*) =", eval(g2, list(x = cp)),
      if (eval(g2, list(x = cp)) < 0) " -> maximum\n" else " -> minimum\n")
}


# --- Slide 13: partial derivatives and the gradient -------------------
# f(x, y) = x^2 y + 3y^2

hd("Slide 13: partial derivatives")

fxy <- expression(x^2 * y + 3 * y^2)
D(fxy, "x")   # 2xy
D(fxy, "y")   # x^2 + 6y

# the gradient at (2, 1), stacked into a vector
grad <- c(eval(D(fxy, "x"), list(x = 2, y = 1)),
          eval(D(fxy, "y"), list(x = 2, y = 1)))
print(grad)


# --- Slides 14-15: integration ---------------------------------------
# A density integrates to 1. An expectation is a weighted average.

hd("Slides 14-15: integration")

print(integrate(dnorm, -Inf, Inf))                         # = 1
print(integrate(function(x) x * dnorm(x, mean = 3), -Inf, Inf))  # = 3, the mean
print(integrate(dnorm, -1.96, 1.96))                       # = 0.95, the familiar area


# =====================================================================
# PART II -- LINEAR ALGEBRA                             (slides 17-23)
# =====================================================================

# --- Slide 18: vectors and the dot product ---------------------------

hd("Slide 18: vectors")

a <- c(2, 5, 1)
b <- c(1, 0, 4)

a + b            # element-wise
3 * a            # scalar multiplication
sum(a * b)       # dot product, the long way: 2*1 + 5*0 + 1*4 = 6
a %*% b          # dot product, matrix style (returns a 1x1 matrix)


# --- Slide 19: matrices, dimensions, transpose -----------------------

hd("Slide 19: matrices")

X <- matrix(c(1, 3,
              1, 5,
              1, 2), nrow = 3, byrow = TRUE)
print(X)
dim(X)           # 3 2  -- always rows first
X[2, 1]          # entry in row 2, column 1
t(X)             # transpose: now 2 x 3


# --- Slide 20: multiplication and conformability ---------------------

hd("Slide 20: conformability")

A <- matrix(1:6, nrow = 2)      # 2 x 3
B <- matrix(1:12, nrow = 3)     # 3 x 4
dim(A %*% B)                    # inner dims match at 3 -> result is 2 x 4

# the reverse order does not exist: 3x4 times 2x3 has inner dims 4 and 2
tryCatch(B %*% A, error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

# (AB)' = B'A'
all.equal(t(A %*% B), t(B) %*% t(A))


# --- Slides 21-22: identity, inverse, solving a system ---------------

hd("Slides 21-22: inverse and Ax = b")

I3 <- diag(3)
print(I3)

Amat <- matrix(c(2, 1,
                 1, -1), nrow = 2, byrow = TRUE)
bvec <- c(5, 1)

solve(Amat)                     # the inverse
solve(Amat) %*% bvec            # x = A^{-1} b
solve(Amat, bvec)               # better in practice: same answer, more stable

# Singularity = perfect collinearity. Column 3 below is col1 + col2.
S <- cbind(c(1, 2, 3), c(4, 5, 6), c(5, 7, 9))
tryCatch(solve(S), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))


# =====================================================================
# PART III -- DATA WRANGLING                            (slides 26-43)
# =====================================================================

# --- Slide 28: projects and paths -------------------------------------
# Real projects: open the .Rproj and use relative paths. Here we build
# the same folder structure so the paths below look like real ones.

proj <- "math-camp-demo"
dir.create(file.path(proj, "data", "raw"),   recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(proj, "data", "clean"), recursive = TRUE, showWarnings = FALSE)
cat("Files will be written under:", normalizePath(proj), "\n")


# --- Section 0: manufacture the messy raw data ------------------------
# This block exists only so the script is self-contained. It plants,
# on purpose, every problem the slides warn about.

set.seed(6029)

countries <- tribble(
  ~country_code, ~country,         ~region,    ~eu,
  "USA",         "United States",  "Americas", 0,
  "JPN",         "Japan",          "Asia",     0,
  "CHN",         "China",          "Asia",     0,
  "KOR",         "Korea, Rep.",    "Asia",     0,
  "DEU",         "Germany",        "Europe",   1,
  "FRA",         "France",         "Europe",   1,
  "GBR",         "United Kingdom", "Europe",   0,
  "BRA",         "Brazil",         "Americas", 0
)

raw_tbl <- expand_grid(countries, year = 1985:2020) |>
  mutate(
    gdp = round(exp(rnorm(n(), 7, 0.8)), 1),
    pop = round(exp(rnorm(n(), 3.5, 0.9)), 1),
    notes  = "",
    source = "synthetic"
  )

# gdp becomes text, with missing codes and one stray footnote character
idx <- sample(seq_len(nrow(raw_tbl)))
raw_tbl$gdp <- as.character(raw_tbl$gdp)
raw_tbl$gdp[idx[1:10]]  <- ".."                                   # a missing code
raw_tbl$gdp[idx[11:20]] <- "-99"                                  # another one
raw_tbl$gdp[idx[21:23]] <- paste0(raw_tbl$gdp[idx[21:23]], "*")   # the footnote
raw_tbl$pop[idx[24:29]] <- NA                                     # honest NAs

# the same country spelled three ways
raw_tbl$country[raw_tbl$country_code == "USA" & raw_tbl$year %in% 2015:2018] <- "USA"
raw_tbl$country[raw_tbl$country_code == "USA" & raw_tbl$year == 2019]        <- "U.S.A."

write_csv(raw_tbl, file.path(proj, "data/raw/wdi.csv"))

# a regime dataset: BRA is missing entirely, and JPN 1995 appears twice
regimes_tbl <- expand_grid(
  iso3 = setdiff(countries$country_code, "BRA"),
  year = 1985:2020
) |>
  mutate(regime = if_else(iso3 %in% c("CHN"), "autocracy", "democracy"))

regimes_tbl <- bind_rows(regimes_tbl,
                         filter(regimes_tbl, iso3 == "JPN", year == 1995))

write_rds(regimes_tbl, file.path(proj, "data/raw/regimes.rds"))

# a wide file, for the pivoting section
raw_tbl |>
  filter(year %in% 2018:2020) |>
  mutate(gdp = parse_number(gdp)) |>
  select(country = country_code, year, gdp) |>
  pivot_wider(names_from = year, values_from = gdp, names_prefix = "gdp_") |>
  write_csv(file.path(proj, "data/raw/wdi_wide.csv"))

rm(raw_tbl, regimes_tbl, idx)   # forget we ever saw them; read from disk like normal


# --- Slide 29: reading files ------------------------------------------
# Declare the missing-value codes on the way in. Read the column
# specification R prints -- it is telling you what it guessed.

hd("Slide 29: read the file")

raw <- read_csv(
  file.path(proj, "data/raw/wdi.csv"),
  na = c("", "NA", ".."),                        # note: -99 handled later, on purpose
  col_types = cols(country_code = col_character(), year = col_integer())
)


# --- Slides 30-31: first look -----------------------------------------

hd("Slides 30-31: inspect before you touch anything")

glimpse(raw)
summary(raw)
count(raw, country)          # three spellings of the United States
count(raw, region)
nrow(raw); ncol(raw); names(raw)

# gdp came in as <chr>. Find out why: which values are not numbers?
raw |>
  filter(!is.na(gdp), is.na(suppressWarnings(as.numeric(gdp)))) |>
  count(gdp)


# --- Slide 32: reading an error message -------------------------------

hd("Slide 32: what errors look like")

tryCatch(raw |> filter(yaer > 2000),
         error = function(e) cat("ERROR:", conditionMessage(e), "\n"))
tryCatch(raw$gdp + 1,
         error = function(e) cat("ERROR:", conditionMessage(e), "\n"))


# --- Slides 33-36: the pipe and the verbs ------------------------------

hd("Slides 33-36: filter, select, mutate")

# filter(): keeping rows
raw |> filter(year >= 2010, region == "Asia")
raw |> filter(region %in% c("Asia", "Europe"))
raw |> filter(between(year, 1990, 2000))
raw |> filter(is.na(pop))

# filter() silently drops NA rows -- compare these two counts
cat("pop > 50            :", nrow(filter(raw, pop > 50)), "rows\n")
cat("pop > 50 | is.na(pop):", nrow(filter(raw, pop > 50 | is.na(pop))), "rows\n")

# select() and rename(), then mutate() to fix the columns
clean <- raw |>
  select(iso3 = country_code, region, year, gdp, pop, eu) |>   # keep, rename, order
  filter(year >= 1990) |>
  mutate(
    gdp     = parse_number(gdp),        # strips the footnote character
    gdp     = na_if(gdp, -99),          # sentinel value -> real NA
    gdp_pc  = gdp / pop,
    log_gdp = log(gdp),
    large   = if_else(pop > 50, "large", "small"),
    bloc    = case_when(
      region == "Europe" & eu == 1 ~ "EU",
      region == "Europe"           ~ "non-EU Europe",
      .default                     = "other"
    )
  )

glimpse(clean)
count(clean, bloc)
cat("NAs in gdp after cleaning:", sum(is.na(clean$gdp)), "\n")


# --- Slide 37: group_by() and summarize() ------------------------------

hd("Slide 37: grouped summaries")

clean |>
  group_by(region, year) |>
  summarize(mean_gdp = mean(gdp, na.rm = TRUE),
            n        = n(),
            .groups  = "drop")

# mutate() on grouped data keeps every row -- group means and shares
clean |>
  group_by(region, year) |>
  mutate(region_share = pop / sum(pop, na.rm = TRUE)) |>
  ungroup() |>
  select(iso3, year, region, pop, region_share)

# without na.rm, one missing value poisons the whole group
clean |>
  group_by(region) |>
  summarize(with_narm = mean(gdp, na.rm = TRUE),
            without   = mean(gdp),
            .groups   = "drop")


# --- Slides 38-39: joins, and a join you can trust ----------------------

hd("Slides 38-39: joins")

regimes <- read_rds(file.path(proj, "data/raw/regimes.rds")) |>
  select(iso3, year, regime)

# 1. row count BEFORE
n_before <- nrow(clean)
cat("rows before join:", n_before, "\n")

# 2. anti_join FIRST -- what will fail to match, and is it systematic?
anti_join(clean, regimes, by = join_by(iso3, year)) |> count(iso3)

# the naive join: no error, no warning, but the row count grew
bad <- left_join(clean, regimes, by = join_by(iso3, year))
cat("rows after naive join:", nrow(bad), " <- silently duplicated\n")

# 3. state the relationship you expect; R errors when it is violated
tryCatch(
  left_join(clean, regimes, by = join_by(iso3, year), relationship = "one-to-one"),
  error = function(e) cat("ERROR:", conditionMessage(e), "\n")
)

# find the offending key, then fix the source
regimes |> count(iso3, year) |> filter(n > 1)
regimes <- distinct(regimes, iso3, year, .keep_all = TRUE)

panel <- clean |>
  left_join(regimes, by = join_by(iso3, year), relationship = "one-to-one")

# 4. assert the row count AFTER
stopifnot(nrow(panel) == n_before)
cat("rows after clean join:", nrow(panel), " -- assertion passed\n")

# why country names are a bad key
tibble(name = c("Korea, Rep.", "United States")) |>
  anti_join(tibble(name = c("South Korea", "United States")), by = "name")


# --- Slides 40-41: tidy data and pivoting -------------------------------

hd("Slides 40-41: pivoting")

wide <- read_csv(file.path(proj, "data/raw/wdi_wide.csv"), show_col_types = FALSE)
print(wide)     # year is hiding in the column names

long <- wide |>
  pivot_longer(
    cols            = starts_with("gdp_"),
    names_to        = "year",
    names_prefix    = "gdp_",
    names_transform = list(year = as.integer),
    values_to       = "gdp"
  )
print(long)

wide_again <- long |> pivot_wider(names_from = year, values_from = gdp)
print(wide_again)


# --- Slide 42: missing values -------------------------------------------

hd("Slide 42: NA behaviour")

NA > 5                          # NA, not FALSE
mean(c(1, NA))                  # NA
mean(c(1, NA), na.rm = TRUE)    # 1
NA == NA                        # NA -- this is why you need is.na()

sum(is.na(panel$gdp))
colSums(is.na(panel))           # missingness by column, in one line

cat("rows kept by !is.na(gdp):", nrow(filter(panel, !is.na(gdp))), "\n")
cat("rows kept by drop_na()  :", nrow(drop_na(panel)), " <- blunter than you think\n")

# is missingness related to anything substantive? A technical question
# with a substantive answer.
panel |>
  group_by(region) |>
  summarize(share_missing = mean(is.na(gdp)), .groups = "drop")


# --- Slide 43: putting it together ---------------------------------------

hd("Slide 43: save the analysis-ready table")

analysis <- panel |>
  select(iso3, region, year, gdp, pop, gdp_pc, log_gdp, eu, bloc, regime)

write_rds(analysis, file.path(proj, "data/clean/panel.rds"))
cat("wrote", file.path(proj, "data/clean/panel.rds"), "with",
    nrow(analysis), "rows and", ncol(analysis), "columns\n")


# =====================================================================
# TYING THE THREE PARTS TOGETHER
# How does a table of numbers become an estimate?
# =====================================================================

hd("y = X beta + epsilon, on the table we just built")

est <- analysis |>
  drop_na(gdp_pc, pop) |>
  mutate(log_gdp_pc = log(gdp_pc), log_pop = log(pop))

y <- est$log_gdp_pc                                   # n x 1
X <- model.matrix(~ log_pop + eu, data = est)         # n x k, ones in column 1

cat("dim(X):", dim(X), "   length(y):", length(y), "\n")
head(X, 3)

# beta_hat = (X'X)^{-1} X'y, exactly as on slide 23
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
print(beta_hat)

# what lm() reports
fit <- lm(log_gdp_pc ~ log_pop + eu, data = est)
print(coef(fit))

all.equal(as.vector(beta_hat), unname(coef(fit)))     # TRUE

# and the collinearity warning made concrete: a full set of region
# dummies alongside the intercept makes X'X singular
Xbad <- cbind(1, model.matrix(~ region - 1, data = est))
tryCatch(solve(t(Xbad) %*% Xbad),
         error = function(e) cat("ERROR:", conditionMessage(e), "\n"))

# lm() does not error -- it silently drops the redundant column
print(coef(lm(log_gdp_pc ~ region, data = est)))

hd("Done. Monday: probability, then visualization and Quarto.")
