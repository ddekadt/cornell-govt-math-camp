# make-data.R ------------------------------------------------------------
# Generates the teaching datasets. Base R only, no packages required.
# You do not need to run this before class -- the CSVs are already in
# data/raw/. Run it only if you want to change the data or regenerate it.
#
# The data is FAKE. It is shaped like a country-year panel and contains
# five deliberate problems, each of which the Day 2 script demonstrates:
#
#   1. -99 used as a missing-value code in gdp_bn
#   2. pop_mn stored as text because large numbers use thousands separators
#   3. one column named with a space ("Vote Share")
#   4. regime.csv spells country names differently -> country name is a bad key
#   5. regime.csv has a duplicated iso3 row -> joining on it inflates row counts
#      and is missing one country entirely -> anti_join() finds it

set.seed(2026)

countries <- data.frame(
  iso3    = c("USA", "JPN", "DEU", "KOR", "NLD", "BRA",
              "IND", "ZAF", "POL", "MEX", "TUR", "IDN"),
  country = c("United States", "Japan", "Germany", "South Korea",
              "Netherlands", "Brazil", "India", "South Africa",
              "Poland", "Mexico", "Turkey", "Indonesia"),
  region  = c("Americas", "Asia", "Europe", "Asia", "Europe", "Americas",
              "Asia", "Africa", "Europe", "Americas", "Europe", "Asia"),
  gdp0    = c(9000, 4500, 2200, 600, 450, 900, 400, 150, 180, 700, 250, 250),
  pop0    = c(266, 126, 82, 45, 15.5, 162, 960, 42, 38, 92, 58, 200),
  growth  = c(.021, .008, .014, .038, .017, .022, .062, .023, .038, .020, .045, .045)
)

years <- 1995:2020
panel <- expand.grid(iso3 = countries$iso3, year = years, stringsAsFactors = FALSE)
panel <- merge(panel, countries, by = "iso3")
panel <- panel[order(panel$iso3, panel$year), ]

t <- panel$year - 1995
panel$gdp_bn <- round(panel$gdp0 * (1 + panel$growth)^t *
                        exp(rnorm(nrow(panel), 0, .05)), 1)
panel$pop_mn <- round(panel$pop0 * (1 + .006)^t, 1)
panel$exports_bn <- round(panel$gdp_bn * runif(nrow(panel), .08, .45), 1)

# incumbent vote share, loosely increasing in income, plus noise
gdp_pc <- panel$gdp_bn / panel$pop_mn
panel$vote_share <- round(38 + 6 * scale(log(gdp_pc))[, 1] +
                            rnorm(nrow(panel), 0, 4), 1)

out <- panel[, c("iso3", "country", "region", "year",
                 "gdp_bn", "pop_mn", "exports_bn", "vote_share")]

# --- problem 1: -99 missing code -----------------------------------------
out$gdp_bn[sample(nrow(out), 9)] <- -99

# --- problem 2: thousands separators and stray markers make it text ------
pop_txt <- trimws(format(out$pop_mn, big.mark = ",", trim = TRUE))
pop_txt[sample(nrow(out), 5)] <- ".."               # World Bank's missing marker
idx <- sample(nrow(out), 4)
pop_txt[idx] <- paste0(pop_txt[idx], "*")           # footnote markers
out$pop_mn <- pop_txt

# --- a handful of genuinely blank cells ----------------------------------
out$vote_share[sample(nrow(out), 6)] <- NA

# --- problem 3: a column name with a space -------------------------------
names(out)[names(out) == "vote_share"] <- "Vote Share"

write.csv(out, "data/raw/trade-panel.csv", row.names = FALSE, na = "")

# --- problems 4 and 5: the lookup table ----------------------------------
regime <- data.frame(
  iso3    = c("USA", "JPN", "DEU", "KOR", "KOR", "NLD", "BRA",
              "IND", "ZAF", "POL", "MEX", "TUR"),
  country = c("USA", "Japan", "Germany, Fed. Rep.", "Korea, Rep.", "Korea, Rep.",
              "Netherlands", "Brazil", "India", "South Africa",
              "Poland", "Mexico", "Turkiye"),          # spellings differ
  regime_type = c("Democracy", "Democracy", "Democracy", "Democracy", "Democracy",
                  "Democracy", "Democracy", "Democracy", "Democracy",
                  "Democracy", "Democracy", "Hybrid"),
  eu_member = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE,
                FALSE, FALSE, TRUE, FALSE, FALSE)
)
# KOR appears twice (row 4 and 5); IDN is missing entirely
write.csv(regime, "data/raw/regime.csv", row.names = FALSE)

# --- a wide table, for the pivot demo ------------------------------------
wide <- panel[panel$year %in% 2018:2020, c("iso3", "year", "gdp_bn")]
wide <- reshape(wide, idvar = "iso3", timevar = "year", direction = "wide")
names(wide) <- c("iso3", "gdp_2018", "gdp_2019", "gdp_2020")
write.csv(wide, "data/raw/gdp-wide.csv", row.names = FALSE)

cat("Wrote:\n",
    " data/raw/trade-panel.csv (", nrow(out), " rows)\n",
    " data/raw/regime.csv      (", nrow(regime), " rows)\n",
    " data/raw/gdp-wide.csv    (", nrow(wide), " rows)\n", sep = "")
