# =============================================================================
# Math Camp 2026 -- Day 3 afternoon
# Visualization with ggplot2
#
# HOW TO USE THIS FILE
#   Run it line by line (Cmd-Enter), top to bottom. Do NOT source it.
#   Each section below matches one slide.
#   Sections marked [BREAKS ON PURPOSE] are meant to fail. Run them anyway.
# =============================================================================


# -----------------------------------------------------------------------------
# 00  Setup
# -----------------------------------------------------------------------------

library(tidyverse)

analysis <- readRDS("data/clean/analysis.rds")

glimpse(analysis)

# If the line above errored with "cannot open file", you do not have
# yesterday's saved data. Run section 00b instead, then carry on from 01.


# -----------------------------------------------------------------------------
# 00b  Rebuild the data (ONLY if you lost yesterday's file)
#
#      This regenerates a panel with the same shape as yesterday's.
#      Your numbers will differ slightly from the ones on the slides.
# -----------------------------------------------------------------------------

set.seed(1234)

countries <- c("Arcadia", "Belgravia", "Caledonia", "Dunmore",
               "Estoria", "Falderan", "Gravia", "Hesperia",
               "Ilyria", "Javara", "Kestrel", "Lorena")

lookup <- tibble(
  country = countries,
  region  = rep(c("Americas", "Europe", "Asia", "Africa"), each = 3)
)

analysis <- expand_grid(country = countries, year = 1995:2020) |>
  left_join(lookup, by = "country") |>
  group_by(country) |>
  mutate(
    base    = rnorm(1, mean = 9.4, sd = 0.7),
    growth  = rnorm(1, mean = 0.018, sd = 0.008),
    gdp_pc  = exp(base + growth * (year - 1995) + rnorm(n(), 0, 0.05))
  ) |>
  ungroup() |>
  mutate(
    vote_share = -8 + 4.9 * log(gdp_pc) + rnorm(n(), 0, 6),
    vote_share = pmin(pmax(vote_share, 5), 85)
  ) |>
  select(country, region, year, gdp_pc, vote_share)

# Put 20 values missing, so the "Removed 20 rows" message shows up later
analysis$vote_share[sample(nrow(analysis), 20)] <- NA

glimpse(analysis)
nrow(analysis)            # 312
n_distinct(analysis$country)  # 12


# -----------------------------------------------------------------------------
# 01  Data and mapping, no geometry
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share))

# Axes, ranges, labels -- and nothing drawn. No geometry was specified.


# -----------------------------------------------------------------------------
# 02  Add a geometry
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point()

# Now change ONLY the geom. Everything before the + stays put.

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_hex()

ggplot(analysis, aes(x = region, y = vote_share)) +
  geom_boxplot()


# -----------------------------------------------------------------------------
# 03  Overplotting
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.4)

# Try 0.1 and 1.0 and compare.


# -----------------------------------------------------------------------------
# 04  Log scales
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.4) +
  scale_x_log10()

# The data did not change. Only the display did.


# -----------------------------------------------------------------------------
# 05  aes(): the error                                  [BREAKS ON PURPOSE]
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(color = "region")

# Error: Invalid colour: region
# geom_point() read "region" as the name of a colour.


# -----------------------------------------------------------------------------
# 06  aes(): the version that does NOT stop you
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share, color = "steelblue")) +
  geom_point()

# A plot appears. The points are red. There is a one-entry legend
# labelled "steelblue".
#
# No error, no warning, wrong figure. This is the one to fear.


# -----------------------------------------------------------------------------
# 07  aes(): both correct versions
# -----------------------------------------------------------------------------

# Colour VARIES with the data -> inside aes()
ggplot(analysis, aes(x = gdp_pc, y = vote_share, color = region)) +
  geom_point(alpha = 0.6) +
  scale_x_log10()

# Colour is CONSTANT -> outside aes()
ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  scale_x_log10()

# The rule: varies with the data -> inside. Constant -> outside.
# Same rule for size, shape, alpha, fill, linewidth.


# -----------------------------------------------------------------------------
# 08  One variable: histogram
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = vote_share)) +
  geom_histogram(bins = 30)

# Run these three and watch the shape move:
ggplot(analysis, aes(x = vote_share)) + geom_histogram(bins = 10)
ggplot(analysis, aes(x = vote_share)) + geom_histogram(bins = 60)

# Leave bins out entirely and read the message ggplot gives you:
ggplot(analysis, aes(x = vote_share)) + geom_histogram()


# -----------------------------------------------------------------------------
# 09  One variable: density
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = vote_share)) +
  geom_density()

# adjust is the bandwidth. The histogram shows its arbitrariness;
# the density curve hides it.
ggplot(analysis, aes(x = vote_share)) + geom_density(adjust = 0.3)
ggplot(analysis, aes(x = vote_share)) + geom_density(adjust = 2)

# Both at once, a cheap habit:
ggplot(analysis, aes(x = vote_share)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "grey80", color = "white") +
  geom_density(linewidth = 0.9)


# -----------------------------------------------------------------------------
# 10  Comparing groups: boxplot
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = region, y = vote_share)) +
  geom_boxplot()

# A boxplot hides multimodality by construction. Show the points:
ggplot(analysis, aes(x = region, y = vote_share)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.25)


# -----------------------------------------------------------------------------
# 11  Ordering categories
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = fct_reorder(region, vote_share),
                     y = vote_share)) +
  geom_boxplot() +
  labs(x = "Region")

# Alphabetical order carries no information about your data.


# -----------------------------------------------------------------------------
# 12  Lines: the group trap
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = year, y = gdp_pc)) +
  geom_line(alpha = 0.4)

# One jagged line. ggplot connected all 312 rows in x order, because
# nothing told it which rows belong to the same country.


# -----------------------------------------------------------------------------
# 13  Lines: fixed
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = year, y = gdp_pc, group = country)) +
  geom_line(alpha = 0.4)

# Twelve lines. Compare with mapping colour instead -- note the legend:
ggplot(analysis, aes(x = year, y = gdp_pc, color = country)) +
  geom_line(alpha = 0.7)


# -----------------------------------------------------------------------------
# 14  Faceting
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = year, y = gdp_pc, group = country)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ region)

# Free scales: shape within a panel, at the cost of comparing levels
ggplot(analysis, aes(x = year, y = gdp_pc, group = country)) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ region, scales = "free_y")


# -----------------------------------------------------------------------------
# 15  Adding a fitted line
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_x_log10()

# Layers draw in the order you add them. Swap the two geoms and the
# points cover the line:
ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.3) +
  scale_x_log10()

# Default is loess, not a straight line:
ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.3) +
  geom_smooth()


# -----------------------------------------------------------------------------
# 16  Piping into ggplot
# -----------------------------------------------------------------------------

analysis |>
  group_by(region, year) |>
  summarize(mean_gdp_pc = mean(gdp_pc, na.rm = TRUE),
            .groups = "drop") |>
  ggplot(aes(x = year, y = mean_gdp_pc, color = region)) +
  geom_line(linewidth = 0.9)

# Watch the switch: |> up to ggplot(), + after it.


# -----------------------------------------------------------------------------
# 17  Bars
# -----------------------------------------------------------------------------

analysis |>
  filter(year == 2020) |>
  slice_max(gdp_pc, n = 8) |>
  ggplot(aes(x = fct_reorder(country, gdp_pc), y = gdp_pc)) +
  geom_col(fill = "grey30") +
  coord_flip() +
  labs(x = NULL, y = "GDP per capita (USD)")

# geom_col() draws the values you supply.
# geom_bar() counts rows for you:
ggplot(analysis, aes(x = region)) + geom_bar()


# -----------------------------------------------------------------------------
# 18  Labels
# -----------------------------------------------------------------------------

ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(
    title    = "Income and incumbent support",
    subtitle = "12 countries, 1995-2020",
    x        = "GDP per capita, USD (log scale)",
    y        = "Incumbent vote share (%)",
    caption  = "Source: simulated data for teaching"
  )

# Axis labels with units, every time. gdp_pc means nothing to a reader.


# -----------------------------------------------------------------------------
# 19  Themes
# -----------------------------------------------------------------------------

p_base <- ggplot(analysis, aes(x = gdp_pc, y = vote_share)) +
  geom_point(alpha = 0.4) +
  scale_x_log10() +
  labs(x = "GDP per capita, USD (log scale)",
       y = "Incumbent vote share (%)")

p_base + theme_minimal(base_size = 12)
p_base + theme_bw()
p_base + theme_classic()

# base_size scales every piece of text at once:
p_base + theme_minimal(base_size = 18)

# Fine control:
p_base +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())


# -----------------------------------------------------------------------------
# 20  Saving
# -----------------------------------------------------------------------------

dir.create("output", showWarnings = FALSE)

p <- ggplot(analysis, aes(gdp_pc, vote_share)) +
  geom_point(alpha = 0.35) +
  labs(x = "GDP per capita, USD", y = "Vote share (%)") +
  theme_minimal(base_size = 12)

ggsave("output/fig1.pdf", p, width = 6.5, height = 4.5)
ggsave("output/fig1.png", p, width = 6.5, height = 4.5, dpi = 300)

# The ggsave surprise: text size is absolute, not relative.
# Open both of these and compare the label sizes.
ggsave("output/fig-small.png", p, width = 4, height = 3, dpi = 300)
ggsave("output/fig-large.png", p, width = 10, height = 7, dpi = 300)

# The fix for "labels came out too small" is a SMALLER canvas, not a bigger one.


# -----------------------------------------------------------------------------
# 21  Colour that everyone can read
# -----------------------------------------------------------------------------

ggplot(analysis, aes(year, gdp_pc, color = region, group = country)) +
  geom_line(alpha = 0.7) +
  scale_color_viridis_d() +
  theme_minimal()

# _d() for discrete variables, _c() for continuous:
ggplot(analysis, aes(gdp_pc, vote_share, color = year)) +
  geom_point() +
  scale_color_viridis_c() +
  scale_x_log10()


# -----------------------------------------------------------------------------
# 22  Errors, translated                                [BREAKS ON PURPOSE]
# -----------------------------------------------------------------------------

# (a) a column name used outside aes()
ggplot(analysis, aes(x = gdp_pc)) + geom_point(y = vote_share)
#   object 'vote_share' not found

# (b) a variable name outside aes()
ggplot(analysis, aes(x = gdp_pc, y = vote_share)) + geom_point(color = region)
#   object 'region' not found

# (c) not an error -- ggplot telling you to choose
ggplot(analysis, aes(x = vote_share)) + geom_histogram()
#   stat_bin() using bins = 30

# (d) not an error -- ggplot telling you about your data
ggplot(analysis, aes(x = gdp_pc, y = vote_share)) + geom_point()
#   Removed 20 rows containing missing values

# (c) and (d) are the two worth pausing on. Both are true statements
# about your data, not complaints about your code.


# -----------------------------------------------------------------------------
# 23  On your own
# -----------------------------------------------------------------------------

# 1. Plot vote_share against year, one line per country, faceted by region.
# 2. Colour those lines by region instead of faceting. Which reads better?
# 3. Make a histogram of gdp_pc. Then put it on a log x axis. What changed?
# 4. Take any figure above, label it properly, and save it at 5 x 3.5 inches.
# 5. Break something on purpose and read the message before fixing it.
