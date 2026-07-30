------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# GambleR

GambleR is a bilingual educational R/Shiny application for visualizing acid-base chemistry with a Gamblegram-inspired representation.

## Scope of v0.1

- Anion gap without potassium: `Na - (Cl + HCO3)`
- Albumin-corrected anion gap: `AG + 2.5 * (4 - albumin)`
- Cation bar: sodium, potassium, twice ionized calcium, and an illustrative residual cation compartment
- Anion bar: chloride, bicarbonate/total CO2, lactate, estimated albumin charge, and an illustrative residual anion compartment
- Albumin graphical charge: `2.8 * albumin`
- Educational use only; no definitive diagnoses

## Development setup

``` r
install.packages(c("devtools", "testthat", "shiny", "ggplot2", "dplyr", "tibble", "rlang"))
devtools::load_all()
devtools::test()
```

The Shiny interface will be implemented after the physiological engine and tests are stable.
