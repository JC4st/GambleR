# GambleR

GambleR is a bilingual educational R/Shiny application for visualizing electrolyte composition using a Gamblegram.

## Scope

The current version accepts:

- Sodium
- Potassium
- Chloride
- Bicarbonate / total CO2
- Albumin
- Lactate
- Ionized calcium

It displays:

- Cation and anion Gamblegram bars
- Observed anion gap
- Albumin-corrected anion gap
- Estimated albumin charge
- Illustrative residual ions required to preserve visual electroneutrality

## Calculations

Observed anion gap:

AG = Na - (Cl + HCO3)

Albumin-corrected anion gap:

Corrected AG = AG + 2.5 x (4 - albumin in g/dL)

Estimated albumin charge:

Albumin charge = 2.8 x albumin in g/dL

## Important limitation

Residual cations and anions are illustrative components used only to preserve visual electroneutrality. They do not represent measured concentrations or a clinical diagnosis.

GambleR is intended for educational use and is not intended for clinical decision-making.

## Run locally

```r
devtools::load_all() shiny::runApp("app")
```
