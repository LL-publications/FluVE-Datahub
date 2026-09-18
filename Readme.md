Welcome to our data repository!

This repository contains the datasets associated with our manuscript:
“Data dashboard to support meta-analyses on influenza vaccine effectiveness across regions, influenza seasons, and outcomes.”
👉 [Read the preprint here: https://doi.org/10.1101/2025.06.12.25329122]

We conducted a comprehensive literature review of studies conducted between 2011 and 2019 that reported on influenza vaccine effectiveness. In total, we identified 239 peer-reviewed articles. From these, we compiled and curated dashboard-ready datasets in neatly organized Excel files.

"SLR_AnalysisDF_21082026" contains individual vaccine effectiveness estimates;
"IVE_SLR_studies_descriptives_21082026" contains study descriptives of studies with test-negative design; 
"IVE_SLR_datapoints_18092026" contains individual vaccine effectiveness included in these studies; and
"IVE_SLR-All_MA_Outputs_21082026" contains all meta-analyses.

These datasets support stratified analyses by:

- Geographic region

- Influenza season

- Clinical outcome

- Influenza subtype

- Vaccine characteristics (valency, type, dose)

- Risk group

- Medical attendance type

We encourage you to download, explore, and analyze the data. You’re also welcome to build on it by incorporating more recent data.

The RStudio project contains the reproducibility materials for the fixed-effect meta-analyses summarised in Table S2 of the manuscript.

## Project contents

- `IVE_fixed_effect_meta_analysis.R` — analysis and forest-plot script.
- `IVE_SLR_datapoints_18092026.xlsx` — supplied analysis dataset.
- `example_outputs/` — outputs from the previously verified run.
- `output/` — created when the script is run interactively in this project.


## Analysis scope

The script selects estimates with:

- test-negative design (`est_design == "tn"`);
- laboratory-confirmed infection (`est_outbeyer == "lab"`);
- adjusted vaccine effectiveness (`est_measure == "ave"`); and
- the relevant `virustype_rec2` inclusion variable.

It runs fixed-effect models for Influenza A, Influenza B, and Any influenza,
each for All, Under 5's, General population, and 65 and over.


## Run in RStudio

1. Open `IVE_meta_analysis_reproducibility.Rproj`.
2. Open `IVE_fixed_effect_meta_analysis.R`.
3. Confirm that the required packages are installed: `readxl`, `dplyr`, and
   `metafor`.
4. Click **Source** or run `source("IVE_fixed_effect_meta_analysis.R")`.

The run writes a results CSV, an example forest-plot PDF, and R session
information to `output/`. The example plot is currently configured as
Influenza A in children under 5; its two configuration labels near the top of
the script can be changed to another Table S2 analysis.


Thanks for visiting, and happy analyzing!

[![License: CC BY-NC-ND 4.0](https://img.shields.io/badge/License-CC%20BY--NC--ND%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-nc-nd/4.0/)

This work is licensed under a [Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License](https://creativecommons.org/licenses/by-nc-nd/4.0/).
