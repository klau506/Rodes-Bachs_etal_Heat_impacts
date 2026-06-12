# Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

Source code, data processing pipelines, analytical, and visualization workflows for the journal publication **Rodés-Bachs, C., Vicedo-Cabrera, A., Sampedro, J., Masselot, P., Koasidis, K., & Van de Ven, D. (2026).
Heat-related health impacts across national mitigation and urban adaptation scenarios in European cities.** Preprint available [here]().


## Abstract
Heat exposure is a critical health risk, yet health outcomes and age-related vulnerability remain underrepresented in climate scenario analysis. This study bridges this gap by presenting a novel framework for estimating heat-health impacts across policy-relevant mitigation scenarios and adaptation strategies. By fusing integrated assessment modeling, climate emulators, and epidemiological methods with Urban Heat Island and age-specific mortality data, we quantify heat impacts across 854 European cities. This research provides a robust methodological foundation to facilitate the integration of public health dimensions into strategic urban and national policy design.

## Related repositories:
*   Preprint publication: Available soon
*   [Data Zenodo archive](https://zenodo.org/uploads/20666094): Processed data to reproduce the figures and the analysis.
*   [Code Zenodo archive](): Code with the methodological pipeline, data download and preprocessing, analysis and figures' creation.
*   [GCAM-Europe 7.2.0](https://github.com/bc3LC-GCAMEurope/gcam-core/releases/tag/gcam-europe-v7.2.0): GCAM version used to run the BASE and MITIG scenarios.
*   [STITCHES](https://github.com/JGCRI/stitches/tree/daily-data-refinements): Downscaling procedure for GCAM-Europe projected temperature.
*   [BASE](https://github.com/JGCRI/basd): Bias Adjustment and Statistical Downscaling procedure.


## What's in this repo?

This repository provides the complete source data download code, computational workflows, analysis, and data visualization code used in the study.


### 1. Data Download Scripts
*   `python/tree_cover_EUROSTAT.ipynb`: Downloads and aggregates tree coverage data.
*   `java/modis2.js`: Downloads land surface grid temperature data from MODIS. Designed to run on the [Google Earth Engine Console](https://console.cloud.google.com/earth-engine).
*   `java/pml_v2.js`: Downloads evapotranspiration grid data from PML_V2. Designed to run on the [Google Earth Engine Console](https://console.cloud.google.com/earth-engine).

---

### 2. Methodological Workflow
All main pipeline scripts are located in the `R/` folder and ordered sequentially by number. 

> ⚠️ **Note on Computation:** Some scripts require high-performance computing capabilities and should be launched in parallel on a cluster computer. Requirements and configurations are specified in the header of each individual script.

To execute the entire workflow from scratch, ensure all downloaded source datasets are placed inside the `data/` directory before running the scripts.

You need to 1) run the Data Download Scripts detailed in the previous step; 2) download the following datasets:
*   `U2018_CLC2018_V2020_20u1`: Land cover by grid cell in Europe; DOI https://doi.org/10.2909/960998c1-1870-4e82-8051-6485205ebbac; Last accessed March 2026. [Resource link](https://land.copernicus.eu/en/products/corine-land-cover/clc2018).
*   `Tree cover UK`: UK tree cover; Last accessed March 2026. [Resource link](https://uk.treeequityscore.org/).
*   `Cerra data`: 3hourly 2m land surface temperature from 1990 to 2019; DOI 10.24381/cds.622a565a; Last accessed March 2026. [Resource link](https://cds.climate.copernicus.eu/datasets/reanalysis-cerra-single-levels?tab=overview/).
  
Further details in Table S2 of the SI.


---

### 3. Analysis & Figures
*   `R/10_figures.R`: Generates primary manuscript figures.
*   `R/10_figures_si.R`: Generates Supplementary Information (SI) figures.

**Execution Order:** These scripts can be run standalone, but you **must run `R/10_figures.R` first**, as the SI script relies on data loaded and cached by the primary figures script. 

**Preprocessed Data:** If you want to skip the execution workflow and proceed straight to the analysis, the preprocessed datasets can be downloaded directly from our [Data Zenodo Repository](https://zenodo.org/uploads/20666094).

---

## Contact & Support
If you have any questions, feedback, or suggestions, please feel free to reach out to:

*   **Email:** [claudia.rodes@bc3research.org](mailto:claudia.rodes@bc3research.org)
*   **Issues:** Alternatively, you can open an issue directly in this repository.


## Funding acknowledgement

<img src="./logo.png" alt="DIAMOND logo" width="130" height="40" align="left"/>
This project has received funding from the European Union's Horizon 2020 research and innovation program under grant agreement number 101081179 (DIAMOND project).
