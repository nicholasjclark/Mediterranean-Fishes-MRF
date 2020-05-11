
<!-- README.md is generated from README.Rmd. Please edit that file -->

<img src="Images/Supp_fig_CRF_schematic.tiff" width="100%" />

This repository contains `R` scripts to extract data and replicate
Conditional Random Fields analyses used in Clark *et al* (IN PRESS in
*Nature Climate Change*)

**Workflow**  
The workflow to extract primary data and complete analyses is as
follows:

1.  Download open-source data on coastal Mediterranean fish species
    occurrences, functional traits and phylogenetic relationships, as
    well as historical and projected Sea Surface Temperature data from
    the [FishMed](http://www.esapubs.org/archive/ecol/E096/203/)
    database (Albouy et al. 2015). Note that these data are licensed and
    cannot be stored in this repository. However, `R` functions in the
    `Functions` directory are available to download the data into your
    working environment. Please follow steps in
    `Workflow/Appendix_S1_Datacollation.pdf` to extract the data

2.  Run Conditional Random Fields models using functions available in
    the `MRFcov` package (Clark et al., 2018) to analyse the binary
    presence-absence data downloaded in Step 1 above. While `R` code in
    `Workflow/Appendix_S2_Models.pdf` is available to replicate this
    step, note that a previous version of `MRFcov` was used and so some
    of the argument names may have changed but most steps should still
    work. Alos please note that these functions were originally run on a
    high-performance computing cluster using 24 processing cores to
    speed up computations. The models will take several hours or more to
    complete even on a powerful machine. We have provided the original
    results to save processing time. These are available in
    `Results/MRF.results.rda` and can be loaded directly into `R` using
    the `load()` command.

3.  Process the models to analyse their results and generate summary
    figures as shown in the original manuscript. The steps are outlined
    in `Workflow/Appendix_S3_ModelProcessing.pdf`. Most of these steps
    should be achievable even on personal laptops, apart from the
    fitting of the multivariate boosted regression trees (which rely on
    the `mvtboost` package, see below for instructions for downloading).
    This step is not partciularly slow but is very memory-heavy because
    each set of `500` regression trees is saved for each of the `215`
    fish species in order to find predictors that minimise joint
    prediction error. We have provided the original results of this step
    to save processing time. These are available in the
    `Results/mvtb.results.rda` and can be loaded directly into `R` using
    the `load()` command.

**Key packages needed for analyses and making
figures**  
`devtools`  
`ggplot2`  
`dplyr`  
`readxl`  
`rvest`  
`tidyverse`  
`stringr`  
`sf`  
`gstat`  
`sp`  
`ape`  
`viridis`  
`MRFcov`  
`mgcv`  
`visreg`  
`PhyloMeasures`  
`BBS.occurrences`(`devtools::install_github('nicholasjclark/BBS.occurrences')`)  
`mvtboost`(`devtools::install_github('patr1ckm/mvtboost)`)

**References**  
Albouy, C., Lasram, F.B.R., Velez, L., Guilhaumon, F., Meynard, C.N.,
Boyer, S., Benestan, L., Mouquet, N., Douzery, E., Aznar, R.,
Troussellier, M., Somot, S., Leprieur, F., Le Loc’h, F. & Mouillot, D.
(2015) FishMed: traits, phylogeny, current and projected species
distribution of Mediterranean fishes, and environmental data. Ecology,
96, 2312-2313.

Clark, N.J., Wells, K. & Lindberg, O. (2018) MRFcov: Markov Random
Fields with additional covariates. R package version 1.0. GitHub.