# GEO-TREES tree inventory quality control toolbox

This repository contains code for quality assurance (QA) and quality control (QC) of tree inventory data before it is processed in the [GEO-TREES INV_processing workflow](https://github.com/GEO-TREES/INV_processing) to generate estimates of above-ground woody biomass density (AGBD).

The functions provided are flexible enough so they can be used to QA/QC any common plot-based tree inventory dataset. Most functions allow users to tweak the scope of the function depending on their data, e.g. acceptable diameter growth rate limits.

This QA/QC toolbox has incorporated ideas from many other previous efforts, including:

* The `TreeData` R package -- https://github.com/VincyaneBadouard/TreeData/tree/TreeData4GEO-TREES
* The SEOSAW R package (`seosawr`) -- https://bitbucket.org/miombo/seosaw/
* The ATFS DataHarmonization Shiny App -- https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization
* The EcoFoG `ForestData` R package -- https://github.com/EcoFoG/ForestData
* The ForestPlots.net `BiomasaFP` R package -- https://github.com/ForestPlots/BiomasaFP
* The ForestGEO `fgeo` R package ecosystem -- https://forestgeo.github.io/fgeo.biomass/

## Workflow 

The functions in the toolbox are designed for data following the GEO-TREES tree inventory data format, but most functions should be generalisable beyond this format. 
See the [GEO-TREES Tree Inventory Guidelines, _Section 16: Data format_](https://docs.google.com/document/d/1Q5Wy_LbNEa0tNaGVntL0Bku41Z3t-TeU85_IxZuC6Uc/edit?usp=sharing). 

For help organising your data according to this format you may wish to use the [ATFS-DataHarmonization](https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization) Shiny app. In this repository we provide an output profile for the Shiny app to ensure all data are output in the GEO-TREES format.

The toolbox contains many small functions designed to check a single aspect of data quality in a tree inventory dataset. These functions are bundled into higher-level convenience functions that allow you to run many related checks in a single function call. 

Some functions require multi-census data to identify potential issues, others require only single census data.

Functions in the toolbox are split into two main groups, those that __flag__ potential data quality issues (`flag*()`), and others which attempt to __correct__ these issues (`correct*()`).

Flag functions typically return ID values which uniquely identify the suspect record. The particular set of ID values depends on the scope of the data quality issue, e.g. measurement, stem, tree, census, plot.

Correction functions typically return adjusted values of one or more variables along with the necessary ID values required to uniquely identify the corrected record.

## Functions

Below, is a description of all base-level functions, grouped by theme. 

### Missing, duplicate and phantom records 

#### Flags

* `flagRecordMissing()` - Flag stems with missing census records.
 
* `flagRecordDup()` - Flag measurements where tree/stem/measurement IDs (records) are duplicated within a census.

* `flagValMissing()` - Flag stem measurements where values are empty and must be filled, with user specification. Includes cross-column dependencies, e.g. diameter missing but stem is alive.

* `flagRecruitPre()` - Flag stem measurements which were probably back-filled post-hoc and were not measured in the field, e.g. pre-recruitment.

* `flagRecruitBig()` - Flag stem measurements as "overgrown recruit" if diameter in recruiting census is greater than a user-specified threshold. 

### Outlier values

#### Flags

* `flagModOutlier()` - Flag stem measurements based on their residuals compared with a linear model. 

* `flagValRange()` - Flag stem measurements based on a user-supplied acceptable range.

* `flagValUnit()` - Flag censuses where the variance of measurement data suggests a mix of units within a variable.

* `flagValRound()` - Flag censuses where rounded measurement values are unusually common. Specify level of precision.

* `flagCodeVal()` - Flag stem measurements with incorrect acceptable code values or inconsistent simultaneous combinations of codes such as fallen and standing, following user specification.

* `flagMultiLoc()` - Flag trees where any pairwise distance between stems is greater than a user-specified threshold.

* `flagTaxonName()` - Flag taxon names with potential errors, such as "aceae" at the end of genus and species names, or non-standard character.
 
* `flagMultiTaxon()` - Flag trees where stems differ in taxonomic name within a census.

#### Corrections

* `correctLocSub()` - Move stems where stem spatial coordinates are missing or outside the subplot to the centre of the labelled subplot.

### Time-series inconsistencies

#### Flags

* `flagTaxonSeries()` - Flag stems where taxonomic name differs among censuses.

* `flagCodeSeries()` - Flag stems with inconsistent code timelines such as fallen to standing, dead to alive, following user specification.

* `flagLocSeries()` - Flag stems where stem spatial coordinate changes among censuses by more than a user-specified distance threshold.

#### Corrections

* TODO: `correctMortSeries()` - Impute missing and flagged mortality measurements using a stepwise algorithm. 
    * Interpolate missing records if preceded and followed by records with an identical mortality status. 
    * Where missing records occur in the first or final census, impute by extrapolation if preceded or proceeded by two or more records with an identical mortality status. 
    * When neither extrapolation or interpolation can be applied, e.g. for a stem with no mortality status information, or where preceding and proceeding mortality status differs, assume dead if no diameter measurement recorded, and alive if there is a diameter measurement. 
    * To correct for resurrection, correct all records to alive prior to when the stem was last recorded as alive. 

* TODO: `correctDiamSeries()` - Impute missing and flagged diameter measurements using a stepwise algorithm.
    * Linear interpolation (regression) needs at least:
        * one diameter measurement available either side of the missing value
        * at least two diameter measurements after the missing value, for recruiting census only
    * Optionally infer growth rate from average growth rates of taxonomic level (species, genus, family [maybe]) where linear interpolation data is not available for that stem.
    * Discriminate "permanent shifts" and "punctual errors". Punctual errors are offset by a complementary decrease or increase in size at a later census.
    * Must handle Point of Measurement (POM) changes across censuses, e.g. Cushman et al. (2021). Or choose not to correct these values.

* TODO: `correctPOMSeries()` - Impute missing and flagged POM measurements using a stepwise algorithm.
    * Impute missing POM records using a stepwise algorithm. 
    * Missing POMs are extrapolated from the last non-missing POM value. 
    * If missing values occur at the start of the census period, back-fill using the next non-missing value.
    * When neither extrapolation or back-filling can be applied, e.g. for a stem with no POM records, fill missing POM records with the default POM for the plot.

### Miscellaenous 

#### Flags

* `flagMultiMany()` - Flag trees where the number of stems is greater than a user-specified threshold.

* TODO: `flagFormAlt()` - Flag taxa from known alternative growth-form groups, e.g. liana, bamboo, tree fern, banana, etc. that are not already labelled as such, using existing data and provided lookup tables.

#### Corrections

* TODO: `correctMultiTaxon()` - Where taxonomic name is missing from a stem within a tree where all other stems have the same taxonomic name, assign this name to all missing values.

* TODO: `correctMultiLoc()` - Replace missing stem coordinates with the centroid of all other stems.

#### Visualisation

* `plotHistogram()` - create a histogram of a numeric variable

* `plotScatter()` - create a 2D scatter plot of two numeric variables, with the option to flag outlier values.
