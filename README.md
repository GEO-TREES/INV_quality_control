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

For help organising your data according to this format you may wish to use the [ATFS-DataHarmonization Shiny app](https://github.com/Alliance-for-Tropical-Forest-Science/DataHarmonization). In this repository we provide an output profile for the Shiny app to ensure all data are output in the GEO-TREES format.

`checkTableSpec()` reads the column specifications of each GEO-TREES L0 table directly from the [INV_processing templates](https://github.com/GEO-TREES/INV_processing/tree/main/templates), to check that tables will be accepted by the processing workflow. To work offline, point `spec_dir` to the `templates/` directory of a local copy of INV_processing.

The QA/QC workflow is provided as a Quarto notebook, `inst/INV_quality_control_nb.qmd`, which runs the checks below in order, with visualisations, and saves the results so they can be submitted alongside the data.

### Focal census

Each GEO-TREES acquisition contains one census of each plot, the focal census, which is submitted to INV_processing. Data from other censuses of the same plots are not submitted, but are very useful for finding errors in the focal census. Each census of a plot is identified by `census_id`, which must be an integer that increases with each census. The notebook loads all censuses, selects the focal census of each plot (by default the latest census, or a census chosen by the user), and then:

1. Runs single-census checks on the focal census only.
2. Runs multi-census checks on all censuses, then keeps only the issues which affect the focal census with `filterFocal()`.
3. Writes the focal census stem table, with only the columns in the INV_processing specification.

The toolbox contains many small functions designed to check a single aspect of data quality in a tree inventory dataset. These functions are bundled into higher-level convenience functions that allow you to run many related checks in a single function call. 

Some functions require multi-census data to identify potential issues, others require only single census data.

Functions in the toolbox are split into two main groups, those that __flag__ potential data quality issues (`flag*()`), and others which attempt to __correct__ these issues (`correct*()`).

Flag functions typically return ID values which uniquely identify the suspect record. The particular set of ID values depends on the scope of the data quality issue, e.g. measurement, stem, tree, census, plot.

Correction functions typically return adjusted values of one or more variables along with the necessary ID values required to uniquely identify the corrected record.

## Functions

Below, is a description of all base-level functions, grouped by theme. 

### Data format

* `filterFocal()` - Keep only the issues found by a multi-census check which affect stems or measurements in the focal census.

* `checkTableSpec()` - Check that a table has the columns, column order, column classes and value ranges required by INV_processing, reporting all issues at once.

* `flagTableMatch()` - Flag ID values in one table which are missing from another, e.g. plots in the stem table which are not in the plot table, or taxonomic names missing from the taxonomy table.

* `calcPlotArea()` - Calculate plot area from the relative coordinates of the plot corners, the denominator of AGB density.

* `flagPlotArea()` - Flag plots where the declared plot dimensions do not match the area of the plot corners.

### Missing, duplicate and phantom records 

#### Flags

* `flagRecordMissing()` - Flag stems with missing census records.
 
* `flagRecordDup()` - Flag measurements where tree/stem/measurement IDs (records) are duplicated within a census.

* `flagStemDupLoc()` - Flag stems from different trees with nearly identical coordinates and diameters, which may be the same stem recorded twice.

* `flagRecordAbsent()` - Flag stems which were alive in their last measurement before the focal census, but have no record in the focal census.

* `flagValMissing()` - Flag stem measurements where values are empty and must be filled, with user specification. Includes cross-column dependencies, e.g. diameter missing but stem is alive.

* `flagValPresent()` - Flag stem measurements where values are present but must be empty, e.g. a diameter on a stem recorded as missing ("M") or broken below the POM ("T").

* `flagRecruitPre()` - Flag stem measurements which were probably back-filled post-hoc and were not measured in the field, e.g. pre-recruitment.

* `flagRecruitBig()` - Flag stem measurements as "overgrown recruit" if diameter in recruiting census is greater than a user-specified threshold. 

### Outlier values

#### Flags

* `flagModOutlier()` - Flag stem measurements based on their residuals from a fitted model, e.g. a height-diameter model, on the scale of the model response.

* `flagValRange()` - Flag stem measurements based on a user-supplied acceptable range.

* `flagValUnit()` - Flag censuses where the variance of measurement data suggests a mix of units within a variable.

* `flagValRound()` - Flag censuses where rounded measurement values are unusually common. Specify level of precision.

* `flagValDigit()` - Flag groups with terminal digit preference, e.g. an excess of diameters ending in 0 or 5, using a chi-squared test against a uniform distribution of terminal digits.

* `flagDiamMin()` - Flag stems below the minimum diameter threshold of their plot, from the plot meta-data table.

* `flagBigStem()` - Flag stems which make up a large proportion of plot basal area, whose measurements have the greatest effect on plot AGB density.

* `flagPomDefault()` - Flag POM values which differ from the plot default without a buttress ("U") or irregular stem ("I") code, codes which imply a raised POM at the default POM, and POM values in the wrong units.

* `flagMeasDate()` - Flag measurement dates which cannot be parsed, are in the future, are far from the median date of their plot, or are far from a reference date such as the date of the ALS acquisition.

* `flagCodeVal()` - Flag stem measurements with incorrect acceptable code values or inconsistent simultaneous combinations of codes such as fallen and standing, following user specification.

* `flagMultiLoc()` - Flag trees where any pairwise distance between stems is greater than a user-specified threshold.

* `flagIdNest()` - Flag IDs nested within more than one parent ID, e.g. stems which belong to more than one tree, or trees with stems in more than one subplot.

* `flagTaxonName()` - Flag taxon names with potential errors, such as "aceae" at the end of genus and species names, or non-standard character.
 
* `flagMultiTaxon()` - Flag trees where stems differ in taxonomic name within a census.

* `flagTaxonLevel()` - Flag stems which are not identified to species, classifying each name as identified to genus, family, or unidentified. These stems have genus, family or plot mean wood density in INV_processing.

* `flagFormAlt()` - Flag taxa from known alternative growth-form groups, e.g. palms, tree ferns, bamboos, bananas, cycads, and possible strangler figs, that are not already labelled as such in `growth_form`, using a lookup table of families and genera.

#### Corrections

* `correctLocSub()` - Move stems where stem spatial coordinates are missing or outside the subplot to the centre of the labelled subplot.

### Spatial

#### Flags

* `flagPlotCorner()` - Flag plot corners where the GNSS coordinates do not fit the relative coordinates of the other corners, and plots where the scale between relative and GNSS coordinates differs from 1, e.g. slope distances or wrong units.

* `flagStemOutside()` - Flag stems outside the boundary of their plot, defined by the plot corners, with the distance outside.

* `flagTreeUnmapped()` - Flag trees where no stems have coordinates. INV_processing places these trees at random within their subplot or plot.

#### Other

* `inferSubplotBounds()` - Estimate the boundaries of each field subplot from the coordinates of the stems labelled with that subplot, for use with `correctLocSub()` and `plotStemMap()` when no table of subplot boundaries is available.

### Biomass estimation

#### Flags

* `flagAgbExclude()` - Flag stem measurements which INV_processing will exclude from AGB, or from AGB density, and the reason, reproducing `recordFilter()` and `agbdFilter()`.

### Time-series inconsistencies

#### Flags

* `flagTaxonSeries()` - Flag stems where taxonomic name differs among censuses.

* `flagCodeSeries()` - Flag stems with inconsistent code timelines such as fallen to standing, dead to alive, following user specification.

* `flagLocSeries()` - Flag stems where stem spatial coordinate changes among censuses by more than a user-specified distance threshold.

* `flagPlotChange()` - Flag plots, or subplots, where the stem count, basal area or AGB changes between censuses by more than a user-specified annual rate.

* `flagDemogRate()` - Flag census intervals with high annual mortality or recruitment rates, which may indicate missed or retagged stems.

* `flagTagChange()` - Flag recruits close to a stem which disappeared since the previous census, with a similar diameter, which may be the same stem with a new tag.

#### Corrections

* `correctMortalitySeries()` - Impute missing and flagged mortality measurements using a stepwise algorithm. 
    * Interpolate missing records if preceded and followed by records with an identical mortality status. 
    * Where missing records occur in the first or final census, impute by extrapolation if preceded or proceeded by two or more records with an identical mortality status. 
    * When neither extrapolation or interpolation can be applied, e.g. for a stem with no mortality status information, or where preceding and proceeding mortality status differs, assume dead if no diameter measurement recorded, and alive if there is a diameter measurement. 
    * To correct for resurrection, correct all records to alive prior to when the stem was last recorded as alive. 

* `correctDiameterSeries()` - Impute missing and flagged diameter measurements using a stepwise algorithm. Diameters of dead stems are never imputed. Permanent shifts are realigned to the segment of the diameter timeline chosen by `shift_anchor`, by default the longest segment, i.e. the majority of measurements, rather than always the first census.
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

* `summariseFlags()` - Count the number of records flagged by each check in a named list of flag outputs.

* `decYear()` - Convert YYYY-MM-DD, YYYY-MM or YYYY dates to decimal years, matching `decDate()` in INV_processing.

#### Corrections

* TODO: `correctMultiTaxon()` - Where taxonomic name is missing from a stem within a tree where all other stems have the same taxonomic name, assign this name to all missing values.

* TODO: `correctMultiLoc()` - Replace missing stem coordinates with the centroid of all other stems.

#### Visualisation

* `plotHistogram()` - create a histogram of a numeric variable

* `plotScatter()` - create a 2D scatter plot of two numeric variables, with the option to flag outlier values.

* `plotStemMap()` - create a map of stem locations within each plot, optionally sized by diameter, coloured by a flag, and with subplot boundaries.

* `plotDiameterCorrections()` - plot original and corrected diameter timelines for each stem corrected by `correctDiameterSeries()`.
