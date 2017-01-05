Supporting material for article : 

C. Tzedakis, M. Crucifix, T. Mitsui and E. Wolff, 
A simple rule to determine which insolation cycles lead to interglacials, 
accepted for publication in Nature (reference to be provided) 


# Directory structure 

## Data

- [caloric_peaks.csv](Data/caloric_peaks.csv) : master data supporting the article containing
    -  row index 
    -  Time.ka.BP : Time in thousands of years BP correponding to Caloric Insolation peaks 
    -  MIS.or.peak.age : Label (MIS number or peakr age) used throughouth the paper
    - is.0..ig.1..c.ig.2 : index = 0 if interstadial, 1 if interglacial, and 2 if continued interglacial 
    - filled.0.or.Open.1 : = 1 if the attribution is considered as less certain and represented as a circle on Figures 1 to 4
    - Caloric.summer.insolation.at.65N.GJm.2 : caloric summer insolation at 65 degrees N in GJ/m2 
    - Elapsed.time.ka : Elapsed time since latest insolation peak yielding an interglacial onset
    - Effective.energy.GJm.2 : Effective energy as used on Figure 5 
- [detrended_LR04_smoothed.csv](Data/detrended_LR04_smoothed.csv)
- [detrended_S05.csv](Data/detrended_S05)
- [insolations.csv](Data/insolations.csv)
    - Insolations computed based on the Laskar et al. (2004) astronomical solution and obtained using the `palinsol` R package (see below)
- [solstice_peaks.csv](Data/solstice_peaks.csv)
    - same as [caloric_peaks.csv](Data/caloric_peaks.csv)  but using summer solstice, as used for Extended Data Figure 9. 

Note : Microsoft Excel Open XML Format (`Xlsx`) are also provided in the `Xlsx` directory, along with the data file ['2016-02-02407A_SI_Tables_S1_and_S2.xlsx'](Data/Xlsx/2016-02-02407A_SI Tables S1 and S2_ew.xlsx') provided as supplementary tables of the Nature article. 
#

## R

This R code is provided under MIT Licence (see LICENSE file attached)

- [main.R](R/master.R) is the master file which load experiment parameters and generate simulation output in the [RData](RData) directory

- [experiment.R](R/experiment.R) contains the MCMC simulation procedure
- [analysis_function.R](R/analysis_function.R) contains supporting functions
- [plot_mpt.R](R/plot_mpt.R) loads  simulation output in the [RData](RData) directory to generate  Extended Data Figure S10
- [plot_frequent_traj](R/plot_frequent_traj) loads simualtion output in the [RData](RData) directory to generate Figure 6
- [diag_last_1000k.R](R/diag_last_1000k.R) generates the data  ([quantiles_part_1.Rda](RData/quantiles_part_1.Rda)) as well as a figure similar to Figure 4, 
but the figure actually published underwent further editing for nice placement of labels. 
- [diag_last_2500k.R](R/diag_last_2500k.R) generates the data 
([quantiles_25Myr_part_3.Rda](RData/quantiles_25Myr_part_3.Rda)) 
as well as a figure similar to Extended Data Figure 5,
but the  published figure underwent further editing for nice placement of labels. 

## RData

Contains `RData` binary files, directly loadable in `R`, with the output of the Monte-Carlo experiments 

## Eps

Figures produced directly with the `R` code. Note that the other figures of the article have been produced based on the different `.csv` files available in the `Data` directory. 

# Software acknowledgements


We are grateful to the developers of R:

  R Core Team (2016). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

we also use the following packages for plotting: 

- Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package
 version 1.1-2. https://CRAN.R-project.org/package=RColorBrewer
- Package 'hexbin' by    Dan Carr, ported by Nicholas Lewin-Koh, Martin Maechler and contains copies of lattice functions written by Deepayan Sarkar (2015).  hexbin: Hexagonal Binning Routines. R package version 1.27.1.  https://CRAN.R-project.org/package=hexbin

insolations are obtained using the Package 'palinsol' : M. Crucifix (2016). palinsol: Insolation for Palaeoclimate
  Studies. R package version 0.94. https://cran.r-project.org/package=palinsol

