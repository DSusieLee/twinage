# twinage
 
This is the repository of R codes (R version 4.2.3) used for producing the paper "More and more twins in low-income countries as maternal age increases and infant mortality declines" by D. Susie Lee and Kieron Barclay. 

Each R script is named in the order of its usage in the process of analyses. For example, "0_1_dhs_reading_in.R" starts with 0, indicating that this is script runs codes that prepare data before conducting analyses. 

==Data==
All original data are aviailable from the sources below. If you wish to directly work on the data we already processed and compiled (combining data from different sources), please use the main analytic data "dhs.RDS" which is uploaded here.

Sources of data: DHS (https://dhsprogram.com/), Infant mortality data and GDP per capita (https://data.worldbank.org/), Human development index (https://hdr.undp.org/data-center/documentation-and-downloads). For the human development index before 1990, we took data from https://github.com/nptodd/BAR/tree/main/data/dev_index/human-development-index_OWID.csv. This data was used in the paper "Socioeconomic development predicts a weaker contraceptive effect of breastfeeding" (Todd & Lerch, 2021)

For projections, the World Population Prospects data was downloaded from https://population.un.org/wpp/.
