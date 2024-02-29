This is the repository of R codes (R version 4.2.3) and data used for the paper "More twins in low-income countries as maternal age at birth increases" by D. Susie Lee and Kieron Barclay. 

Each R script is named in the order of its usage in the process of analyses. For example, a script starts with "0" if it runs codes that prepare data before conducting analyses. The original DHS data are not uploaded here, and "dhs_past10yrs.rds" is the file that contains the analytical sample obtained from the original data, after processing variables and selecting only necessary variables. The data include the following variables:

  -twin: twin (1) / singleton (0)
  -b0: order within multiple births (0 if singleton)
  -mab: maternal age at birth in years
  -harm_mat_edu: highest maternal education attained (0 <primary, 1 primary, 2 secondary, 3 tertiary, 4 not classified)*
  -parity: parity of a given birth (same value if multiple births)
  -year_child_birth: year in which a birth occurred
  -year_child_birth_group: 'year_child_birth' grouped if too small number size (see Methods)
  -year_survey: year of survey
  -year_mom_birth: year in which mother was born

*Some codes for 'harm_mat_edu' are larger than 4, because some WFSs used different categorization for education level. This does not affect our analysis, because we only cared whether a woman had at least primary education or not (0 or not.)

In addition to "dhs_past10yrs.rds", we have uploaded the following World Population Prospects data from (https://population.un.org/wpp/)
-WPP2022_FERT_F04_BIRTHS_BY_5-YEAR_AGE_GROUPS_OF_MOTHER.xlsx

All original data are aviailable from the sources below. If you wish to directly work on the data we already processed and compiled (combining data from different sources), please use the main analytic data "dhs.RDS" which is uploaded here.

Sources of data: DHS (https://dhsprogram.com/), GDP per capita (https://data.worldbank.org/), World Population Prospects data (https://population.un.org/wpp/)
