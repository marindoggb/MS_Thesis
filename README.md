# MS_Thesis
Codes for MS thesis

The best way to envision this code mess is:

For the whole data study:

1. Impute_a_WD.R    - This is where the imputations happen
2. WD_analyses.Rmd  - This is where the analysis was done on the whole data


For the sample study analysis:

1. patterns.R               - This is where the patterns used for creating missingness is made.
2. ampute.R                 - This is where the missingness itself is created
3. Impute_a_ampute_nytt.R   - This is where the imputations are created
4. ritgerd-1000r.R          - This is where the analysis takes place on the 1000 row sample. I only included this file since for other sample sizes the files
                               are identical with the only difference is to Ctrl-F and replace 1000r with 5000r etc..
5. plots_1000r_290623.Rmd   - This is where the results are plotted. The same reasoning for only including this sample size as in 4.


The other files:

Auxiliary_analysis_plots.Rmd  - This is where the auxiliary plots are created. Here is the 20.000 sample size, and again the only difference is to Ctrl-F.
NA_plots.Rmd                  - This is where the NA plots are created
toflur_ritgerd.R              - This is where some of the tables are created that are not created in the other files.


Just to note, the analysis on when the missing data mechanism was MNAR, it was done exactly the same as in the sample study analysis with the only difference changing the
mechanism to MNAR in the ampute function, that is adding : ,mech= "MNAR"  inside the ampute function.

