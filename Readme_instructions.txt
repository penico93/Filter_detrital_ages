Grain ages were filtered using standard methodologies (Gehrels et al., 2008). Due to the magnitude of uncertainties inherent to U-Pb ages, a cut-off of 1000 Ma was chosen for selecting the 206Pb/238U age in younger grains and the 206Pb/207Pb in older grains. In U-Pb ages younger than 300 Ma, the discordance or inverse discordance was not evaluated, while for grains older than 300 Ma, those with discordances greater than 25% and inverse discordance greater than 10%, or error greater than 10% were discarded. 

Each file should have at least the U238/Pb206 and Pb207/Pb206 ages and their respective errors. Instead of leaving these names, you should change these names as follows:
U238/Pb206    ---> u1
U238/Pb206   error-> u1error
Pb207/Pb206   ----> u2
Pb207/Pb206   error----> u2error

After you have all the files organized in a single folder, you can proceed to run the code, but before you do it, you should define some variables to constrain your filter.

1) Select the filter_percentage which defines the maximum value (decimal) that will be multiplied by the preferred age and compared with the error of this age. 
Default value: filter_percentage = 0.1

2) Select the filter_agelimit which defines the value of age for which the discordance used as a test.
filter_agelimit = 300

3) Select the filter_age_choosing which defines the limit age to choose between the 206/207 or 206/238 age as the preferred age.
filter_age_choosing = 1000

4) filter_discordance defines the value of tolerance of the discordance
filter_discordance = 15

