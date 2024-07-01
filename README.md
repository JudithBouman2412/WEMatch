# R-package to perform patient matching using ICD-10 codes

This package contains functions that can be used to perform gold standard matching using ICD-10 codes. For this we rely on functionality from X and X packages. 
Additionaly, we provide an easy to use function to perform W2v Enhanced patient MATCHing (WEMATCH) as described by Nguyen et al. (2019). 

# Simulated data

There is a simulated dataset available within the package that can be used to test the functionality of the package. These data can be accessed by running "data("simulated_data")". 

# Algorithms 

## WEMatch

(1) Select possible matches based on age, BMI and sex. 
(2) Try to find a match that has the exact same ICD-10 codes in the same order
(3) Matched based on the sequential order of the ICD-10 codes 
