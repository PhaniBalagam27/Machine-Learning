Platform:R-Studio

A short note on the distribution of the variables observed from the plots. 
Attribute list 
   V1. Number of times pregnant
   V2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
   V3. Diastolic blood pressure (mm Hg)
   V4. Triceps skin fold thickness (mm)
   V5. 2-Hour serum insulin (mu U/ml)
   V6. Body mass index (weight in kg/(height in m)^2)
   V7. Diabetes pedigree function
   V8. Age (years)
   V9. Class variable (0 or 1)

Attributes V2,V3 and V6 are likely to be normal as they are close to that of a normally distributed curve.The rest of the attributes are not normal as inferred from the histogram.

2.Find the correlation between each of the attributes and the class variable
Which attributes seem to have a strong correlation with the output (class) variable?

Correlation of every attribute with the class variable V9
[V1] 0.22189815
[V2] 0.46658140 
[V3] 0.06506836
[V4] 0.07475223 
[V5] 0.13054795 
[V6] 0.29269466
[V7] 0.17384407 
[V8] 0.23835598
Attribute V4 has the highest correlation with the class variable.

Correlation between all pairs of the 8 attributes:

   V1        V2        V3         V4         V5         V6         V7
V1  0 0.1294587 0.1412820 0.08167177 0.07353461 0.01768309 0.03352267
V2  0 0.0000000 0.1525896 0.05732789 0.33135711 0.22107107 0.13733730
V3  0 0.0000000 0.0000000 0.20737054 0.08893338 0.28180529 0.04126495
V4  0 0.0000000 0.0000000 0.00000000 0.43678257 0.39257320 0.18392757
V5  0 0.0000000 0.0000000 0.00000000 0.00000000 0.19785906 0.18507093
V6  0 0.0000000 0.0000000 0.00000000 0.00000000 0.00000000 0.14064695
V7  0 0.0000000 0.0000000 0.00000000 0.00000000 0.00000000 0.00000000
V8  0 0.0000000 0.0000000 0.00000000 0.00000000 0.00000000 0.00000000
           V8
V1 0.54434123
V2 0.26351432
V3 0.23952795
V4 0.11397026
V5 0.04216295
V6 0.03624187
V7 0.03356131
V8 0.00000000

The above matrix shows the correlation between each of the variables. The maximum is between V1 and V8.
