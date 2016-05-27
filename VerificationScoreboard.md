VerificationScoreboard
========================================================
author: NORVILLE Jeff
date: 2016-05-27
autosize: true

Verification Tools
========================================================

Ensemble Model verification 



- Bullet 1
- Bullet 2
- Bullet 3

Finley's tornadoes
========================================================

Finley (1884) 

pull from 
http://www.cawcr.gov.au/projects/verification/Finley/Finley_Tornados.html



Scores
========================================================

http://iri.columbia.edu/wp-content/uploads/2013/07/scoredescriptions.pdf

Skill scores are metrics based on predictive performance relative to a baseline forecast.

Numerous calculations exist to help evaluate forecast and predictions.

A simple example is the
    root-mean squared error (RMSE) 
a comparison between model prediction and observation:
    $$ RMSE = {\sqrt {\frac{1} {N}{\sum\limits_{i = 1}^N {(c_{i} - \bar{c}_{i} } })^{2} } } $$

Or, in R:

```r
RMSE <- sqrt(mean((y-y_pred)^2))
```

A Skill score (SS) associated with RMSE, then, is one minus the ratio of forecast over observation:
    $$ SS = 1 - \frac{ RMSE _{forecasts}}{RMSE _{observations}} $$


Slide With Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-3](VerificationScoreboard-figure/unnamed-chunk-3-1.png)
