VerificationScoreboard
========================================================
author: RAMOS Maria-Héléna, NORVILLE Jeff
date: 2016-05-27
autosize: true

Verification Tools
========================================================
Ensemble Model verification

- Observations
- Forecasts
- Statistics

User Base
========================================================
Who are these "model fans" who track performance stats?
- Eletric utility operators
- Water managers / planners
- Aviation / shipping planners
- City emergency response administration


A European movement to "Learn from today to anticipate tomorrow"
========================================================
![Imprex](sbdtest1/www/imprex.png)
<!-- img(src = "imprex.png", height = 100) -->
"IMproving PRedictions and management of hydrological EXtremes"


The IMPREX project team 
========================================================


|fullname                                                                                         |acronym   |country        |
|:------------------------------------------------------------------------------------------------|:---------|:--------------|
|Koninklijk Nederlands Meteorological Institute                                                   |KNMI      |Netherlands    |
|European Centre for Medium-Range Weather Forecasts                                               |ECMWF     |United Kingdom |
|Sveriges Meteorologiska Och Hydrologiska Institut                                                |SMHI      |Sweden         |
|Institut National de Recherche en Sciences et Technologies pour l'Environnement et l'Agriculture |IRSTEA    |France         |
|Potsdam-Institut fÃ¼r Klimafolgenforschung                                                       |PIK       |Germany        |
|Arctik SPRL                                                                                      |ARCTIK    |Belgium        |
|Barcelona Supercomputing Center - Centro Nacional de Supercomputacion                            |BSC       |Spain          |
|Met Office                                                                                       |METOFFICE |United Kingdom |
|The Research Committee of The Technical University of Crete                                      |TUC       |Greece         |
|The University of Reading                                                                        |UREAD     |United Kingdom |
|Helmholtz-Zentrum Geesthacht Zentrum fÃ¼r Material- und KÃ¼stenforschung GmBH                    |HZG       |Germany        |
|Stichting Deltares                                                                               |DELTARES  |Netherlands    |
|Stichting VU-VUMC                                                                                |IVM       |Netherlands    |
|Adelphi Research GGmBH                                                                           |ADELPHI   |Germany        |
|HKV Lijn in Water B.V.                                                                           |HKV       |Netherlands    |
|FUTUREWATER SL                                                                                   |FW        |Spain          |
|Centro TecnolÃ³gico del Agua                                                                     |CETAQUA   |Spain          |
|Universitat PolitÃ¨cnica de ValÃ¨ncia                                                            |UPV       |Spain          |
|Politecnico di Milano                                                                            |POLMIL    |Italy          |
|Centro Internazionale in Monitoraggio Ambientale - Fondazione CIMA                               |CIMA      |Italy          |
|Helmholtz-Zentrum Potsdam Deutsches Geoforschungszentrum                                         |GFZ       |Germany        |
|Bundesanstalt fuer Gewaesserkunde                                                                |BfG       |Germany        |
|Stichting Water Footprint Network                                                                |WFN       |Germany        |

The Famous Finley Tornadoes
========================================================

Finley (1884) 



found at [http://www.cawcr.gov.au/projects/verification/Finley/Finley_Tornados.html](http://www.cawcr.gov.au/projects/verification/Finley/Finley_Tornados.html#Murphy%201996)
reference to **Murphy, A.H.**, *1996: The Finley affair: A signal event in the history of forecast verification. Wea. Forecasting, 11, 3-20.* 

Scores
========================================================

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

discuss sample size?

Present Case for Ensemble Forecast Scoreboard
========================================================

Model and Data Uncertainty and Bias
========================================================
Observations
Forecasts


Forecast Types
========================================================
Gridded forecasts
 - interpolation uncertainty
 - magnitude 
 - i.e. take largest value or nearest point?

Basin-level forecasts
 - in use in our scoreboard development
 - 


Forecast Types
========================================================

- Ensemble
- Deterministic
- etc

*Non-determnisitc models require scores evaluated by distribution...*


Skill Metrics
========================================================

- Brier Score
- CRPS
- MAE
- PIT
- Regression Coefficient
- RMSE


