# howto install
# from https://github.com/MeteoSwiss/easyVerification/blob/master/vignettes/ecoms_forecast_verification.Rmd
devtools::install_github(c("SantanderMetGroup/downscaleR.java",
                           "SantanderMetGroup/downscaleR",
                           "SantanderMetGroup/loadeR.ECOMS"))
                          # "SantanderMetGroup/ecomsUDG.Raccess"))


library(ecomsUDG.Raccess)

loginECOMS_UDG(username = "jnorville", password = "RTIlfelI") 

tx.forecast <- loadECOMS(dataset = "System4_seasonal_15",
                         var = "tasmax",
                         members = 1:9,
                         lonLim = c(-10 ,15),
                         latLim = c(35, 50),
                         season = 6:8,
                         years = 1991:2000,
                         leadMonth = 2)


