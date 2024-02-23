capture log close
cd "H:\Econometrics\EC3301 project"
use projectcrime.dta, clear
log using "ProjectLog.log", replace
/*labelling variables*/
label data "Crime Rate and Related Variables by County, North Carolina,
1985"
label var county "County"
label var crimerate "Crime Rate in 1985"
label var prbarr "Probability of Arrest"
label var prbconv "Probability of Conviction if Arrested"
label var prbpris "Probability of Prison Sentence if Convicted"
label var avgsen "Average Prison Sentence in Days"
label var polpc "Police per Capita"
label var density "Number of People per Square Mile"
label var taxpc "Tax Revenue per Capita"
label var west "(=1) if Living in Western NC"
label var central "(=1) if Living in Central NC"
label var urban "(=1) if Living in a Standard Metropolitan Statistical
Area"
label var pctmin80 "Percentage of Ethnic Minority in 1980"
label var wcon "Weekly Wage in Construction in Dollars"
label var wtuc "Weekly Wage in Transport, Utilities, and Communications
in Dollars"
label var wtrd "Weekly Wage in Wholesale or Retail Trade in Dollars"
label var wfir "Weekly Wage in Financial Services, Insurance, and Real
Estate in Dollars"
label var wser "Weekly Wage in the Service Industry in Dollars"
label var wmfg "Weekly Wage in Manufacturing in Dollars"
label var wfed "Weekly Wage of Federal Employees in Dollars"
label var wsta "Weekly Wage of State Employees in Dollars"
label var wloc "Weekly Wage of Local Government Employees in Dollars"
label var pctymle "Young Males as a Percentage of Population"
label var crmrtelag "Crime Rate in North Carolina in 1984"
/*looking at the dataset*/
browse
/*getting a summary for the data*/
summarize
/*Notice that wcon has an extremely high maximum value compared to its
minimum, average, and maximum for wages in other industries. Similarly,
pctymle has a lower mean than expected.*/
scatter crimerate wcon
scatter crimerate pctymle
/*As predicted, there is a large outlier for both of these variables.*/
/*using the correlation matrix to identify key relationships between
variables according to the dataset*/
correlate
/*we can see that urban and density are highly correlated. the wage
variables also seem to be correlated. can reasonably assume that most
wages move together*/
/*using scatterplots to identify outliers in chosen variables*/
scatter crimerate prbarr || lfit crimerate prbarr
/*seems to fit reasonably well*/
scatter crimerate prbconv || lfit crimerate prbconv
/*seems to fit reasonably well*/
scatter crimerate prbpris || lfit crimerate prbpris
/*seems to fit reasonably well*/
scatter crimerate avgsen || lfit crimerate avgsen
/*seems to fit reasonably well*/
scatter crimerate polpc || lfit crimerate polpc , title("Effect of
Police per Capita on Crime Rate") , xtitle("Police per Capita") ,
ytitle("Crime Rate")
/*there may be a potential outlier for polpc>0.03*/
scatter crimerate density || lfit crimerate density
/*seems to fit reasonably well*/
scatter crimerate pctmin80 || lfit crimerate pctmin80
/*seems to fit reasonably well*/
scatter crimerate wmfg || lfit crimerate wmfg
/*seems to fit reasonably well*/
scatter crimerate pctymle || lfit crimerate pctymle
/*there may be a potential outlier for pctymle>0.25*/
/*trying out initial regression without adjusting for outliers. I will
adjust functional form based on distrubution of errors.*/
generate polpcdum=0
replace polpcdum=1 if polpc>0.03
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 wmfg pctymle
estat vif
ovtest
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 wmfg pctymle
predict res, r
histogram res, normal
sktest res
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 wmfg pctymle
estat imtest, white
hettest
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 wmfg pctymle
test polpc polpcdum
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 wmfg pctymle, vce(robust)
/*comparing with and without wmfg. adding different wages and
checking*/
generate log_wmfg=log(wmfg)
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 log_wmfg pctymle
estat imtest, white
generate log_wtuc=log(wtuc)
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 log_wtuc pctymle
generate log_wtrd=log(wtrd)
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum density
pctmin80 log_wtrd pctymle
/*wmfg seems to fit the best in its logged form.*/
/*trying white's test with urban in place of density*/
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum urban
pctmin80 log_wmfg pctymle
estat imtest, white
/*does better on the white test. keeping this change.*/
generate log_avgsen=log(avgsen)
regress crimerate prbarr prbconv prbpris avgsen polpc polpcdum urban
pctmin80 log_wmfg pctymle
regress crimerate prbarr prbconv prbpris log_avgsen polpc polpcdum
urban pctmin80 log_wmfg pctymle
/*log of avgsen is not very significant, and avgsen is also not
significant at 5%.*/
/*avgsen seems to be insignificant at 5%. trying a quadratic approach*/
generate avgsen2=avgsen^2
regress crimerate prbarr prbconv prbpris avgsen avgsen2 polpc polpcdum
urban pctmin80 log_wmfg pctymle
/*both variables are insignificant. removing avgsen from the
regression*/
regress crimerate prbarr prbconv prbpris polpc polpcdum urban pctmin80
log_wmfg pctymle
/*pctmin80 has an extremely small coefficient. trying without*/
regress crimerate prbarr prbconv prbpris polpc polpcdum urban log_wmfg
pctymle
/*trying west and central*/
regress crimerate prbarr prbconv prbpris polpc polpcdum urban log_wmfg
pctymle west central
/*west is very significant, central is not. removing central*/
regress crimerate prbarr prbconv prbpris polpc polpcdum urban log_wmfg
pctymle west
estat imtest, white
/*passes the white test*/
regress crimerate prbarr prbconv prbpris polpc polpcdum urban log_wmfg
pctymle west
ovtest
/*trying squares of deterrence variables*/
generate prbarr2 = prbarr^2
generate prbconv2 = prbconv^2
generate prbpris2 = prbpris^2
regress crimerate prbarr prbconv prbpris polpc polpcdum urban log_wmfg
pctymle west
regress crimerate prbarr prbarr2 prbconv prbconv2 prbpris prbpris2
polpc polpcdum urban log_wmfg pctymle west
test prbarr prbarr2
test prbpris prbpris2
test prbconv prbconv2
regress crimerate prbarr prbconv prbconv2 prbpris polpc polpcdum urban
log_wmfg pctymle west
test prbconv prbconv2
/*keep only square of prbconv*/
regress crimerate prbarr prbconv prbconv2 polpc polpcdum urban log_wmfg
pctymle west
estat imtest, white
test polpc polpcdum
/*polpc is much less significant. testing without*/
regress crimerate prbarr prbconv prbconv2 polpc polpcdum urban log_wmfg
pctymle west
ovtest
regress crimerate prbarr prbconv prbconv2 urban log_wmfg pctymle west
ovtest
estat imtest, white
estat vif
regress crimerate prbarr prbconv prbconv2 prbpris urban log_wmfg
pctymle west
ovtest
estat imtest, white
estat vif
/*RESET is much better for the second model. although first model is
less heteroskedastic, both pass white's test*/
/*trying taxpc*/
regress crimerate prbarr prbconv prbconv2 urban log_wmfg pctymle west
taxpc
predict res1, r
histogram res1, normal
sktest res1
/*there is still skewness and kurtosis but much better model than
before. taxpc is very insignificant. removing from regression*/
regress crimerate prbarr prbconv prbconv2 urban log_wmfg pctymle west
predict res2, r
histogram res2, normal
sktest res2
regress crimerate prbarr prbconv prbconv2 urban log_wmfg pctymle west
estat vif
ovtest
estat imtest, white
/*deciding whether to keep pctymle*/
regress crimerate prbarr prbconv prbconv2 urban log_wmfg pctymle west
ovtest
estat imtest, white
regress crimerate prbarr prbconv prbconv2 urban log_wmfg west
ovtest
estat imtest, white
/*misspecification increases by a lot without it. keeping it for now.*/
/*trying log of crimerate*/
generate log_CR = log(crimerate)
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
predict res3, r
histogram res3, normal
sktest res3
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
estat vif
ovtest
estat imtest, white
/*a better model - better RESET/White's test and normality is the
same.*/
/*trying to add lagged crimerate*/
generate log_LCR=log(crmrtelag)
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
log_LCR
estat vif
ovtest
estat imtest, white
predict res4, r
histogram res4, normal
sktest res4
test prbconv prbconv2
correlate prbarr crmrtelag
/*prbarr is insignificant. this is because prbarr and lagged crimerate
are reasonably correlated. trying without*/
regress log_CR prbconv prbconv2 urban log_wmfg pctymle west log_LCR
ovtest
estat imtest, white
test prbconv prbconv2
estat vif
predict res5, r
histogram res5, normal
sktest res5
regress log_CR prbconv prbconv2 density log_wmfg pctymle west log_LCR
ovtest
estat imtest, white
test prbconv prbconv2
estat vif
/*density is significant but this model has a worse specification and
more heteroskedasticity*/
generate log_dens=log(density)
regress log_CR prbconv prbconv2 log_dens log_wmfg pctymle west log_LCR
ovtest
estat imtest, white
/*does much worse on heteroskedasticity. not keeping density*/
regress log_CR prbconv prbconv2 log_wmfg pctymle west log_LCR
ovtest
estat imtest, white
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
ovtest
estat imtest, white
/*removing urban makes it much worse. keeping second specification.*/
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
ovtest
estat imtest, white
estat ic
regress log_CR prbarr prbconv prbconv2 urban log_wmfg west
ovtest
estat imtest, white
estat ic
/*better R squared, better White's test, similar RESET and lower AIC if
pctymle is included. BIC lower for the second model and it has slightly
better specification.*/
/*trying log of prbconv instead of quadratic*/
generate log_pconv=log(prbconv)
regress log_CR prbarr log_pconv urban log_wmfg pctymle west
estat vif
ovtest
estat imtest, white
/*makes it very heteroskedastic. sticking to quadratics*/
generate log_wcon=log(wcon)
scatter crimerate log_wcon
generate log_wcondum=0
replace log_wcondum=1 if log(wcon)>7
regress log_CR prbarr prbconv prbconv2 urban log_wmfg log_wcon
log_wcondum pctymle west
estat vif
ovtest
estat imtest, white
/*not very significant; makes heteroskedasticity much worse*/
regress log_CR prbarr prbconv prbconv2 urban log_wcon log_wcondum
pctymle west
estat vif
ovtest
estat imtest, white
estat ic
/*does worse on White's test. much higher AIC/BIC. keeping only wmfg*/
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
test prbconv prbconv2
estat vif
ovtest
estat imtest, white
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
predict res6, r
histogram res6, normal
sktest res6
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west
regress log_CR prbarr prbconv prbconv2 urban log_wmfg pctymle west,
vce(robust)
estat vif
ovtest
estat imtest, white
predict res7, r
histogram res7, normal
sktest res7
log close
                                                        
