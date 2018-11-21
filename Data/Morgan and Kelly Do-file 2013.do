set more off

******Unit-Root Tests******

local varlist "gini_gross redist gini_net lpbp_cum15 csedhlth_cent csssw_cent rgdpch_cent inflationcpimfbr unemplywdi inwardstockgdp demrss01_cum45 repressive_cum15 edyears pop014wdi ethdiv"

foreach variable of local varlist {
xtunitroot fisher `variable', dfuller lags(2)
}

local depvars "gini_gross redist gini_net"

local unitrootvars "csssw_cent rgdpch_cent inwardstockgdp demrss01_cum45 edyears pop014wdi ethdiv"

foreach xvar of local unitrootvars {
foreach yvar of local depvars {
capture drop res1
regress `yvar' `xvar'
predict res1, res
xtunitroot fisher res1, dfuller lags(2)
}
}

****** Analysis *******

*Table 1

**Model 1

regress d.gini_gross l.gini_gross d.lpbp_cum15 l.lpbp_cum15 d.csedhlth_cent l.csedhlth_cent d.csssw_cent l.csssw_cent d.rgdpch_cent l.rgdpch_cent d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi l.unemplywdi d.inwardstockgdp l.inwardstockgdp d.demrss01_cum45 l.demrss01_cum45 d.repressive_cum15 l.repressive_cum15 d.edyears l.edyears d.pop014wdi l.pop014wdi l.ethdiv, vce(cluster idn)
estimates store model2a

**Coding an Indicator for Cases Included in Full Analysis (non-missing political variables)**

capture drop res1
predict res1, res
capture drop included_gross
gen included_gross=0
replace included_gross=1 if res1~=.
drop res1

**Model 2

regress d.gini_gross l.gini_gross d.lpbp_cum15 d.csedhlth_cent l.csedhlth_cent d.rgdpch_cent d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi d.inwardstockgdp l.inwardstockgdp d.edyears if included_g==1, vce(cluster idn)
estimates store model2b

**Model 3

regress d.gini_gross l.gini_gross d.lpbp_cum15 d.csedhlth_cent l.csedhlth_cent d.rgdpch_cent d_rgdpchXl_csedhlth d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi d.inwardstockgdp l.inwardstockgdp d.edyears if included_g==1, vce(cluster idn)
estimates store model2c

estimates drop _all

* Table 2

**Model 1

regress d.redist l.redist d.lpbp_cum15 l.lpbp_cum15 d.csedhlth_cent l.csedhlth_cent d.csssw_cent l.csssw_cent d.rgdpch_cent l.rgdpch_cent d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi l.unemplywdi d.inwardstockgdp l.inwardstockgdp d.demrss01_cum45 l.demrss01_cum45 d.repressive_cum15 l.repressive_cum15 d.edyears l.edyears d.pop014wdi l.pop014wdi l.ethdiv, vce(cluster idn)
estimates store model3a

capture drop res1
predict res1,res
capture drop included_redist
gen included_redist=0
replace included_redist=1 if res1~=.
drop res1

**Model 2

regress d.redist l.redist d.csssw_cent l.csssw_cent d.rgdpch_cent l.rgdpch_cent d.unemplywdi l.unemplywdi d.inwardstockgdp l.inwardstockgdp d.repressive_cum15 d.edyears l.edyears , vce(cluster idn)
estimates store model3b

jackknife _b _se, cluster(idn) saving(jnfile_redist, replace) notable: regress d.redist l.redist d.csssw_cent l.csssw_cent d.rgdpch_cent l.rgdpch_cent d.unemplywdi l.unemplywdi d.inwardstockgdp l.inwardstockgdp d.repressive_cum15 d.edyears l.edyears , vce(cluster idn)


**Model 3

regress d.gini_net l.gini_net d.lpbp_cum15 l.lpbp_cum15 d.rgdpch_cent l.rgdpch_cent d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi d.inwardstockgdp l.inwardstockgdp d.demrss01_cum45 l.demrss01_cum45 d.repressive_cum15 l.repressive_cum15 d.edyears l.edyears  d.pop014wdi l.pop014wdi l.ethdiv if included_g==1, vce(cluster idn)
estimates store model3c

**Model 4

regress d.gini_net l.gini_net d.lpbp_cum15 l.lpbp_cum15 d.csedhlth_cent l.csedhlth_cent d.csssw_cent l.csssw_cent d.rgdpch_cent l.rgdpch_cent d_rgdpchXl_csedhlth d.inflationcpimfbr l.inflationcpimfbr d.unemplywdi d.inwardstockgdp l.inwardstockgdp d.demrss01_cum45 l.demrss01_cum45 d.repressive_cum15 l.repressive_cum15 d.edyears l.edyears  d.pop014wdi l.pop014wdi l.ethdiv if included_g==1, vce(cluster idn)
estimates store model3d

estimates drop _all

capture drop included_gross
capture drop included_redist
capture drop res1
