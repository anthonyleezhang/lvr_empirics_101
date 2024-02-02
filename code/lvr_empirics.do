*** Change the following directory to where lvr_101 locates in your PC
cd "/Users/hbaiyang/Dropbox/lvr_101"
clear

import delimited "intermediate_data/binance_ETH_data.csv"
*** Stata reads the timestamp as "YYYY-MM-DDTHH:MM:SS.sssZ", so we need to reformat it
gen double time=clock(close_time, "YMD#hm#"), after(close_time)
format time %tc

rename close close_price
keep time close_price
*** Save it to a tempfile for future use
tempfile binance
save `binance'

*** We need this additional line to deal with the format in univ2_ETH_USDC_data.csv as well
import delimited "intermediate_data/univ2_ETH_USDC_data.csv", clear
gen double time_reformat=clock(time, "YMD#hms#"), after(time)
format time_reformat %tc
drop time
rename time_reformat time

*** We merge the two datasets together
merge 1:1 time using `binance', keep(3) nogen
sort time
*** We need to do diff for t and t-1, so we set the time series
gen time_id=_n
tsset time_id

*** There is a check in the Rcode whether the difference is 1 min (60000 for the timestamp in Stata), we can also do this
* gen diff=time-L1.time
* gen check=(diff==60000) if !missing(diff)
* tab check
* drop time_id diff check
*** 10 differences not equal to 1 min; This fails. Proceeding, but be careful

*** Pool value at time t is USDC holding, plus ETH holding, valued at the last closing price in the period
gen pool_value = usdc_holding + eth_holding * close_price

*** Value mints and burns at closing price
gen double mint_value = usdc_minted + eth_minted * close_price
gen double burn_value = usdc_burned + eth_burned * close_price

** Pool PnL is difference in pool value
gen double pool_pnl = pool_value - L1.pool_value + burn_value - mint_value

*** Consider three different rebalancing strategies: 
*** Matching the LP holdings minutely, hourly, and daily

*** Minutely rebalancing strategy just holds lagged ETH

gen double rebal_minute_ETH = L1.eth_holding

*** 5min, Hourly and daily hold the start-of-5min, start-of-hour and start-of-day ETH holdings

*** Construct a strategy which holds, at each time, the ETH holdings at the start of 5 min
gen double min5_rounded = int(time/300000)*300000
format min5_rounded %tc
bys min5_rounded (time): gen double rebal_5min_ETH = eth_holding[1]

*** Construct a strategy which holds, at each time, the ETH holdings at the start of the hour
gen double hour_rounded = int(time/3600000)*3600000
format hour_rounded %tc
bys hour_rounded (time): gen double rebal_hour_ETH = eth_holding[1]

*** Construct a strategy which holds, at each time, the ETH holdings at the start of 4 hours
gen double hour4_rounded = int(time/14400000)*14400000
format hour4_rounded %tc
bys hour4_rounded (time): gen double rebal_4hour_ETH = eth_holding[1]

*** Start of the day
gen date=dofc(time)
format date %td
bys date (time): gen double rebal_day_ETH = eth_holding[1]

*** Rebalancing strategies pnl
tsset time_id
gen double rebal_minute_pnl = rebal_minute_ETH * (close_price-L1.close_price)
gen double rebal_5minute_pnl = rebal_5min_ETH * (close_price-L1.close_price)
gen double rebal_hour_pnl = rebal_hour_ETH * (close_price-L1.close_price)
gen double rebal_4hour_pnl = rebal_4hour_ETH * (close_price-L1.close_price)
gen double rebal_day_pnl = rebal_day_ETH * (close_price-L1.close_price)

*** Aggregate to daily P&L for ease of graphing
gcollapse (mean) pool_value (sum) *_pnl, by(date)

sort date

gen double hedged_pnl_minute = pool_pnl - rebal_minute_pnl
gen double hedged_pnl_5minute = pool_pnl - rebal_5minute_pnl
gen double hedged_pnl_hour = pool_pnl - rebal_hour_pnl
gen double hedged_pnl_4hour = pool_pnl - rebal_4hour_pnl
gen double hedged_pnl_day = pool_pnl - rebal_day_pnl

*** Start at 2021-08, for consistency with LVR paper
*** 22493 is 2021-08-01 in Stata
keep if date >= 22493

gen double cum_pool_pnl=pool_pnl[1]
replace cum_pool_pnl=pool_pnl[_n]+cum_pool_pnl[_n-1] if _n>1

gen double cum_hedged_pnl_minute=hedged_pnl_minute[1]
replace cum_hedged_pnl_minute=hedged_pnl_minute[_n]+cum_hedged_pnl_minute[_n-1] if _n>1

gen double cum_hedged_pnl_5minute=hedged_pnl_5minute[1]
replace cum_hedged_pnl_5minute=hedged_pnl_5minute[_n]+cum_hedged_pnl_5minute[_n-1] if _n>1

gen double cum_hedged_pnl_hour=hedged_pnl_hour[1]
replace cum_hedged_pnl_hour=hedged_pnl_hour[_n]+cum_hedged_pnl_hour[_n-1] if _n>1

gen double cum_hedged_pnl_4hour=hedged_pnl_4hour[1]
replace cum_hedged_pnl_4hour=hedged_pnl_4hour[_n]+cum_hedged_pnl_4hour[_n-1] if _n>1

gen double cum_hedged_pnl_day=hedged_pnl_day[1]
replace cum_hedged_pnl_day=hedged_pnl_day[_n]+cum_hedged_pnl_day[_n-1] if _n>1

summarize hedged_pnl_day
local sd_day=`r(sd)'
summarize pool_pnl
local sd_pool=`r(sd)'
display `sd_day'/`sd_pool'

*** Plot results

****************
*** Flow P&L ***
****************
tsset date
label var pool_pnl "Unhedged"
label var rebal_minute_pnl "Rebal Strat"
label var hedged_pnl_minute "Hedged"

*** We change the unit of Daily P&L to 1000s
preserve
replace pool_pnl=pool_pnl/1000
replace rebal_minute_pnl=rebal_minute_pnl/1000
replace hedged_pnl_minute=hedged_pnl_minute/1000

tsline rebal_minute_pnl pool_pnl hedged_pnl_minute, legend(row(1)) xtitle("Date", m("medsmall")) ytitle("Daily P&L (USD 1000s)", m("medsmall")) ylabel(-15000(5000)15000, ang(h))
graph export "plots/flow_pnl_Stata.png", as(png) replace

tsline hedged_pnl_minute, xtitle("Date", m("medsmall")) ytitle("Daily P&L (USD 1000s)", m("medsmall")) ylabel(0(100)300,ang(h))
graph export "plots/flow_pnl_hedgeonly_Stata.png", as(png) replace
restore

*****************************************
*** Cumulative P&L, hedging frequency ***
*****************************************
label var cum_pool_pnl "Unhedged"
label var cum_hedged_pnl_minute "Hedged (1 Min)"
label var cum_hedged_pnl_5minute "Hedged (5 Min)"
label var cum_hedged_pnl_hour "Hedged (1 Hr)"
label var cum_hedged_pnl_4hour "Hedged (4 Hr)"
label var cum_hedged_pnl_day "Hedged (1 Day)"

*** We change the unit of Cumul. P&L to 1000s
preserve
replace cum_pool_pnl=cum_pool_pnl/1000
replace cum_hedged_pnl_minute=cum_hedged_pnl_minute/1000
replace cum_hedged_pnl_5minute=cum_hedged_pnl_5minute/1000
replace cum_hedged_pnl_hour=cum_hedged_pnl_hour/1000
replace cum_hedged_pnl_4hour=cum_hedged_pnl_4hour/1000
replace cum_hedged_pnl_day=cum_hedged_pnl_day/1000

tsline cum_hedged_pnl_minute cum_hedged_pnl_5minute cum_hedged_pnl_hour cum_hedged_pnl_4hour cum_hedged_pnl_day cum_pool_pnl, legend(row(2) colfirst symx(6)) xtitle("Date", m("medsmall")) ytitle("Cumul. P&L (USD 1000s)", m("medsmall")) ylabel(-25000(25000)75000,ang(h))
graph export "plots/cum_pnl_freq_Stata.png", as(png) replace
restore

******************************
*** P&L, hedging frequency ***
******************************
label var hedged_pnl_minute "1 Min"
label var hedged_pnl_5minute "5 Min"
label var hedged_pnl_hour "1 Hr"
label var hedged_pnl_4hour "4 Hr"

*** We change the unit of Daily P&L to 1000s
preserve
replace hedged_pnl_minute=hedged_pnl_minute/1000
replace hedged_pnl_5minute=hedged_pnl_5minute/1000
replace hedged_pnl_hour=hedged_pnl_hour/1000
replace hedged_pnl_4hour=hedged_pnl_4hour/1000

tsline hedged_pnl_minute hedged_pnl_5minute hedged_pnl_hour hedged_pnl_4hour, legend(row(1)) xtitle("Date", m("medsmall")) ytitle("Daily P&L (USD 1000s)", m("medsmall")) ylabel(-250(250)500,ang(h))
graph export "plots/pnl_freq_Stata.png", as(png) replace
restore

