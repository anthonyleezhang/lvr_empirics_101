
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)

setwd('C:/Dropbox/projects/lvr_101')
rm(list = ls())

binance_data_full = fread('intermediate_data/binance_ETH_data.csv')
binance_data_full[, time := floor_date(close_time, unit = '1min')]
binance_data = binance_data_full[, .(time, close_price = close)]

dune_merged = fread('intermediate_data/univ2_ETH_USDC_data.csv')

data = merge(binance_data, dune_merged, by = 'time')
data = data[order(time)]

# timevec = data[, as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
# all(diff(timevec) == 1)
# CHECK: This fails. Proceeding, but be careful

# Pool value at time t is USDC holding, plus ETH holding, valued at the last closing price in the period
data[, pool_value := USDC_holding + ETH_holding * close_price]

# Value mints and burns at closing price
data[, mint_value := USDC_minted + ETH_minted * close_price]
data[, burn_value := USDC_burned + ETH_burned * close_price]

# Pool PnL is difference in pool value
data[, pool_pnl := c(NA, diff(pool_value)) + burn_value - mint_value]

# Consider three different rebalancing strategies: 
# Matching the LP holdings minutely, hourly, and daily

# Minutely rebalancing strategy just holds lagged ETH

data[, rebal_minute_ETH := as.numeric(shift(ETH_holding, n = 1, type = 'lag'))]

# 5min, Hourly and daily hold the start-of-5min, start-of-hour and start-of-day ETH holdings

# Construct a strategy which holds, at each time, the ETH holdings at the start of the hour
data[, min5_rounded := floor_date(time, '5 minutes')]
data[, rebal_5min_ETH := ETH_holding[1], by = min5_rounded]

# Construct a strategy which holds, at each time, the ETH holdings at the start of the hour
data[, hour_rounded := floor_date(time, 'hour')]
data[, rebal_hour_ETH := ETH_holding[1], by = hour_rounded]

# Construct a strategy which holds, at each time, the ETH holdings at the start of 4 hours
data[, hour4_rounded := floor_date(time, '4 hours')]
data[, rebal_4hour_ETH := ETH_holding[1], by = hour4_rounded]

# Start of the day
data[, date := as.Date(time)]
data[, rebal_day_ETH := ETH_holding[1], by = date]

# Rebalancing strategies pnl
data[, rebal_minute_pnl := rebal_minute_ETH * c(NA, diff(close_price))]
data[, rebal_5minute_pnl := rebal_5min_ETH * c(NA, diff(close_price))]
data[, rebal_hour_pnl := rebal_hour_ETH * c(NA, diff(close_price))]
data[, rebal_4hour_pnl := rebal_4hour_ETH * c(NA, diff(close_price))]
data[, rebal_day_pnl := rebal_day_ETH * c(NA, diff(close_price))]

# Aggregate to daily P&L for ease of graphing
aggdata = data[, .(
  pool_value = mean(pool_value, na.rm = TRUE),
  pool_pnl = sum(pool_pnl, na.rm = TRUE),
  rebal_minute_pnl = sum(rebal_minute_pnl, na.rm = TRUE),
  rebal_5minute_pnl = sum(rebal_5minute_pnl, na.rm = TRUE),
  rebal_hour_pnl = sum(rebal_hour_pnl, na.rm = TRUE),
  rebal_4hour_pnl = sum(rebal_4hour_pnl, na.rm = TRUE),
  rebal_day_pnl = sum(rebal_day_pnl, na.rm = TRUE)
), by = date]

aggdata = aggdata[order(date)]

aggdata[, hedged_pnl_minute := pool_pnl - rebal_minute_pnl]
aggdata[, hedged_pnl_5minute := pool_pnl - rebal_5minute_pnl]
aggdata[, hedged_pnl_hour := pool_pnl - rebal_hour_pnl]
aggdata[, hedged_pnl_4hour := pool_pnl - rebal_4hour_pnl]
aggdata[, hedged_pnl_day := pool_pnl - rebal_day_pnl]

# Start at 2021-08, for consistency with LVR paper
aggdata = aggdata[date >= '2021-08-01']

aggdata[, cum_pool_pnl := cumsum(pool_pnl)]
aggdata[, cum_hedged_pnl_minute := cumsum(hedged_pnl_minute)]
aggdata[, cum_hedged_pnl_5minute := cumsum(hedged_pnl_5minute)]
aggdata[, cum_hedged_pnl_hour := cumsum(hedged_pnl_hour)]
aggdata[, cum_hedged_pnl_4hour := cumsum(hedged_pnl_4hour)]
aggdata[, cum_hedged_pnl_day := cumsum(hedged_pnl_day)]

aggdata[, sd(hedged_pnl_day) / sd(pool_pnl)]

fwrite(data, file = 'temp_data.csv')
fwrite(aggdata, file = 'temp_aggdata.csv')

# Plot results

############
# Flow P&L
############

flow_plotdata = rbindlist(list(
  aggdata[, .(date, y = pool_pnl, type = 'Unhedged')],
  aggdata[, .(date, y = rebal_minute_pnl, type = 'Rebal Strat')],
  aggdata[, .(date, y = hedged_pnl_minute, type = 'Hedged')]
))

my_linewidth = 1
my_textsize = 40
axistextsize = 30
axistitletextsize = 30
legendlinesize = 7
legendlinewidth = 2

flow_plotdata[, type := factor(type, levels = c('Rebal Strat', 'Unhedged', 'Hedged'))]

plot1 = ggplot(copy(flow_plotdata), aes(x = date, y = y, group = type, color = type)) + 
  geom_line(linewidth = my_linewidth, alpha = 0.7) + 
  scale_x_date(name = 'Date') + 
  scale_y_continuous(name = 'Daily P&L (USD 1000s)', label = function(x) {return(x/1000)}, 
                     breaks = (-10:10)*5000000, limits = c(-16000000, 16000000)) + 
  scale_color_gdocs(name = '') + 
  theme_bw() + 
  theme(legend.text = element_text(size = axistextsize),
        axis.text = element_text(size = axistextsize),
        axis.title.y = element_text(size = axistitletextsize),
        axis.title.x = element_text(size = axistitletextsize),
        legend.key.size = unit(legendlinesize, "lines"), 
        # legend.key.linewidth = unit(5, "lines"), 
        legend.position = 'bottom') +
  guides(color = guide_legend(override.aes = list(linewidth = legendlinewidth)))

ggsave(plot1, file = 'plots/flow_pnl.png', width = 20, height = 11)

plot2 = ggplot(copy(flow_plotdata[type == 'Hedged']), aes(x = date, y = y)) + 
  geom_line(linewidth = my_linewidth, alpha = 0.7) + 
  scale_x_date(name = 'Date') + 
  scale_y_continuous(name = 'Daily P&L (USD 1000s)', label = function(x) {return(x/1000)}) + 
  scale_color_gdocs(name = '') + 
  theme_bw() + 
  theme(legend.text = element_text(size = axistextsize),
        axis.text = element_text(size = axistextsize),
        axis.title.y = element_text(size = axistitletextsize),
        axis.title.x = element_text(size = axistitletextsize),
        legend.key.size = unit(legendlinesize, "lines"), 
        # legend.key.linewidth = unit(5, "lines"), 
        legend.position = 'bottom') +
  guides(color = guide_legend(override.aes = list(linewidth = legendlinewidth)))

ggsave(plot2, file = 'plots/flow_pnl_hedgedonly.png', width = 20, height = 11)

############
# Cumulative P&L, hedging frequency
############

cum_plotdata = rbindlist(list(
  aggdata[, .(date, y = cum_pool_pnl, type = 'Unhedged')],
  aggdata[, .(date, y = cum_hedged_pnl_minute, type = 'Hedged (1 Min)')],
  aggdata[, .(date, y = cum_hedged_pnl_5minute, type = 'Hedged (5 Min)')],
  aggdata[, .(date, y = cum_hedged_pnl_hour, type = 'Hedged (1 Hr)')],
  aggdata[, .(date, y = cum_hedged_pnl_4hour, type = 'Hedged (4 Hr)')],
  aggdata[, .(date, y = cum_hedged_pnl_day, type = 'Hedged (1 Day)')]
))

cum_plotdata[, type := factor(type, levels = c('Hedged (1 Min)', 'Hedged (5 Min)', 'Hedged (1 Hr)', 'Hedged (4 Hr)', 'Hedged (1 Day)', 'Unhedged'))]

plot3 = ggplot(cum_plotdata, aes(x = date, y = y, group = type, color = type)) + 
  geom_line(linewidth = my_linewidth, alpha = 0.7) + 
  scale_x_date(name = 'Date') + 
  scale_y_continuous(name = 'Cumul. P&L (USD 1000s)', label = function(x) {return(x/1000)},
                     breaks = (-10:10)*25000000) + 
  scale_color_gdocs(name = '') + 
  theme_bw() + 
  theme(legend.text = element_text(size = axistextsize),
        axis.text = element_text(size = axistextsize),
        axis.title.y = element_text(size = axistitletextsize),
        axis.title.x = element_text(size = axistitletextsize),
        legend.key.size = unit(legendlinesize*0.7, "lines"), 
        # legend.key.linewidth = unit(5, "lines"), 
        legend.position = 'bottom') +
  guides(color = guide_legend(override.aes = list(linewidth = legendlinewidth)))

ggsave(plot3, file = 'plots/cum_pnl_freq.png', width = 20, height = 11)


############
# P&L, hedging frequency
############

freq_plotdata = rbindlist(list(
  aggdata[, .(date, y = hedged_pnl_minute, type = '1 Min')],
  aggdata[, .(date, y = hedged_pnl_5minute, type = '5 Min')],
  aggdata[, .(date, y = hedged_pnl_hour, type = '1 Hr')],
  aggdata[, .(date, y = hedged_pnl_4hour, type = '4 Hr')]
))

freq_plotdata[, type := factor(type, levels = c('1 Min', '5 Min', '1 Hr', '4 Hr', 'Daily'))]

plot4 = ggplot(freq_plotdata, aes(x = date, y = y, group = type, color = type)) + 
  geom_line(linewidth = my_linewidth, alpha = 0.7) + 
  scale_x_date(name = 'Date') + 
  scale_y_continuous(name = 'Daily P&L (USD 1000s)', label = function(x) {return(x/1000)}) + 
  scale_color_gdocs(name = '') + 
  theme_bw() + 
  theme(legend.text = element_text(size = axistextsize),
        axis.text = element_text(size = axistextsize),
        axis.title.y = element_text(size = axistitletextsize),
        axis.title.x = element_text(size = axistitletextsize),
        legend.key.size = unit(legendlinesize, "lines"), 
        # legend.key.linewidth = unit(5, "lines"), 
        legend.position = 'bottom') +
  guides(color = guide_legend(override.aes = list(linewidth = legendlinewidth)))

ggsave(plot4, file = 'plots/pnl_freq.png', width = 20, height = 11)



