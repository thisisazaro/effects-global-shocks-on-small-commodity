# Другие переменные
library(readxl)
library(corrplot)
library(tseries)
library(dplyr)
library(ggplot2)
library(vars)
data <- read_excel("dataset.xlsx", sheet = 2)
getwd() #показ директории 

#Модель на годовом уровне
#Экстраполяция данных монетарной неопределенности по временному ряду макро неопределенности
fit <- lm(monet_un ~ macro_un, data = data, na.action = na.exclude)
data$monet_un[is.na(data$monet_un)] <- predict(fit, newdata = data[is.na(data$monet_un), ])

#Создаем факторы
activity_vars <- c("prod_index", "macro_un", "kilian_index", "fdi" , "trade") # подумать над доп переменной для того, чтобы переменные были связаны
price_vars    <- c("median_cpi_wb","median_ppi_wb", "median_deflator_wb","median_core_wb") 
#"ppi_high_wb","ppi_upper_mid_wb", "ppi_lower_mid_wb","cpi_high_wb","cpi_upper_mid_wb", "cpi_lower_mid_wb"
commodity_vars <- c("commodity_price", "energy_price", "agr_price", "fert_price", "metal_price")

#Корреляция переменных
plot_correlation <- function(df, vars, title) {
  mat <- cor(df[vars], use = "complete.obs")
  corrplot(mat, method = "color", addCoef.col = "black", tl.col = "black",
           number.cex = 0.7, title = title, mar = c(0,0,2,0))
}

plot_correlation(data, activity_vars, "global activity variables")
plot_correlation(data, price_vars, "price variables") #ТОПЧИК!
plot_correlation(data, commodity_vars, "commodity variables")

check_stationarity <- function(df, vars) {
  sapply(vars, function(var) {
    ts_data <- ts(na.omit(df[[var]]))
    adf.test(ts_data)$p.value #вытащили p-value
  })
}

adf_activity <- check_stationarity(data, activity_vars)
print(adf_activity) #нестац
adf_price <- check_stationarity(data, price_vars)
print(adf_price) #нестац
adf_commodity <- check_stationarity(data, commodity_vars)
print(adf_commodity) #нестац. p-value

diff_data <- data |> 
  mutate(across(c(all_of(activity_vars), all_of(price_vars), all_of(commodity_vars)),
                ~ c(NA, diff(.)), .names = "d_{.col}")) |> 
  slice(-1)
check_stationarity_diff <- function(df, vars) {
  sapply(vars, function(var) {
    diff_var <- paste0("d_", var)
    ts_data <- ts(na.omit(df[[diff_var]]))
    adf.test(ts_data)$p.value
  })
}
adf_diff_activity  <- check_stationarity_diff(diff_data, activity_vars)
print(adf_diff_activity) #стац
adf_diff_commodity  <- check_stationarity_diff(diff_data, commodity_vars)
print(adf_diff_commodity) #стац
adf_diff_prices  <- check_stationarity_diff(diff_data, price_vars)
print(adf_diff_prices) # НА НОВЫХ ДАННЫХ СТАЦИОНАРНА НА ПЕРВОМ УРОВНЕ РАЗНОСТИ

#check_stationarity_diff <- function(df, vars) {
#  sapply(vars, function(var) {
#    diff_var <- paste0("d_", var)
#    ts_data <- ts(na.omit(df[[diff_var]]))
#    adf.test(ts_data)$p.value
#  })
#}
#nonstationary_prices <- names(adf_diff_prices[adf_diff_prices > 0.05])
#diff_data <- diff_data %>%
#  mutate(across(all_of(paste0("d_", nonstationary_prices)),
#                ~ c(NA, diff(.)), .names = "d2_{.col}")) %>%
# slice(-1)
#check_stationarity_d2 <- function(df, vars) {
#  sapply(vars, function(var) {
#    varname <- paste0("d2_d_", var)
#    ts_data <- ts(na.omit(df[[varname]]))
#    adf.test(ts_data)$p.value
#  })
#}
#adf_d2_prices <- check_stationarity_d2(diff_data, nonstationary_prices)
#print(adf_d2_prices) #cpi нестационарные CPI tot, CPI OECD; cpi _g7 стационарна на альфа=10% = убрали 

# создаем финальные группы для дальнейшего факторного анализа
price_final <- diff_data %>%
  dplyr::select(
    d_median_cpi_wb,
    d_median_ppi_wb,
    d_median_core_wb,
    d_median_deflator_wb
  )
colnames(price_final) <- c("median_cpi_wb", "median_ppi_wb", "median_core_wb","median_deflator_wb","ppi_lower_mid_wb")

activity_final <- diff_data %>%
  dplyr::select(
    d_prod_index,
    d_macro_un,
    d_kilian_index,
    d_fdi,
    d_trade
  )
colnames(activity_final) <- c("prod_index", "macro_un", "kilian_index", 
                              "fdi", "trade")
commodity_final <- diff_data %>%
  dplyr::select(
    d_commodity_price, 
    d_energy_price, 
    d_agr_price, 
    d_fert_price, 
    d_metal_price
  )
colnames(commodity_final) <- c("commodity_price", "energy_price", "agr_price", 
                               "fert_price", "metal_price")

#шаг 3 факторы сделать выполнение РСА в каждой группе = МГК - сжимаем вместе данные для того чтобы делаем одну компоненту
activity_pca <- prcomp(activity_final, scale. = TRUE) #мы берем 1 компоненту, которые объясняет больше всего наших остальных переменных 
summary(activity_pca) # 1 компоненты объясняет 48% данных - для макроэкономики всегда 50%
factor_activity <- activity_pca$x[, 1] #выбираем 1 компоненту в данных
#вместе рса1 и рса2 объясняют 60% данных
price_pca <- prcomp(price_final, scale. = TRUE)
summary(price_pca) #80% очень высокое объяснение, но попробовать заменить ипц на мирбанковские 
factor_price <- price_pca$x[, 1] #выбираем 1 компоненту в данных
commodity_pca <- prcomp(commodity_final, scale. = TRUE)
summary(commodity_pca) #очень высокие показатели 61%
factor_commodity <- commodity_pca$x[, 1] #выбираем 1 компоненту в данных

#делаем датафрейм из факторов = глобальные латентные факторы
factors_df <- data.frame(
  date = tail(data$date, length(factor_activity)),
  factor_activity,
  factor_price,
  factor_commodity
)

p1 <- ggplot(factors_df, aes(x = date, y = factor_activity)) +
  geom_line(color = "steelblue") +
  labs(title = "global activity factor", x = "year", y = "factor") #нет тренда, нет сезонности
# резкие падения - это шоки 2020, 2008, 2010 - возможно 

p2 <- ggplot(factors_df, aes(x = date, y = factor_price)) +
  geom_line(color = "steelblue") +
  labs(title = "global price factor", x = "year", y = "factor")
#
p3 <- ggplot(factors_df, aes(x = date, y = factor_commodity)) +
  geom_line(color = "steelblue") +
  labs(title = "global commodity factor", x = "year", y = "factor")
p1+p2+p3

#VAR model => ЛАГ 1
VARselect(factors_df[, -1], lag.max = 10, type = "const")$selection #lag = 1 по инфо критериям
var_model <- VAR(factors_df[, -1], p = 1, type = "const")
summary(var_model)
#байесовская модель? = АРСТАН пакет = на подумать 

#как коммодити шок в сырьевом факторе влияет на глобальную деловую активность и на ценовой фактор
irf_commodity_shock <- irf(var_model, impulse = "factor_commodity", response = c("factor_activity", "factor_price"),
                           n.ahead = 12, boot = TRUE)
plot(irf_commodity_shock)

irf_price_shock <- irf(var_model, impulse = "factor_price", response = c("factor_activity", "factor_commodity"),
                       n.ahead = 12, boot = TRUE)
plot(irf_price_shock)

irf_activity_shock <- irf(var_model, impulse = "factor_activity", response = c("factor_price", "factor_commodity"),
                          n.ahead = 12, boot = TRUE)
plot(irf_activity_shock)


