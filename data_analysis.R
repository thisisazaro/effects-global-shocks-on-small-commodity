

library(readxl)
library(corrplot)
library(tseries)

########################################
# Загружаем данные
########################################

data <- read_excel("dataset.xlsx", sheet = 2)
str(data)

########################################
# заполняем NA в monet_un экстраполяцией по переменной macro_un
########################################

# Построим модель на тех строках, где есть и macro_un, и monet_un
fit <- lm(monet_un ~ macro_un, data = data, na.action = na.exclude)

# Предскажем значения там, где monet_un отсутствует
data$monet_un[is.na(data$monet_un)] <- predict(fit, newdata = data[is.na(data$monet_un), ])

# проверить, как выглядела подгонка
plot(data$macro_un, data$monet_un,
     pch = 16, col = "blue", main = "monet_un vs macro_un",
     xlab = "macro_un", ylab = "monet_un")
abline(fit, col = "red", lwd = 2) 

########################################
# задаем группы переменных
########################################

# группы переменных по факторам
activity_vars <- c("prod_index", "gdp_world", "macro_un", "monet_un", "kilian_index")
price_vars    <- c("cpi_total", "cpi_oecd", "cpi_g7", "ppi_high_wb", "ppi_upper_mid_wb", "ppi_lower_mid_wb")
commodity_vars <- c("commodity_price", "energy_price", "agr_price", "fert_price", "metal_price")

########################################
# проверка корреляций по группам
########################################

# Функция для построения корреляций
plot_correlation <- function(df, vars, title) {
        mat <- cor(df[vars], use = "complete.obs")
        corrplot(mat, method = "color", addCoef.col = "black", tl.col = "black", 
                 number.cex = 0.7,
                 title = title, mar = c(0,0,2,0))
}

plot_correlation(data, activity_vars, "global activity variables")
plot_correlation(data, price_vars, "price variables")
plot_correlation(data, commodity_vars, "commodity variables")

########################################
# проверка стационарности (тест ADF)
########################################

# Функция для ADF-теста
check_stationarity <- function(df, vars) {
        sapply(vars, function(var) {
                ts_data <- ts(na.omit(df[[var]]))
                adf.test(ts_data)$p.value
        })
}

adf_activity <- check_stationarity(data, activity_vars)
adf_prices   <- check_stationarity(data, price_vars)
adf_commodities <- check_stationarity(data, commodity_vars)

print("ADF p-values for activity:")
print(adf_activity)
print("ADF p-values for prices:")
print(adf_prices)
print("ADF p-values for commodities:")
print(adf_commodities)

# переменные нестационарные
# Дифференцируем все переменные по группам
library(dplyr)

diff_data <- data |> 
        mutate(across(c(all_of(activity_vars), all_of(price_vars), all_of(commodity_vars)),
                      ~ c(NA, diff(.)), .names = "d_{.col}")) |> 
        slice(-1)  # убрать первую строку с NA

# Обновлённая функция ADF-теста для дифференцированных переменных
check_stationarity_diff <- function(df, vars) {
        sapply(vars, function(var) {
                diff_var <- paste0("d_", var)
                ts_data <- ts(na.omit(df[[diff_var]]))
                adf.test(ts_data)$p.value
        })
}

# Повторим ADF для дифференцированных переменных
adf_diff_activity  <- check_stationarity_diff(diff_data, activity_vars)
adf_diff_prices    <- check_stationarity_diff(diff_data, price_vars)
adf_diff_commodities <- check_stationarity_diff(diff_data, commodity_vars)

# Посмотрим результаты
cat("ADF p-values after differencing — ACTIVITY:\n")
print(adf_diff_activity)

cat("\nADF p-values after differencing — PRICES:\n")
print(adf_diff_prices)

cat("\nADF p-values after differencing — COMMODITIES:\n")
print(adf_diff_commodities)


# Выбираем переменные, у которых p-value > 0.05 после первой разности
nonstationary_prices <- names(adf_diff_prices[adf_diff_prices > 0.05])
print(nonstationary_prices)

# Добавим d2_ переменные (вторые разности)
diff_data <- diff_data %>%
        mutate(across(all_of(paste0("d_", nonstationary_prices)),
                      ~ c(NA, diff(.)), .names = "d2_{.col}")) %>%
        slice(-1)  # ещё один NA в начале

# Проверим стационарность d2_ переменных
check_stationarity_d2 <- function(df, vars) {
        sapply(vars, function(var) {
                varname <- paste0("d2_d_", var)
                ts_data <- ts(na.omit(df[[varname]]))
                adf.test(ts_data)$p.value
        })
}

adf_d2_prices <- check_stationarity_d2(diff_data, nonstationary_prices)

cat("ADF p-values after 2nd differencing — PRICES:\n")
print(adf_d2_prices)


# Итоговая версия ценовых переменных для PCA
# Используем только те, которые стали точно стационарными:

# Собираем финальный датафрейм для PCA по ценам
price_final <- diff_data %>%
        select(
                d2_d_cpi_g7,
                d2_d_ppi_high_wb,
                d2_d_ppi_upper_mid_wb,
                d_ppi_lower_mid_wb
        )

# Переименование для удобства
colnames(price_final) <- c("cpi_g7", "ppi_high", "ppi_upper_mid", "ppi_lower_mid")

########################################
# Выполнение PCA для каждой группы
########################################

# PCA для Activity (используем все переменные с префиксом "d_")
activity_pca <- prcomp(activity_diff %>% select(starts_with("d_")), scale. = TRUE)
summary(activity_pca)
factor_activity <- activity_pca$x[, 1]  # первый компонент

# PCA для Prices (используем подготовленные переменные)
price_pca <- prcomp(price_final, scale. = TRUE)
summary(price_pca)
factor_price <- price_pca$x[, 1]  # первый компонент

# PCA для Commodities (используем все переменные с префиксом "d_")
commodity_pca <- prcomp(commodity_diff %>% select(starts_with("d_")), scale. = TRUE)
summary(commodity_pca)
factor_commodity <- commodity_pca$x[, 1]  # первый компонент

########################################
# Собираем итоговый датафрейм с факторами
########################################

# Заметим, что:
# - activity_diff имеет (n-1) наблюдений,
# - price_final имеет (n-2) наблюдений (так как для цен применяли вторую разность),
# - commodity_diff имеет (n-1) наблюдений.
# Поэтому для итогового объединения берем пересечение по наблюдениям.

min_obs <- min(nrow(activity_diff), nrow(price_final), nrow(commodity_diff))

# Приводим факторы к общей длине
factors_df <- data.frame(
        date = data$date[(nrow(data) - min_obs + 1):nrow(data)],
        factor_activity = factor_activity[1:min_obs],
        factor_price    = factor_price[1:min_obs],
        factor_commodity = factor_commodity[1:min_obs]
)

head(factors_df)










