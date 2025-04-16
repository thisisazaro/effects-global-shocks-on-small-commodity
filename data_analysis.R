

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
activity_vars <- c("prod_index", "macro_un", "kilian_index", "fdi", "trade")
price_vars    <- c("median_cpi_wb","median_ppi_wb", "median_deflator_wb","median_core_wb") 
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


# Итоговая версия ценовых переменных для PCA
# Используем только те, которые стали точно стационарными:

# Собираем финальный датафрейм для PCA по ценам
price_final <- diff_data %>%
        dplyr::select(
                d_median_cpi_wb,
                d_median_ppi_wb,
                d_median_deflator_wb,
                d_median_core_wb
        )

# Переименование для удобства
colnames(price_final) <- c("median_cpi_wb", "median_ppi_wb", "median_deflator_wb", "median_core_wb")

activity_final <- diff_data %>%
        dplyr::select(
                d_prod_index,
                d_macro_un,
                d_kilian_index,
                d_fdi,
                d_trade
        )

# Переименование для удобства
colnames(activity_final) <- c("prod_index", "macro_un", "kilian_index", "fdi", "trade")

commodity_final <- diff_data %>%
        dplyr::select(
                d_commodity_price,
                d_energy_price,
                d_agr_price,
                d_fert_price,
                d_metal_price
        )

# Переименование для удобства
colnames(commodity_final) <- c("commodity_price", "energy_price", "agr_price", 
                           "fert_price", "metal_price")



########################################
# Выполнение PCA для каждой группы
########################################

# PCA: Глобальная активность
activity_pca <- prcomp(activity_final, scale. = TRUE)
factor_activity <- activity_pca$x[, 1] # первый компонент
summary(activity_pca)

# PCA: Цены
price_pca <- prcomp(price_final, scale. = TRUE)
factor_price <- price_pca$x[, 1] # первый компонент
summary(price_pca)

# PCA: Сырьевые цены
commodity_pca <- prcomp(commodity_final, scale. = TRUE)
factor_commodity <- commodity_pca$x[, 1] # первый компонент
summary(commodity_pca)


# Scree Plot для активности
plot(activity_pca, type = "l", main = "Scree Plot — Глобальная активность")

# Scree Plot для цен
plot(price_pca, type = "l", main = "Scree Plot — Цены")

# Scree Plot для сырьевых цен
plot(commodity_pca, type = "l", main = "Scree Plot — Сырьевые цены")

# Barplot объяснённой дисперсии
barplot(activity_pca$sdev^2 / sum(activity_pca$sdev^2),
        main = "PCA — Активность", ylab = "Доля дисперсии", names.arg = paste0("PC", 1:5))

barplot(price_pca$sdev^2 / sum(price_pca$sdev^2),
        main = "PCA — Цены", ylab = "Доля дисперсии", names.arg = paste0("PC", 1:4))

barplot(commodity_pca$sdev^2 / sum(commodity_pca$sdev^2),
        main = "PCA — Сырьевые цены", ylab = "Доля дисперсии", names.arg = paste0("PC", 1:5))

# Biplot для активности
biplot(activity_pca, main = "PCA activity")

# Biplot для цен
biplot(price_pca, main = "PCA - price")

# Biplot для сырьевых цен
biplot(commodity_pca, main = "PCA - commodity")


########################################
# Собираем итоговый датафрейм с факторами
########################################

factors_df <- data.frame(
        date = tail(data$date, length(factor_activity)),
        factor_activity,
        factor_price,
        factor_commodity
)

library(ggplot2)

# Активность
ggplot(factors_df, aes(x = date, y = factor_activity)) +
        geom_line(color = "steelblue") +
        labs(title = "global activity factor", x = "year", y = "factor")

# Цены
ggplot(factors_df, aes(x = date, y = factor_price)) +
        geom_line(color = "darkred") +
        labs(title = "Price Pressure Factor", x = "Year", y = "Factor")

# Сырьё
ggplot(factors_df, aes(x = date, y = factor_commodity)) +
        geom_line(color = "forestgreen") +
        labs(title = "Commodity Factor", x = "Year", y = "Factor")


########################################
# построим VAR-модель
########################################

library(vars)
# VAR по 3 факторам (автоопределение лага по AIC)
VARselect(factors_df[, -1], lag.max = 10, type = "const")$selection

# Строим модель с выбранным числом лагов (например, 1)
var_model <- VAR(factors_df[, -1], p = 1, type = "const")
summary(var_model)


# построим IRF (импульсные отклики)
irf_commodity_shock <- irf(var_model, impulse = "factor_commodity", response = c("factor_activity", "factor_price"),
                  n.ahead = 12, boot = TRUE)

plot(irf_commodity_shock)



########################################
# построим SVAR-модель
########################################

library(vars)
library(svars)

# VAR с одним лагом — у тебя уже построен как var_model
# Теперь идентификация SVAR — например, методом Cholesky:
svar_model <- id.chol(var_model)
summary(svar_model)

# Историческое разложение
hd <- hd(svar_model)
str(hd)


########################################
# historical decomposition
########################################

library(dplyr)
library(tidyr)
library(ggplot2)

# Извлекаем data.frame
df_hd <- hd$hidec
df_hd

# Переименуем столбцы для удобства
colnames(df_hd) <- c("time", "demeaned", "constructed", "shock_activity", 
                     "shock_price", "shock_commodity")
df_hd$year <- 1972:2024

# Собираем шоки в long-формат
df_long <- df_hd %>%
        dplyr::select(time, shock_activity, shock_price, shock_commodity) %>%
        pivot_longer(-time, names_to = "Shock", values_to = "Contribution") %>%
        mutate(Shock = recode(Shock,
                              "shock_activity" = "Global Demand Shock",
                              "shock_price" = "Global Price Shock",
                              "shock_commodity" = "Global Commodity Shock"))
df_long <- df_long %>%
        left_join(dplyr::select(df_hd, time, year), by = "time")

ggplot() +
        geom_area(data = df_long, aes(x = year, y = Contribution, fill = Shock), alpha = 0.4) +
        geom_line(data = df_hd, aes(x = year, y = constructed), linewidth = 1, color = "blue") +
        facet_wrap(~Shock, scales = "free_y", ncol = 1) +
        scale_x_continuous(breaks = seq(1972, 2024, by = 5)) +
        theme_minimal(base_size = 12) +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                strip.text = element_text(face = "bold"),
                axis.text.x = element_text(angle = 0, size = 10),
                plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(
                title = "historical decomposition of factor activity",
                y = "accumulated contribution",
                x = "year"
        ) +
        theme(legend.position = "none")



# factor_activity (1-я переменная)
hd_activity <- hd(svar_model, series = 1)$hidec

# factor_price (2-я переменная)
hd_price <- hd(svar_model, series = 2)$hidec

# factor_commodity (3-я переменная)
hd_commodity <- hd(svar_model, series = 3)$hidec


process_hd <- function(df_hd, var_name) {
        colnames(df_hd) <- c("time", "demeaned", "constructed", 
                             "shock_activity", "shock_price", "shock_commodity")
        df_hd$year <- 1972:2024
        df_long <- df_hd %>%
                dplyr::select(time, shock_activity, shock_price, shock_commodity) %>%
                pivot_longer(-time, names_to = "Shock", values_to = "Contribution") %>%
                mutate(Shock = recode(Shock,
                                      "shock_activity" = "Global Demand Shock",
                                      "shock_price" = "Global Price Shock",
                                      "shock_commodity" = "Global Commodity Shock")) %>%
                left_join(dplyr::select(df_hd, time, year, constructed), by = "time") %>%
                mutate(Variable = var_name)
        return(df_long)
}

df_all <- bind_rows(
        process_hd(hd_activity, "Global Activity"),
        process_hd(hd_price, "Global Prices"),
        process_hd(hd_commodity, "Global Commodities")
)

ggplot(df_all, aes(x = year, y = Contribution, fill = Shock)) +
        geom_area(alpha = 0.4) +
        geom_line(aes(y = constructed), color = "blue", linewidth = 1) +
        facet_grid(Variable ~ Shock, scales = "free_y") +
        scale_x_continuous(breaks = seq(1972, 2024, by = 5)) +
        theme_minimal(base_size = 12) +
        theme(
                panel.grid.minor = element_blank(),
                panel.grid.major.y = element_blank(),
                strip.text = element_text(face = "bold"),
                axis.text.x = element_text(angle = 0, size = 9),
                plot.title = element_text(face = "bold", hjust = 0.5)
        ) +
        labs(
                title = "Historical Decomposition of Global Factors 1972-2024",
                y = "Accumulated Contribution",
                x = "Year"
        ) +
        theme(legend.position = "none")
