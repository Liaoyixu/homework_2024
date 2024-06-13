##HEADER --------------------------------------------------------------------
#
#Scrip name:   "ts_fs_ml"
#Purpose:      "Timeseries data analysis and model with R"
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/5/8        
#
#SETUP ---------------------------------------------------------------------

#创建时间簇timestamp

install.packages("lubridate")
library("lubridate")

date = ymd("2017-01-31")
print(date)

library(tidyverse)
install.packages("nycflights13")
data(flights,package = "nycflights13")
head(flights)

flights %>%
  select(year,month,day,hour,minute) %>%
  mutate(dep_date_time = make_datetime(year,month,day,hour,minute)) %>% # mutate增加列
  head()

# 创建时间序列timeseries

data = read.table('Desktop/研一下学期课程/数据驱动的生态学研究方法/RawBiomassData.txt',h=TRUE)
head(data)

# 去除无需数据
data_clean <- data %>%
  dplyr::select(-YEAR) %>%
  #drop_na() %>% 去除na值导致后期分析效果不好，先不去除
  distinct()
head(data_clean)

# 查看采样点信息及数量
unique(data_clean$STATION)
table(data_clean$STATION)

# 查看物种信息
unique(data_clean$SP)
table(data_clean$SP)

# 筛选所需表格信息
sdata <- data_clean %>%
  subset(STATION == "VERCah" & SP == "VAI") #选择采样点为VERCah物种为VAI的信息

# 转换时间序列，从1994年开始
data_ts = ts(data = sdata[,-c(1:5)],start = c(1994),frequency = 1)

# 可视化VAI随年时间的变化
install.packages("forecast")
library(forecast)
library(ggplot2)

autoplot(data_ts,facets = TRUE) + ggtitle("VAI of Doubs river") +
  ylab("Changes") + xlab("Year")

# 使用timetk创建时间序列
install.packages("timetk")
library(timetk)
install.packages("tsibble")
library(tsibble)

# 数据转换与清理
data_ts_tk <- sdata %>% tk_tbl() %>% select(-1) %>%
  # 将数据集sdata转换为tibble格式，便于后续操作
  rename(date = DATE) %>% #重名名date为DATE
  relocate(date, .before = STATION) %>%  # 将date移动到STATION之前
  pivot_longer(cols = c("BIOMASS", "DENSITY"))  
# 宽表（包含BIOMASS和DENSITY列）转换为长表

# 绘制时间序列图
data_ts_tk %>% group_by(name) %>%
  plot_time_series(date, value, .facet_ncol = 2, .facet_scale = "free",
                   .interactive = FALSE,.title = "VAI of Le Doubs river" )

# 按年汇总和填补缺失值，然后绘图
data_ts_tk1 <- data_ts_tk %>% group_by(name) %>% summarise_by_time(date, 
    .by = "year",value = first(value)) %>% # 按年汇总数据，取每年第一条记录的值
  pad_by_time(date, .by = "year") %>% # 按年填补时间序列中的缺失值
  plot_time_series(date, value,.facet_ncol = 2,.facet_scale = "free",
                   .interactive = FALSE,.title = "VAI of Le Doubs river")

###############################################################################
# 数据重表达、降维
# 下载并载入所需包
install.packages("TSrepr")
library(TSrepr)
install.packages("patchwork")
library(patchwork)
library(ggplot2)
sdata

# 创建BIOMASS时间序列数据
biomass_ts <- ts(sdata[,-c(1:5,7)], # 保留表格中BIOMASS
              start= c(1994,1),frequency =1)

# 绘制BIOMASS时间序列图
p1 <- autoplot(biomass_ts) + ggtitle("VAI biomass of Doubs river1") +
  ylab("Changes") + xlab("Year")

# 小波变换
data_dwt <- repr_dwt(sdata$BIOMASS, level = 1) # 对sdata数据集中的BIOMASS列进行小波变换，使用level = 1参数指定变换级别

# 创建小波变换后的时间序列数据
data_dwt_ts <- ts(data_dwt,start= c(1994,1),frequency =1)

# 小波变换可以同时在时间域和频率域中分析信号。与传统的傅里叶变换不同，小波变换能够提供信号在不同时间点上的频率信息，这对处理非平稳信号（其统计特性随时间变化的信号）特别有用

# 绘制小波变换后的时间序列图
p2 <- autoplot(data_dwt_ts) + ggtitle("VAI biomass of Doubs river2") +
  ylab("Changes") + xlab("Year")

# 可视化降维数据
p1+p2 

################################################################################
# 处理数据中异常值，比较使用时间序列插值方法填补缺失值，生成的数据集

# 下载并加载包
install.packages("DataExplorer")
library(DataExplorer)
install.packages("ggthemes")
library(ggthemes)

# 找到数据中缺失值

# 按年汇总和填补缺失值
data_ts_tk2 <- data_ts_tk %>% group_by(name) %>% # 将data_ts_tk数据集按name列分组
  summarise_by_time(date, .by = "year",value = first(value)) %>%
  #按年汇总数据，取每年第一条记录的值作为该年的值
  pad_by_time(date, .by = "year") # 按年填补时间序列中的缺失值

# 查看数据集的最后几行
tail(data_ts_tk2)

# 绘制缺失值图
data_ts_tk2 %>% plot_missing(ggtheme = theme_calc(), 
    # 使用plot_missing函数绘制数据集中每列缺失值的百分比
    title = "Percent Missing Values by Column" )

# 填补缺失值并创建新数据集
data_ts_tk3 <- data_ts_tk2 %>% group_by(name) %>%
  pad_by_time(date, .by = "year") %>%
  # 按年填补时间序列中的缺失值，确保所有年份都有数据
  mutate_at(vars(value), .funs = ts_impute_vec, period = 1) 
  # 对value列应用ts_impute_vec函数，使用周期为1的时间序列插值方法填补缺失值

# 绘制时间序列图
data_ts_tk3 %>% plot_time_series(date, value, .facet_ncol = 2, 
                   .facet_scale = "free",.interactive = FALSE,
                   .title = "VAI of Le Doubs river") 

tail(data_ts_tk3)

###############################################################################

# 查找时间序列中的离异值
data_ts_tk3 %>% group_by(name) %>%
  plot_anomaly_diagnostics(.date = date,.value = value,.facet_ncol = 2,
    .interactive=FALSE,.title = "Anomaly Diagnostics",.anom_color ="#FB3029", 
    .max_anomalies = 0.07,.alpha = 0.05)  # 显著性水平为0.05
# 结果无离异值

# ACF自相关验证
 data_ts_tk3 %>% group_by(name) %>%
  plot_acf_diagnostics(date, value, # 绘制自相关函数（ACF）和偏自相关函数（PACF）图
    .lags = "5 years",.interactive = FALSE) # 显示前5年的滞后

# CCF交叉相关验证
# 下载并加载包
install.packages("tidyquant")
library(tidyquant)
install.packages("gt")
library(gt) 

# 数据转换和处理
data_ts_tk4 <- data_ts_tk3 %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  # 将长表转换为宽表，使name列的值变为列名，value列的值作为相应列的值
  summarise_by_time(.date_var = date,.by = "year", 
    across(BIOMASS:DENSITY, .fns = mean))  
  # 按年汇总数据，并计算BIOMASS和DENSITY的均值

# 计算并显示CCF
data_ts_tk4 %>% 
  tk_acf_diagnostics(date,BIOMASS,.ccf_vars = DENSITY) %>%
  # 计算BIOMASS和DENSITY的交叉相关函数（CCF）
  select(lag, CCF_DENSITY) %>% # 选择滞后和CCF_DENSITY列
  slice(1:10) %>% # 选择前10行数据
  gt() %>% # 将数据转换为gt表格格式
  data_color(columns = vars(CCF_DENSITY),colors = scales::col_numeric(
      palette = c("#ffffff", "#f2fbd2", "#FA6047", "#F25555", "#FA1B1B"), 
      domain = NULL))

# 可视化交叉相关函数
data_ts_tk4 %>% 
  plot_acf_diagnostics(
    date,BIOMASS,
    .ccf_vars = DENSITY, #指定交叉相关的变量为DENSITY
    .show_ccf_vars_only = TRUE,
    .interactive=FALSE, 
    .line_color = "black",
    .point_color =palette_light()[[2]],
    .line_size = 1.5,
    .title = "Cross Correlation of BIOMASS and DENSITY") 

###############################################################################
# 特征生成和线性回归模型的拟合
# 下载并加载所需包
library(tidyverse)
install.packages("tidymodels")
library(tidymodels)
install.packages("modeltime")
library(modeltime)
library(timetk)
library(lubridate)
# 数据准备
biomass_ts_tk <- sdata %>%
  tk_tbl() %>% select(DATE, BIOMASS)

# 对时间序列进行检查
biomass_ts_tk %>%
  tk_summary_diagnostics(.date_var = DATE)

# 生成基于日历的特征:Calendar-based features

biomass_ts_tk_features_C <- biomass_ts_tk %>%
  mutate(BIOMASS =  log1p(x = BIOMASS)) %>% # 增加列
  mutate(BIOMASS =  standardize_vec(BIOMASS)) %>% # 标准化模型
  tk_augment_timeseries_signature(.date_var = DATE) %>%
  glimpse()

biomass_ts_tk_features_C

# 线性回归模型拟合 
timetk::plot_time_series_regression(.date_var = DATE,
                                    .data = biomass_ts_tk_features_C,
                                    .formula = BIOMASS ~ as.numeric(DATE) + index.num
                                    + year + half + quarter + month + month.lbl,
                                    .show_summary = TRUE)
# 使用plot_time_series_regression函数拟合线性回归模型，并显示回归摘要

# 生成傅立叶特征: Fourier terms features

biomass_ts_tk_features_F <- biomass_ts_tk |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 
# 使用tk_augment_fourier函数生成傅立叶特征，周期为5，阶数为1

biomass_ts_tk_features_F

# 线性回归模型拟合
plot_time_series_regression(.date_var = DATE, 
                            .data = biomass_ts_tk_features_F,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)
                           # 使用傅立叶特征拟合线性回归模型，并显示回归摘要

# 生成滞后特征：Lag features
biomass_ts_tk_features_L <- biomass_ts_tk |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7))  
# 使用tk_augment_lags函数生成滞后特征，滞后步数为4和7

biomass_ts_tk_features_L

# 线性回归模型拟合
plot_time_series_regression(.date_var = DATE, 
                            .data = biomass_ts_tk_features_L,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_lag4 + BIOMASS_lag7,
                            .show_summary = TRUE)
# 使用滞后特征拟合线性回归模型，并显示回归摘要

# 生成移动窗口特征：Moving window statistics

biomass_ts_tk_features_M <- biomass_ts_tk |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7)) |>
  # 生成滞后特征，滞后步数为4和7
  tk_augment_slidify(.value   = contains("BIOMASS"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center")
# 使用tk_augment_slidify函数生成移动窗口特征，窗口期为3和6，计算窗口期内的均值
biomass_ts_tk_features_M

# 线性回归模型拟合
plot_time_series_regression(.date_var = DATE, 
                            .data = biomass_ts_tk_features_M,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            .show_summary = TRUE)
# 回归公式包含日期、3期和6期的移动均值特征

# 将所有特征进行比较
# 生成所有特征
biomass_ts_tk_features_all <- biomass_ts_tk |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_timeseries_signature(.date_var = DATE) |> 
  # 使用tk_augment_timeseries_signature函数生成基于日历的特征
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |> 
  # 使用tk_augment_fourier函数生成傅立叶特征（周期为5，阶数为1）
  tk_augment_lags(.value = BIOMASS, .lags = c(4,7)) |> 
  # 使用tk_augment_lags函数生成滞后特征（滞后步数为4和7）
  tk_augment_slidify(.value   = contains("BIOMASS"),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6),
                     .partial = TRUE,
                     .align   = "center") 
  # 使用tk_augment_slidify函数生成移动窗口特征（3期和6期的移动均值）

# 查看生成的所有特征
biomass_ts_tk_features_all |> glimpse()
# 使用glimpse函数快速查看生成的所有特征

# 线性回归模型拟合
plot_time_series_regression(.date_var = DATE, 
                            .data = biomass_ts_tk_features_all,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              index.num + year + half + quarter + month + 
                              DATE_sin5_K1 + DATE_cos5_K1 + 
                              # 傅立叶特征
                              BIOMASS_lag4 + BIOMASS_lag7 + 
                              # 滞后特征
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            # 移动窗口特征（3期和6期的移动均值）
                             #BIOMASS_lag4_roll_3 + BIOMASS_lag7_roll_3 + 
                             #BIOMASS_lag4_roll_6 + BIOMASS_lag7_roll_6,
                            .show_summary = TRUE)

################################################################################
# 时间序列的机器学习 
# 下载并加载所需包
library(tidyverse)  
library(timetk) 
library(tidymodels)
library(modeltime)
library(timetk)
library(tidyquant)

# 所需数据
sdata
biomass_ts_tk1 <- sdata |> tk_tbl() |> 
  select(index, DATE, BIOMASS) # 从sdata数据中选择index、DATE和BIOMASS列

# 可视化DATE和BIOMASS的变化
ggplot(biomass_ts_tk1,aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("Biomass of VAI Fishes in Doubs")

# 数据集划分
n_rows <- nrow(biomass_ts_tk1)
train_rows <- round(0.8 * n_rows)
# 将数据集划分为训练集（80%）和测试集（20%）
train_data <- biomass_ts_tk1 |>
  slice(1:train_rows)  
test_data <- biomass_ts_tk1 |>
  slice((train_rows):n_rows)

# 可视化训练集和测试集
ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal() #使用ggplot2将训练集和测试集的数据可视化

# 特征工程：使用recipes包创建features
# 加载所需包
library(recipes)
library(tidymodels)

recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)  
#生成时间序列特征，移除DATE列，处理零方差特征，并对分类变量进行独热编码

summary(prep(recipe_spec_final))

# 训练Boosted Tree模型
boosted_tree <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")) |>
  add_recipe(recipe_spec_final) |>fit(train_data)  
# 使用workflow函数创建一个工作流，添加BoostedTree模型，并使用训练集数据进行拟合
boosted_tree

# 评估Boosted Tree模型
bt_test <- boosted_tree |> 
  predict(test_data) |>
  bind_cols(test_data) 

bt_test

pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# 计算Boosted Tree模型的预测误差
bt_test |> metrics(BIOMASS, .pred)

# 训练随机森林模型
# 安装并加载ranger包
install.packages("ranger")
library(ranger)
 
# 使用workflow函数创建一个工作流，添加随机森林模型，并使用训练集数据进行拟合
rf <- workflow() |>
  add_model(spec = rand_forest("regression") |> set_engine("ranger")
  ) |> add_recipe(recipe_spec_final) |> fit(train_data)

# 评估随机森林模型

rf_test <- rf |> predict(test_data) |> bind_cols(test_data) 

prf <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") + theme_minimal()

# 计算随机森林模型的预测误差
rf_test |> metrics(BIOMASS, .pred)

library(patchwork)
pbt + prf

# 比较不同模型的表现
# 创建模型表，包含Boosted Tree和随机森林模型
model_tbl <- modeltime_table( boosted_tree,rf )

# 校准模型
calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)
calibrated_tbl
# 计算预测误差，比较不同模型的表现
calibrated_tbl |> modeltime_accuracy(test_data) |>
  arrange(rmse)

# 可视化预测结果
calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = biomass_ts_tk1,
    keep_data   = TRUE  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE)
calibrated_tbl
# 保存工作流
workflow_Doubs <- list( workflows = list(
    wflw_random_forest = rf,
    wflw_xgboost = boosted_tree),
  calibration = list(calibration_tbl = calibrated_tbl))

workflow_Doubs |>
  write_rds("Desktop/研一下学期课程/数据驱动的生态学研究方法/workflows1.rds")
