#Googleトレンドから取得した検索トレンドの推移をもとに向こう1ヶ月の変化を予測する


setwd("/Users/ikumi/github/data_analysis/time_series_analysis/game_trend_analysis") #mac
library(dlm)
library(tidyverse)
library(ggplot2)
library(zoo)

# read data ---------------------------------------------------------------
#期間：2016-03-06 2018-03-18
data <- readr::read_csv(file = "google_trend.csv")
data_ts <- zoo(data[,-1], data$week) #zoo class

plot(data_ts)

# データ分割 -------------------------------------------------------------------
#2016-03-06〜2018-01-21までを学習データとし、残りをテストデータとして残す
data_train <- stats::window(data_ts, end="2018-01-21") #引数がzooなどの時系列クラスでないとエラー

data_train <- data.frame(data_train)
data_train$week <- rownames(data_train)
rownames(data_train) <- 1:99

# モデルへの当てはめ ---------------------------------------------------------------
#水準のみがランダムウォークするローカルレベルモデル(1階差分のモデル)に当てはめる

#まず、モデルを生成する関数を定義
dlm.build3 <- function(par) {
  dlmModPoly(order = 1, 
             dV=exp(par[1]), dW=exp(par[2]))
  #観測ノイズdV、システムノイズdWは非負の値しかとらないため、指数関数を適用してから渡すことでエラーを防ぐ
}

#dlmMLE関数でパラメータの最尤推定を行う
#内部でoptim関数を用いており、最適化の初期値(parm引数)を渡す必要がある
dlm.mle3 <- dlmMLE(data_train$google_trend, parm = c(0, 0), #parm引数に渡すベクトルの要素数は推定するパラメータの個数と同じ
                   build = dlm.build3) #build引数に渡す値はモデル式を記述(構築)する関数だと思われる

#dlm.mle3$parで推定されたパラメータの値が取り出せる
#推定したパラメータでモデル構築
dlm.mod3 <- dlm.build3(dlm.mle3$par)


# 推定されたノイズ分散の確認 -----------------------------------------------------------
#観測ノイズ
dlm.mod3$V

#システムノイズ
dlm.mod3$W


# フィルタリング -----------------------------------------------------------------
dlm.fit3 <- dlmFilter(data_train$google_trend, dlm.mod3)
head(dropFirst(dlm.fit3$m))
#80.99952 78.88559 79.71826 77.77467 74.69045 71.74528


# 平滑化 ---------------------------------------------------------------------
dlm.smt3 <- dlmSmooth(data_train$google_trend, dlm.mod3)
head(dropFirst(dlm.smt3$s))
#76.11500 75.52350 74.75321 73.22644 71.60601 70.42225


# モデルによる予測 -------------------------------------------------------
#dlmForecast関数で予測ができる
dlm.for3 <- dlmForecast(dlm.fit3, nAhead = 8) #8期先までを予測

#観測値の予測値を取り出す
head(dlm.for3$f, 3) #観測値の予測値
# [,1]
# [1,] 55.77745
# [2,] 55.77745
# [3,] 55.77745

#状態の予測値を取り出す
head(dlm.for3$a, 3)
# [,1]
# [1,] 55.77745
# [2,] 55.77745
# [3,] 55.77745

#観測値の予測値の分散
#リスト形式であり、各期ごとの行列の形になっているのでunlistする
head(unlist(dlm.for3$Q))


# visualization -----------------------------------------------------------
#元データ、フィルタリングした水準の値、予測値
plot_data1 <- data.frame(cbind(seq(1:107), data$google_trend))
colnames(plot_data1) <- c("time", "google_trend")

plot_data1 <- plot_data1 %>% 
  tidyr::gather(key=variable, value = value, -time)

plot_data2 <- data.frame(cbind(seq(1:99), dropFirst(dlm.fit3$m), dropFirst(dlm.smt3$s)))
colnames(plot_data2) <- c("time", "prediction", "prediction_smoothed")

plot_data2 <- plot_data2 %>% 
  tidyr::gather(key=variable, value = value, -time)

plot_data3 <- data.frame(cbind(100:107, dlm.for3$a))
colnames(plot_data3) <- c("time", "prediction_8weeks")

plot_data3 <- plot_data3 %>% 
  tidyr::gather(key=variable, value = value, -time)

g1 <- ggplot(data = plot_data1, 
             aes(
               x=time,
               y=value,
               group=variable,
               colour=variable
             )) + 
  geom_line() + 
  geom_point() + 
  labs(x="time", y="the number of items sold") + 
  ggtitle("ts data")

g2 <- g1 + geom_line(data = plot_data2) + geom_line(data = plot_data3)

plot(g2)

#元データ + 予測データのプロット
#予測値のプロット
plot_data2 <- data.frame(cbind(45:52, "observation_pred", dlm.for4$f), stringsAsFactors = FALSE)
colnames(plot_data2) <- c("time", "variable", "value") #plot_datと変数名、データ型を揃える必要がある
plot_data2$time <- as.numeric(plot_data2$time)
plot_data2$value <- as.numeric(plot_data2$value)

#前回のプロットに予測データを追加
g2 <- g1 + 
  geom_line(data = plot_data2) + 
  geom_point(data = plot_data2)

plot(g2)



