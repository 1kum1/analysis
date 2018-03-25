#岩波DSvol.6 「Rによる状態空間モデリング dlmとKFASを用いて」に掲載しているコードの実践


setwd("/Users/ikumi/github/data_analysis/ts_analysis_with_iwanamiDS") #mac
library(dlm)
library(tidyverse)
library(ggplot2)
#library(reshape2)
library(KFAS)


# remarks -----------------------------------------------------------------
# dlmは状態空間モデルのうち、線形・正規分布の動的線形モデル(dynamic liner model)を扱う


# 【1】dlmによるローカルレベルモデル --------------------------------------------------------
# read data ------------------------------------------
#(季節もの商品の四半期ごと販売数データ)
data.url <- "https://raw.githubusercontent.com/iwanami-datascience/vol1/master/matsuura/example2/input/data-season.txt"
data <- read.csv(url(data.url))
Y <- data$Y


# モデルへの当てはめ ---------------------------------------------------------------
#水準のみがランダムウォークするローカルレベルモデル(1階差分のモデル)に当てはめる
#dlmModPoly関数はdlmでローカルレベルモデルを使う際に使用する関数
#dlm, KFASではモデルをオブジェクトとして構築し、それに対して操作を行うようにしている

dlm.mod1 <- dlmModPoly(order = 1, dV=1, dW=1, m0=0, C0=1e+07) #m0→初期状態の期待値、C0→初期状態のノイズ分散

#カルマンフィルタに適用
dlm.fit1 <- dlmFilter(Y, dlm.mod1)

#フィルタリングにより推定された状態の値はdlm.fit1$mで取り出すことが可能
#options(digits = 3) #表示桁数を3桁にする
head(dlm.fit1$m)
#[1]  0.0 18.1 21.8 18.4 16.3 16.8

#最初の0.0は先に与えた初期値であることに注意。取り除く必要がある
head(dropFirst(dlm.fit1$m))


# visualization ------------------------------------------------------
#観測値とフィルタリング値を重ねて図示
plot_data <- data.frame(cbind(seq(1:44), data$Y, dropFirst(dlm.fit1$m))) #seq(1:44)はX軸の値
colnames(plot_data) <- c("time", "sales_count", "prediction")

plot_data <- plot_data %>% 
  tidyr::gather(key = variable, value = value, -time)

# plot_data2 <- melt(plot_data, id.vars = "time") #X列はmelt対象外
ggplot(data = plot_data, 
       aes(
         x=time,
         y=value,
         group=variable,
         colour=variable
         )) + 
  geom_line() + 
  labs(x="time", y="the number of items sold") + 
  ggtitle("ts data")


# ノイズ分散の変更 ----------------------------------------------
#観測ノイズ分散=1、システムノイズ分散=5としてみる
dlm.mod2 <- dlmModPoly(order = 1, dV=1, dW=5, m0=0, C0=1e+07)

#カルマンフィルタに適用
dlm.fit2 <- dlmFilter(Y, dlm.mod2)
#最初の0.0は先に与えた初期値であることに注意。取り除く必要がある
head(dropFirst(dlm.fit2$m))
#[1] 18.07300 22.86614 17.35152 15.28415 16.84020 21.43339


# visualization ------------------------------------------------------
#前回の結果に付け足す
plot_data <- data.frame(cbind(seq(1:44), data$Y, dropFirst(dlm.fit1$m), dropFirst(dlm.fit2$m))) #seq(1:44)はX軸の値
colnames(plot_data) <- c("time", "sales_count", "prediction1", "prediction2")

plot_data <- plot_data %>% 
  tidyr::gather(key = variable, value = value, -time)

ggplot(data = plot_data, 
       aes(
         x=time,
         y=value,
         group=variable,
         colour=variable
       )) + 
  geom_line() + 
  labs(x="time", y="the number of items sold") + 
  ggtitle("ts data")
#さっきより観測値とフィルタリング値の差が小さくなった


# ノイズ分散の最尤推定 -----------------------------------------------------
#データからノイズ分散を最尤推定する方法がある
#まず、モデルを生成する関数を定義
dlm.build3 <- function(par) {
  dlmModPoly(order = 1, 
             dV=exp(par[1]), dW=exp(par[2]))
  #観測ノイズdV、システムノイズdWは非負の値しかとらないため、指数関数を適用してから渡すことでエラーを防ぐ
}

#dlmMLE関数でパラメータの最尤推定を行う
#内部でoptim関数を用いており、最適化の初期値(parm引数)を渡す必要がある
dlm.mle3 <- dlmMLE(Y, parm = c(0, 0), #parm引数に渡すベクトルの要素数は推定するパラメータの個数と同じ
                   build = dlm.build3) #build引数に渡す値はモデル式を記述(構築)する関数だと思われる

#dlm.mle3$parで推定されたパラメータの値が取り出せる
#推定したパラメータでモデル構築
dlm.mod3 <- dlm.build3(dlm.mle3$par)

#ノイズ分散の値
dlm.mod3$V
#V(dlm.mod3) #こちらの式でも同じ
# [,1]
# [1,] 17.23128

dlm.mod3$W
#W(dlm.mod3) #こちらの式でも同じ
# [,1]
# [1,] 0.7410732

#フィルタリング
dlm.fit3 <- dlmFilter(Y, dlm.mod3)
head(dropFirst(dlm.fit3$m))
#[1] 18.07297 20.92784 19.31811 18.06619 17.82883 18.81609

#平滑化
dlm.smt3 <- dlmSmooth(Y, dlm.mod3)
head(dropFirst(dlm.smt3$s))
#[1] 17.69674 17.68056 17.40701 17.17633 17.04222 16.90536


# visualization ------------------------------------------------------
#前回の結果に付け足す
plot_data <- data.frame(cbind(seq(1:44), data$Y, dropFirst(dlm.fit1$m), dropFirst(dlm.fit2$m), dropFirst(dlm.fit3$m), dropFirst(dlm.smt3$s)))
colnames(plot_data) <- c("time", "sales_count", "prediction1", "prediction2", "prediction3", "prediction3_smoothed")

plot_data <- plot_data %>% 
  tidyr::gather(key = variable, value = value, -time)

ggplot(data = plot_data, 
       aes(
         x=time,
         y=value,
         group=variable,
         colour=variable
       )) + 
  geom_line() + 
  labs(x="time", y="the number of items sold") + 
  ggtitle("ts data")


# 対数尤度の確認 -----------------------------------------------------------------
dlmLL(Y, dlm.mod3)
#[1] 95.75525

dlmLL(Y, dlm.mod2)
#[1] 161.9497

dlmLL(Y, dlm.mod1)
#[1] 268.4364


# モデルへの当てはめver2 -----------------------------------------------------------
#2階差分のモデル(2次のトレンドモデル)でパラメータを最尤推定する場合
#dlmModPoly関数のorder引数を2に設定する
#まず、モデルを生成する関数を定義
dlm.build3.2 <- function(par) {
  mod <- dlmModPoly(order = 2, 
                    dV=exp(par[1]), dW=c(0, exp(par[2])))
}

#dlmMLE関数でパラメータの最尤推定を行う
#内部でoptim関数を用いており、最適化の初期値(parm引数)を渡す必要がある
dlm.mle3.2 <- dlmMLE(Y, parm = c(0, 0), 
                     build = dlm.build3.2)

#推定したパラメータでモデル構築
dlm.mod3.2 <- dlm.build3.2(dlm.mle3.2$par)

#ノイズ分散の値
dlm.mod3.2$V
# [,1]
# [1,] 16.98613

dlm.mod3.2$W
# [,1]       [,2]
# [1,]    0 0.00000000
# [2,]    0 0.01146633

#フィルタリング
dlm.fit3.2 <- dlmFilter(Y, dlm.mod3.2)
head(dropFirst(dlm.fit3.2$m))
#2列の結果が返るが、2列目は何なのか。2次のトレンドモデルなので、2個目の変数の推定結果と思われる。

#平滑化
dlm.smt3.2 <- dlmSmooth(Y, dlm.mod3.2)
head(dropFirst(dlm.smt3.2$s))
#2列の結果が返るが、2列目は何なのか


# visuarilation -----------------------------------------------------------
#1次のトレンドモデルとの比較
plot_data <- data.frame(cbind(seq(1:44), data$Y, dropFirst(dlm.fit3$m), dropFirst(dlm.smt3$s), dropFirst(dlm.fit3.2$m[ , 1]), dropFirst(dlm.smt3.2$s[ , 1])))
colnames(plot_data) <- c("time", "sales_count", "prediction3", "prediction3_smoothed", "prediction3.2", "prediction3.2_smoothed")

plot_data <- plot_data %>% 
  tidyr::gather(key = variable, value = value, -time)

ggplot(data = plot_data, 
       aes(
         x=time,
         y=value,
         group=variable,
         colour=variable
       )) + 
  geom_line() + 
  labs(x="time", y="the number of items sold") + 
  ggtitle("ts data")
#平滑化した値はローカルレベルモデルより2次のトレンドモデルのほうが滑らか


# 【2】dlmによる季節調整モデル -----------------------------------------------------------------
#dlmMoDSeas関数で季節調整成分の組み込みができる
#ローカルレベルモデルに季節調整成分を加えたモデルを生成する関数の定義
dlm.build4 <- function(par) {
  mod <- dlmModPoly(order = 1) + #モデルの基本部分の定義。水準成分の組み込み
    dlmModSeas(frequency = 4) #モデルの基本部分の定義。周期4の季節調整成分の組み込み
  V(mod) <- exp(par[1]) #モデルの観測ノイズの分散を指定
  diag(W(mod))[1:2] <- exp(par[2:3]) #モデルのシステムノイズの分散を指定。Wの対角成分1つめ→水準成分、2つめ→季節調整成分
  m0(mod) <- c(20, 0, 0, 0) #初期状態の期待値。C0は省略しているのでデフォルト値(diag(1e+7, 4))が使われる
  return(mod)
}

#パラメータの最尤推定
dlm.mle4 <- dlmMLE(Y, parm = rep(0, 3),
                   build = dlm.build4)

#最尤推定値を当てはめたモデルの生成
dlm.mod4 <- dlm.build4(dlm.mle4$par)


# 係数行列(F及びG)の確認 ---------------------------------------------
#観測モデル
dlm.mod4$FF
#FF(dlm.mod4) #同じ
# [,1] [,2] [,3] [,4]
# [1,]    1    1    0    0

#システムモデル
dlm.mod4$GG
#GG(dlm.mod4) #同じ
# [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0   -1   -1   -1
# [3,]    0    1    0    0
# [4,]    0    0    1    0


# ノイズ分散の確認 ----------------------------------------------------------------
#観測ノイズ
dlm.mod4$V
# [,1]
# [1,] 4.402579e-07

#システムノイズ
dlm.mod4$W
# [,1]      [,2] [,3] [,4]
# [1,] 0.4470163 0.0000000    0    0
# [2,] 0.0000000 0.4115065    0    0
# [3,] 0.0000000 0.0000000    0    0
# [4,] 0.0000000 0.0000000    0    0
 
#対数尤度の確認
dlmLL(Y, dlm.mod4)
# [1] 68.4441


# カルマンフィルタによる予測 -------------------------------------------------------
#dlmForecast関数で予測ができる

#フィルタリング
dlm.fil4 <- dlmFilter(Y, dlm.mod4)
head(dropFirst(dlm.fil4$m)[,1]) #水準のフィルタリング結果

#平滑化
dlm.smt4 <- dlmSmooth(Y, dlm.mod4)
head(dropFirst(dlm.smt4$s)[,1]) #水準の平滑化結果
head(dropFirst(dlm.smt4$s)[,2]) #状態の季節調整成分θ2,tの平滑化した値


# モデルによる予測と各種推定結果の取得 -------------------------------------------------------
#予測
dlm.for4 <- dlmForecast(dlm.fil4, nAhead = 8) #8期先までを予測

#観測値の予測値を取り出す
head(dlm.for4$f, 3) #観測値の予測値
# [,1]
# [1,] 24.01961
# [2,] 32.72886
# [3,] 20.71521

#状態の予測値を取り出す
head(dlm.for4$a, 3)
# [,1]      [,2]      [,3]      [,4]
# [1,] 25.08242 -1.062813 -2.216420 -4.367209
# [2,] 25.08242  7.646442 -1.062813 -2.216420
# [3,] 25.08242 -4.367209  7.646442 -1.062813

#観測値の予測値の分散
#リスト形式であり、各期ごとの行列の形になっているのでunlistする
head(unlist(dlm.for4$Q))


# visualization -----------------------------------------------------------
#フィルタリングした水準の値、(予測値)、水準の平滑化値、季節調整成分の平滑化値
plot_data <- data.frame(cbind(seq(1:44), data$Y, dropFirst(dlm.fil4$m)[,1], dropFirst(dlm.smt4$s)[,1], dropFirst(dlm.smt4$s)[,2]))
colnames(plot_data) <- c("time", "sales_count", "prediction4", "prediction4_smoothed", "seasonality_smoothed")

plot_data <- plot_data %>% 
  tidyr::gather(key = variable, value = value, -time)

g1 <- ggplot(data = plot_data, 
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

plot(g1)

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

# KFASパッケージ ---------------------------------------------------------------


# KFASによる季節調整モデル --------------------------------------------------------------
kfas.mod1 <- SSModel(Y ~ SSMtrend(degree = 1, 
                                  Q=NA, 
                                  a1=20) + 
                       SSMseasonal(period = 4, 
                                   sea.type = "dummy",
                                   Q=NA), H=NA)

kfas.mod1$Z

kfas.mod1$T

kfas.fit1 <- fitSSM(kfas.mod1, inits = rep(0, 3))

kfas.fit1$model$H































