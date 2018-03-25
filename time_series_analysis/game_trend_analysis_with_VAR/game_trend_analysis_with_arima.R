#Googleトレンドデータでゲームの時系列解析

#ARIMAで予測
#状態空間モデル、prophetでそれぞれ予測してみて結果を比較

setwd("/Users/ikumi/github/data_analysis/game_trend_analysis")

library(tidyverse)
library(zoo)
library(tseries)
library(forecast)

# 某有名海賊アニメのゲームについARIMAで予測してみる -----------------------------------------------------
# データ読み込み -----------------------------------------------------------------
#Googleトレンドデータ
data <- read_csv("google_trend_optc.csv")

#グラフ作成
ts <- zoo(data[,-1], data$week) #zoo class
main <- 'optc google trend'
#全てのデータを1つのグラフにプロット
plot(ts, screens=1, main=main, xlab='month (2015/2/22-2018/3/11)', ylab='trend data')

# 標本自己相関のチェック ------------------------------------------------------------------
#acf 自己相関関数のプロット
test1 <- acf(ts, type = "correlation", na.action = na.pass, lag.max = 20)

#pacf 偏自己相関関数のプロット
test2 <- acf(ts, type = "partial", na.action = na.pass, lag.max = 20) #type = "partial"だと偏自己相関でpacf関数と同じ結果になる

#かばん検定(自己相関の有無を検定する)
#デフォルトはBox-Pierceで、たいていこちらで大丈夫。データが少ない時にはLjung-Boxを使う
test3 <- Box.test(ts)
# Box-Pierce test
# 
# data:  ts
# X-squared = 115.77, df = 1, p-value < 2.2e-16
#自己相関あり

# 単位根検定 -------------------------------------------------------------------
#期間を2017年~2018年に絞る
ts.win <- window(ts, start="2017-01-01", end="2017-12-31")
plot(ts.win)

#ADF検定
adf.test(ts.win)
#定常過程

# ARIMA予測 ------------------------------------------------------------
#次数の探索
auto.arima(ts.win)
#モデル構築
mod <- arima(ts.win, order = c(0,0,1))
#診断
tsdiag(mod)
#→標準化残差に周期性のようなものが見られるので良くない
#→季節成分がある可能性

#予測
predict(mod, n.ahead = 10)

# ARIMA予測(移動平均から) -----------------------------------------------------------------
ts.avg <- rollmean(ts, 7, align = "right")
plot(ts.avg)
plot(ts)

# ARIMA予測(トレンド除去して実行してみる) ------------------------------------------------------------
#トレンドの除去
ts.del <- lm(coredata(ts.win)~index(ts.win))
detr <- zoo(resid(ts.del), index(ts.win))
plot(detr)

acf(detr, type = "correlation", na.action = na.pass, lag.max = 20)
Box.test(detr)
#自己相関がなくなるので予測不可

# 【参考】Rで季節変動のある時系列データを扱ってみる -----------------------------------------------
#https://tjo.hatenablog.com/entry/2013/10/30/190552


# 【作業】 --------------------------------------------------------------------
x1<-rnorm(300) # ホワイトノイズ
x2<-rep(c(15,rep(0,49)),6) # スパイク異常値が6回生じる時系列で周期は50期
x3<-c(rep(0,150),rep(3,150)) # ステップ状の不連続な変化
x<-x1+x2+x3 # 全部合わせる
xt<-ts(as.numeric(x),frequency=50) # 必ずnumeric型で与えること！！！
# なお、frequency=50で50期ごとという周期の情報がインプットされる
plot(xt)


