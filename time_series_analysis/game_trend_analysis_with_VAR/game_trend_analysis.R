#Googleトレンドデータでゲームの時系列解析

#上位ゲームについてVARで予測してみる
#うまく予測できていたら、グレンジャー因果性検定してみる

# メモ ----------------------------------------------------------------------
#原系列で自己相関、定常性ともに満たすのはlineのみ
#1階差分系列で自己相関、定常性ともに満たすのはdragon_ballとidol_masterのみ

setwd("/Users/ikumi/github/data_analysis/game_trend_analysis")
#setwd("/Users/ikumi/Documents/data_science/time_series_analysis/game_trend_analysis")

library(tidyverse)
library(zoo)
library(tseries)
library(vars)


# 上位ゲームについてVARで予測してみる -----------------------------------------------------
# データ読み込み -----------------------------------------------------------------
#Google playでの売上上位10位ゲームのGoogleトレンドデータ
data <- read_csv("google_trend.csv")
colnames(data) <- c('week', 'fgo', 'line', 'dragon_ball', 'puzzle_dragon', 'monster_strike', 'pokemon_go', 'princess_connect', 'black_cat', 'fire_emblem', 'idol_master')

#グラフ作成
ts <- zoo(data[,-1], data$week) #zoo class
main <- 'game trend'
#全てのデータを1つのグラフにプロット
plot(ts, screens=1, main=main, xlab='month (2017-2018)', ylab='google trend data')
#ゲームごとにプロット
plot(ts, screens=c(1:11), main=main, xlab='month (2017-2018)')


# 標本自己相関のチェック ------------------------------------------------------------------
#1つのゲームをピックアップして関数の挙動を試してみる
#時系列データ用意
ts_fgo <- zoo(data[ ,"fgo"], order.by=data$week) #zoo class

#acf 自己相関関数のプロット
test1 <- acf(ts_fgo, type = "correlation", na.action = na.pass, lag.max = 20)
test2 <- acf(ts_fgo, type = "partial", na.action = na.pass, lag.max = 20) #type = "partial"だと偏自己相関でpacf関数と同じ結果になる

#pacf 偏自己相関関数のプロット
test3 <- pacf(ts_fgo, na.action = na.pass, lag.max = 20) #type = "correlation"

#かばん検定(自己相関の有無を検定する)
#デフォルトはBox-Pierceで、たいていこちらで大丈夫。データが少ない時にはLjung-Boxを使う
test4 <- Box.test(ts_fgo, type = "Ljung-Box")
#自己相関あり

#11個のゲーム全てに対してかばん検定を実施、自己相関係数を算出
box_test_res <- list()
acf_res <- list()
for (i in 2:ncol(data)) {
  ts_tmp <- zoo(data[ , i], order.by=data$week) #zoo class
  
  #acf
  acf_res[[i]] <- acf(ts_tmp, na.action = na.pass, lag.max = 20) #type = "correlation"
  
  #pacf
  #pacf(ts_tmp, na.action = na.pass, lag.max = 20) #type = "correlation"
  
  #かばん検定
  box_test_res[[i]] <- Box.test(ts_tmp)
}

#p.valueでp値を抽出できる
box_test_res[[2]]$p.value #fgo

#p値が一定以下のゲームを洗い出す
for (i in 2:ncol(data)) {
  if(box_test_res[[i]]$p.value <= 0.01) {
    print(colnames(data)[i])
  }
}

# [1] "fgo"
# [1] "line"
# [1] "dragon_ball"
# [1] "puzzle_dragon"
# [1] "pokemon_go"
# [1] "princess_connect"
# [1] "black_cat"
# [1] "fire_emblem"

#モンスト、アイドルマスター以外は自己相関ありそう


# 単位根検定 -------------------------------------------------------------------
# 1つのゲームをピックアップして関数の挙動を試してみる ----------------------------------------------
#時系列データ用意
ts_fgo <- zoo(data[ ,"fgo"], order.by=data$week) #zoo class
plot(ts_fgo)

#ADF検定
adf.test(ts_fgo)
#有意差なし→非定常過程

#差分系列を求める
ts_fgo.d <- diff(ts_fgo) #k=1
head(ts_fgo.d)
plot(ts_fgo.d)

#ADF検定
adf.test(ts_fgo.d)
#定常過程になった


# 全ゲームについて -----------------------------------------------------------
# 単位根検定 -------------------------------------------------------------------
adf_res <- list()
for (i in 2:ncol(data)) {
  ts_tmp <- zoo(data[ , i], order.by=data$week) #zoo class
  adf_res[[i]] <- adf.test(ts_tmp)
  print(paste(colnames(data)[i], round(adf_res[[i]]$p.value, 4), sep=": ")) #ゲーム名とp値を表示
}
# [1] "fgo: 0.0889"
# [1] "line: 0.01"
# [1] "dragon_ball: 0.1748"
# [1] "puzzle_dragon: 0.4065"
# [1] "monster_strike: 0.2473"
# [1] "pokemon_go: 0.4301"
# [1] "princess_connect: 0.99"
# [1] "black_cat: 0.3513"
# [1] "fire_emblem: 0.6439"
# [1] "idol_master: 0.0167"

#0.01以下のゲームを洗い出し
for (i in 2:ncol(data)) {
  if (adf_res[[i]]$p.value <= 0.01) {
    print(colnames(data)[i])
  }
}
#line以外は非定常過程


# 差分系列の単位根検定 --------------------------------------------------------------
adf_res <- list()
for (i in 2:ncol(data)) {
  ts_tmp <- zoo(data[ , i], order.by=data$week) #zoo class
  ts_tmp.d <- diff(ts_tmp) #k=1
  adf_res[[i]] <- adf.test(ts_tmp.d)
  print(paste(colnames(data)[i], round(adf_res[[i]]$p.value, 4), sep=": ")) #ゲーム名とp値を表示
}

# [1] "fgo: 0.01"
# [1] "line: 0.01"
# [1] "dragon_ball: 0.01"
# [1] "puzzle_dragon: 0.01"
# [1] "monster_strike: 0.01"
# [1] "pokemon_go: 0.01"
# [1] "princess_connect: 0.99"
# [1] "black_cat: 0.0209"
# [1] "fire_emblem: 0.1031"
# [1] "idol_master: 0.01"

#0.01以下のゲームを洗い出し
for (i in 2:ncol(data)) {
  if (adf_res[[i]]$p.value <= 0.01) {
    print(colnames(data)[i])
  }
}

#1階差分系列であれば定常過程のゲーム
# [1] "fgo"
# [1] "line"
# [1] "dragon_ball"
# [1] "puzzle_dragon"
# [1] "monster_strike"
# [1] "pokemon_go"
# [1] "idol_master"


# 差文系列の自己相関検定 -----------------------------------------------------------
box_test_res <- list()
acf_res <- list()
for (i in 2:ncol(data)) {
  ts_tmp <- zoo(data[ , i], order.by=data$week) #zoo class
  ts_tmp.d <- diff(ts_tmp) #k=1
  
  #acf
  acf_res[[i]] <- acf(ts_tmp.d, na.action = na.pass, lag.max = 20) #type = "correlation"
  
  #pacf
  #pacf(ts_tmp, na.action = na.pass, lag.max = 20) #type = "correlation"
  
  #かばん検定
  box_test_res[[i]] <- Box.test(ts_tmp.d)
}

#p.valueでp値を抽出できる
box_test_res[[2]]$p.value #fgo

#p値が一定以下のゲームを洗い出す
for (i in 2:ncol(data)) {
  if(box_test_res[[i]]$p.value <= 0.01) {
    print(colnames(data)[i])
  }
}

#差分系列が自己相関を持つもの
# [1] "dragon_ball"
# [1] "idol_master"


plot(ts_tmp.d)


# VARで予測できないか実験 ---------------------------------------------------------------------

# モデルの作成 ------------------------------------------------------------------
#1階差分系列を対象とする
#1階差分系列において自己相関を持ち、定常過程である時系列データはdragon_ballとidol_masterのみ
#時系列データの作成
ts_data <- data %>% dplyr::select(week, dragon_ball,  idol_master)
ts_data <- zoo(ts_data[,-1], ts_data$week) #zoo class

ts_data.d <- diff(ts_data) #差分をとる #k=1
#データを1つのグラフにプロット
plot(ts_data.d, screens=1, main="google trend", xlab='month (2017-2018)', ylab='google trend data')
#ゲームごとにプロット
plot(ts_data.d, screens=c(1, 2), main="google trend", xlab='month (2017-2018)')

#学習用データの作成
ts_data.train <- window(ts_data.d, end="2017-12-31")
length(ts_data.train) #84
length(ts_data.d) #102

#AIC最小モデルの次数を調べる
p_est <- VARselect(ts_data.train)

# $selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
# 2      2      2      2 
# 
# $criteria
# 1           2            3            4           5            6           7           8           9          10
# AIC(n)     9.453468    9.192572     9.424918     9.603895     9.78799     9.907966    10.02210    10.03513    10.16753    10.12335
# HQ(n)      9.544564    9.344400     9.637478     9.877186    10.12201    10.302719    10.47758    10.55135    10.74448    10.76103
# SC(n)      9.728293    9.650614    10.066178    10.428372    10.79568    11.098877    11.39623    11.59247    11.90810    12.04713
# FPE(n) 12766.400828 9874.724537 12572.786580 15291.481059 18889.19840 22190.357754 26396.00910 29076.03773 37282.80229 41904.27113

#モデル作成
var.models <- VAR(ts_data.train, p=p_est$selection[1])
summary(var.models)


# VARモデルでの予測 --------------------------------------------------------------
pre <- predict(var.models, n.ahead = 9, ci = 0.95)

#日付データ作成
dates <- data.frame(week=seq(as.Date('2018-01-07'), as.Date('2018-03-04'), by=7))
#ドラゴンボール
res <- data.frame(prediction=pre$fcst$dragon_ball[, "fcst"])
res <- cbind(dates, res)

#アイドルマスター
res2 <- data.frame(prediction=pre$fcst$idol_master[, "fcst"])
res2 <- cbind(dates, res2)

# 可視化 ---------------------------------------------------------------------
#dragon_ball
plot_data <- cbind(rownames(data.frame(ts_data.d)), data.frame(ts_data.d))
names(plot_data)[1] <- "week"
plot_data$week <- as.Date(plot_data$week)

ggplot(data = plot_data, aes(x=week, y=dragon_ball)) + 
  geom_line() + 
  labs(title="dragon_ball", x="week", y="google trend") +
  geom_line(data = res, aes(x=week, y=prediction), colour = "blue")

#idol_master
plot_data <- cbind(rownames(data.frame(ts_data.d)), data.frame(ts_data.d))
names(plot_data)[1] <- "week"
plot_data$week <- as.Date(plot_data$week)

ggplot(data = plot_data, aes(x=week, y=idol_master)) + 
  geom_line() + 
  labs(title="idol_master", x="week", y="google trend") +
  geom_line(data = res2, aes(x=week, y=prediction), colour = "blue")


# グレンジャー因果性検定 ---------------------------------------------------------------
causality(var.models, cause = "dragon_ball")
# $Granger
# 
# Granger causality H0: dragon_ball do not Granger-cause idol_master
# 
# data:  VAR object var.models
# F-Test = 2.6375, df1 = 2, df2 = 70, p-value = 0.07864
#dragon_ballからidol_masterへのグレンジャー因果性は認められない

causality(var.models, cause = "idol_master")
# $Granger
# 
# Granger causality H0: idol_master do not Granger-cause dragon_ball
# 
# data:  VAR object var.models
# F-Test = 3.6861, df1 = 2, df2 = 70, p-value = 0.03006
#idol_masterからdragon_ballへのグレンジャー因果性は認められない


# 【作業】 ----------------------------------------------------------------------

VARselect(ts_data.train, lag.max = 5, type="none")
lag1 <- VARselect(ts_data.train, lag.max = 5, type="none")$selection[1] #5 (次数)
var.1 <- VAR(ts_data.train,　p=lag1,　type="none")

yosoku <- predict(var.1, n.ahead = 9, ci = 0.95, dumvar = NULL)

dates <- data.frame(week=seq(as.Date('2018-01-07'), as.Date('2018-03-04'), by=7))
ts_data <- data %>% dplyr::select(week, fgo, line, dragon_ball, puzzle_dragon, monster_strike, pokemon_go, idol_master)

kekka <- eval(parse(text = paste0("data.frame(", colnames(ts_data)[2], " = yosoku$fcst$fgo[,1])")))
#kekka <- data.frame(fgo = yosoku$fcst$fgo[,1])
kekka <- cbind(dates, kekka)

ts_data_fgo <- data %>% dplyr::select(week, fgo)
ts_data_fgo <- zoo(ts_data_fgo[,-1], ts_data_fgo$week) #zoo class
ts_data_fgo.d <- data.frame(diff(ts_data_fgo))
ts_data_fgo.d <- data.frame(cbind(rownames(ts_data_fgo.d), ts_data_fgo.d["fgo"]), stringsAsFactors = FALSE)
names(ts_data_fgo.d)[1] <- "week"

ts_data_fgo.d$week <- as.character(ts_data_fgo.d$week)
ts_data_fgo.d$week <- as.Date(ts_data_fgo.d$week)

ggplot(data = ts_data_fgo.d, aes(x=week, y=fgo)) + geom_line() + 
  geom_line(data = kekka, aes(x=week, y=fgo))

plot(ts_data_fgo.d)
par(new=T)
plot(kekka)



lines(kekka, type="l",col=1,lwd=2)


lines(ue,type="l",col=2,lwd=2)
lines(sita,type="l",col=2,lwd=2)

# 【参考】Rを用いたVAR ------------------------------------------------------------
#https://logics-of-blue.com/var%E3%83%A2%E3%83%87%E3%83%AB/

data(Canada)
plot(Canada)
test <- data.frame(Canada)

Canada.1998<-window(Canada,end=c(1998,2)) #1988年の2Qまでということ
plot(Canada.1998)
test <- data.frame(Canada.1998)

VARselect(Canada.1998, lag.max = 5, type="const")

lag1 <- VARselect(Canada.1998, lag.max = 5, type="const")$selection[1] #3 (次数)
var.1 <- VAR(Canada.1998,　p=lag1,　type="const")

yosoku <- predict(var.1, n.ahead = 8, ci = 0.95, dumvar = NULL)
kekka <- ts(yosoku$fcst$e[,1], start=1999, frequency=4)
sita <- ts(yosoku$fcst$e[,2], start=1999, frequency=4)
ue <- ts(yosoku$fcst$e[,3], start=1999, frequency=4)

# 【参考】window関数{stats}について -------------------------------------------------
#http://www.cis.doshisha.ac.jp/mjin/R/33/33.html

# window(x, start = NULL, end = NULL, frequency = NULL, 
#        deltat = NULL, extend = FALSE, ...)

# > window(UKgas,c(1975,2),c(1979,3))
# Qtr1	Qtr2	Qtr3	Qtr4
# 1975	　	321.8	177.7	409.8
# 1976	593.9	329.8	176.1	483.5
# 1977	584.3	395.4	187.3	485.1
# 1978	669.2	421.0	216.1	509.1
# 1979	827.7	467.5	209.7	

# 【参考】多変量の時系列データを分析する -----------------------------------------------------
#http://business.nikkeibp.co.jp/atclbdt/15/recipe/102500014/?ST=print

