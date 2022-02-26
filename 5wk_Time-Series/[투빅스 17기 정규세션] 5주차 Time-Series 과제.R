##############################################################
## [투빅스 17기 정규세션] 5주차 Time-Series 과제 - 17기 나다경 ##
##############################################################

# 패키지 설치

#install.packages("forecast")
library(forecast)
library(timsac)

# 데이터 불러오기

data <- read.csv('C:/Users/user/Desktop/교안/[투빅스 17기 정규세션] 5주차 Time-Series_16기 이예림/과제/kingage.csv')
Y <- ts(data)
plot.ts(Y) # 추세, 계절성이 있다고 판단

ndiffs(Y) # 1번의 차분 과정이 필요

diff1_Y <- diff(Y, differences = 1) # 1차 차분한 경우
plot.ts(diff1_Y)

diff12_Y <- diff(diff1_Y, lag=12) # 1차 차분한 후 계절 차분을 수행한 경우(선택)
plot.ts(diff12_Y)

# 1. 모형 식별

par(mfrow=c(2,1))
acf(diff12_Y, main='ACF') # 절단값 x
pacf(diff12_Y, main="PACF") # lag 4에서 절단점을 가짐
auto.arima(diff12_Y)
forecast::tsdisplay(diff12_Y)
timsac::autoarmafit(diff12_Y)
## 후보 1 : ARIMA(3,1,0)
## 후보 2 : ARIMA(1,1,2) # auto.fit
## 후보 2 : ARIMA(0,0,1) # auto.arima


# 2. 모수 추정

fit1 <- arima(Y, c(3,1,0), seasonal = list(order = c(1,1,0), period=12))
fit2 <- arima(Y, c(1,1,2), seasonal = list(order = c(1,1,0), period=12))
fit3 <- arima(Y, c(0,0,1), seasonal = list(order = c(1,1,0), period=12))

fit1 # aic 262.85 (선택)
fit2 # aic 264.57
fit3 # aic 271.85

# 3. 모형 적합성 진단

tsdiag(fit1) # 잔차의 ACF에서 자기 상관성 x, 
             # p-value가 기준선 위에 위치하기 때문에 
             # 모형이 잘 나왔다고 판단

# 4. 모형 확정 및 예측 (예측값 17개 출력하기)

diff12_Y.forecasts <- forecast(fit1, h=17)
diff12_Y.forecasts
plot(diff12_Y.forecasts)
