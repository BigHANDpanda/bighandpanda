'''
컴퓨터를 통해 자동으로 가상화폐를 매수/매도하거나 가상화폐 시장을 분석해서 화폐를 트레이딩하는 컴퓨터 프로그램
투자 기법 알고리즘과  API를 통해 실시간 가격정보 ,거래량을 통해 분석하고 예측하여 매수/매도시점의 타이밍을 알아보는 툴
거래량이 갑자기 늘거나 매수가 혹은 매도가가 갑자기 낮아지거나 높아질때 비율로 계산을 하여 알고리즘 조건에 맞게 되면 
즉각 매수/매도하게 됨
if(종가 - 손절 최적화값 < 현재가) 이렇게 되면 매도한 후 평균가로 매수
특정값 = [(현)고가-저가] * 가중치값(0.2, 0.5, 1)

'''

'''1시간 또는 하루 단위로 데이터를 저장 후 시계열 예측만 하는것이 아니라 1시간,하루 단위로 학습을 시키려고 함데이터를 많이 모으되 1시간 분량의 데이터들로 나눈는 작업을 한 후 훈련데이터 테스트 데이터로 분류 후 지도 학습을 시킴하신간 별로 하루별로 어떤 모형을 형성하고있는지 제일 거래량이 높은 시간대는 언제인지 등등.. 다양한 결과를 만들어 보도록 하겠음
시계열 분석 중에서도 가상화폐의 주가를 알아보는 시계열은 비정상성을 가진 시계열이다즉, 비정상시계열은 차분을 하여 정상성을 만족하는 시계열로 만듦AR,MA,ARIMA에서도 ARIMA
- 지난 200일 또는 30일 단위로 모델을 학습시켜서 가중치를 받아냄 앞으로 어떤 방향으로 갈지 기계학습을 시킬 예정'''

import os
os.getcwd()os.chdir("/Users/stu/Anaconda3")
import sysfrom xcoin_api_client import *import timefrom pandas import *from json import JSONDecodeError
api_key = "1a30f002cdf21b3760b8c04323056a7a"api_secret = "7d864d0842a931d52a43667e141bd0d0" # 시크릿키는 보호api = XCoinAPI(api_key, api_secret)coin = "BTC"
rgParams = { "order_currency" : coin, "payment_currency" : "KRW"};
df = DataFrame(columns=['DATE','STATUS','LAST_PRICE','BUY_PRICE','SELL_PRICE','AVG_PRICE','1DAY'])
while True:         
  try:             
    result = api.xcoinApiCall("/public/ticker", rgParams);
         print("date :"  + time.strftime('%x %X', time.localtime()))         
         print("status : " + result["status"]);         
         print("last : " + result["data"]["closing_price"]+'원');         
         print("buy : " + result["data"]["buy_price"]+'원');         
         print("sell : " + result["data"]["sell_price"]+'원');         
         print("sell : " + result["data"]["max_price"]+'원');         
         print("sell : " + result["data"]["min_price"]+'원');         
         print("average : " + result["data"]["average_price"]+'원');         
         print("1day : " + result["data"]["volume_1day"]+'개');         
         print('\n')                 
          if float(result1["data"]["available_krw"]) >= 1000:                                       
            for_bids = {"quantity" : float(result1["data"]["available_krw"] / float(result["data"]["sell_price"])), "price" : float(result["data"]["average_price"])}             
            result2 = api.xcoinApiCall("public/orderbook", for_bids)                          
            print(float(result["data"]["average_price"]))             
            print("bids OK")
             
            price1 = (float(result["data"]["sell_price"]) - float(result["data"]["buy_price"])) * 0.2                  
            
              if float(result1["data"]["available_btc"]) >= 0:                          
                if ( float(result["data"]["closing_price"]) - price1 ) < float(result["data"]["sell_price"]):                                                   
                  for_sell = {"units" : float(result1["data"]["available_btc"]), "currency" : coin}                 
                  result3 = api.xcoinApiCall("/trade/market_sell", for_sell)                              
                  print(float(result["data"]["buy_price"]))                 
                  print("sell OK")                                         
                  f = open("C:\\Users\\stu\\Anaconda3\\contact_db.txt", "a")         
                  f.write(time.strftime('%x %X', time.localtime())+','+                 
                  result["status"]+','+                 
                  result["data"]["closing_price"]+','+                 
                  result["data"]["buy_price"]+','+                 
                  result["data"]["sell_price"]+','+                 
                  result["data"]["average_price"]+','+                 
                  result["data"]["volume_1day"]+'\n')         
                  f.close()                 
                  time.sleep(5)
                  
     except JSONDecodeError as e:         
     print(e)         
     time.sleep(1)
    
sys.exit(0);

