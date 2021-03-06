for(i in 1:200){
  url<-paste("http://movie.naver.com/movie/point/af/list.nhn?target=after&page=",i,sep = "")
  
  line<-c(line,readLines(url,encoding = "euc-kr"))
}

m_title<-line[str_detect(line,"class=\"movie\">")]
m_title<-gsub("[[:punct:]]","",m_title)
m_title<-gsub("[A-z]","",m_title)
m_title<-substr(m_title,12,length(m_title))


m_review<-line[which(str_detect(line,"class=\"movie\">"))+4]
m_review<-gsub("[[:punct:]]","",m_review)
m_review<-gsub("[A-z]","",m_review)
m_review<-gsub("\t\t\t","",m_review)

table(m_title)

total_top5<-head(sort(table(m_title),decreasing = T),5)
total1<-paste(m_title,": ",m_review,sep = "")


un_word<-readLines("c:/R/한국어불용어100.txt")

m_r1<-total1[str_detect(total1,"그것만이 내 세상")]
m_r1<-gsub("그것만이 내 세상:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z가-힣]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"피아노")]<-"피아노"
m_r1.1[str_detect(m_r1.1,"가족")]<-"가족"
m_r1.1[str_detect(m_r1.1,"이병헌")]<-"이병헌 연기"
m_r1.1[str_detect(m_r1.1,"박정민")]<-"박정민"
m_r1.1[str_detect(m_r1.1,"감동")]<-"감동적"
m_r1.1[str_detect(m_r1.1,"눈물")]<-"눈물이 왈칵"
m_r1.1[str_detect(m_r1.1,"장애인")]<-"장애인 연기"
m_r1.1[str_detect(m_r1.1,"배우")]<-"명배우들"
m_r1.1[str_detect(m_r1.1,"영화")]<-""
m_r1.1[str_detect(m_r1.1,"진태")]<-"진태 귀여움"
m_r1.1[str_detect(m_r1.1,"자체")]<-"지체장애"
m_r1.1[str_detect(m_r1.1,"너무")]<-"너무 슬픔"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시


m_r2<-total1[str_detect(total1,"코코")]
m_r2<-gsub("코코:","",m_r2)
m_r2.1<-SimplePos09(m_r2)
m_r2.1<-unlist(str_match_all(m_r2.1, '([A-Z가-힣]+)/N'))
m_r2.1<-m_r2.1[!str_detect(m_r2.1, '/')]
m_r2.1[m_r2.1%in%un_word]<-''

m_r2.1[str_detect(m_r2.1,"지루")]<-"지루함"
m_r2.1[str_detect(m_r2.1,"가족")]<-"가족"
m_r2.1[str_detect(m_r2.1,"재밌")]<-"재밌음"
m_r2.1[str_detect(m_r2.1,"절대")]<-"절대 보지마"
m_r2.1[str_detect(m_r2.1,"감동")]<-"감동적"
m_r2.1[str_detect(m_r2.1,"눈물")]<-"눈물이 왈칵"
m_r2.1[str_detect(m_r2.1,"짜증")]<-"짜증"
m_r2.1[str_detect(m_r2.1,"강추")]<-"강추"
m_r2.1[str_detect(m_r2.1,"가지")]<-"해골"

m_r2.1<-m_r2.1[str_length(m_r2.1)>1]

cnt<-head(sort(table(m_r2.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시


m_r3<-total1[str_detect(total1,"메이즈 러너 데스 큐어")]
m_r3<-gsub("메이즈 러너 데스 큐어:","",m_r3)
m_r3.1<-SimplePos09(m_r3)
m_r3.1<-unlist(str_match_all(m_r3.1, '([A-Z가-힣]+)/N'))
m_r3.1<-m_r3.1[!str_detect(m_r3.1, '/')]
m_r3.1[m_r3.1%in%un_word]<-''

m_r3.1[str_detect(m_r3.1,"시리즈")]<-"시리즈"
m_r3.1[str_detect(m_r3.1,"뉴트")]<-"뉴트"
m_r3.1[str_detect(m_r3.1,"민호")]<-"민호만 구함"
m_r3.1[str_detect(m_r3.1,"예측")]<-"예측된다"
m_r3.1[str_detect(m_r3.1,"감동")]<-"감동적"
m_r3.1[str_detect(m_r3.1,"트리사")]<-"트리사"
m_r3.1[str_detect(m_r3.1,"짜증")]<-"짜증"
m_r3.1[str_detect(m_r3.1,"실")]<-"실망"
m_r3.1[str_detect(m_r3.1,"재밌")]<-"재밌음"
m_r3.1[str_detect(m_r3.1,"영화")]<-""
m_r3.1[str_detect(m_r3.1,"진짜")]<-""
m_r3.1[str_detect(m_r3.1,"메이즈")]<-""
m_r3.1[str_detect(m_r3.1,"기억")]<-"기억 안남"
m_r3.1[str_detect(m_r3.1,"미노")]<-"미로"
m_r3.1[str_detect(m_r3.1,"이름")]<-"이름 묘비"
m_r3.1[str_detect(m_r3.1,"가슴")]<-"가슴에 권총"
m_r3.1[str_detect(m_r3.1,"권총")]<-"가슴에 권총"
m_r3.1[str_detect(m_r3.1,"기대")]<-"기대안했는데"
m_r3.1[str_detect(m_r3.1,"주인공")]<-"주인공 버프"
m_r3.1[str_detect(m_r3.1,"버프")]<-"주인공 버프"
m_r3.1[str_detect(m_r3.1,"노잼")]<-"핵노잼"
m_r3.1[str_detect(m_r3.1,"위키드")]<-"위키드"
m_r3.1[str_detect(m_r3.1,"알바")]<-"댓글 알바"

m_r3.1<-m_r3.1[str_length(m_r3.1)>1]

cnt<-head(sort(table(m_r3.1),decreasing = T),20)

wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시

# 최근 top3의 영화를 통해 현 관객들이 영화를 관람시 영화를 선택하는 관점은,,
# 이 예측을 통해 미래에 개봉전 영화들중 시사회에서의 관객들의 평가 리뷰를 통해 
# 어떤류의 영화가 흥행 할 수 있을 지 예측이 가능함 후에 연관성분석,감성분석등 다양한 분석기법은 후에 
line<-NULL

for(i in 1:200){
  url<-paste("http://movie.naver.com/movie/point/af/list.nhn?target=before&page=",i,sep = "")
  
  line<-c(line,readLines(url,encoding = "euc-kr"))
}

m_title<-line[str_detect(line,"class=\"movie\">")]
m_title<-gsub("[[:punct:]]","",m_title)
m_title<-gsub("[A-z]","",m_title)
m_title<-substr(m_title,12,length(m_title))


m_review<-line[which(str_detect(line,"class=\"movie\">"))+4]
m_review<-gsub("[[:punct:]]","",m_review)
m_review<-gsub("[A-z]","",m_review)
m_review<-gsub("\t\t\t","",m_review)

table(m_title)

total_top5<-head(sort(table(m_title),decreasing = T),5)
total1<-paste(m_title,": ",m_review,sep = "")



m_r1<-total1[str_detect(total1,"염력")]
m_r1<-gsub("염력:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z가-힣]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"한국")]<-"한국식 코미디"
m_r1.1[str_detect(m_r1.1,"가족")]<-"가족영화"
m_r1.1[str_detect(m_r1.1,"히어로")]<-"히어로물"
m_r1.1[str_detect(m_r1.1,"시사회")]<-"시사회 터짐"
m_r1.1[str_detect(m_r1.1,"류승룡")]<-"류승룡기모찌"
m_r1.1[str_detect(m_r1.1,"비현")]<-"비현실적"
m_r1.1[str_detect(m_r1.1,"연기")]<-"연기 구멍"
m_r1.1[str_detect(m_r1.1,"잼")]<-"핵꾸르르르잼삘"
m_r1.1[str_detect(m_r1.1,"영화")]<-""
m_r1.1[str_detect(m_r1.1,"초능력")]<-"초능력"
m_r1.1[str_detect(m_r1.1,"폭망")]<-"폭망각"
m_r1.1[str_detect(m_r1.1,"연상호")]<-"연상호감독"

m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시


m_r1<-total1[str_detect(total1,"맨 프럼 어스2 홀로신")]
m_r1<-gsub("맨 프럼 어스2 홀로신:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z가-힣]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"쿠키영상")]<-"쿠키영상"
m_r1.1[str_detect(m_r1.1,"기독교")]<-"기독교적"
m_r1.1[str_detect(m_r1.1,"살인")]<-"연쇄살인마"
m_r1.1[str_detect(m_r1.1,"교수")]<-"교수집"
m_r1.1[str_detect(m_r1.1,"떡밥")]<-"떡밥"
m_r1.1[str_detect(m_r1.1,"실")]<-"실망"
m_r1.1[str_detect(m_r1.1,"연기")]<-"발연기"
m_r1.1[str_detect(m_r1.1,"개")]<-"개망"
m_r1.1[str_detect(m_r1.1,"쓰레")]<-"쓰레기"
m_r1.1[str_detect(m_r1.1,"재")]<-"재밌는 소재를,,,"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시


m_r1<-total1[str_detect(total1,"1급기밀")]
m_r1<-gsub("1급기밀:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z가-힣]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"영화")]<-"영화 강추"
m_r1.1[str_detect(m_r1.1,"방산비")]<-"방산비리"
m_r1.1[str_detect(m_r1.1,"군")]<-"군납비"
m_r1.1[str_detect(m_r1.1,"지루")]<-"지루함"
m_r1.1[str_detect(m_r1.1,"답답")]<-"현실이 답답함"
m_r1.1[str_detect(m_r1.1,"실")]<-"실망"
m_r1.1[str_detect(m_r1.1,"연기")]<-"연기파 배우"
m_r1.1[str_detect(m_r1.1,"평론")]<-"평론가 평점"
m_r1.1[str_detect(m_r1.1,"평점")]<-"평론가 평점"
m_r1.1[str_detect(m_r1.1,"김상경")]<-"김상경"



m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # 데이터프레임으로 변환
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr패키지

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y축 길이
  geom_col()+               # 막대 그래프 그리기
  coord_flip()+               # 가로로 표시
  scale_x_discrete(limit=order)+         # 빈도순 막대 정렬
  geom_text(aes(label=freq), hjust=-0.3)      # 빈도 표시
