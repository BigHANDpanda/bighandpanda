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


un_word<-readLines("c:/R/ÇÑ±¹¾îºÒ¿ë¾î100.txt")

m_r1<-total1[str_detect(total1,"±×°Í¸¸ÀÌ ³» ¼¼»ó")]
m_r1<-gsub("±×°Í¸¸ÀÌ ³» ¼¼»ó:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z°¡-ÆR]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"ÇÇ¾Æ³ë")]<-"ÇÇ¾Æ³ë"
m_r1.1[str_detect(m_r1.1,"°¡Á·")]<-"°¡Á·"
m_r1.1[str_detect(m_r1.1,"ÀÌº´Çå")]<-"ÀÌº´Çå ¿¬±â"
m_r1.1[str_detect(m_r1.1,"¹ÚÁ¤¹Î")]<-"¹ÚÁ¤¹Î"
m_r1.1[str_detect(m_r1.1,"°¨µ¿")]<-"°¨µ¿Àû"
m_r1.1[str_detect(m_r1.1,"´«¹°")]<-"´«¹°ÀÌ ¿ĞÄ¬"
m_r1.1[str_detect(m_r1.1,"Àå¾ÖÀÎ")]<-"Àå¾ÖÀÎ ¿¬±â"
m_r1.1[str_detect(m_r1.1,"¹è¿ì")]<-"¸í¹è¿ìµé"
m_r1.1[str_detect(m_r1.1,"¿µÈ­")]<-""
m_r1.1[str_detect(m_r1.1,"ÁøÅÂ")]<-"ÁøÅÂ ±Í¿©¿ò"
m_r1.1[str_detect(m_r1.1,"ÀÚÃ¼")]<-"ÁöÃ¼Àå¾Ö"
m_r1.1[str_detect(m_r1.1,"³Ê¹«")]<-"³Ê¹« ½½ÇÄ"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã


m_r2<-total1[str_detect(total1,"ÄÚÄÚ")]
m_r2<-gsub("ÄÚÄÚ:","",m_r2)
m_r2.1<-SimplePos09(m_r2)
m_r2.1<-unlist(str_match_all(m_r2.1, '([A-Z°¡-ÆR]+)/N'))
m_r2.1<-m_r2.1[!str_detect(m_r2.1, '/')]
m_r2.1[m_r2.1%in%un_word]<-''

m_r2.1[str_detect(m_r2.1,"Áö·ç")]<-"Áö·çÇÔ"
m_r2.1[str_detect(m_r2.1,"°¡Á·")]<-"°¡Á·"
m_r2.1[str_detect(m_r2.1,"Àç¹Õ")]<-"Àç¹ÕÀ½"
m_r2.1[str_detect(m_r2.1,"Àı´ë")]<-"Àı´ë º¸Áö¸¶"
m_r2.1[str_detect(m_r2.1,"°¨µ¿")]<-"°¨µ¿Àû"
m_r2.1[str_detect(m_r2.1,"´«¹°")]<-"´«¹°ÀÌ ¿ĞÄ¬"
m_r2.1[str_detect(m_r2.1,"Â¥Áõ")]<-"Â¥Áõ"
m_r2.1[str_detect(m_r2.1,"°­Ãß")]<-"°­Ãß"
m_r2.1[str_detect(m_r2.1,"°¡Áö")]<-"ÇØ°ñ"

m_r2.1<-m_r2.1[str_length(m_r2.1)>1]

cnt<-head(sort(table(m_r2.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã


m_r3<-total1[str_detect(total1,"¸ŞÀÌÁî ·¯³Ê µ¥½º Å¥¾î")]
m_r3<-gsub("¸ŞÀÌÁî ·¯³Ê µ¥½º Å¥¾î:","",m_r3)
m_r3.1<-SimplePos09(m_r3)
m_r3.1<-unlist(str_match_all(m_r3.1, '([A-Z°¡-ÆR]+)/N'))
m_r3.1<-m_r3.1[!str_detect(m_r3.1, '/')]
m_r3.1[m_r3.1%in%un_word]<-''

m_r3.1[str_detect(m_r3.1,"½Ã¸®Áî")]<-"½Ã¸®Áî"
m_r3.1[str_detect(m_r3.1,"´ºÆ®")]<-"´ºÆ®"
m_r3.1[str_detect(m_r3.1,"¹ÎÈ£")]<-"¹ÎÈ£¸¸ ±¸ÇÔ"
m_r3.1[str_detect(m_r3.1,"¿¹Ãø")]<-"¿¹ÃøµÈ´Ù"
m_r3.1[str_detect(m_r3.1,"°¨µ¿")]<-"°¨µ¿Àû"
m_r3.1[str_detect(m_r3.1,"Æ®¸®»ç")]<-"Æ®¸®»ç"
m_r3.1[str_detect(m_r3.1,"Â¥Áõ")]<-"Â¥Áõ"
m_r3.1[str_detect(m_r3.1,"½Ç")]<-"½Ç¸Á"
m_r3.1[str_detect(m_r3.1,"Àç¹Õ")]<-"Àç¹ÕÀ½"
m_r3.1[str_detect(m_r3.1,"¿µÈ­")]<-""
m_r3.1[str_detect(m_r3.1,"ÁøÂ¥")]<-""
m_r3.1[str_detect(m_r3.1,"¸ŞÀÌÁî")]<-""
m_r3.1[str_detect(m_r3.1,"±â¾ï")]<-"±â¾ï ¾È³²"
m_r3.1[str_detect(m_r3.1,"¹Ì³ë")]<-"¹Ì·Î"
m_r3.1[str_detect(m_r3.1,"ÀÌ¸§")]<-"ÀÌ¸§ ¹¦ºñ"
m_r3.1[str_detect(m_r3.1,"°¡½¿")]<-"°¡½¿¿¡ ±ÇÃÑ"
m_r3.1[str_detect(m_r3.1,"±ÇÃÑ")]<-"°¡½¿¿¡ ±ÇÃÑ"
m_r3.1[str_detect(m_r3.1,"±â´ë")]<-"±â´ë¾ÈÇß´Âµ¥"
m_r3.1[str_detect(m_r3.1,"ÁÖÀÎ°ø")]<-"ÁÖÀÎ°ø ¹öÇÁ"
m_r3.1[str_detect(m_r3.1,"¹öÇÁ")]<-"ÁÖÀÎ°ø ¹öÇÁ"
m_r3.1[str_detect(m_r3.1,"³ëÀë")]<-"ÇÙ³ëÀë"
m_r3.1[str_detect(m_r3.1,"À§Å°µå")]<-"À§Å°µå"
m_r3.1[str_detect(m_r3.1,"¾Ë¹Ù")]<-"´ñ±Û ¾Ë¹Ù"

m_r3.1<-m_r3.1[str_length(m_r3.1)>1]

cnt<-head(sort(table(m_r3.1),decreasing = T),20)

wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã

# ÃÖ±Ù top3ÀÇ ¿µÈ­¸¦ ÅëÇØ Çö °ü°´µéÀÌ ¿µÈ­¸¦ °ü¶÷½Ã ¿µÈ­¸¦ ¼±ÅÃÇÏ´Â °üÁ¡Àº,,
# ÀÌ ¿¹ÃøÀ» ÅëÇØ ¹Ì·¡¿¡ °³ºÀÀü ¿µÈ­µéÁß ½Ã»çÈ¸¿¡¼­ÀÇ °ü°´µéÀÇ Æò°¡ ¸®ºä¸¦ ÅëÇØ 
# ¾î¶²·ùÀÇ ¿µÈ­°¡ ÈïÇà ÇÒ ¼ö ÀÖÀ» Áö ¿¹ÃøÀÌ °¡´ÉÇÔ ÈÄ¿¡ ¿¬°ü¼ººĞ¼®,°¨¼ººĞ¼®µî ´Ù¾çÇÑ ºĞ¼®±â¹ıÀº ÈÄ¿¡ 
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



m_r1<-total1[str_detect(total1,"¿°·Â")]
m_r1<-gsub("¿°·Â:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z°¡-ÆR]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"ÇÑ±¹")]<-"ÇÑ±¹½Ä ÄÚ¹Ìµğ"
m_r1.1[str_detect(m_r1.1,"°¡Á·")]<-"°¡Á·¿µÈ­"
m_r1.1[str_detect(m_r1.1,"È÷¾î·Î")]<-"È÷¾î·Î¹°"
m_r1.1[str_detect(m_r1.1,"½Ã»çÈ¸")]<-"½Ã»çÈ¸ ÅÍÁü"
m_r1.1[str_detect(m_r1.1,"·ù½Â·æ")]<-"·ù½Â·æ±â¸ğÂî"
m_r1.1[str_detect(m_r1.1,"ºñÇö")]<-"ºñÇö½ÇÀû"
m_r1.1[str_detect(m_r1.1,"¿¬±â")]<-"¿¬±â ±¸¸Û"
m_r1.1[str_detect(m_r1.1,"Àë")]<-"ÇÙ²Ù¸£¸£¸£Àë»â"
m_r1.1[str_detect(m_r1.1,"¿µÈ­")]<-""
m_r1.1[str_detect(m_r1.1,"ÃÊ´É·Â")]<-"ÃÊ´É·Â"
m_r1.1[str_detect(m_r1.1,"Æø¸Á")]<-"Æø¸Á°¢"
m_r1.1[str_detect(m_r1.1,"¿¬»óÈ£")]<-"¿¬»óÈ£°¨µ¶"

m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã


m_r1<-total1[str_detect(total1,"¸Ç ÇÁ·³ ¾î½º2 È¦·Î½Å")]
m_r1<-gsub("¸Ç ÇÁ·³ ¾î½º2 È¦·Î½Å:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z°¡-ÆR]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"ÄíÅ°¿µ»ó")]<-"ÄíÅ°¿µ»ó"
m_r1.1[str_detect(m_r1.1,"±âµ¶±³")]<-"±âµ¶±³Àû"
m_r1.1[str_detect(m_r1.1,"»ìÀÎ")]<-"¿¬¼â»ìÀÎ¸¶"
m_r1.1[str_detect(m_r1.1,"±³¼ö")]<-"±³¼öÁı"
m_r1.1[str_detect(m_r1.1,"¶±¹ä")]<-"¶±¹ä"
m_r1.1[str_detect(m_r1.1,"½Ç")]<-"½Ç¸Á"
m_r1.1[str_detect(m_r1.1,"¿¬±â")]<-"¹ß¿¬±â"
m_r1.1[str_detect(m_r1.1,"°³")]<-"°³¸Á"
m_r1.1[str_detect(m_r1.1,"¾²·¹")]<-"¾²·¹±â"
m_r1.1[str_detect(m_r1.1,"Àç")]<-"Àç¹Õ´Â ¼ÒÀç¸¦,,,"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã


m_r1<-total1[str_detect(total1,"1±Ş±â¹Ğ")]
m_r1<-gsub("1±Ş±â¹Ğ:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z°¡-ÆR]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"¿µÈ­")]<-"¿µÈ­ °­Ãß"
m_r1.1[str_detect(m_r1.1,"¹æ»êºñ")]<-"¹æ»êºñ¸®"
m_r1.1[str_detect(m_r1.1,"±º")]<-"±º³³ºñ"
m_r1.1[str_detect(m_r1.1,"Áö·ç")]<-"Áö·çÇÔ"
m_r1.1[str_detect(m_r1.1,"´ä´ä")]<-"Çö½ÇÀÌ ´ä´äÇÔ"
m_r1.1[str_detect(m_r1.1,"½Ç")]<-"½Ç¸Á"
m_r1.1[str_detect(m_r1.1,"¿¬±â")]<-"¿¬±âÆÄ ¹è¿ì"
m_r1.1[str_detect(m_r1.1,"Æò·Ğ")]<-"Æò·Ğ°¡ ÆòÁ¡"
m_r1.1[str_detect(m_r1.1,"ÆòÁ¡")]<-"Æò·Ğ°¡ ÆòÁ¡"
m_r1.1[str_detect(m_r1.1,"±è»ó°æ")]<-"±è»ó°æ"



m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # µ¥ÀÌÅÍÇÁ·¹ÀÓÀ¸·Î º¯È¯
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyrÆĞÅ°Áö

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # yÃà ±æÀÌ
  geom_col()+               # ¸·´ë ±×·¡ÇÁ ±×¸®±â
  coord_flip()+               # °¡·Î·Î Ç¥½Ã
  scale_x_discrete(limit=order)+         # ºóµµ¼ø ¸·´ë Á¤·Ä
  geom_text(aes(label=freq), hjust=-0.3)      # ºóµµ Ç¥½Ã
