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


un_word<-readLines("c:/R/�ѱ���ҿ��100.txt")

m_r1<-total1[str_detect(total1,"�װ͸��� �� ����")]
m_r1<-gsub("�װ͸��� �� ����:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z��-�R]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"�ǾƳ�")]<-"�ǾƳ�"
m_r1.1[str_detect(m_r1.1,"����")]<-"����"
m_r1.1[str_detect(m_r1.1,"�̺���")]<-"�̺��� ����"
m_r1.1[str_detect(m_r1.1,"������")]<-"������"
m_r1.1[str_detect(m_r1.1,"����")]<-"������"
m_r1.1[str_detect(m_r1.1,"����")]<-"������ ��Ĭ"
m_r1.1[str_detect(m_r1.1,"�����")]<-"����� ����"
m_r1.1[str_detect(m_r1.1,"���")]<-"������"
m_r1.1[str_detect(m_r1.1,"��ȭ")]<-""
m_r1.1[str_detect(m_r1.1,"����")]<-"���� �Ϳ���"
m_r1.1[str_detect(m_r1.1,"��ü")]<-"��ü���"
m_r1.1[str_detect(m_r1.1,"�ʹ�")]<-"�ʹ� ����"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��


m_r2<-total1[str_detect(total1,"����")]
m_r2<-gsub("����:","",m_r2)
m_r2.1<-SimplePos09(m_r2)
m_r2.1<-unlist(str_match_all(m_r2.1, '([A-Z��-�R]+)/N'))
m_r2.1<-m_r2.1[!str_detect(m_r2.1, '/')]
m_r2.1[m_r2.1%in%un_word]<-''

m_r2.1[str_detect(m_r2.1,"����")]<-"������"
m_r2.1[str_detect(m_r2.1,"����")]<-"����"
m_r2.1[str_detect(m_r2.1,"���")]<-"�����"
m_r2.1[str_detect(m_r2.1,"����")]<-"���� ������"
m_r2.1[str_detect(m_r2.1,"����")]<-"������"
m_r2.1[str_detect(m_r2.1,"����")]<-"������ ��Ĭ"
m_r2.1[str_detect(m_r2.1,"¥��")]<-"¥��"
m_r2.1[str_detect(m_r2.1,"����")]<-"����"
m_r2.1[str_detect(m_r2.1,"����")]<-"�ذ�"

m_r2.1<-m_r2.1[str_length(m_r2.1)>1]

cnt<-head(sort(table(m_r2.1),decreasing = T),30)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��


m_r3<-total1[str_detect(total1,"������ ���� ���� ť��")]
m_r3<-gsub("������ ���� ���� ť��:","",m_r3)
m_r3.1<-SimplePos09(m_r3)
m_r3.1<-unlist(str_match_all(m_r3.1, '([A-Z��-�R]+)/N'))
m_r3.1<-m_r3.1[!str_detect(m_r3.1, '/')]
m_r3.1[m_r3.1%in%un_word]<-''

m_r3.1[str_detect(m_r3.1,"�ø���")]<-"�ø���"
m_r3.1[str_detect(m_r3.1,"��Ʈ")]<-"��Ʈ"
m_r3.1[str_detect(m_r3.1,"��ȣ")]<-"��ȣ�� ����"
m_r3.1[str_detect(m_r3.1,"����")]<-"�����ȴ�"
m_r3.1[str_detect(m_r3.1,"����")]<-"������"
m_r3.1[str_detect(m_r3.1,"Ʈ����")]<-"Ʈ����"
m_r3.1[str_detect(m_r3.1,"¥��")]<-"¥��"
m_r3.1[str_detect(m_r3.1,"��")]<-"�Ǹ�"
m_r3.1[str_detect(m_r3.1,"���")]<-"�����"
m_r3.1[str_detect(m_r3.1,"��ȭ")]<-""
m_r3.1[str_detect(m_r3.1,"��¥")]<-""
m_r3.1[str_detect(m_r3.1,"������")]<-""
m_r3.1[str_detect(m_r3.1,"���")]<-"��� �ȳ�"
m_r3.1[str_detect(m_r3.1,"�̳�")]<-"�̷�"
m_r3.1[str_detect(m_r3.1,"�̸�")]<-"�̸� ����"
m_r3.1[str_detect(m_r3.1,"����")]<-"������ ����"
m_r3.1[str_detect(m_r3.1,"����")]<-"������ ����"
m_r3.1[str_detect(m_r3.1,"���")]<-"�����ߴµ�"
m_r3.1[str_detect(m_r3.1,"���ΰ�")]<-"���ΰ� ����"
m_r3.1[str_detect(m_r3.1,"����")]<-"���ΰ� ����"
m_r3.1[str_detect(m_r3.1,"����")]<-"�ٳ���"
m_r3.1[str_detect(m_r3.1,"��Ű��")]<-"��Ű��"
m_r3.1[str_detect(m_r3.1,"�˹�")]<-"��� �˹�"

m_r3.1<-m_r3.1[str_length(m_r3.1)>1]

cnt<-head(sort(table(m_r3.1),decreasing = T),20)

wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,100)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��

# �ֱ� top3�� ��ȭ�� ���� �� �������� ��ȭ�� ������ ��ȭ�� �����ϴ� ������,,
# �� ������ ���� �̷��� ������ ��ȭ���� �û�ȸ������ �������� �� ���並 ���� 
# ����� ��ȭ�� ���� �� �� ���� �� ������ ������ �Ŀ� �������м�,�����м��� �پ��� �м������ �Ŀ� 
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



m_r1<-total1[str_detect(total1,"����")]
m_r1<-gsub("����:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z��-�R]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"�ѱ�")]<-"�ѱ��� �ڹ̵�"
m_r1.1[str_detect(m_r1.1,"����")]<-"������ȭ"
m_r1.1[str_detect(m_r1.1,"�����")]<-"����ι�"
m_r1.1[str_detect(m_r1.1,"�û�ȸ")]<-"�û�ȸ ����"
m_r1.1[str_detect(m_r1.1,"���·�")]<-"���·�����"
m_r1.1[str_detect(m_r1.1,"����")]<-"��������"
m_r1.1[str_detect(m_r1.1,"����")]<-"���� ����"
m_r1.1[str_detect(m_r1.1,"��")]<-"�ٲٸ��������"
m_r1.1[str_detect(m_r1.1,"��ȭ")]<-""
m_r1.1[str_detect(m_r1.1,"�ʴɷ�")]<-"�ʴɷ�"
m_r1.1[str_detect(m_r1.1,"����")]<-"������"
m_r1.1[str_detect(m_r1.1,"����ȣ")]<-"����ȣ����"

m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��


m_r1<-total1[str_detect(total1,"�� ���� �2 Ȧ�ν�")]
m_r1<-gsub("�� ���� �2 Ȧ�ν�:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z��-�R]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"��Ű����")]<-"��Ű����"
m_r1.1[str_detect(m_r1.1,"�⵶��")]<-"�⵶����"
m_r1.1[str_detect(m_r1.1,"����")]<-"������θ�"
m_r1.1[str_detect(m_r1.1,"����")]<-"������"
m_r1.1[str_detect(m_r1.1,"����")]<-"����"
m_r1.1[str_detect(m_r1.1,"��")]<-"�Ǹ�"
m_r1.1[str_detect(m_r1.1,"����")]<-"�߿���"
m_r1.1[str_detect(m_r1.1,"��")]<-"����"
m_r1.1[str_detect(m_r1.1,"����")]<-"������"
m_r1.1[str_detect(m_r1.1,"��")]<-"��մ� ���縦,,,"


m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��


m_r1<-total1[str_detect(total1,"1�ޱ��")]
m_r1<-gsub("1�ޱ��:","",m_r1)
m_r1.1<-SimplePos09(m_r1)
m_r1.1<-unlist(str_match_all(m_r1.1, '([A-Z��-�R]+)/N'))
m_r1.1<-m_r1.1[!str_detect(m_r1.1, '/')]
m_r1.1[m_r1.1%in%un_word]<-''

m_r1.1[str_detect(m_r1.1,"��ȭ")]<-"��ȭ ����"
m_r1.1[str_detect(m_r1.1,"����")]<-"����"
m_r1.1[str_detect(m_r1.1,"��")]<-"������"
m_r1.1[str_detect(m_r1.1,"����")]<-"������"
m_r1.1[str_detect(m_r1.1,"���")]<-"������ �����"
m_r1.1[str_detect(m_r1.1,"��")]<-"�Ǹ�"
m_r1.1[str_detect(m_r1.1,"����")]<-"������ ���"
m_r1.1[str_detect(m_r1.1,"���")]<-"��а� ����"
m_r1.1[str_detect(m_r1.1,"����")]<-"��а� ����"
m_r1.1[str_detect(m_r1.1,"����")]<-"����"



m_r1.1<-m_r1.1[str_length(m_r1.1)>1]

cnt<-head(sort(table(m_r1.1),decreasing = T),15)
wordcloud2(cnt)

df_word <- as.data.frame(cnt,header=T,stringsAsFactors=F)   # ���������������� ��ȯ
names(df_word) <- c("word","freq")

order <- arrange(df_word, freq)$word         # dplyr��Ű��

ggplot(data=df_word, aes(x=word, y=freq))+
  ylim(0,150)+               # y�� ����
  geom_col()+               # ���� �׷��� �׸���
  coord_flip()+               # ���η� ǥ��
  scale_x_discrete(limit=order)+         # �󵵼� ���� ����
  geom_text(aes(label=freq), hjust=-0.3)      # �� ǥ��