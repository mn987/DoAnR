setwd("C:/Users/MSI/Downloads/Ptdl R/")


library("readxl")
library("ggplot2")

data <- read_excel("Hotel.xlsx") #d???c d??? li???u t??? file hotel.xlsx

df <- na.omit(data) #l???c các bi???n NA
df <- data.frame(df)
# review <- df$Reviews
# plot(x=review,y=names(review),type="l",col='red',main = "d??? li???u khách s???n bi???u d??? plot",ylab = "count")

#các giá tr??? th???ng kê suy di???n
print(paste0('Giá tr??? trung binh: ',mean(df[,16])))
print(paste0('Giá tr??? trung v???: ',median(df[,16])))
print(paste0('Giá tr??? l???n nh???t: ',max(df[,16])))
print(paste0('Giá tr??? nh??? nh???t: ',min(df[,16])))
print(paste0('Phuong sai: ',var(df[,16])))


#L???y 2 c???t average và terrible
which(names(df)=='Average')
which(names(df)=='Terrible')
aver_ter <- data.frame(label <- c('Average','Terrible'),
                       sum <- c(sum(df$Average),sum(df$Terrible)))

ggplot(data=aver_ter, aes(x=label, y=sum,fill=label))+
  geom_bar(stat="identity")


#l???y c???t Hotel language
lang <- df$HotelLanguage
lang
lang_unique <- NA
for (i in lang){
  lang_unique <- lang_unique + i
}

lang_unique

which(names(df)=='Excellent')
which(names(df)=='Terrible')
new_df <- df[,13:17]
length(names(new_df))
sum_of_column_in_new_df <- c()
for (i in new_df){
  sum_of_column_in_new_df<-append(sum_of_column_in_new_df,sum(i))
}


labels <- c('Excellent','VeryGood','Average','Poor','Terrible')
new_df <- data.frame(review = c(labels),
                     sum = sum_of_column_in_new_df)
new_df
# barplot(t(as.matrix(new_df)),beside=TRUE,names.arg = labels)
ggplot(data=new_df, aes(x=review, y=sum,fill=review))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#9933FF",
                             "#33FFFF",
                             "red",
                             "darkblue",
                             'blue'))

data_3 = sample(data[,18], 50)
which(names(data)=='TripAdvisorRank')
unique_data <- unique(data_3)
unique_data
sum_rank <- c()
temp = 0
for (x in unique_data){
  for (y in data_3){
    if(x==y){
      temp = temp + 1
    }
  }
  sum_rank = append(sum_rank,temp)
  temp = 0
}
sum_rank
data_rate_rank_hotel <- data.frame(rank = unique_data, 
                                   sum_rank = sum_rank)
data_rate_rank_hotel
library(RColorBrewer)
myPalette <- brewer.pal(5, "Set2") 


pie(sum_rank,unique_data, border="white",col=myPalette, main='Bieu do ti le so sao lay mau ngau nhien 50 khach san')



value <- data[,16]
missing <- df[,16]


average <- df[,'Average']
poor <- df[,'Poor']


plot(average,type = "o",col = "red", xlab = "ID", ylab = "Money", 
     main = "Bi???u d??? du???ng c???a average v???i poor")


lines(poor, type = "o", col = "blue")

boxplot(average,poor)


#Ve bieu do
#Tao cac list chua tong cua tung du lieu (kieu so) trong tap du lieu

#Tao data frame tu 2 list tren
data_1 = data.frame(class, sum)

#Ve bieu do the hien tong gia tri
#cua tung du lieu dinh luong cua tap du lieu
ggplot(data_1, aes(x = class, y = sum, 
                   fill = class)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=sum), vjust=0, position = position_stacknudge(y=0.55)) +
  scale_y_log10() +
  labs(title="Total value of the quantitative data",
       x = "Rate",
       y = "Amount", 
       fill = "Class") + 
  coord_flip() +
  theme(axis.text.x = element_text(size = 8, angle = 90))
# Tao cac list theo gia max tu thap nhat
# den cao nhat va khong co gia tri (NA)
low = subset(data[,12], data$Reviews <= 300)
ave = subset(data[,12], data$Reviews <= 700 & data$Reviews > 300)
high = subset(data[,12], data$Reviews  > 700)
na = subset(data[,12], is.na(data$Reviews ) == TRUE)
#Tao list chua length cua tung muc do gia max
price = c(length(low),
          length(ave),
          length(high),
          length(na))
classify = c("Low",
             "Average",
             "High",
             "NA")
#tao data frame tu 2 list tren
data_2 = data.frame(classify, price)

#Ve bieu thi the hien so luong gia tung khach san s???a l???i 
#theo tung muc do tu thap den cao va khong co gia (NA)

ggplot(data_2, aes(x = classify, y = price, fill = classify)) +    
  geom_col(position = "stack") +
  geom_text(aes(label=price), vjust=0) +
  labs(title="Number of prices for each hotel from low to high price",
       subtitle = "Hoi An",
       x = "Price levels",
       y = "Amount",
       fill = "Class") +
  scale_y_log10()

#Ve bieu do the hien so luong khach san duoc danh gia
#theo tung muc do tu terrible cho den excellent va NA

ggplot(data, aes(x= data$LocationRank, fill = data$LocationRank)) + 
  geom_bar(stat="count") +
  labs(title = "Quality of hotels in Hoi An",
       x = "Rate",
       y = "Amount",
       fill = "Class") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_log10()



#Ph???n crawl d??? li???u web v???
setwd("C:/Users/MSI/Downloads/Ptdl R/")
df = read.csv("popconvert2.csv", header =TRUE)
df
df[1,4]

#Tinh cac gia tri
fert<-df$Ti.Le.Sinh
fert
table(fert)

#Do dai
dodai<-length(fert)
print(dodai)
#Trung binh  
trung_binh<-mean(fert)
print(trung_binh)

#Yeu vi
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
yeu_vi<-getmode(fert)
print(yeu_vi)

#Trung vi
trung_vi<- median(fert)
print(trung_vi)

#Phuong sai
phuong_sai<-var(fert)
print(phuong_sai)

#Do lech chuan
do_lech_chuan<-sd(fert)
print(do_lech_chuan)

#Sai so chuan
sai_so_chuan = sd(fert)/sqrt(length(fert))
print(sai_so_chuan)

#Ve bieu do

#Bieu do tron(phan tram dan so)
country<-c("Trung Quoc", "An Do", "Hoa Ky", "Indonesia", "Pakistan", "Brazil", "Cac quoc gia khac")
pie(df$X..Dan.So, main = "Phan tram dan so", labels = country, radius = 0.8, clockwise = FALSE, col = rainbow(length(fert)))

#Bieu do cot ket hop(Tan suat ti le sinh)

a<-data.frame(QuocGia=c(df$Quoc.Gia),TiLe=c(df$Ti.Le.Sinh))
table(df$Ti.Le.Sinh)
barplot(table(a), main = "Ti Le Sinh Theo Quoc Gia", col = "yellow", horiz = TRUE)

#Bieu do duong

ggplot(a, aes(x = QuocGia, y = TiLe, group = 1)) + geom_line(color = "red") + geom_point(color ="blue")
