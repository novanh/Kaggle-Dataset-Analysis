library(dplyr)
library(ggplot2)
data = read.csv("kaggle.csv", sep = ";", fileEncoding = "latin1")
head(data)
str(data)

#Korelasi
cor(data$File_Count, data$Upvote)

#Kategori paling diminati berdasarkan upvote
data %>%
  group_by(Data_Type) %>%
  summarise(Total_Upvote = sum(Upvote)) %>%
  arrange(desc(Total_Upvote)) %>%
  arrange(desc(Total_Upvote)) %>%
  ggplot(aes(x = reorder(Data_Type, Total_Upvote), y = Total_Upvote)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Dataset Paling Diminati", x = "Data Type", y = "Total Upvote") +
  theme_minimal()

#Hubungan antara Jumlah File dan Upvote
ggplot(data, aes(x = File_Count, y = Upvote)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "pink") +
  labs(title = "Hubungan antara File Count dan Upvote",
       x = "File Count", y = "Upvote") +
  theme_minimal()

#Tren Upvote berdasarkan tahun
data$Year <- as.numeric(data$Last_Update)
data %>%
  group_by(Year) %>%
  summarise(Total_Upvote = sum(Upvote),
            Dataset_Count = n()) %>%
  ggplot(aes(x = Year, y = Total_Upvote)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Tren Upvotes Dataset Berdasarkan Tahun",
       x = "Tahun", y = "Total Upvote") +
  theme_minimal()