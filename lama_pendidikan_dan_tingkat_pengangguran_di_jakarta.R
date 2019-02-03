# Korelasi tingkat pendidikan dan tingkat pengangguran di Jakarta

# import csv
#source: www.data.go.id
tingkat_pendidikan_data = read.csv('tingkat_pendidikan.csv')
tingkat_ketenagakerjaan_data = read.csv('tingkat_ketenagakerjaan.csv')

# data set for simple linear regression
#cleanup
lama_sekolah = tingkat_pendidikan_data[19:23,] #dalam satuan tahun
lama_sekolah$indikator_pendidikan <- NULL
lama_sekolah$satuan <- NULL
TPT = tingkat_ketenagakerjaan_data[40:44,] #dalam satuan persen
TPT$indikator_angkatan_kerja <- NULL
TPT$satuan <- NULL
#rename columns
colnames(lama_sekolah)[2] <- 'rata-rata lama sekolah'
colnames(TPT)[2] <- 'tingkat pengangguran terbuka'
#merge columns
dataframe_simpleLinearRegression <-  merge(x=lama_sekolah, y=TPT, by="tahun", all.y = TRUE)

# Because the dataset is small. We do not have to split the dataset

# Fitting the regression model to the dataset
#Create your regressor
set.seed(123)
regressor = lm(formula= dataframe_simpleLinearRegression$`tingkat pengangguran terbuka` ~ 
                 dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
               data= dataframe_simpleLinearRegression)

# Predicting a new result 

# Visualising the trained data
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
                 y = dataframe_simpleLinearRegression$`tingkat pengangguran terbuka`),
             colour = 'red') +
  geom_line(aes(x = dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
                y = predict(regressor, newdata = dataframe_simpleLinearRegression)),
            colour = 'blue') +
  ggtitle('Korelasi antara Rata-Rata Lama Sekolah & Tingkat Pengangguran Terbuka') +
  xlab('rata-rata lama sekolah (tahun)') +
  ylab('tingkat pengangguran terbuka (%)')

