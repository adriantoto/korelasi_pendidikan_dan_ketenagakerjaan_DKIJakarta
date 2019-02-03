# Korelasi tingkat pendidikan dan tingkat partisipasi ketenagakerjaan di jakarta

# import csv
#source: www.data.go.id
tingkat_pendidikan_data = read.csv('tingkat_pendidikan.csv')
tingkat_ketenagakerjaan_data = read.csv('tingkat_ketenagakerjaan.csv')

# data set for simple linear regression
#cleanup
lama_sekolah = tingkat_pendidikan_data[19:23,] #dalam satuan tahun
lama_sekolah$indikator_pendidikan <- NULL
lama_sekolah$satuan <- NULL
TPAK = tingkat_ketenagakerjaan_data[34:38,] #dalam satuan persen
TPAK$indikator_angkatan_kerja <- NULL
TPAK$satuan <- NULL
#rename columns
colnames(lama_sekolah)[2] <- 'rata-rata lama sekolah'
colnames(TPAK)[1] <- 'tahun'
colnames(TPAK)[2] <- 'tingkat partisipasi angkatan kerja'
#merge columns
dataframe_simpleLinearRegression <-  merge(x=lama_sekolah, y=TPAK, by="tahun", all.y = TRUE)

# Because the dataset is small. We do not have to split the dataset

# Fitting the regression model to the dataset
#Create your regressor
set.seed(123)
regressor = lm(formula= dataframe_simpleLinearRegression$`tingkat partisipasi angkatan kerja` ~ 
                        dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
              data= dataframe_simpleLinearRegression)

# Predicting a new result 

# Visualising the trained data
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
                 y = dataframe_simpleLinearRegression$`tingkat partisipasi angkatan kerja`),
              colour = 'red') +
  geom_line(aes(x = dataframe_simpleLinearRegression$`rata-rata lama sekolah`,
                y = predict(regressor, newdata = dataframe_simpleLinearRegression)),
            colour = 'blue') +
  ggtitle('Korelasi antara Rata-Rata Lama Sekolah & Tingkat Partisipasi Angkatan Kerja') +
  xlab('rata-rata lama sekolah (tahun)') +
  ylab('tingkat partisipasi angkatan kerja (%)')

