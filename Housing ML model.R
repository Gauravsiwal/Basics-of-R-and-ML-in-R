df = read.csv("D:/desktop/ML by R Alison/Complete ML in R/1. Linear Regression/House_Price.csv", header = TRUE)
View(df)
dim(df)
summary(df)

df$waterbody<- as.factor(df$waterbody)
df$airport<- as.factor(df$airport)
df$bus_ter<- as.factor(df$bus_ter)

summary(df)
head(df)
str(df)
colnames(df)
library('ggplot2')
p = ggplot(data = df, aes(waterbody))
p+ geom_bar(fill='skyblue')

num_cols=Filter(is.numeric,df)
corr_data = cor(num_cols, method = ("pearson"))
cor(x=df$crime_rate,y=df$price)
View(corr_data)



# Removing bus_ter as it has only one value 'Yes'
df <- df[,c(-18)]
head(df,5)
View(df)

# Correlation Matrix
df_num = df[,c(-13,-16)]
View(df_num)
cor_mat = cor(df_num)
View(cor_mat)
View(cor_mat[,'price'])

install.packages('corrplot')
library('corrplot')
corrplot(corr_data, method = c('color'),
         addCoef.col = 'black',
         tl.cex = 0.5,
         number.cex = 0.5)

# Scatter plot of Price
qplot(price,poor_prop, data=df, geom=c('point'))
qplot(price,room_num, data=df, geom=c('point'))


# Treating the missing value in n_hos_bed
mean_val <- mean(df$n_hos_beds, na.rm=TRUE)
mean_val
df$n_hos_beds[is.na(df$n_hos_beds)]= mean_val
df$n_hos_beds[is.na(df$n_hos_beds)]
corrplot(cor_mat, method = c('color'),
         addCoef.col = 'darkgreen',
         tl.cex = 0.7,
         number.cex = 0.5)

# Outlier treatment

# Make Boxplot
uv_cr = quantile(df$crime_rate,0.95)
df$crime_rate[df$crime_rate> uv_cr] = uv_cr
summary(df$crime_rate)

# Make boxplot
p = ggplot(df, aes(x=crime_rate))
p+ geom_boxplot()

p = ggplot(df, aes(x=n_hot_rooms))
p+ geom_boxplot()


uv_hr = quantile(df$n_hot_rooms,0.95)
df$n_hot_rooms[df$n_hot_rooms> uv_hr] = uv_hr
summary(df$n_hot_rooms)

p = ggplot(df, aes(x=rainfall))
p+ geom_boxplot()

lv_rain = quantile(df$rainfall,0.05)
df$rainfall[df$rainfall< lv_rain] = lv_rain
summary(df$rainfall)

head(df)


# Feature Selection
df$avg_dist = (df$dist1 + df$dist2+ df$dist3 +df$dist4)/4
# Removing the dist1, dist2, dist3 and dist4 by replacing them with avg_dist
df = df[,c(-7,-8,-9,-10)]
View(df)


# Categorical treatment
install.packages('dummies')
library('dummies')
df = dummy.data.frame(df)
View(df)
df = df[,c(-9,-15)]


# Train Test Split
install.packages('caTools')
library('caTools')

split = sample.split(df, SplitRatio = 0.8)
train = subset(df, split== TRUE)
test = subset(df, split== FALSE)
View(train)



# Linear Regression
lin_model = lm(price~., data = train)
summ = (summary(lin_model))
summ
adj_r2 = 0.7236 
cat('The Adjusted R squared value of the model is', adj_r2)


pred_y = predict(lin_model, test)
pred_y

predictions = data.frame(test$price,pred_y)
View(predictions)

mse = mean((pred_y - test$price)^2)
cat('The mean squared error of the model on test data is', mse)







