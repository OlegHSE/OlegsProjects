# Выгружаем данные:
#library(readr)
df <- read.csv("~/Эконометрика/insurance.csv")
head(df)
str(df)
dim(df)
df$children <- factor(df$children)
cat_features <- c("sex", "children", "smoker", "region")

# Создаем дамми-переменные с помощью библиотеки fastDummies:
library(fastDummies)
df_enc <- dummy_cols(df, 
                     select_columns = cat_features,
                     remove_selected_columns = TRUE,
                     remove_first_dummy = TRUE, )

# закодируем переменные с children с накоплением
# для лучшей интерпретации:
df_enc[df_enc$children_5 == 1, c('children_1', 'children_2',
                                 'children_3', 'children_4')] <- 1
df_enc[df_enc$children_4 == 1, c('children_1', 'children_2',
                                 'children_3')] <- 1
df_enc[df_enc$children_3 == 1, c('children_1', 'children_2')] <- 1
df_enc[df_enc$children_2 == 1, c('children_1')] <- 1

D_children_0 <- rep(1, 6)
D_children_1 <- c(0, rep(1, 5))
D_children_2 <- c(rep(0, 2), rep(1, 4))
D_children_3 <- c(rep(0, 3), rep(1, 3))
D_children_4 <- c(rep(0, 4), rep(1, 2))
D_children_5 <- c(rep(0, 5), 1)
children_enc <- data.frame(D_children_0, D_children_1, D_children_2,
                        D_children_3, D_children_4, D_children_5, 
                        row.names = c('children_0', 'children_1', 'children_2',
                                      'children_3', 'children_4', 'children_5'))
children_enc
write.table(children_enc, file = "children_enc", sep = ",",
            row.names = c('children_0', 'children_1', 'children_2',
                          'children_3', 'children_4', 'children_5'))

# Разведочный анализ данных:
library(ggplot2)
install.packages("stargazer")
library(stargazer)

# Summary statistics:

stargazer(df_enc, type = 'html', out='summary_table', 
          median = TRUE)

# correlation 

install.packages("ggcorrplot")
library(ggcorrplot)
corr <- round(cor(df_enc), 1)
corrplot <- ggcorrplot(corr, hc.order = TRUE, 
           outline.col = "white", lab = TRUE) + 
  labs(title = 'Корреляционная матрица')
corrplot
ggsave('corrplot.png', corrplot, width = 12, height = 7)


# charges

ggplot(data=df, mapping=aes(x=bmi)) + 
  geom_histogram(bins = 30, color='black', fill="steelblue") +
  theme_bw()

ggplot(data=df, mapping=aes(x=charges, y=..density..)) + 
  geom_histogram(bins = 30, color='black', fill="aliceblue") +
  geom_density(color = "steelblue", size = 0.7) + theme_bw() +
  labs(title='Распределение charges')

ggplot(data=df, mapping=aes(x=log(charges))) + 
  geom_histogram(bins = 30, color='black', fill="white") + theme_bw()

ggplot(data=df, mapping=aes(x=log(charges), y=..density..)) + 
  geom_histogram(bins = 30, color='black', fill="aliceblue") +
  geom_density(color = "steelblue", size = 0.7) + theme_bw() +
  labs(title='Распределение log(charges)')

# charges / sex

ggplot(data=df, mapping=aes(y=charges, x=sex, fill=sex)) +
  geom_boxplot() + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=sex, fill=sex)) +
  geom_violin() + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=sex, fill=sex)) +
  geom_boxplot() + theme_bw()

ggplot(data=df, mapping=aes(x=log(charges), fill=sex)) +
  geom_histogram() + theme_bw()

t.test(charges ~ sex, df)
t.test(log(charges) ~ sex, df)

# charges / smoker

ggplot(data=df, mapping=aes(y=charges, x=smoker, 
                            fill=smoker)) +
  geom_boxplot() + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=smoker, 
                            fill=smoker)) +
  geom_boxplot() + theme_bw()

ggplot(data=df, mapping=aes(x=charges, y=..density.., color=smoker, 
                            fill=smoker)) +
         geom_histogram(bins = 30, alpha=0.5, color='white') + 
  geom_density(alpha=0) + theme_bw()

ggplot(data=df, mapping=aes(x=log(charges), y=..density.., 
                            color=smoker, 
                            fill=smoker)) +
  geom_histogram(bins = 30, alpha=0.5, color='white') + 
  geom_density(alpha=0) + theme_bw()

install.packages("rempsyc")
library(rempsyc)

t_test_smoker <- nice_t_test(
  data = df,
  response = "charges",
  group = "smoker",
  warning = FALSE,
  var.equal = TRUE
)
t_test_smoker_table <- nice_table(t_test_smoker)
flextable::save_as_docx(t_test_smoker_table, path = "t_test_smoker.docx")

t.test(log(charges) ~ smoker, df)
wilcox.test(log(charges) ~ smoker, df)

# charges / children

ggplot(data=df, mapping=aes(y=log(charges), x=children, 
                            fill=children)) +
  geom_boxplot() + theme_bw()

library(dplyr)

plotdata <- df %>%
  group_by(children) %>%
  summarize(n=n(),
            mean = mean(charges),
            sd = sd(charges),
            ci = qt(0.975, df=n-1) * sd / sqrt(n))
ggplot(plotdata, 
       aes(x=children, y=mean, group=1)) + 
  geom_point(size=3, color='red') +
  geom_line(linetype='dashed', color='darkgrey') +
  geom_errorbar(aes(ymin = mean - ci,
                    ymax = mean + ci),
                width = .1) + theme_bw()

anova_fit <- aov(charges ~ children, df)
anova_fitlog <- aov(log(charges) ~ children, df)
install.packages("multcomp")
library(multcomp)
tuk <- glht(anova_fit, linfct=mcp(children="Tukey"))
tuklog <- glht(anova_fitlog, linfct=mcp(children="Tukey"))

summary(tuk)
summary(tuklog)

# charges / region

ggplot(data=df, mapping=aes(y=log(charges), x=region, 
                            fill=region)) +
  geom_boxplot() + theme_bw()

# charges / bmi

ggplot(data=df, mapping=aes(y=charges, x=bmi)) +
  geom_point(alpha=.5, color='red') + theme_bw()

ggplot(data=df, mapping=aes(y=charges, x=bmi, color=smoker)) +
  geom_point(alpha=.5) + geom_smooth() + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=bmi)) +
  geom_point(alpha=.5, color='red') + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=bmi, 
                            color=smoker)) +
  geom_point(alpha=.5) + geom_smooth(method="lm") + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=bmi, 
                            color=smoker, shape=sex)) +
  geom_point(alpha=.7) + theme_bw()

# charges / age

ggplot(data=df, mapping=aes(y=charges, x=age, 
                            color=smoker)) +
  geom_point(alpha=.7) + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=age, 
                            color=smoker)) +
  geom_point(alpha=.7) + geom_smooth(method='lm') + theme_bw()

ggplot(data=df, mapping=aes(y=log(charges), x=age, 
                            color=children, shape=smoker)) +
  geom_point(alpha=.7) + theme_bw()

# all numerical variables

library(car)
scatterplotMatrix(select_if(df, is.numeric), smooth=FALSE)

# sex / smoker 
library(vcd)
mytable <- xtabs(~ sex + smoker, df)
mytable
chi <-chisq.test(mytable)
chi$expected

ggplot(data=df, mapping=aes(y=bmi, x=age, color=smoker)) +
  geom_point() + theme_bw()

# bmi / smoker

ggplot(data=df, mapping=aes(x=bmi, fill=smoker)) +
  geom_histogram(alpha=0.7) + theme_bw()

cor(df_enc$bmi, df_enc$smoker_yes)

t.test(bmi ~ smoker, df)

# Делаем тест Чоу, строим модели: 

mFull <- lm(log(charges) ~ . - smoker_yes, data=df_enc)

mSmoker_yes <- lm(log(charges) ~ . - smoker_yes, data=df_enc, subset=(smoker_yes==1))
mSmoker_no <- lm(log(charges) ~ . - smoker_yes, data=df_enc, subset=(smoker_yes==0))

RSSFull <- sum(mFull$residuals^2)
RSS_smoker_yes <- sum(mSmoker_yes$residuals^2)
RSS_smoker_no<- sum(mSmoker_no$residuals^2)

k <- mFull$rank
n <- nrow(df_enc)

Fstat <- ((RSSFull - RSS_smoker_yes - RSS_smoker_no) / k)/
  ((RSS_smoker_yes + RSS_smoker_no)/(n - 2*k))
Fcrit <- qf(0.95, k, n-2*k)
p_val <- df(Fstat, k, n-2*k)

ChowTest <- data.frame(RSSFull, RSS_smoker_yes, RSS_smoker_no, n, k, 
                       Fstat, Fcrit, p_val)
write.table(ChowTest, file = "chowtest_results", sep = ",")

stargazer(mFull, mSmoker_yes, mSmoker_no,
          type = 'text', 
          title="Regression Results", 
          model.numbers = FALSE,
          column.labels = c("Full", "Smokers only",
                            "No Smokers"))


