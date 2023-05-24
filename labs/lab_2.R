# Дятлов Илья Сергеевич
# Задание 2
# создайте модель множественной линейной регрессии дневных потоков углекислого газа
# за весенний период 2013 года по данным измерений методом турбулентной пульсации

library(dplyr)
library(tidyverse)
library(ellipse)
library(lubridate)
library(car)
library(caret)

# сначала откроем данные в Экселе и посмотрим на их организацию:
# первая строчка данных содержит не названия столбцов, а комментарии к ним => чтобы пропустить эту строчку при считывании данных, используем параметр skip
# вторая строчка (после удаления первой - первая) строчного типа, из-за этого все данные так же строчного типа. Игнорируем вторую строчку => используем параметр comment со значением символа, встречающегося в этой строке, например, "[" вместо удаления этой строки
# после изменения этой строки в ней во всех столбцах будут NA => используем параметр na, чтобы заменить это значение
# после этого можно убрать первую строку => [-1,]
df = read.csv("eddypro.csv",
              skip=1,
              na=c("","NA","-9999","-9999.0"),
              comment=c("["))[-1,]

# выведем полученные данные
df

# выведем информацию о каждой колонке
glimpse(df)

# при рассмотрении выведенных данных можно заметить, что есть колонка "roll", в которой видимые выведенные значения - NA. Проверим, что они все NA
print(nrow(df) == sum(is.na(df['roll'])))

# результат TRUE, это значит, что в колонке roll действительно во всех строках стоит NA. Значит, её можно убрать
df = select(df, -(roll))

# меняем формат даты
df$date <- as.Date(df$date, format="%Y-%m-%d")

# добавляем дополнительные колонки с годом и месяцем замера
df = mutate(df, month = months(df$date))
df = mutate(df, year = year(df$date))

# так как в таблице строковые значения в колонках повторяются, возможно, имеет смысл изменить тип этих колонок на категориальные (факторы)
# df = df %>% mutate_if(is.character, factor)
# впоследствии убрано, т. к. иначе работа с датой производится некорректно 

# заменим все символы, которые могут быть интерпретированы как операции
names(df) = names(df) %>% 
  str_replace_all("[!]","_emph_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(df)

# оставляем только дневные измерения
df = filter(df, daytime %in% TRUE) # дневные потоки

# оставляем необходимый год
df = filter(df, year %in% 2013) # год

# оставляем необходимые месяцы
df = filter(df, month %in% c("Март","Апрель","Май")) # весенний период
# проверяем, какие месяцы остались
unique(df$month)

# удаляем пустые значения
df = drop_na(df)

# т. к. cor() работает только с численными функциями, выберем все переменные типа numeric
df_numeric = df[,sapply(df, is.numeric)]

# считаем корреляцию, преобразуем матрицу в таблицу и выбираем интересующий нас столбец
cor_df = cor(drop_na(df_numeric)) %>% as.data.frame %>% select(co2_flux)
cor_df

# оставляем столбец имён с эмиссией CO2 и значениями > 0.1 (для первой модели)
# co2_flux^2 означает, что в модели нужно учесть все взаимодействия первого порядка(с одной переменной)
# и все взаимодействия второго порядка (взаимодействия переменных между собой)
varsCor1 = row.names(cor_df)[cor_df$co2_flux^2 > .1] %>% na.exclude
varsCor1

# оставляем столбец имён с эмиссией CO2 и со всеми значениями (для второй модели)
varsCor2 = row.names(cor_df)
varsCor2

# запись параметров с кореляцией > 0.1 для первой модели в виде формулы в формате y~x1+x2+...
# sep - разделитель объектов в строке
# collapse - разделитель результатов
formula1 = as.formula(paste("co2_flux~", paste(varsCor1, collapse = "+"), sep=""))
formula1

# запись всех параметров для второй модели в виде формулы
formula2 = as.formula(paste("co2_flux~", paste(varsCor2, collapse = "+"), sep=""))
formula2



# далее строим модели:



# первая модель - с параметрами с кореляцией > 0.1:


# создаём (обучаем) модель 1
model_1 = lm(formula=formula1, data=df_numeric) 
summary(model_1)

# проверим, есть ли сильно коррелирующие друг с другом переменные,
# т. к. это может вызвать проблемы при подгонке и интерпретации регрессионной модели
alias(lm(formula=formula1, data=df_numeric))

# выведем линейно зависимые переменные
# complete и dimnames - встроенные свойства у объекта-результата работы метода alias
ld.vars <- attributes(alias(model_1)$Complete)$dimnames[[1]]
ld.vars

# удалим линейно зависимые переменные из формулы
formula1_new <- as.formula(
  paste(
    paste(deparse(formula1), collapse=""), 
    paste(ld.vars, collapse="-"),
    sep="-"
  )
)
formula1_new

# пересоздаём модель
model_1 <- lm(formula1_new, data=df_numeric)

# оцениваем мультиколлинеарность (vif = Variance Inflation Factors, коэффициент инфляции дисперсии)
vif_values <- car::vif(model_1)
vif_values = as.matrix(vif_values)
vif_values

# определяем коэффицент детерминации
summary(model_1)$r.squared

# оцениваем значимость переменных
anova_results <- car::Anova(model_1, type="III")
summary(anova_results)

# оцениваем важность переменных
varImp_values <- caret::varImp(model_1)
print(varImp_values)

# выводим результаты модели
summary(model_1)

# оцениваем значимость переменных
anova(model_1)

# строим графики для визуализации результатов (нужно вводить Enter в консоли/Run, чтобы появлялся каждый следующий график)
plot(model_1)

coef(model_1) # коэффициенты модели



# вторая модель со всеми параметрами:


# создаём (обучаем) модель 2
model_2 = lm(formula=formula2, data=df_numeric) 
summary(model_2)

# проверим, есть ли сильно коррелирующие друг с другом переменные
alias(lm(formula=formula2, data=df_numeric))

# выведем линейно зависимые переменные
ld2.vars <- attributes(alias(model_2)$Complete)$dimnames[[1]]
ld2.vars

# удалим линейно зависимые переменные из формулы
formula2_new <- as.formula(
  paste(
    paste(deparse(formula2), collapse=""), 
    paste(ld2.vars, collapse="-"),
    sep="-"
  )
)
formula2_new

# пересоздаём модель
model_2 <- lm(formula2_new, data=df_numeric)

# оцениваем мультиколлинеарность (vif = Variance Inflation Factors, коэффициент инфляции дисперсии)
vif_values <- car::vif(model_2)
vif_values = as.matrix(vif_values)
vif_values

# определяем коэффицент детерминации
summary(model_2)$r.squared

# оцениваем значимость переменных
anova_results2 <- car::Anova(model_2, type="III")
summary(anova_results2)

# оцениваем важность переменных
varImp_values2 <- caret::varImp(model_2)
print(varImp_values2)

# выводим результаты модели
summary(model_2)

# оцениваем значимость переменных
anova(model_2)

# строим графики для визуализации результатов (нужно вводить Enter в консоли/Run, чтобы появлялся каждый следующий график)
plot(model_2)

coef(model_2) # коэффициенты модели



# сравним модели

anova(model_1, model_2)

