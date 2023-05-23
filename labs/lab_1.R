# Дятлов Илья Сергеевич
# Задание 1
# для региона 27 рассчитайте урожайность пшеницы в 2011 году,
# взяв для рассчета средние суммы активных температур за предыдущие 9 лет,
# с 10 ближайших метеостанций на расстоянии до 150 км

# загружаем библиотеки
library(tidyverse)
library(rnoaa)
library(lubridate)

# получаем список всех станций
station_data = ghcnd_stations()
print(station_data)

# получаем список ближайших к столице региона станций
habarovsk = data.frame(id = "HABAROVSK", latitude = 48.483333,  longitude = 135.066667)
habarovsk_around = meteo_nearby_stations(lat_lon_df = habarovsk,
                                         station_data = station_data,
                                         limit = 10,
                                         var = c("PRCP", "TAVG"), # какие данные хотим получить: prcp - осадки, tavg - средняя температура воздуха
                                         year_min = "2002-01-01",
                                         year_max = "2011-12-31",
                                         radius = 150)
print(habarovsk_around)

# получим и запишем в вектор id полученных выше станций
nearby_stations_ids <- c()  # создаём пустой вектор
for (i in habarovsk_around){
  nearby_stations_ids <- append(nearby_stations_ids, i[['id']])  # добавляем в цикле id каждой станции
}

print(nearby_stations_ids)

# получаем средние температуры по каждому дню для каждой станции
all_habarovsk_data = meteo_tidy_ghcnd(stationid = nearby_stations_ids, 
                                      keep_flags = FALSE,   # флаги, которые дают дополнительную информацию (как проводились исследования, точность, источник). Они нам не нужны, поэтому FALSE
                                      var = c("TAVG"),  # какие данные хотим получить: tavg - средняя температура воздуха
                                      date_min = "2002-01-01",
                                      date_max = "2010-12-31"
                                      )

# т. к. данные в десятых долях единицы, то разделим на 10, чтобы получить значения в целых единицах
all_habarovsk_data['tavg'] = all_habarovsk_data['tavg'] / 10
print(all_habarovsk_data, n=50)

# смотрим, сколько у нас изначально NA-значений в столбце "tavg"
print(sum(is.na(all_habarovsk_data['tavg'])))

# сохраняем в переменную агрегированные по дате данные с подсчётом медианы для выборки одного и того же дня всех лет
# параметр na.rm=TRUE нужен, чтобы функция median не вернула NA
aggregated_data = aggregate(x = all_habarovsk_data[c("tavg")], by = all_habarovsk_data[c("date")], FUN = median, na.rm=TRUE)
print(aggregated_data, max=10)

# пройдёмся циклом по данным. Там, где в столбце 'tavg' стоит NA, поменяем его на медиану для этой даты из датафрейма aggregated_data
for(i in 1:nrow(all_habarovsk_data)) {
  row <- all_habarovsk_data[i,]
  if (is.na(row['tavg'])) {
    all_habarovsk_data[i,]['tavg'] <- aggregated_data[aggregated_data$date == row['date'], 'tavg']  # здесь обязательно писать all_habarovsk_data[i,]['tavg'], а не row['tavg']!!!
  }
}

# проверяем, что NA-значений в столбце "tavg" после преобразований не осталось
print(sum(is.na(all_habarovsk_data['tavg'])))


# создаём таблицу со справочными данными:
afi <- c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi <- c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di <- c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

help_data <- data.frame(afi = afi, bfi = bfi, di = di)
print(help_data)

# указываем остальные данные из задания
y = 1.0
kf = 300
qj = 1600
lj = 2.2
ej = 25



# ищем урожайность:


# сгруппируем данные по станциям и посчитаем сумму температур > 5 в каждом месяце
grouped1 = all_habarovsk_data %>% 
  group_by(id, month = lubridate::floor_date(date, 'month')) %>%
  summarize(tavg = sum(tavg[tavg > 5]))
print(grouped1)

# найдём среднее арифметическое сумм средних температур > 5 по месяцам каждого года среди всех станций
grouped2 = grouped1 %>% 
  group_by(month = lubridate::floor_date(month, 'month')) %>%
  summarize(tavg = mean(tavg))
print(grouped2)

# найдём среднее арифметическое сумм средних температур > 5 по месяцам всех лет

grouped3 <- grouped2 %>% 
  separate(month, c("Year","Month")) %>% 
  group_by(Month) %>% 
  summarise(mean_tavg = mean(tavg))
print(grouped3)

summa <- 0  # это переменная, в которой будет накапливаться сумма
for (i in 1:12) {
  s <- as.numeric(grouped3[i,'mean_tavg'])  # as.numeric() необходимо для того, чтобы привести к типу "число"
  summa <- summa + (help_data[i, 'afi'] + help_data[i, 'bfi'] * y * s) * help_data[i, 'di'] * kf / (qj * lj * (100 - ej))
}
print(summa)

# результат: 16.90768 ц/га



  