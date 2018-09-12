#library(maptools)
library(sp) 
library(RColorBrewer)
library(raster)
#library(broom)
library(ggplot2)

# Подготовка карты для рисования

# Подгружаем Карту России

rus <- getData('GADM', country='RUS', level=1)
rus <- fortify(rus)
rus$long[rus$long<0] <- rus$long[rus$long<0] + 360


# т.к. Крым и Севастополь не прикреплены к карте, вытаскиваем их из карты Украины

ukr <- getData('GADM', country='UKR', level=1)
ukr <- fortify(ukr)
ukr <- ukr[ukr$id==4 |ukr$id==20,]
ukr$id <- as.numeric(ukr$id)+100
ukr$group <- as.factor(as.numeric(ukr$group)+100)

# несколько регионов изначально в искаженном формате. Например, Екатеринбург
# отмечен как отдельный регион. Исправляем это, присоединив все фрагменты к области

rus$id <- as.numeric(rus$id)
sver <- rus[rus$group=='67.2' |rus$group== '68.1',]
rus <- rus[rus$id!=67 & rus$group!='68.2'& rus$group!='68.3',]
sver <- sver[sver$id!=66,]# & sver$group!='68.2'& sver$group!='68.3',]
sver[sver$id == 67,]$id <- 68
sver[sver$group == '67.2',]$group <- '68.1'
sver <- sver[!duplicated(sver[c('lat', 'long')]),]

# Также Москва представлена в старых границах. За немиением четких геоданных,
# скорректируем форму Москвы примерно, расширив в сторону Калужской области

mos <- rus[rus$group == '43.1' | rus$group == '44.1' | rus$group=='44.2',]
rus <- rus[rus$id != 43 & rus$id != 44,]
levels(mos$group) <- c(levels(mos$group), '43', '44')
levels(rus$group) <- c(levels(rus$group), '43', '44')

mos[mos$id==43,]$group <- '43'
mos[mos$id==44,]$group <- '44'

mos[mos$long > 37 & mos$long < 37.3 & mos$lat < 55.3 & mos$lat > 54.7,]$group <- '43'
mos[mos$long > 37 & mos$long < 37.3 & mos$lat < 55.3 & mos$lat > 54.7,]$id <- 43
mos <- mos[mos$long < 37.3 | mos$long > 37.65 | mos$lat<55.4 | mos$lat>55.66,]

moscow <- mos[mos$id == 43,]
mos <- mos[mos$id == 44,]

# создаем примерную  границу Москвы от МКАД до Калужской области

left <- approx(x=c(37.00029, 37.44089), y=c(55.18350, 55.66026), n = 500)
right <- approx(c(37.65645, 37.29979), c(55.55833, 54.81003), n = 500)

# собираем по частям границы Москвы

moscow <- rbind(moscow[moscow$order <530 & moscow$order>0,],
                data.frame(long = right$x, lat = right$y, order = rep(0, 500),
                           hole = rep(0, 500), piece= rep(0, 500), id= rep(43, 500), group= rep('43', 500)),
                moscow[moscow$order > 2481,],
                data.frame(long = left$x, lat = left$y, order = rep(0, 500),
                           hole = rep(0, 500), piece= rep(0, 500), id= rep(43, 500), group= rep('43', 500)),
                moscow[moscow$order >530 & moscow$order<2481,])

#собираем по частям границы Московской области

mos <- rbind(mos[mos$order<2824,],
             data.frame(long = rev(right$x), lat = rev(right$y), order = rep(0, 500),
                        hole = rep(0, 500), piece= rep(0, 500), id= rep(44, 500), group= rep('44', 500)),
             mos[mos$order>6842,],
             mos[mos$order>4895 & mos$order<5967,],
             data.frame(long = rev(left$x), lat = rev(left$y), order = rep(0, 500),
                        hole = rep(0, 500), piece= rep(0, 500), id= rep(44, 500), group= rep('44', 500)),
             mos[mos$order>2823 & mos$order<4896,])


# Соединяем все фрагменты карты

rus <- rbind(rus, sver)
rus <- rbind(rus, ukr)
rus <- rbind(rus, mos)
rus <- rbind(rus, moscow)

remove(sver, ukr, mos, moscow, left, right)


# Можем рисовать
library(dplyr)

# Данные

data <- read.csv('final.csv', row.names = 1)

# считаем количество трудоспособных граждан на одного пенсионера
data$penspw <- data$working/(data$num_all*1000)


map <- left_join(rus, subset(data, year == 2017) %>% select(id, pens_total, penspw, pens2livmin))

# подгружаем шрифты
library(extrafont)
loadfonts(device = 'pdf')

# Карты со средними начисленными пенсиями в 2017 году

pens <- ggplot(map) +
  theme_minimal() +
  geom_polygon(aes(x = long, y = lat, group=group, fill = pens_total), alpha = 1, size = .05) +
  coord_map("azequalarea") +
  scale_fill_distiller(name = "В рублях", type = "seq",
                       palette = "YlGn", direction = 1)+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text=element_text(family = "CMU Serif", size=14)) 
ggsave('pens.png', pens, width = 11, height = 7)

#  карта с отношением пенсий к прожиточному минимум пенсионера
pens.2.livmin <- ggplot(map) +
  theme_minimal() +
  geom_polygon(aes(x = long, y = lat, group=group, fill = pens2livmin), alpha = 1, size = .05) +
  coord_map("azequalarea") +
  scale_fill_distiller(name = "В %", type = "seq",
                       palette = "BuPu", direction = 1) +
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text=element_text(family = "CMU Serif", size=14)) 

ggsave('pens2livmin.png', pens.2.livmin, width = 11, height = 7)



# подсчитываем инфляцию, чтобы вычислить размер реальных пенсий

data$infl <- data$cpi/100
data[data$year<2003,]$infl <- 1

data[data$year==2016,]$pens_total <- data[data$year==2016,]$pens_total-5000

data[data$grppc == 0 & !is.na(data$grppc),]$grppc <- NA

# считаем реальные пенсии и рост пенсий
data <- data %>% group_by(region) %>%
  mutate(cuminfl = cumprod(infl),
         pens_total_real = pens_total/cuminfl,
         pens_growth = (pens_total_real - lag(pens_total_real))/lag(pens_total_real))


# график трендов реальных пенсий по ФО и России
h <- ggplot(subset(data, year > 2001 & unit_type < 1 & !is.na(pens_total_real)),
       aes(x=year, y=pens_total_real, color=region))+
  geom_point()+
  geom_line()+
  theme_minimal()+
  scale_color_brewer(palette='Set2') +
  theme(text=element_text(family = "CMU Serif", size=14), legend.title = element_blank())+
  labs(x=NULL, y=NULL)
ggsave('trends.png', h, width = 11, height = 7)


#считаем рост реальныз пенсий с 2010 года

map <- left_join(map, data %>% filter(year == 2010 | year == 2017) %>% group_by(region) %>% 
  mutate(pens_7growth = (pens_total_real - lag(pens_total_real))/lag(pens_total_real)) %>%
  filter(year==2017) %>% select(id, pens_7growth))


#карта роста за 7 лет
growth <- ggplot(map) +
  theme_minimal() +
  geom_polygon(aes(x = long, y = lat, group=group, fill = pens_7growth*100), alpha = 1, size = .05) +
  coord_map("azequalarea") +
  scale_fill_distiller(name = "В %", type = "seq",
                       palette = "Blues", direction = 1)+
  theme(legend.position = "right",
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        text=element_text(family = "CMU Serif", size=14)) 
ggsave('growth.png', growth, width = 11, height = 7)


# график изменения количества работоспособных на одного пенсионера

d <- ggplot(data %>% filter(year %in% c(2007, 2012, 2017)), aes(x = factor(year), y=penspw)) +
  geom_bar(stat = "summary", fun.y = "mean", na.rm = TRUE, fill = 'paleturquoise2')+
  stat_summary(aes(label=round(..y..,2), family = "CMU Serif"),  fun.y=mean, geom="text", size=6,
                vjust = -0.5)+
  theme_minimal()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x=element_blank(),
        text=element_text(family = "CMU Serif", size=14), )
ggsave('worcperpens.png', d, width = 11, height = 7)


data <- data %>% group_by(region) %>%
  mutate(grppc_real = grppc/cuminfl,
         grppc_growth = (grppc_real - lag(grppc_real))/lag(grppc_real),
         grppc_growth_l1 = lag(grppc_growth,1))


