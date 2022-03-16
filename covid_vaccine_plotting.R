library(tidyverse)
library(lubridate)
library(geomtextpath)
library(gridExtra)

#read the file
df <- read.csv('https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_global.csv')

country <- c('Guyana', 'Bhutan', 'Comoros', 'Fiji', 'Luxembourg')

df_filtered <- df %>% 
  filter(Country_Region %in% country) %>%
  mutate(Date = as.Date(Date)) %>% 
  filter(Date <= '2021-12-31') %>% 
  mutate(month = format(Date, '%m'),
         month1= format (Date, '%B'),
         dose = Doses_admin/1000)


df_plot <- df_filtered %>% 
  group_by(Country_Region, month1) %>% 
  slice_max(Date, n=1)


# create table and themes
table <- data.frame(Country=country,
                    Population = c('786,552', '771,608', '869,601', '896,445', '625,978'))

table <- table %>% 
  arrange(Country)

tt <- ttheme_default(core=list(bg_params=list(fill='black'),
                               fg_params=list(col='white')),
                     colhead=list(bg_params=list(fill='black'),
                                  fg_params=list(col='white')),
                     rowhead = list(fg_params=list(col='white')))




#plotting

df_plot %>%
  ggplot(aes(as.numeric(month), dose, colour=Country_Region))+
  geom_textline(aes(label=Country_Region), hjust=.0001, size=6, linewidth=1.5)+
  geom_text(data = subset(df_plot, month=='12'),
                       aes(x=12, y=dose, label=Country_Region), size=6, hjust=-.05)+
  scale_x_continuous(limits = c(1,13), breaks = seq(1,12,1), labels = month.abb )+
  scale_y_continuous(breaks = seq(0,1400, 200))+
  labs (title = 'Total Dosage of COVID-19 Vaccine Administered in 2021',
        y = 'Total Dosage (in thousand)',
        x='',
        caption = 'Source: John Hopkins University')+
  theme(legend.position = 'none',
        plot.background = element_rect(fill='black'),
        panel.background = element_rect(fill='black'),
        panel.grid = element_blank(),
        axis.title = element_text(size = 18, colour = 'white'),
        axis.text = element_text(size = 16, colour = 'red'),
        plot.title = element_text(size=24, colour = 'white'),
        plot.caption = element_text(size = 13, colour = 'white', face='italic'),
        axis.title.y = element_text(margin = margin(t = 0, r = 25, b = 0, l = 0)),
        axis.line = element_line(colour='red'),
        axis.ticks = element_line(colour='red'))+
  annotation_custom(tableGrob(table, theme=tt), xmin = .5, xmax=5, ymin=750, ymax = 1000)


