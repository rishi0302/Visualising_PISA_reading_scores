library(ggplot2)
library(dplyr)
library(remotes)
library(countrycode)

#Read the data into dataframe
pisa_df <- read.csv('OECD_PISA.csv')

colnames(pisa_df)[1] <- "LOCATION"

#Map country codes with full names
pisa_df$Country <- countrycode(pisa_df$LOCATION, origin = "iso3c", destination = "country.name")
oecd_df <- pisa_df

#Drop unnecessary columns
drops <- c("INDICATOR","MEASURE", "FREQUENCY", "Flag.Codes", "LOCATION")
oecd_df <- oecd_df[ , !(names(oecd_df) %in% drops)]

#Filter data for year 2018 for plot 2 and also filter out the records for SUBJECT equal to TOT
oecd_df <- oecd_df %>% filter(TIME == 2018)
oecd_df <- oecd_df %>% filter(SUBJECT != 'TOT')

#Since OECD AVG is not a country so its country name is na and replace it by OECD - Average
oecd_df[["Country"]][is.na(oecd_df[["Country"]])] <- 'OECD - Average'
#Get the min values for each country
minScores <-
  oecd_df %>% group_by(Country) %>% summarise(minScore = min(Value))
#Get the max values for each country
maxScores <-
  oecd_df %>% group_by(Country) %>% summarise(maxScore = max(Value))
minScores <- minScores[rep(seq_len(nrow(minScores)), each = 2), ]
maxScores <- maxScores[rep(seq_len(nrow(maxScores)), each = 2), ]

#Add minimum and maximum score for each country as column
df <- full_join(oecd_df, minScores, by = "Country")
df <- full_join(df, maxScores, by = "Country")



# #Create ordered sequence of countries
country_label_colour <-
  data.frame("Country" = levels(reorder(df$Country, df$minScore)))
#add colour for IReland as red and OECD - Average as black and all other light grey axis text
country_label_colour <-
  country_label_colour %>% mutate(color = ifelse(Country == "Ireland", "red", ifelse(Country == "OECD - Average", "#202020", "#757575")))

#Change SUBJECT for Ireland and OECD-AVG boys and girls to differentiate them from others for colour
df <- within(df, SUBJECT[Country == 'Ireland' & SUBJECT == 'BOY'] <- 'BOY_IRELAND')
df <- within(df, SUBJECT[Country == 'Ireland' & SUBJECT == 'GIRL'] <- 'GIRL_IRELAND')

df <- within(df, SUBJECT[Country == 'OECD - Average' & SUBJECT == 'BOY'] <- 'BOY_OECD')
df <- within(df, SUBJECT[Country == 'OECD - Average' & SUBJECT == 'GIRL'] <- 'GIRL_OECD')

theme_set(theme_classic())

##########################################################################################################

plt2 <- ggplot(df, (aes(x= reorder(Country, minScore) , y=Value, colour = SUBJECT, shape = SUBJECT))) +
  geom_point(size = 2.5,stroke = 1.1) +
  labs(title="Reading performance (PISA)",
       subtitle="Boys / Girls, Mean score, 2018         Source: PISA: Programme for International Student Assessment") +
  scale_shape_manual(
    breaks = c("BOY", "GIRL"),
    labels = c("Boys", "Girls"),
    values = c(19, 19, 19, 5, 18, 18),
    name = "SUBJECT"
  ) +
  #Colour : BOY - "#4B7690" , BOY-IRL -"red", GIRL - "#4B7690" and GIRL-IRL - "red" and OECD - AVERAGE Black
  scale_color_manual(
    breaks = c("BOY", "GIRL"),
    labels = c("Boys", "Girls"),
    values = c("#4B7690", "red", "black", "#4B7690", "red", "black")
  ) +
  scale_y_continuous(limits = c(340, 560), 
                     expand = c(0, 18),
                     breaks = seq(340, 560, by = 20)) +
  
  # Draw line segment from x axis to boys score
  geom_segment(
    aes(
      x = reorder(Country, minScore),
      y = 340,
      xend = reorder(Country, minScore),
      yend = minScore - 5
    ),
    colour = "white",
    size = 0.5
  ) +
  #To draw line segment from boys to girls score
  geom_segment(
    aes(
      x = reorder(Country, minScore),
      y = minScore+4,
      xend = reorder(Country, minScore),
      yend = maxScore - 6
    ),
    colour = "#4B7690",
    size = 0.1,
    alpha = 0.8
  ) +
  expand_limits(x = -2, y =-1) +
  annotate(geom = 'segment', y = Inf, yend = Inf, color = '#6eb4d5', x = -Inf, xend = Inf, size = 1.5) +
  theme(panel.background = element_rect(fill = '#e2edf3', colour = 'white'),
        # panel.grid.major.x = element_line(size=0.02, colour = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size=0.02, colour = "white"),
        axis.text.x = element_text(
          angle = 60,
          hjust = 1,
          colour = country_label_colour$color
        ),
        axis.text.y = element_text(vjust = -0.4,margin = margin(l = 10, r = -30)),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        plot.title=element_text(size=14, hjust=0.005, face="bold", colour="#585251", vjust=-3),
        plot.subtitle=element_text(size=9, hjust=0.98, face="bold", color="#757575", vjust = 4),
        legend.position= c(0.06, -0.2),
        legend.direction = "horizontal",
        legend.text = element_text(size = 7), # legend text  was a little large
        legend.key.size = unit(0.6, "lines"),
        legend.title = element_blank())# legend keys were a little large
plt2
##########################################################################################################

#Part 3 plotting
df_final <- pisa_df %>% select(Country, SUBJECT, TIME, Value)
df_final <- df_final %>% filter(SUBJECT != 'BOY' & SUBJECT != 'GIRL')

# Filter data for top 5 performing countries 'Ireland','Canada','South Korea','Finland','Poland'
df_final <- df_final %>% filter(Country %in% c('Ireland','Canada','South Korea','Finland','Poland'))


plt3 <- ggplot(df_final, aes(TIME, Value, color = Country, fill = Country, shape = Country))+ geom_line(size = 0.5, alpha = 0.8)+
  geom_point(color = "white", size = 2) +
  labs(title="Reading Performance over last years for top 5 performing countries",
       x = 'Year',
       y = "PISA score / year") +
  
  scale_shape_manual(values = c(21, 22, 23, 24, 25),
                     name = NULL) + 
  
  scale_y_continuous(limits = c(460, 560), expand = c(0, 0)) +
  
  
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73", "#E69F00","#999999"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2018, 3), 
                     labels = factor(seq(2000, 2018, 3)), 
                     limits = c(2000,2018)) +
  scale_fill_manual(values = c("#0072b2", "#D55E00", "#009e73", "#E69F00","#999999"),
                    name = NULL) +
  
  theme_classic() +
  
  theme(legend.title.align = 0.5,
        legend.position = c(0.8, 1.005),
        legend.just = c(0, 1),
        plot.margin = margin(14, 7, 3, 1.5))
plt3
################################################
