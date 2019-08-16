library(colorspace)
library(readr)
library(RColorBrewer)
library(tidyverse)
options(scipen=999)



### Quarter comparisons for Contract Obligations - Civilian/Defense Breakout####


setwd("x:/1 Marielle Folder/Visualizations/Government-Wide")
data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Civilian and Defense Data by quarter.csv")

#data<-rename(data, civ_def = "Civ/Def", total_obligations = "Contract Obligations (in Billions)")

data$Year = as.character(data$Year)

# data.civdef <- data %>%
#   rename(civ_def = "Civ/Def",
#          total_obligations = "Contract Obligations (in Billions)") %>%
#   #filter(Year!=2019) %>%
#   filter(Year == 2014|Year ==2015| Year == 2016 | Year == 2017 | Year == 2018) %>%
#   group_by(Year, civ_def) %>%
#   mutate(label_y = cumsum(total_obligations))
#
# data.civdef$Year = as.character(data.civdef$Year)

data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(civ_def == "Defense") %>%             ## if only civ or def
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  #filter(civ_def == "Defense") %>%              ## if only civ or def
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year == 2016 | Year == 2017 | Year == 2018 | (Year == 2019 #& `civ_def`== "Civilian" ###if first quarter
                                                       )) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

#display.brewer.all()

data.civdef$civ_def <- c("Defense - Q2", "Civilian - Q3")  ### Change based on quarter

plotyr <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
 geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
 geom_text(data = subset(data.civdef, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
 #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
# geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
                         #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
 #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
 # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
                                      #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
 facet_grid(~civ_def, labeller = label_wrap_gen(20), scales = "free")+
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)", 
title = "Contract Obligations Comparison FY16-FY19",    ####PSC Analysis: for dist
        subtitle = NULL
        #, caption = "Data Source: Bloomberg Government"
        ) +                                                                                                     ##Change Depending on Year/Q
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 9, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))



ggsave("Contract Obligations by Quarter- FY16-FY19Q3 for dist.jpg", plotyr,                  ###Change name 
       width = 15, height = 7, units = "in")
#########################
##Quarter by Quarter seperate slides####
##Defense####
data.civdef_def <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(civ_def == "Defense") %>%             ## if only civ or def
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_def$Year = as.character(data.civdef_def$Year)

data.def <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  filter(civ_def == "Defense") %>%              ## if only civ or def
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year == 2016 | Year == 2017 | Year == 2018 | (Year == 2019 #& `civ_def`== "Civilian" ###if first quarter
  )) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

#display.brewer.all()

data.def$civ_def <- c("Defense - Q2")  ### Change based on quarter

plotdef <- ggplot(data.def, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.def, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
  # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
  #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
  #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
  # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
  #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
  facet_grid(~civ_def, labeller = label_wrap_gen(20), scales = "free")+
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)", 
       title = "Contract Obligations Comparison FY16-FY19",    ####PSC Analysis: for dist
       subtitle = NULL
       , caption = "Data Source: Bloomberg Government"
  ) +                                                                                                     ##Change Depending on Year/Q
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 9, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))


##Civilian####

data.civdef_civ <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(civ_def == "Civilian") %>%             ## if only civ or def
  #filter(Year!=2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_civ$Year = as.character(data.civdef_civ$Year)

data.civ <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  #filter(Year!=2019) %>%
  filter(civ_def == "Civilian") %>%              ## if only civ or def
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  filter(Year == 2016 | Year == 2017 | Year == 2018 | (Year == 2019 #& `civ_def`== "Civilian" ###if first quarter
  )) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))

#display.brewer.all()

data.civ$civ_def <- c("Civilian - Q3")  ### Change based on quarter

plotciv <- ggplot(data.civ, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civ, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
  # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
  #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
  #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
  # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
  #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(1,3,5,7)])+
  facet_grid(~civ_def, labeller = label_wrap_gen(20), scales = "free")+
  labs(x="Fiscal Year", y = "Contract Obligations (in Billions)", 
       title = "Contract Obligations Comparison FY16-FY19",    ####PSC Analysis: for dist
       subtitle = NULL
       , caption = "Data Source: Bloomberg Government"
  ) +                                                                                                     ##Change Depending on Year/Q
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 9, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))


ggsave("Defense Contract Obligations by Quarter- FY16-FY19.jpg", plotdef,                  ###Change name 
       width = 15, height = 8, units = "in")

ggsave("Civilian Contract Obligations by Quarter- FY16-FY19.jpg", plotciv,                  ###Change name 
       width = 15, height = 8, units = "in")

####################################################################################################

####Total contract spending - services categories + products####

## Government-Wide Total Contract Spending####

data <- read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/DPAP (services and total) Data - Government Wide.csv")


setwd("X:/1 Marielle Folder/Visualizations/Government-Wide")

data$`DPAP Category` <- factor(data$`DPAP Category`,
                               levels = c("Products", "Construction Services",
                                          "Electronic & Communication Services",
                                          "Equipment Related Services",
                                          "Facility Related Services",
                                          "Knowledge Based Services",
                                          "Logistics Management Services",
                                          "Medical Services",
                                          "Research and Development",
                                          "Transportation Services"),
                               ordered = is.ordered(data$`DPAP Category`))


##Government-Wide data - process##

DPAP_all <- data %>%
  filter(`DPAP Category` != "Total") %>% 
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$_billions_all`))


label_height_all <- DPAP_all %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$_billions_all`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_all, by = c("Fiscal Year", "pors", "pors$") )


##Defense-Wide data - process##

DPAP_dod <- data %>%
  filter(`DPAP Category` != "Total") %>% 
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$_billions_DoD`))


label_height_dod <- DPAP_dod %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$_billions_DoD`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_dod, by = c("Fiscal Year", "pors", "pors$") )

##Civilian-Wide data - process##

DPAP_civ <- data %>%
  filter(`DPAP Category` != "Total") %>% 
  group_by(`Fiscal Year`) %>%
  arrange(desc(`DPAP Category`)) %>%
  #mutate(label_y = cumsum(`$ billions`))
  mutate(pors = ifelse(`DPAP Category`=="Products","Product","Service")) %>%
  group_by(`Fiscal Year`, pors) %>%
  mutate(`pors$` = sum(`$_billions_Civilian`))


label_height_civ <- DPAP_civ %>%
  group_by(`Fiscal Year`, pors) %>%
  summarize(`pors$` = sum(`$_billions_Civilian`)) %>%
  group_by(`Fiscal Year`) %>%
  arrange(desc(`pors`)) %>%
  mutate(label_y2 = cumsum(`pors$`)) %>%
  left_join(DPAP_civ, by = c("Fiscal Year", "pors", "pors$") )


##Government-Wide data - plot##

plot_all <- ggplot(label_height_all, aes(x = `Fiscal Year`, y = `$_billions_all`,
                                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_all, `DPAP Category` %in% c("Medical Services", "Products")), aes(x=`Fiscal Year`, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = paste("Government-Wide", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 

##Defense-Wide data - plot##

plot_dod <- ggplot(label_height_dod, aes(x = `Fiscal Year`, y = `$_billions_DoD`,
                                              fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_dod, `DPAP Category` %in% c("Medical Services", "Products")), aes(x=`Fiscal Year`, y=`pors$`), colour = "black", fill = NA, stat = "identity"
           )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = paste("DoD", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank())

##Civilian-Wide data - plot##

plot_civ <- ggplot(label_height_civ, aes(x = `Fiscal Year`, y = `$_billions_Civilian`,
                                         fill = `DPAP Category`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(x = `Fiscal Year`, label = round(`pors$`, digits = 2), y = label_y2), size = 4, vjust = 1.5, check_overlap = TRUE)+
  geom_bar(data = subset(label_height_civ, `DPAP Category` %in% c("Medical Services", "Products")), aes(x=`Fiscal Year`, y=`pors$`), colour = "black", fill = NA, stat = "identity"
  )+
  scale_fill_brewer(name = "Services/Products Contract Category", palette = "Set3") +
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions",
       title = paste("Civilian", " Total Contract Spending", sep = ""))+
  theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"), axis.ticks.x = element_blank()) 


###Change to relevent year span when saving

ggsave(paste("Civilian", " Total Contract Spending Service Product ", "FY09-FY18.jpg", sep = ""), plot_civ,
       width = 13, height = 6.5, units = "in")

ggsave(paste("Defense", " Total Contract Spending Service Product ", "FY09-FY18.jpg", sep = ""), plot_dod,
       width = 13, height = 6.5, units = "in")

ggsave(paste("Government-Wide", " Total Contract Spending Service Product ", "FY09-FY18.jpg", sep = ""), plot_all,
       width = 13, height = 6.5, units = "in")


