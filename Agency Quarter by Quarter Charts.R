library(tidyverse)
library(RColorBrewer)
options(scipen=999)

#Location for saving charts
setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts") #location charts are saved

#### One Chart####
Agency <- "DOJ"
Year <- "FY17-19Q3"
dis_year <- "FY17-19Q3" ###displayed year (in case different from file name)
##SubAgency <- "National Oceanic and Atmospheric Administration (NOAA)"

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", Agency," ", Year,".csv", sep = ""))

data.organized <- data %>% 
  rename("transaction_value" = `Transaction Value`,
         "fiscal_quarter" = `Fiscal Quarter`) %>% 
 # filter(`Funding Bureau` == SubAgency)  %>%                                         #### if subset of Department/Agency 
 # filter(`Funding Office Level 3` == SubAgency) %>%                                         #### if subset of Department/Agency
  filter(`Fiscal Year` %in% c(2017:2019)) %>%                               ###date range for chart
  select(transaction_value, fiscal_quarter) %>% 
  group_by(fiscal_quarter) %>%
  summarise(sum = sum(transaction_value)) %>% 
  separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>% 
  mutate(total_obligations = round((sum)/1000000000, digits=2)) %>%           ###division of $$
  group_by(FY) %>% 
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations))%>% 
  mutate(FYYear = paste("FY", FY, sep = "")) %>% 
  mutate(Q_quarter = paste("Q", quarter, sep =""))



plot <- 
  ggplot(data.organized, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.organized, FY != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  #facet_grid(~agency_comp, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
       title = paste(Agency, " Contract Obligations Comparison - ", dis_year, sep = ""),                   ##Change based on Agency and year/quarter
       caption = "Data Source: Bloomberg Government"
 #      ,subtitle = SubAgency                   ##if subsection of department
       ) +                                     ##Can change or remove caption
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))

plot

ggsave(filename = paste(Agency,
                        #" ", SubAgency,
                        " Contract Obligations ", dis_year, " by quarter.jpg", sep = ""), plot,          ##Change Based on Agency and year/quarter
       width = 13, height = 6.5, units = "in")


####Multiple Charts ####

Agency<- as.list(c("DHS", "DOE","DOS","HHS","USAID", "VA"))
Year <- "FY17-19Q3"

data_list<- lapply(Agency, function(x){
  data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", x," ", Year,".csv", sep = ""), col_types = cols("NAICS Code" = col_character()))

  data_list <-data %>%
    mutate(id = x)
  })

data.organized<- lapply(data_list, function(x){
  data.organized <- data %>%
    filter(`Funding Bureau` == x) %>% 
    rename("transaction_value" = `Transaction Value`,
           "fiscal_quarter" = `Fiscal Quarter`) %>%
    filter(`Fiscal Year` %in% c(2017:2019)) %>%                               ###date range for chart
    select(transaction_value, fiscal_quarter, id) %>%
    group_by(fiscal_quarter, id) %>%
    summarise(sum = sum(transaction_value)) %>%
    separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>%
    mutate(total_obligations = round((sum)/1000000000, digits=2)) %>%           ###division of $$
    group_by(FY) %>%
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations))%>%
    mutate(FYYear = paste("FY", FY, sep = "")) %>%
    mutate(Q_quarter = paste("Q", quarter, sep =""))
  
})

number <-as.list(1:6)

plot<- lapply(data.organized, function(a){
  plot <-
    ggplot(a, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(a, FY != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    #facet_grid(~agency_comp, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
         title = paste(a$id[1], " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
         caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          plot.caption = element_text(size = 8, face = "italic"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20),
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
  
  
  
})

matrix <- matrix(c(Agency, 1:6), nrow=6, ncol=2)
matrix

lapply(matrix, function(x) {
  for (i in Agency){
    ggsave(filename = paste(i," Contract Obligations ", Year, " by quarter.jpg", sep = ""), plot=plot[[x]],
           width = 13, height = 6.5, units = "in")
    
  } 
  
  
})

# func <-  function(a,b) {
#   ggsave(filename = paste(a," Contract Obligations ", Year, " by quarter.jpg", sep = ""), plot=plot[[b]],
#          width = 13, height = 6.5, units = "in")
# }
# 
# 
# mapply(func, Agency, 1:6)



###Subagency Charts (all as list)####

Agency <- "DOJ"
Year <- "FY17-19Q3"
dis_year <- "FY17-19Q3" ###displayed year (in case different from file name)

data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", Agency," ", Year,".csv", sep = ""))

Subagency <- as.list(unique(data$`Funding Bureau`))

data.organized<- lapply(Subagency, function(x){
  data.organized <- data %>%
    filter(`Funding Bureau` == x) %>% 
    rename("transaction_value" = `Transaction Value`,
           "fiscal_quarter" = `Fiscal Quarter`) %>%
    filter(`Fiscal Year` %in% c(2017:2019)) %>%                               ###date range for chart
    select(transaction_value, fiscal_quarter, `Funding Bureau`) %>%
    group_by(fiscal_quarter, `Funding Bureau`) %>%
    summarise(sum = sum(transaction_value)) %>% 
    separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>%
    mutate(total_obligations = round((sum)/1000000000, digits=2)) %>%           ###division of $$
    group_by(FY, `Funding Bureau`) %>%
    mutate(label_y = cumsum(total_obligations),
           prop = 100*total_obligations/sum(total_obligations))%>%
    mutate(FYYear = paste("FY", FY, sep = "")) %>%
    mutate(Q_quarter = paste("Q", quarter, sep =""))
  
})

plot<- lapply(data.organized, function(a){
  plot <-
    ggplot(a, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(a, FY != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    #facet_grid(~agency_comp, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
         title = paste(Agency, " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
         subtitle = a$`Funding Bureau`[1],
         caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          plot.caption = element_text(size = 8, face = "italic"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20),
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
})


lapply(1:length(Subagency), function(x) {
  ggsave(filename = paste(Agency, " - ", Subagency[[x]]," Contract Obligations", dis_year, " by quarter.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 6.5, units = "in")
})

#####DoD####


data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Civilian and Defense Data by quarter.csv")

Agency <- "DoD"
Year <- "FY17-19Q2"
data$Year = as.character(data$Year)


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
  filter(Year %in% c(2017:2019)) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))


plotdef <- ggplot(data.def, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.def, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 4, fontface = "bold")+   ####Adds total to top
  #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
  # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
  #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
  #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
  # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
  #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
       title = paste(Agency, " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
       caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption                                                                                                    ##Change Depending on Year/Q
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))





ggsave(filename = paste(Agency," Contract Obligations ", Year, "by quarter.jpg", sep = ""), plotdef,          ##Change Based on Agency and year/quarter
       width = 13, height = 6.5, units = "in")
##############################################################################
###Services vs Products COmparison quarter by Quarter#####

###By Agency####

#### One Chart####
Agency <- "USAID"
Year <- "FY17-19Q3"


data <- read_csv(paste("X:/1 Marielle Folder/Data Sets/By Agency/Quarter by Quarter/", Agency," ", Year,".csv", sep = ""))

dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")
setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts/Product-Service")

data.organized <- data %>% 
  rename("PSC" = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         "transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year",
         "fiscal_quarter" = `Fiscal Quarter`) %>% 
  # filter(`Funding Bureau` == SubAgency)  %>%                                         #### if subset of Department/Agency 
  # filter(`Funding Office Level 6` == SubAgency) %>%                                         #### if subset of Department/Agency
  select(PSC, transaction_value, fiscal_quarter, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code","P.S")), by = c(PSC = "PSC Code")) %>% 
  filter(PSC != "UNKN") %>% 
  filter(fiscal_year %in% c(2017:2019)) %>% ###date range for chart
  select(transaction_value, fiscal_quarter, `P.S`) %>% 
  group_by(`P.S`, fiscal_quarter) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  separate(fiscal_quarter, into = c("FY","quarter"), sep = "Q") %>% 
  mutate(total_obligations = round((sum)/1000000000, digits=2)) %>%           ###division of $$
  group_by(FY, `P.S`) %>% 
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations))%>% 
  mutate(FYYear = paste("FY", FY, sep = "")) %>% 
  mutate(Q_quarter = paste("Q", quarter, sep =""))



plot <- 
  ggplot(data.organized, aes(x = FYYear, y = total_obligations, fill = factor(Q_quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(data = subset(data.organized, total_obligations>(max(data.organized$label_y)/30)), aes(label = round(total_obligations, digits = 2), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.organized, FY != 2019 & total_obligations >(max(data.organized$label_y)/15)), aes(label = sprintf('%.0f%%', prop), 
                                                                                                                 y = label_y), size = 4, vjust = 3, fontface = "bold", check_overlap = T)+
  stat_summary(fun.y = sum, aes(label = ..y.., group = FY),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "Blues")[c(1,2,4,6)])+
  facet_wrap(~`P.S`, labeller = label_wrap_gen(20)#, scales = "free"
  )+
  labs(y = "Contract Obligations (in) Billions",                                            ##Change based on $$ division
       title = paste(Agency, " Contract Obligations Comparison - ", Year, sep = ""),                   ##Change based on Agency and year/quarter
       caption = "Data Source: Bloomberg Government") +                                     ##Can change or remove caption
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))

plot

ggsave(filename = paste(Agency," Contract Obligations ", Year, "by quarter - P-S.jpg", sep = ""), plot,          ##Change Based on Agency and year/quarter
       width = 13, height = 6.5, units = "in")

###Defense####


setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/QbyQ charts/Product-Service")
data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/Defense Serv-Prod Data by quarter.csv")

data$Year = as.character(data$Year)

data.civdef_total <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(Year %in% 2017:2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations))

data.civdef_total$Year = as.character(data.civdef_total$Year)

data.civdef <- data %>%
  rename(civ_def = "Civ/Def",
         total_obligations = "Contract Obligations (in Billions)") %>%
  filter(Year %in% 2017:2019) %>%
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = 100*total_obligations/sum(total_obligations)) %>%
  mutate(FYYear = paste("FY",Year, sep = ""))



plot <- ggplot(data.civdef, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.civdef, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = 5, fontface = "bold")+   ####Adds total to top
  #geom_text(aes(color = Quarter == "Q1", label = round(total_obligations, digits = 1), y = label_y), size = 3, vjust = 1.5) +## white on dark
  # geom_text(data = subset(data.civdef, Year != 2018), aes(color = Quarter == "Q1",
  #                   label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3)+ ## white on dark
  #scale_color_manual(guide = FALSE, values = c("black", "white")) +   ## White on dark
  # scale_fill_manual(name = NULL, values = c("Q4" = "lightcyan", "Q3" = "lightblue2",
  #     "Q2" = "skyblue3", "Q1" = "skyblue4")) +
  #scale_fill_brewer(name = "Quarter", palette = "YlOrRd")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "Blues")[c(1,2,4,6)])+
  facet_grid(~civ_def, labeller = label_wrap_gen(20))+
  labs(x="Fiscal Year", y = "Contract Obligations (in) Billions", 
       title = "Defense Contract Obligations Comparison FY16-FY19",
       subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), 
        plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        plot.caption = element_text(size = 8, face = "italic"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))




ggsave("Defense Contract Obligations by Quarter - FY17-FY19 - P-S.jpg", plot,                ######
       width = 13, height = 6.5, units = "in")








####################################################3

###Comparison of Agencies#####

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons") #location charts are saved

data <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/comparisons/HHS, VA, DHS, DOS, USAID, EPA quarter by quarter.csv")

data$Year = as.character(data$Year)

data.agency_total <- data %>%
  rename(total_obligations = Value) %>%
  #filter(Year!=2019) %>%
  group_by(Year, Agency) %>%
  mutate(label_y = cumsum(total_obligations)/1000000000)

data.agency_total$Year = as.character(data.agency_total$Year)

data.agency <- data %>%
  mutate(total_obligations = round(Value/1000000000, digit = 2)) %>%
  group_by(Year, Agency) %>%
  mutate(label_y = cumsum(total_obligations),
         prop = round(100*total_obligations/sum(total_obligations), digit = 1)) %>%
  filter(Year %in% c(2016, 2017, 2018, 2019)) %>% 
  mutate(FYYear = paste("FY", Year, sep = ""))


plot <- ggplot(data.agency, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(data.agency, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
               geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
  facet_grid(~Agency, labeller = label_wrap_gen(20))+
  labs(y = "Contract Obligations (in) Billions",
       title = "Contract Obligations Comparison") +
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))


ggsave("Health Contract Obligations FY16-FY19Q1 by quarter.jpg", plot,
       width = 13, height = 6.5, units = "in")

###if split####
Agency_list<- split(data.agency, data.agency$Agency)

plot<-lapply(Agency_list, function(xy) {
  plot <- ggplot(xy, aes(x = FYYear, y = total_obligations, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(total_obligations, digits = 1), y = label_y), size = 4, vjust = 1.5, fontface = "bold")+
    geom_text(data = subset(xy, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
                 geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    scale_fill_manual(name = "Quarter", values = brewer.pal(9, "GnBu")[c(1,3,5,7)])+
    facet_grid(~Agency, labeller = label_wrap_gen(20))+
    labs(y = "Contract Obligations (in) Billions",
         title = "Contract Obligations Comparison") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20), 
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))
} )

plot 

lapply(names(plot), function(x) {
  ggsave(filename = paste(x," Contract Obligations FY17-19Q1 by quarter.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 7, units = "in")
})



