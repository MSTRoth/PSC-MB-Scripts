install.packages("readxl")
library(tidyverse)
library(readxl)
library(lubridate)
library(RColorBrewer)

############USA Psending Grant Data by Agency
data1<-read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Grant Data USASpendingFY16.csv")
data2<-read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Grant Data USASpendingFY17.csv")
data3<-read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Grant Data USASpendingFY18.csv")
data4<-read_csv("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Grant Data USASpendingFY19Q2.csv")



setwd("X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/")

filenames <- list.files(path = "X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/", pattern = "Grant Data")
data <- do.call("rbind", lapply(filenames, read_csv))


write.csv(data, "X:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/USASpending Grants FY16_FY19Q2.csv")

data$action_date<-as.POSIXct(data$action_date,format="%m/%d/%Y")
data$total_loan_value <- as.numeric(data$total_loan_value)

grant_data1619Q2 <- data%>% 
  select(c("federal_action_obligation", "non_federal_funding_amount", "total_funding_amount", 
          "total_loan_value", "action_date", 
           #"period_of_performance_start_date", "period_of_performance_current_end_date", "awarding_agency_code",
           "awarding_agency_name"
          #, "recipient_duns", "recipient_name", "recipient_parent_name",
      #     "recipient_parent_duns", "assistance_type_description", "award_description", "action_type_description"
          )) %>% 
  mutate(fiscal_year = ifelse(month(action_date) %in% 10:12, year(action_date)+1, year(action_date))) %>% 
 # filter(fiscal_year != 2016) %>% 
  filter(!is.na(federal_action_obligation)) %>% 
  filter(!is.na(non_federal_funding_amount)) %>% 
  group_by(awarding_agency_name, fiscal_year) %>% 
  summarise(fed.action_obligation = sum(federal_action_obligation),
            non.fed_funding_amount = sum(non_federal_funding_amount))
            #total.funding_amount = sum(total_funding_amount),
            #total.loan_value = sum(total_loan_value)) %>% 
 # mutate(total.addition = fed.action_obligation + non.fed_funding_amount) #%>% 
 # gather("funding_Type", "funding_amount", c(fed.action_obligation, non.fed_funding_amount))


grant_data_fed <- grant_data1619Q2 %>%
  rename(
         "Agency" = awarding_agency_name,
         "Year" = fiscal_year) %>%
  group_by(Year, Agency) %>%
  mutate(Value = cumsum(fed.action_obligation)/1000000) %>% 
  mutate(FYYear = paste("FY", Year, sep = ""))

grant_data_fed$Year = as.character(grant_data_fed$Year)

# grant.data <- grant_data1619Q2 %>%
#   rename("Value" = funding_amount,
#          "Agency" = awarding_agency_name,
#          "Year" = fiscal_year) %>%
#   group_by(Year, Agency) %>%
#   mutate(total_obligations = round(Value/1000000, digit = 2)) %>%
#   group_by(Year, Agency) %>%
#   mutate(label_y = cumsum(total_obligations),
#          prop = round(100*total_obligations/sum(total_obligations), digit = 1)) %>%
#   filter(Year %in% c(2016, 2017, 2018, 2019)) %>% 
#   mutate(FYYear = paste("FY", Year, sep = "")) %>% 
#   filter(funding_Type == "fed.action_obligation") 
  



# grant.data2 <- grant.data[!is.na(grant.data$total_obligations),]
# grant.agencies <- unique(grant.data2$Agency)

# unique(grant.data$Agency)
# 
# sum(!is.na(grant.data$total_obligations==TRUE))



agency_list <- split(grant_data_fed, grant_data_fed$Agency)
#list2env(agency_list, .GlobalEnv)

plot<-lapply(agency_list, function(xy) {
    plot <- ggplot(xy, aes(x = FYYear, y = Value, fill = "red")) +
    geom_bar(stat = "identity", color = "Black") +
    geom_text(aes(label = round(Value, digits = 2), y = Value), size = 4, vjust = 1.5, fontface = "bold")+
    #geom_text(data = subset(xy, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
    # stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
    #              geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
    facet_grid(~Agency, labeller = label_wrap_gen(20))+
    labs(y = "Funding Amount (in Millions)",
         title = "Grant Funding Amounts") +
    # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
    #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
    #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
    theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
          axis.ticks.x = element_blank(),
          strip.text = element_text(face = "bold", size = 20), 
          axis.title.x = element_blank(),
          panel.spacing = unit(4, "lines"))+
      guides(fill = "none")
    
} )

setwd("X:/1 Marielle Folder/Visualizations/Grants")

lapply(names(plot), function(x) {
  ggsave(filename = paste(x," grant data FY17-19Q2.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 7, units = "in")
})

plot


###chr to date conversion??
 x<- head(grant_data$action_date, 10)
y<-as.POSIXct(x,format="%m/%d/%Y")

ifelse(month(y) %in% 10:12, year(y)+1, year(y))









#####Total Grand funding

data$action_date<-as.POSIXct(data$action_date,format="%m/%d/%Y")
data$total_loan_value <- as.numeric(data$total_loan_value)

grant_data_total <- data%>% 
  select(c("federal_action_obligation", "non_federal_funding_amount", "total_funding_amount", 
           "total_loan_value", "action_date", 
           #"period_of_performance_start_date", "period_of_performance_current_end_date", "awarding_agency_code",
           "awarding_agency_name"
           #, "recipient_duns", "recipient_name", "recipient_parent_name",
           #     "recipient_parent_duns", "assistance_type_description", "award_description", "action_type_description"
  )) %>% 
  mutate(fiscal_year = ifelse(month(action_date) %in% 10:12, year(action_date)+1, year(action_date))) %>% 
  # filter(fiscal_year != 2016) %>% 
  filter(!is.na(federal_action_obligation)) %>% 
  filter(!is.na(non_federal_funding_amount)) %>% 
  group_by(fiscal_year) %>% 
  summarise(fed.action_obligation = sum(federal_action_obligation),
            non.fed_funding_amount = sum(non_federal_funding_amount),
           # total.funding_amount = sum(total_funding_amount)
            )
 # mutate(total.addition = fed.action_obligation + non.fed_funding_amount) #%>% 
# gather("funding_Type", "funding_amount", c(fed.action_obligation, non.fed_funding_amount))


grant_data_fed <- grant_data_total %>%
  rename("Year" = fiscal_year) %>%
  group_by(Year) %>%
  mutate(Value = cumsum(fed.action_obligation)/1000000000) %>% 
  mutate(FYYear = paste("FY", Year, sep = ""))

grant_data_fed$Year = as.character(grant_data_fed$Year)

plot <- ggplot(grant_data_fed, aes(x = FYYear, y = Value, fill = "red")) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(Value, digits = 2), y = Value), size = 4, vjust = 1.5, fontface = "bold")+
  #geom_text(data = subset(xy, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  # stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
  #              geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  labs(y = "Funding Amount (in Billions)",
       title = "Grant Funding Amounts") +
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))+
  guides(fill = "none")

ggsave(filename = "grant data total.jpg", plot=plot,
       width = 13, height = 7, units = "in")


#######By civ and Def

data$action_date<-as.POSIXct(data$action_date,format="%m/%d/%Y")
data$total_loan_value <- as.numeric(data$total_loan_value)

grant_data_civdef <- data%>% 
  select(c("federal_action_obligation", "non_federal_funding_amount", "total_funding_amount", 
           "total_loan_value", "action_date", 
           #"period_of_performance_start_date", "period_of_performance_current_end_date", "awarding_agency_code",
           "awarding_agency_name"
           #, "recipient_duns", "recipient_name", "recipient_parent_name",
           #     "recipient_parent_duns", "assistance_type_description", "award_description", "action_type_description"
  )) %>% 
  mutate(fiscal_year = ifelse(month(action_date) %in% 10:12, year(action_date)+1, year(action_date))) %>% 
  mutate(civ_def = ifelse(awarding_agency_name == "DEPARTMENT OF DEFENSE (DOD)", "Defense", "Civilian")) %>% 
  # filter(fiscal_year != 2016) %>% 
  filter(!is.na(federal_action_obligation)) %>% 
  filter(!is.na(non_federal_funding_amount)) %>% 
  group_by(civ_def, fiscal_year) %>% 
  summarise(fed.action_obligation = sum(federal_action_obligation),
            non.fed_funding_amount = sum(non_federal_funding_amount),
            # total.funding_amount = sum(total_funding_amount)
  )
# mutate(total.addition = fed.action_obligation + non.fed_funding_amount) #%>% 
# gather("funding_Type", "funding_amount", c(fed.action_obligation, non.fed_funding_amount))


grant_data_fed <- grant_data_civdef %>%
  rename("Year" = fiscal_year) %>%
  group_by(Year, civ_def) %>%
  mutate(Value = cumsum(fed.action_obligation)/1000000000) %>% 
  mutate(FYYear = paste("FY", Year, sep = ""))

grant_data_fed$Year = as.character(grant_data_fed$Year)

plot <- ggplot(grant_data_fed, aes(x = FYYear, y = Value, fill = civ_def)) +
  geom_bar(stat = "identity", color = "Black") +
  geom_text(aes(label = round(Value, digits = 2), y = Value), size = 4, vjust = 1.5, fontface = "bold")+
  #geom_text(data = subset(xy, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 4, vjust = 3, fontface = "bold")+
  # stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
  #              geom = "text", vjust = -.5, size = sum(4,1), fontface = "bold")+   ####Adds total to top
  labs(y = "Funding Amount (in Billions)",
       title = "Grant Funding Amounts") +
  facet_wrap(~civ_def, labeller = label_wrap_gen(20), scales = "free_y")+
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))+
  guides(fill = "none")

plot

ggsave(filename = "grant data civdef.jpg", plot=plot,
       width = 13, height = 7, units = "in")



######by Civ and Def by Quarter

data$action_date<-as.POSIXct(data$action_date,format="%m/%d/%Y")
data$total_loan_value <- as.numeric(data$total_loan_value)

grant_data_civdef_Q <- data%>% 
  select(c("federal_action_obligation", "non_federal_funding_amount", "total_funding_amount", 
           "total_loan_value", "action_date", 
           #"period_of_performance_start_date", "period_of_performance_current_end_date", "awarding_agency_code",
           "awarding_agency_name"
           #, "recipient_duns", "recipient_name", "recipient_parent_name",
           #     "recipient_parent_duns", "assistance_type_description", "award_description", "action_type_description"
  )) %>% 
  mutate(fiscal_year = ifelse(month(action_date) %in% 10:12, year(action_date)+1, year(action_date))) %>% 
  mutate(fiscal_quarter = ifelse(month(action_date) %in% 10:12, "Q1", ifelse(month(action_date) %in% 1:3, "Q2", 
                                            ifelse(month(action_date) %in% 4:6, "Q3", "Q4")))) %>%
  mutate(civ_def = ifelse(awarding_agency_name == "DEPARTMENT OF DEFENSE (DOD)", "Defense", "Civilian")) %>% 
  # filter(fiscal_year != 2016) %>% 
  filter(!is.na(federal_action_obligation)) %>% 
  filter(!is.na(non_federal_funding_amount)) %>% 
  group_by(civ_def, fiscal_year, fiscal_quarter) %>% 
  summarise(fed.action_obligation = sum(federal_action_obligation),
            non.fed_funding_amount = sum(non_federal_funding_amount),
            # total.funding_amount = sum(total_funding_amount)
  )
# mutate(total.addition = fed.action_obligation + non.fed_funding_amount) #%>% 
# gather("funding_Type", "funding_amount", c(fed.action_obligation, non.fed_funding_amount))


grant_data_fed <- grant_data_civdef_Q %>%
  rename("Year" = fiscal_year,
         "Quarter" = fiscal_quarter) %>%
  group_by(Year, Quarter, civ_def) %>%
  mutate(Value = cumsum(round(fed.action_obligation/1000000000, digits = 1))) %>% 
  group_by(Year, civ_def) %>%
  mutate(label_y = cumsum(Value),
         prop = 100*Value/sum(Value)) %>%
  mutate(FYYear = paste("FY", Year, sep = ""))

grant_data_fed$Year = as.character(grant_data_fed$Year)

plot <- ggplot(grant_data_fed, aes(x = FYYear, y = Value, fill = factor(Quarter, levels = c("Q4","Q3", "Q2","Q1")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(Value, digits = 2), y = label_y), size = 3, vjust = 1.5, fontface = "bold")+
  geom_text(data = subset(grant_data_fed, Year != 2019), aes(label = sprintf('%.0f%%', prop), y = label_y), size = 3, vjust = 3, fontface = "bold")+
  stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
                geom = "text", vjust = -.5, size = sum(3,1), fontface = "bold")+   ####Adds total to top
  labs(y = "Funding Amount (in Billions)",
       title = "Grant Funding Amounts") +
  facet_wrap(~civ_def, labeller = label_wrap_gen(20), scales = "free_y")+
  scale_fill_manual(name = "Quarter", values = brewer.pal(9, "YlOrRd")[c(7,5,3,1)])+
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(4, "lines"))

plot

ggsave(filename = "grant data civ_def by Quarter.jpg", plot=plot,
       width = 13, height = 7, units = "in")




