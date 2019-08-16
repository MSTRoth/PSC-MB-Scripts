##Bar charts by NAICS code -- red spectrum####

library(tidyverse)
library(RColorBrewer)
options(scipen=999)

#Location for saving charts
setwd("X:/1 Marielle Folder/Visualizations/NAICS") #location charts are saved

data <- read_csv("X:/1 Marielle Folder/Data Sets/By NAICS or PSC/csv/FMP NAICS group.csv")   ##csv source

data$`Fiscal Year` = as.character(data$`Fiscal Year`)
NAICS_total <- data %>% 
  rename(NAICS = "NAICS Code",
         transaction_value = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  group_by(NAICS, fiscal_year) %>% 
  summarize(sum = sum(transaction_value)/1000000)

NAICS_list<- split(data, data$`NAICS Code`)

NAICS_data<-lapply(NAICS_list, function(x) {                    ###Find top 10 (?) NAICS codes; format data for plot
  Funding_top_ten <- x %>% 
    group_by(`Funding Agency`) %>% 
    summarize(sum = sum(`Transaction Value`)) %>% 
    arrange(-sum) %>% 
    head(10)
  
  NAICS_data <- x %>%  
    rename(NAICS = "NAICS Code",
           transaction_value = "Transaction Value",
           fiscal_year = "Fiscal Year",
           funding_agency = "Funding Agency") %>%  
    group_by(NAICS, funding_agency, fiscal_year) %>% 
    summarize(sum = sum(transaction_value)/1000000) %>%                          ##can change division (thousands, millions, billions)
    filter(funding_agency %in% Funding_top_ten$`Funding Agency`)
     
} )

list2env(NAICS_data,envir=.GlobalEnv) 


###Compare NAICS

cc <- scales::seq_gradient_pal("red1", "orange", "Lab")(seq(0,1,length.out=5))

plot <- ggplot(NAICS_total, aes(x = fiscal_year, y = sum, fill = fiscal_year)) +
  geom_bar(stat = "identity", color = "Black", position = "dodge") +
  geom_text(aes(label = round(sum, digits = 0)), size = 4, vjust = -.7, fontface = "bold", position = position_dodge(width = 1))+
  # facet_grid(~Agency, labeller = label_wrap_gen(20))+
  labs(y = "Transaction Value (in Millions)",
       title = "Obligations Comparison by NAICS Code") +
  scale_fill_manual(name = "Fiscal Year", values = cc) +
  # theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
  #       plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"), axis.ticks.x = element_blank(),
  #       strip.text = element_text(face = "bold"), axis.title.x = element_blank())
  facet_grid(~NAICS, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"), plot.subtitle = element_text(hjust = 0.5, size = 18, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        panel.spacing = unit(1, "lines"))

plot

ggsave("Obligation Comparison by NAICS Code FY14-FY18 - spectrum.jpg", plot,
       width = 13, height = 6, units = "in")
####NAICS by to Agency


cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

##if an entire DPAP category is missing
# agency_chart[1,]
# df_add <- data.frame(DPAP_category = c("Construction Services","Construction Services", "Construction Services",
#                        "Construction Services","Construction Services"),
#                      fiscal_year = c(2014,2015, 2016, 2017, 2018),
#                      sum = c(0,0,0,0,0))
# agency_chart1 <- merge(agency_chart, df_add,by=c("DPAP_category", "fiscal_year", "sum"),
#                        all=TRUE)
plot1<-lapply(NAICS_data, function(xy) {
plot1 <- ggplot(xy, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = paste("Top Agencies for ",unique(xy$NAICS), sep = ""),
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~funding_agency, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        #panel.spacing = unit(1, "lines"),
        # panel.border = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 14),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_line(colour="#f0f0f0"),
        # legend.key = element_rect(colour = NA),
        # legend.position = "bottom",
        # legend.direction = "horizontal",
        # legend.key.size= unit(0.2, "cm"),
        # legend.margin = unit(0, "cm"),
        legend.title = element_text(face="bold"),
        #legend.text = element_text(size = 11),
        #plot.margin=unit(c(10,5,5,5),"lines"),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)
})
plot1

lapply(names(plot1), function(x) {
  ggsave(filename = paste(x," by Top Agencies FY14-18.jpg", sep = ""), plot=plot1[[x]],
         width = 13, height = 6, units = "in")
})
