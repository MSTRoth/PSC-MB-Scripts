library(scales)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
options(scipen = 999)

###Blue, Multiple: DPAP Agency by Year - Blue Spectrum, Multiple FIscal Years####

data <- read_csv("x:/1 Marielle Folder/Data Sets/By Agency/DOD/DOD_NAVY_USMC FY15-18.csv")

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

agency_chart <- data %>% 
  rename("PSC" = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         "transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  filter(`Funding Office Level 4` == "Marine Corps Systems Command (MCSC)") %>%    ##Choose Agency/SubAgency
  #filter(str_detect(`BGOVMarkets`,"Health Information Technology")) %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  filter(P.S != "Products") %>% ##include if only Services
  filter(fiscal_year != 2019) %>% ##include if there is an inclomplete year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000)

 agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
# agency_chart$DPAP_category <- factor(agency_chart$DPAP_category, levels=c("Construction Services", 
#                                                               "Electronic & Communication Services", 
#                                                               "Equipment Related Services",
#                                                               "Facility Related Services",
#                                                               "Knowledge Based Services",
#                                                               "Logistics Management Services",
#                                                               "Medical Services",
#                                                               "Research and Development",
#                                                               "Transportation Services",
#                                                               "Products"))

cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=5))

##if an entire DPAP category is missing
# agency_chart[1,]
# df_add <- data.frame(DPAP_category = c("Construction Services","Construction Services", "Construction Services",
#                        "Construction Services","Construction Services"),
#                      fiscal_year = c(2014,2015, 2016, 2017, 2018),
#                      sum = c(0,0,0,0,0))
# agency_chart1 <- merge(agency_chart, df_add,by=c("DPAP_category", "fiscal_year", "sum"),
#                        all=TRUE)

plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)", title = "MCSC",                                 ##Change $$ division, Agency/SUbagency Name
        subtitle = "FY15-FY18", x = NULL)+                                                       ##and Year
  facet_grid(~DPAP_category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
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


plot


ggsave("MCSC DPAP FY15-FY18 wo products.jpg", plot,                   ###Change file name
       width = 15, height = 7, units = "in") 

#------------------------------------------------------------------

###Blue, single: DPAP Agency by Year - single Fiscal Year####

setwd("x:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories")

data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/DOI/DOI FY18.csv")             ##
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

agency_chart <- data %>% 
  rename("PSC" = "Product Service Code (PSC) / Federal Supply Code (FSC)",
         "transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(PSC, transaction_value, fiscal_year) %>% 
  left_join(select(dpap, c("PSC Code", DPAP, "P.S")), by = c(PSC = "PSC Code")) %>% 
  mutate(DPAP_category = ifelse(P.S == "Products", "Products", DPAP)) %>% 
  #filter(P.S == "Service") %>% ##Choose if just services
  #filter(fiscal_year == 2018) %>% ##choose desired year
  filter(PSC != "UNKN") %>% 
  group_by(DPAP_category, fiscal_year) %>%
  summarise(sum = sum(transaction_value)/1000000)                                    ##Change depending on $$

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)

plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual("Fiscal Year", values = c("2018" = "steelblue2")) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Millions)",                                         ##$$ amount
       title = "DOI Contract Spending FY18", x = NULL)+                                  ##Agency
  facet_grid(~DPAP_category, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 36, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 28, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
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
  guides(fill = "none")

plot

ggsave("DOI DPAP FY18.jpg", plot, 
       width = 17, height = 8, units = "in") 

#------------------------------------------------------------------
##DoD Services

setwd("X:/1 Marielle Folder/Visualizations/Agency Charts/DPAP Categories/")

data <- read_csv("X:/1 Marielle Folder/Data Sets/By Agency/DOD/DoD services, by DPAP.csv")


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
data$Service2 <- data$Service

DoD_service_list <- split(data, data$Service2)

label_height<- lapply(DoD_service_list, function(x){
  DPAP_service<- x %>%  
  filter(`Fiscal Year` %in% c("FY15","FY16", "FY17","FY18")) %>%           ##FYs included
  group_by(`Fiscal Year`, `DPAP Category`) %>%
  mutate(sumB = `Total (in millions)`/1000
         )
})


cc <- scales::seq_gradient_pal("azure3", "steelblue3", "Lab")(seq(0,1,length.out=4))

##if an entire DPAP category is missing
# agency_chart[1,]
# df_add <- data.frame(DPAP_category = c("Construction Services","Construction Services", "Construction Services",
#                        "Construction Services","Construction Services"),
#                      fiscal_year = c(2014,2015, 2016, 2017, 2018),
#                      sum = c(0,0,0,0,0))
# agency_chart1 <- merge(agency_chart, df_add,by=c("DPAP_category", "fiscal_year", "sum"),
#                        all=TRUE)

plot<- lapply(label_height, function(xy){
plot <- ggplot(xy, aes(fill = `Fiscal Year`,
                                 x = `Fiscal Year`,
                                 y = sumB))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Billions)", title = unique(xy$Service),             ## $$
       subtitle = "FY15-FY18", x = NULL)+                                           ########change title adn subtitle
  facet_grid(~`DPAP Category`, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 26, face = "bold"),        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
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
        strip.text.x = element_text(face="bold", size = 11),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  #guides(fill = guide_legend(reverse = TRUE))
  guides(fill = FALSE)

})
plot


lapply(names(plot), function(x) {
  ggsave(filename = paste(x," DPAP FY15-FY18.jpg", sep = ""), plot=plot[[x]],
         width = 13, height = 6.5, units = "in")
})

