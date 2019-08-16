###Contract Spend by Agency - By Subcategories####


###Health

data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/IT O&M- Cloud, DevOps contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
 # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 
  

agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by IT O&M Contract Spend (BGOV Defined)",
       subtitle = "Including Cloud and DevOps \n FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
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


ggsave("Top 10 Agencies by IT O&M Contract Spend (BGOV Defined) FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 



###Cloud

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Cloud services contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 


agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by Cloud Services Contract Spend (BGOV Defined)",
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 32, face = "bold"),
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


ggsave("Top 10 Agencies by Cloud Services Contract Spend (BGOV Defined) FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 


###Cybersecurity

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Cybersecurity contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 


agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by Cybersecurity Contract Spend (BGOV Defined)",
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 34, face = "bold"),
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


ggsave("Top 10 Agencies by Cybersecurity Contract Spend (BGOV Defined) FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 


###IoT

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/IoT contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 


agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by IoT Contract Spend (BGOV Defined)",
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
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


ggsave("Top 10 Agencies by IoT Contract Spend (BGOV Defined) FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 


###Blockchain

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/Blockchain contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 

null_entries <- data.frame("Funding Agency"= c("Commodity Futures Trading Commission (CFTC)", 
                                                "Commodity Futures Trading Commission (CFTC)",
                                                "Commodity Futures Trading Commission (CFTC)",
                                                "Department of Defense (DOD)",
                                                "Department of Defense (DOD)",
                                                "Department of Defense (DOD)",
                                                "Department of Health and Human Services (HHS)",
                                                "Department of Health and Human Services (HHS)",
                                                "Department of Homeland Security (DHS)",
                                                "General Services Administration (GSA)",
                                                "General Services Administration (GSA)",
                                                "National Aeronautics and Space Administration (NASA)",
                                                "National Aeronautics and Space Administration (NASA)",
                                                "Securities and Exchange Commission (SEC)",
                                                "Securities and Exchange Commission (SEC)"), 
                           "fiscal_year" = c(2015, 2016, 2017,
                                             2015, 2016, 2017,
                                             2016, 2017,
                                             2015,
                                             2015, 2016,
                                             2015, 2016,
                                             2015, 2016),
                           "sum" = 0)



agency_chart<- merge.default(agency_chart, null_entries, by = 1:3, all=T)


agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by Blockchain Contract Spend (BGOV Defined)",
       subtitle = "FY15-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
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


ggsave("Top 10 Agencies by Blockchain Contract Spend (BGOV Defined) FY15-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 


###AI/ML

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/AI contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")
dpap <- read_csv("~/Reference Tables/DPAP Crosswalk.csv")

top_10 <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  # filter(fiscal_year == 2018) %>% 
  group_by(`Funding Agency`) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)) %>% 
  arrange(-sum) %>% 
  head(10)

x<- top_10[1]


agency_chart <- data %>% 
  rename("transaction_value" = "Transaction Value",
         fiscal_year = "Fiscal Year") %>% 
  select(`Funding Agency`, transaction_value, fiscal_year) %>% 
  filter(`Funding Agency` %in% top_10$`Funding Agency`) %>% 
  group_by(`Funding Agency`, fiscal_year) %>%    ##### DPAP, fiscal_year or PSC Code, PSC Description, fiscal_year
  summarise(sum = sum(transaction_value)/1000000) 


agency_chart$fiscal_year <- as.character(agency_chart$fiscal_year)
agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency` , levels = top_10$`Funding Agency`)

cc <- scales::seq_gradient_pal("darkolivegreen1", "chartreuse4", "Lab")(seq(0,1,length.out=5))

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
  labs(y = "Contract Obligations (in Millions)", title = "Top Agencies by AI/ML Contract Spend (BGOV Defined)",
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
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


ggsave("Top 10 Agencies by AI-ML Contract Spend (BGOV Defined) FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 

#####################################################
### Large Data, DPAP categories by top agencies

data <- read_csv("S:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/KBS contract spend FY14-18.csv")

setwd("S:/1 Marielle Folder/Visualizations/Agency Charts/Comparisons")

agency_chart <- data %>% 
  filter(`Funding Agency` != "Total") %>% 
  select(1:6) %>% 
  gather("fiscal_year", "transaction_value", 2:6) %>% 
  mutate(sum = transaction_value/1000000000) 


agency_order <-data %>% 
  filter(`Funding Agency` != "Total") %>% 
  arrange(-`FY 2010 - 2019 Total`)

agency_chart$`Funding Agency` <- factor(agency_chart$`Funding Agency`, levels = agency_order$`Funding Agency`)


plot <- ggplot(agency_chart, aes(fill = fiscal_year,
                                 x = fiscal_year,
                                 y = sum))+
  geom_bar(stat = "identity", position = position_dodge()) +
  #geom_text(aes(label = dollar(round(sum, digits = 1)), vjust = -1), size = 4)+
  scale_fill_manual(values = cc) +
  #scale_y_continuous(labels = dollar) +
  labs(y = "Contract Obligations (in Billions)", title = "Top Agencies by Knowledge Based Services Contract Spend",
       subtitle = "FY14-FY18", x = NULL)+
  facet_grid(~`Funding Agency`, labeller = label_wrap_gen(10), scales = "free")+
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

ggsave("Top 10 Agencies by KBS FY14-FY18.jpg", plot,
       width = 15, height = 7, units = "in") 
