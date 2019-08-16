####BCA Caps
library(colorspace)
library(readr)
library(RColorBrewer)
library(tidyverse)
options(scipen=999)


setwd("x:/1 Marielle Folder/Visualizations/Government-Wide")
data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/BCA chart_R Data.csv")


data$Year <- factor(data$Year,
                    levels = c("FY12", "FY13", "FY14", "FY15", "FY16", "FY17", "FY18",
                               "FY19",
                               "FY20",
                               "FY21",
                               ordered = is.ordered(data$Year)))

data$Type <- factor(data$Type,
                    levels = c("OCO (Non-Defense)","Non-Defense", "OCO (Defense)","Defense",
                               ordered = is.ordered(data$Type)))

# data$Year <- factor(data$Year,
#                     levels = c("FY17", "FY18",
#                                "FY19",
#                                "FY20",
#                                "FY20\\nOMB",
#                                "FY20\\nDem (House) Proposal\\n(4/2/19)",
#                                "FY21",
#                                "FY21\\nOMB",
#                                "FY21\\nDem (House) Proposal\\n(4/2/19)",
#                                ordered = is.ordered(data$Year)))


data.BCA_total <- data %>%
  ##  filter(!(Year %in% c("FY12", "FY13", "FY14", "FY15", "FY16"))) %>%  ##To show limit historical trends
  group_by(`Budget Type`, Year) %>%
  mutate(label_y = cumsum(Amount),
         sum = sum(Amount)) %>% 
  group_by(`Budget Type`, Year, DND) %>% 
  mutate(DND_Amount = sum(Amount)) %>% 
  filter(`Budget Type` %in% c("BBA-19 (8/2/19)", "Current") | `Budget Type` == "Original Caps" & Year == "FY20"     ##if not using all budget types
         #| `Budget Type` == "Current"
          ) 

data.BCA_total$`Budget Type` <- factor(data.BCA_total$`Budget Type`, levels = c("Current", "Original Caps", "BBA-19 (8/2/19)"))    ##ordering budget types


plot <- ggplot(data.BCA_total, aes(x = Year, y = Amount, fill = factor(Type, levels = c("OCO (Non-Defense)","Non-Defense", "OCO (Defense)","Defense")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(data =subset(data.BCA_total, Type != "OCO (Non-Defense)" &Type != "OCO (Defense)" ), aes(label = Amount), position = position_stack(vjust = .7),
            , size = 5, fontface = "bold")+
  geom_text(data = subset(data.BCA_total, Type == "OCO (Non-Defense)" |Type == "OCO (Defense)" ), aes(label = DND_Amount, y=label_y), vjust=1.1
            ,size = 6, fontface = "bold")+
  geom_text(data = subset(data.BCA_total, Type == "OCO (Non-Defense)" |Type == "OCO (Defense)" ), aes(label = paste("(",Amount,")",sep = ""), y=label_y), vjust=3
            ,size = 4, fontface = "bold")+  ####OCO text
  geom_label(data = subset(data.BCA_total, Type == "OCO (Non-Defense)"), aes(label = sum, y=1500)
             ,size = 6, fontface = "bold", fill = "white", show.legend = F) +
  # stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
  #              geom = "text", size = 6, fontface = "bold", vjust= -4)+   ####Adds total to top
  scale_fill_manual(name = "BA Type", values = c("OCO (Non-Defense)" = "#FFE4C8", "Non-Defense" = "#EC8418",
                                                 "OCO (Defense)" = "#CFE8FF", "Defense" = "#72B3F0"))+
  facet_grid(~`Budget Type`, labeller = label_wrap_gen(width=10), scales = "free_x", space = "free_x")+
  labs(x = NULL, y = "Dollar Amount (in Billions)", title = "BCA (as of 8/2/19)",                                           ###Change as of data
       subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size =15),
        panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        #legend.margin = margin(c(5, 5, 5, 0)),
        legend.spacing.x = unit(.5, "char"),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))+
  scale_y_continuous(limits = c(0, 1500))

plot

ggsave("BCA - BBA-19 w OCO full.jpg", plot,
       width = 14, height = 7.5, units = "in")




###BCA possibilities####

setwd("x:/1 Marielle Folder/Visualizations/Government-Wide")
data <- read_csv("x:/1 Marielle Folder/Visualizations/Government-Wide/BCA chart_R Data_411.csv")


data$Year <- factor(data$Year,
                               levels = c("FY12", "FY13", "FY14", "FY15", "FY16", "FY17", "FY18",
                                          "FY19",
                                          "FY20",
                                          "FY21",
                                           ordered = is.ordered(data$Year)))

data$Type <- factor(data$Type,
                    levels = c("OCO (Non-Defense)","Non-Defense", "OCO (Defense)","Defense",
                               ordered = is.ordered(data$Type)))

# data$Year <- factor(data$Year,
#                     levels = c("FY17", "FY18",
#                                "FY19",
#                                "FY20",
#                                "FY20\\nOMB",
#                                "FY20\\nDem (House) Proposal\\n(4/2/19)",
#                                "FY21",
#                                "FY21\\nOMB",
#                                "FY21\\nDem (House) Proposal\\n(4/2/19)",
#                                ordered = is.ordered(data$Year)))


data.BCA_total <- data %>%
##  filter(!(Year %in% c("FY12", "FY13", "FY14", "FY15", "FY16"))) %>%  ##To show limit historical trends
  group_by(`Budget Type`, Year) %>%
  mutate(label_y = cumsum(Amount),
         sum = sum(Amount)) %>% 
  group_by(`Budget Type`, Year, DND) %>% 
  mutate(DND_Amount = sum(Amount))
  


plot <- ggplot(data.BCA_total, aes(x = Year, y = Amount, fill = factor(Type, levels = c("OCO (Non-Defense)","Non-Defense", "OCO (Defense)","Defense")))) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(data =subset(data.BCA_total, Type != "OCO (Non-Defense)" &Type != "OCO (Defense)" ), aes(label = Amount), position = position_stack(vjust = .7),
            , size = 5, fontface = "bold")+
  geom_text(data = subset(data.BCA_total, Type == "OCO (Non-Defense)" |Type == "OCO (Defense)" ), aes(label = DND_Amount, y=label_y), vjust=1.1
            ,size = 6, fontface = "bold")+
  geom_text(data = subset(data.BCA_total, Type == "OCO (Non-Defense)" |Type == "OCO (Defense)" ), aes(label = paste("(",Amount,")",sep = ""), y=label_y), vjust=3
            ,size = 4, fontface = "bold")+  ####OCO text
  geom_label(data = subset(data.BCA_total, Type == "OCO (Non-Defense)"), aes(label = sum, y=1500)
            ,size = 6, fontface = "bold", fill = "white", show.legend = F) +
  # stat_summary(fun.y = sum, aes(label = ..y.., group = Year),
  #              geom = "text", size = 6, fontface = "bold", vjust= -4)+   ####Adds total to top
  scale_fill_manual(name = "BA Type", values = c("OCO (Non-Defense)" = "#FFE4C8", "Non-Defense" = "#EC8418",
                                                          "OCO (Defense)" = "#CFE8FF", "Defense" = "#72B3F0"))+
  facet_grid(~`Budget Type`, labeller = label_wrap_gen(15), scales = "free_x", space = "free_x")+
  labs(x = NULL, y = "Dollar Amount (in Billions)", title = "BCA",
       subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 3, size = 24, face = "bold"),
        axis.ticks.x = element_blank(),
        strip.text = element_text(face = "bold", size = 20), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 17),
        axis.text = element_text(size =15),
        panel.spacing = unit(2, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        #legend.margin = margin(c(5, 5, 5, 0)),
        legend.spacing.x = unit(.5, "char"),
        legend.text = element_text(margin = margin(r = 7, unit = "pt")))+
  scale_y_continuous(limits = c(0, 1500))

plot

ggsave("BCA - Scenarios w OCO full.jpg", plot,
       width = 14, height = 7, units = "in")
