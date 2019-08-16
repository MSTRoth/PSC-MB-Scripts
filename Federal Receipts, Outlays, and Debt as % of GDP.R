library(colorspace)
library(readr)
library(RColorBrewer)
library(tidyverse)
options(scipen=999)

data <- read_csv("x:/1 Marielle Folder/Data Sets/Government-Wide Data/csv/To Build/receipts, outlays, etc as GDP.csv")

setwd("X:/1 Marielle Folder/Visualizations/Government-Wide")

data_gather <- data %>% 
  gather("Type", "total_value", `Total USG Receipts`:`Federal Debt Held by the Public`) 
  

data_gather$Type <- factor(data_gather$Type,
                           levels = c("Federal Debt Held by the Public","Total USG Receipts",
                                      "Total USG Outlays", "Mandatory Programs", 
                                      "Defense Discretionary","Nondefense Discretionary",
                                      "Net Interest"),
                           ordered = is.ordered(data_gather$Type))


###plot all up to current year####
current_yr <- data_gather %>% 
  filter(Type != "Federal Debt Held by the Public",
         Year %in% c(1965:2018))

plot_current_yr <- ggplot(current_yr, aes(color = Type,
                                           x = Year,
                                           y = total_value))+
  geom_line(size = 1.1, aes(linetype = Type))+
  scale_linetype_manual(values=c("Total USG Receipts" = "solid",
                                 "Total USG Outlays" = "solid", 
                                 "Mandatory Programs" = "dashed", 
                                 "Defense Discretionary" = "dotdash",
                                 "Nondefense Discretionary" = "dotdash",
                                 "Net Interest" = "dotted"))+
  scale_color_manual(values=c("Total USG Receipts" = "#000000",
                              "Total USG Outlays" = "#FF0303", 
                              "Mandatory Programs" = "#2179E7", 
                              "Defense Discretionary" = "#5DA677",
                              "Nondefense Discretionary" = "#611E9F",
                              "Net Interest" = "#FFB822"))+
  labs(y = "Total Value (as a percentage of GDP)", 
       title = "Federal Receipts and Outlays as a % of GDP (1965-2018)",
       caption = "Source: Congressional Budget Office (CBO), Updated Budget Projections: 2019 to 2029, May 2019",
       x = NULL)+
  scale_x_continuous(limits = c(1965, 2018), breaks = pretty(data_gather$Year, n = 64),
                     expand = c(0, 0))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.caption = element_text(vjust = -.1, hjust = 1, size = 8, face = "italic"),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 13),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 7, angle = 90),
        panel.grid.major.y = element_line(colour="#bcbcbc"),
        panel.grid.minor.y = element_line(colour="#bcbcbc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  guides(fill = FALSE)

plot_current_yr

### plot all up to current year plus 10 year projection (CBO)####
projection <- data_gather %>% 
  filter(Type != "Federal Debt Held by the Public")

plot_projection <- ggplot(projection, aes(color = Type,
                                          x = Year,
                                          y = total_value))+
  # geom_rect(aes(xmin = 2019, xmax = 2029, ymin = -Inf, ymax = Inf), alpha = 0.1, 
  #               fill = "#E5E5E5", color = NA)+
  geom_line(size = 1.1, aes(linetype = Type))+
  geom_vline(xintercept = 2019)+
  annotate("rect", xmin=2019, xmax=2029, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+ 
  scale_linetype_manual(values=c("Total USG Receipts" = "solid",
                                 "Total USG Outlays" = "solid", 
                                 "Mandatory Programs" = "dashed", 
                                 "Defense Discretionary" = "dotdash",
                                 "Nondefense Discretionary" = "dotdash",
                                 "Net Interest" = "dotted"))+
  scale_color_manual(values=c("Total USG Receipts" = "#000000",
                              "Total USG Outlays" = "#FF0303", 
                              "Mandatory Programs" = "#2179E7", 
                              "Defense Discretionary" = "#5DA677",
                              "Nondefense Discretionary" = "#611E9F",
                              "Net Interest" = "#FFB822"))+
  labs(y = "Total Value (as a percentage of GDP)", 
       title = "Federal Receipts and Outlays as a % of GDP (1965-2029)",
       caption = "Source: Congressional Budget Office (CBO), Updated Budget Projections: 2019 to 2029, May 2019",
       x = NULL)+
  scale_x_continuous(limits = c(1965, 2029), breaks = pretty(data_gather$Year, n = 64),
                     expand = c(0, 0))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.caption = element_text(vjust = -.1, hjust = 1, size = 8, face = "italic"),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 13),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 7, angle = 90),
        panel.grid.major.y = element_line(colour="#bcbcbc"),
        panel.grid.minor.y = element_line(colour="#bcbcbc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1))


### plot with debt  ####
plot_debt <- ggplot(data_gather, aes(color = Type,
                                 x = Year,
                                 y = total_value))+
  geom_line(size = 1.1, aes(linetype = Type))+
  geom_vline(xintercept = 2019)+
  annotate("rect", xmin=2019, xmax=2029, ymin=-Inf, ymax=Inf, alpha=0.2, fill="gray")+ 
  scale_linetype_manual(values=c("Federal Debt Held by the Public" = "solid",
                                 "Total USG Receipts" = "solid",
                                 "Total USG Outlays" = "solid", 
                                 "Mandatory Programs" = "dashed", 
                                 "Defense Discretionary" = "dotdash",
                                 "Nondefense Discretionary" = "dotdash",
                                 "Net Interest" = "dotted"))+
  scale_color_manual(values=c("Federal Debt Held by the Public" = "#364D70",
                              "Total USG Receipts" = "#000000",
                              "Total USG Outlays" = "#FF0303", 
                              "Mandatory Programs" = "#2179E7", 
                              "Defense Discretionary" = "#5DA677",
                              "Nondefense Discretionary" = "#611E9F",
                              "Net Interest" = "#FFB822"))+
  labs(y = "Total Value (as a percentage of GDP)", 
       title = "Federal Receipts, Outlays, and Debt as a % of GDP",
       caption = "Source: Congressional Budget Office (CBO), Updated Budget Projections: 2019 to 2029, May 2019",
       x = NULL)+
  scale_x_continuous(limits = c(1965, 2029), breaks = pretty(data_gather$Year, n = 64),
                     expand = c(0, 0))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.caption = element_text(vjust = -.1, hjust = 1, size = 8, face = "italic"),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 13),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 7, angle = 90),
        panel.grid.major.y = element_line(colour="#bcbcbc"),
        panel.grid.minor.y = element_line(colour="#bcbcbc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  guides(fill = FALSE)


###Zoomed in####
zoom <- data_gather %>% 
  filter(Year %in% c(2007:2018))

plot_zoom <- ggplot(zoom, aes(color = Type,
                              x = Year,
                              y = total_value))+
  geom_line(size = 1.1, aes(linetype = Type))+
  annotate(geom = "label", x=2009, y=18, label = "Financial Collapse")+
  annotate(geom = "label", x=2009, y=27, label = "TARP")+
  annotate(geom = "label", x=2011, y=7, label = "BCA")+
  scale_linetype_manual(values=c("Federal Debt Held by the Public" = "solid",
                                  "Total USG Receipts" = "solid",
                                 "Total USG Outlays" = "solid", 
                                 "Mandatory Programs" = "dashed", 
                                 "Defense Discretionary" = "dotdash",
                                 "Nondefense Discretionary" = "dotdash",
                                 "Net Interest" = "dotted"))+
  scale_color_manual(values=c("Federal Debt Held by the Public" = "#364D70",
                              "Total USG Receipts" = "#000000",
                              "Total USG Outlays" = "#FF0303", 
                              "Mandatory Programs" = "#2179E7", 
                              "Defense Discretionary" = "#5DA677",
                              "Nondefense Discretionary" = "#611E9F",
                              "Net Interest" = "#FFB822"))+
  labs(y = "Total Value (as a percentage of GDP)", 
       title = "Federal Receipts, Outlays, and Debt as a % of GDP (2007-2018)",
       caption = "Source: Congressional Budget Office (CBO), Updated Budget Projections: 2019 to 2029, May 2019",
       x = NULL)+
  scale_x_continuous(limits = c(2007, 2018), breaks = pretty(data_gather$Year, n = 64),
                     expand = c(0, 0))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
        plot.caption = element_text(vjust = -.1, hjust = 1, size = 8, face = "italic"),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 13),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        panel.grid.major.y = element_line(colour="#bcbcbc"),
        panel.grid.minor.y = element_line(colour="#bcbcbc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  guides(fill = FALSE)

plot_zoom

###Zoomed in####
zoom2 <- data_gather %>% 
  filter(Type != "Federal Debt Held by the Public",
         Year %in% c(2007:2018))

plot_zoom2 <- ggplot(zoom2, aes(color = Type,
                              x = Year,
                              y = total_value))+
  geom_line(size = 1.1, aes(linetype = Type))+
  annotate(geom = "label", x=2009, y=15.5, label = "Financial Collapse")+
  annotate(geom = "label", x=2009, y=25, label = "TARP")+
  annotate(geom = "label", x=2011, y=5.5, label = "BCA")+
  scale_linetype_manual(values=c(#"Federal Debt Held by the Public" = "solid",
    "Total USG Receipts" = "solid",
    "Total USG Outlays" = "solid", 
    "Mandatory Programs" = "dashed", 
    "Defense Discretionary" = "dotdash",
    "Nondefense Discretionary" = "dotdash",
    "Net Interest" = "dotted"))+
  scale_color_manual(values=c(#"Federal Debt Held by the Public" = "#364D70",
    "Total USG Receipts" = "#000000",
    "Total USG Outlays" = "#FF0303", 
    "Mandatory Programs" = "#2179E7", 
    "Defense Discretionary" = "#5DA677",
    "Nondefense Discretionary" = "#611E9F",
    "Net Interest" = "#FFB822"))+
  labs(y = "Total Value (as a percentage of GDP)", 
       title = "Federal Receipts, Outlays, and Debt as a % of GDP (2007-2018)",
       caption = "Source: Congressional Budget Office (CBO), Updated Budget Projections: 2019 to 2029, May 2019",
       x = NULL)+
  scale_x_continuous(limits = c(2007, 2018), breaks = pretty(data_gather$Year, n = 64),
                     expand = c(0, 0))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(plot.title = element_text(hjust = 0.5, size = 26, face = "bold"),
        plot.caption = element_text(vjust = -.1, hjust = 1, size = 8, face = "italic"),
        axis.ticks.x = element_line(),
        panel.background = element_blank(),
        plot.background = element_rect(colour = NA, fill = NA),
        axis.title = element_text(face = "bold",size = 13),
        axis.title.y = element_text(angle=90,vjust =2),
        axis.line = element_line(colour="#bcbcbc"),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size = 11, face = "bold"),
        axis.text.x = element_text(size = 11),
        panel.grid.major.y = element_line(colour="#bcbcbc"),
        panel.grid.minor.y = element_line(colour="#bcbcbc"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.width = unit(1, "cm"),
        legend.title = element_blank(),
        strip.background=element_rect(colour="#bcbcbc",fill="#f0f0f0"),
        strip.text.x = element_text(face="bold", size = 13),
        panel.border = element_rect(color = "#bcbcbc", fill = NA, size = 1)
  )+
  guides(fill = FALSE)

plot_zoom2



 
plot_current_yr
plot_projection
plot_debt

ggsave("Reciepts and Outlay to current year.jpg", plot_current_yr,
       width = 13, height = 6.5, units = "in")

ggsave("Reciepts and Outlay to projection.jpg", plot_projection,
       width = 13, height = 6.5, units = "in")

ggsave("Reciepts, Outlay and debt to projection.jpg", plot_debt,
       width = 13, height = 6.5, units = "in")

ggsave("Reciepts, Outlay and debt Zoom.jpg", plot_zoom,
       width = 13, height = 6.5, units = "in")

ggsave("Reciepts, Outlay and debt Zoom2.jpg", plot_zoom2,
       width = 13, height = 6.5, units = "in")
