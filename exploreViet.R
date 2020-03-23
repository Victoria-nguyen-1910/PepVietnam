library (readxl)
library (ggplot2)
library(expss)
library(stringr)
library(stringi)
library(extrafont)
library(extrafontdb)
library(dplyr)
library(treemap)
library(treemapify)
##install.packages("corrplot")
library(corrplot)
##install.packages("Hmisc")
library(Hmisc)
library(RColorBrewer)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(circlize)
#install.packages("circlize")
#install.packages("gplots")
library("gplots")

## import dataset
data1 <- read_excel("D:/OneDrive - CGIAR/Pepsico/Pepsico exploratory Vie/Tracking DS crop 2018-2019_HangReuben 1.xlsx", 
              sheet = "all", col_types = c("numeric", 
                                               "text", "text", "text", "text", "numeric", 
                                               "text", "text", "numeric", "text", 
                                               "text", "numeric", "numeric", "date", 
                                               "date", "numeric", "date", "numeric", 
                                               "numeric", "text"))
#### convert all data to ton and ha and make new variable
data <- mutate(data1, volume = PC_size_kg/1000,
               land_loss1 = contract_area - planting_area,
               land_loss2 = planting_area - harvested_area,
               land_loss_total = contract_area - harvested_area,
               land_loss_sum = contract_area - harvested_area,
               season_duration=harvesting_date-planting_date,
               germination_duration = emergence_date-planting_date)
data <- mutate(data, seed_rate= 
                 ifelse(price_vnd == 8000, 	32600000, 
                        ifelse(price_vnd == 9000, 57000000, NA)))
## this is assumption of seed rate based on seed rate of Duc Trong region

data$land_loss_sum[which(data$land_loss_sum != 0)] = "Loss"
data$land_loss_sum[which(data$land_loss_sum == 0)] = "No Loss"

View(data)
str(data)

### compute summary table
df1 <- group_by(data,potato_region) %>% 
  summarise(avg_yield = mean(yield, na.rm = TRUE))

df2 <- group_by(data,potato_region) %>% 
  count(potato_region)
df1$Farm_no <- df2$n

df3 <- data %>% group_by(potato_region) %>% 
  summarise(total_area = sum(contract_area))

# summery table to excel
write.csv(df1,"dataframe1.csv",row.names = F)
##################

### combine plot
g1 <- ggplot(data = df1, aes(x=potato_region)) +
  geom_bar(stat="identity", aes(y=Farm_no))
g2 <- df1%>% ggplot(aes(x=potato_region)) +
                      geom_line(aes(y=yield))
###############

#####bar graph of average yield                   
df1 %>% ggplot() +
  geom_col(aes(x=potato_region, y =Farm_no)) +
  geom_line(aes(x=potato_region, y= avg_yield*4, group = 1))+
  scale_y_continuous(sec.axis = sec_axis(~./4, name = "Yield by region"))

df1 %>% ggplot(aes (x = potato_region, y = Farm_no)) +
  geom_col()

jpeg( filename="yieldregion.jpeg", width=8, height=5, units="in", res=500)  
ggplot(data, aes(x=potato_region, y = yield)) +
  stat_summary(fun.y ="mean",geom = "bar", fill = "darkgreen", na.rm= T) +
  labs(y = "Yield (ha)", x = "Region", title = "Average yield by region")+
  theme_classic() +
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3,
               vjust = -0.5, na.rm= T) #data label with the start_summary function
dev.off()  

### treemap with ggplot of planting area with comnic sans font

ggplot(df3, 
       aes(fill = potato_region, 
           area = total_area, label = potato_region)) +
  geom_treemap() +
  scale_fill_brewer(palette="Set3")+
  theme(legend.position = "none", text = element_text(size = 16, family = "Comic Sans MS")) + 
  geom_treemap_text(colour = "black", 
                    place = "centre")+
  labs(title = "Total planting area by region",
       subtitle = "(Contract area - Ha)" ) 

## treemap with treemap package
jpeg( filename="treemap area.jpeg", width=9, height=5, units="in", res=500) 
treemap(data,
        index="potato_region",
        vSize="contract_area",
        type="index",                
        fontcolor.labels="white",
        border.col="white",
        fontface.labels=2,
        bg.labels=c("transparent"),
        title="Total contract area by region",
        fontsize.title=20,
        palette = "Set2",
        fontsize.labels = 14)
dev.off()

jpeg( filename="treemap area soil.jpeg", width=9, height=5, units="in", res=500) 
treemap(data,
        index=c("potato_region","soil_type"),
        vSize="contract_area",
        type="index",
        fontsize.labels=c(14,11),                
        fontcolor.labels="white",
        border.col="white",
        fontface.labels=c(2,4),
        bg.labels=c("transparent"),
        title="Soil type of PepsiCo contract area",
        fontsize.title=20,
        palette = "Set2",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ))
dev.off()

jpeg( filename="treemap area irr.jpeg", width=9, height=5, units="in", res=500) 
treemap(data,
        index=c("potato_region","irrigation_system"),
        vSize="contract_area",
        type="index",
        fontsize.labels=c(14,11),                
        fontcolor.labels="white",
        border.col="white",
        fontface.labels=c(2,4),
        bg.labels=c("transparent"),
        title="Irrigation system of PepsiCo contract area",
        fontsize.title=20,
        palette = "Set2",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ))
dev.off()

jpeg( filename="treemap area var.jpeg", width=9, height=5, units="in", res=500)
treemap(data,
        index=c("potato_region","variety"),
        vSize="contract_area",
        type="index",
        fontsize.labels=c(14,11),                
        fontcolor.labels="white",
        border.col="white",
        fontface.labels=c(2,4),
        bg.labels=c("transparent"),
        title="Variety of PepsiCo contract area",
        fontsize.title=20,
        palette = "Set2",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ))
dev.off()

jpeg( filename="treemap area price.jpeg", width=9, height=5, units="in", res=500)
treemap(data,
        index=c("potato_region","price_vnd"),
        vSize="contract_area",
        type="index",
        fontsize.labels=c(14,11),                
        fontcolor.labels="white",
        border.col="white",
        fontface.labels=c(2,4),
        bg.labels=c("transparent"),
        title="Sourcing price (VND) of PepsiCo contract area",
        fontsize.title=20,
        palette = "Set2",
        align.labels=list(
          c("center", "center"), 
          c("right", "bottom")
        ))
dev.off()

## landloss
jpeg( filename="landloss.jpeg", width=6, height=4, units="in", res=500) 
ggplot(data, aes(y=land_loss_total, x = potato_region)) + 
  geom_point(color = "darkred", size = 3) + 
  labs(y = "Land loss (ha)", x = "Region", title = "Total land loss by region")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off() 
jpeg( filename="landloss1.jpeg", width=6, height=4, units="in", res=500) 
ggplot(data, aes(y=land_loss1, x = potato_region)) +
  geom_point(color = "darkred", size = 3)+
  coord_cartesian(ylim = c(0, 0.70))+
  labs(y = "Land loss (ha)", x = "Region", title = "Land loss between contracted area and planted area")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()
jpeg( filename="landloss2.jpeg", width=6, height=4, units="in", res=500) 
ggplot(data, aes(y=land_loss2, x = potato_region)) + 
  geom_point(color = "darkred", size = 3) + 
  labs(y = "Land loss (ha)", x = "Region", title = "Land loss between planted area and harvested area")+
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


jpeg( filename="landlossproportion.jpeg", width=6, height=4, units="in", res=500)
ggplot(data, aes(y=contract_area, x=potato_region, fill= land_loss_total )) + 
  geom_col() + theme_classic() +   
   scale_fill_gradient(high="darkred", low="grey")+
  labs(y= "Contract area (ha)", x = "Region",
  title = "Land loss by region", fill= "Land loss") +
  theme(plot.title = element_text(hjust = 0.5))
       
dev.off()

##correlation matrix, heatmap, cor plot
my_data <- data[, c(6,9,12,19,21,26,27,28)]

my_data <- transform(my_data, season_duration = as.numeric(season_duration),
          germination_duration = as.numeric(germination_duration))
str(my_data)
M <- cor(my_data, use = "complete.obs")
jpeg( filename="corplot.jpeg", width=10, height=6, units="in", res=500)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
dev.off()

jpeg( filename="matrixheatmap.jpeg", width=10, height=6, units="in", res=500)
col<- colorRampPalette(c("steelblue", "white", "darkred"))(20)
heatmap(x = M, col = col, symm = TRUE)
dev.off()

jpeg( filename="matrix.jpeg", width=9, height=5, units="in", res=500)
chart.Correlation(my_data, histogram=F, pch=19)
dev.off()

##only keep scatter plot of correlated variable
jpeg( filename="scat1.jpeg", width=8, height=6, units="in", res=500)
ggplot(my_data, aes(x=season_duration, y=yield)) + geom_point() +
  geom_smooth(se = F)+
  labs(y="Yield(ton/ha)",x="Season Length",
       title= "Relationship between yield and season length")
dev.off()  
jpeg( filename="scat11.jpeg", width=8, height=6, units="in", res=500)
ggplot(my_data, aes(x=factor(seed_rate), y=yield)) + geom_point() +
  geom_boxplot()+
  labs(y="Yield(ton/ha)",x="Seed rate",
       title= "Relationship between yield and seed rate")       
dev.off()       
###box plot yield
jpeg( filename="yieldregion1.jpeg", width=8, height=5, units="in", res=500)
ggplot(data, aes(x=potato_region, y=yield, fill=potato_region)) +
  geom_boxplot()+ geom_point(color = "#A9A9A9", alpha = 1/2)+
  labs(title = "Yield (ton/ha) by region") +
  labs(x = "Region", y= "Yield (ton/ha)" ) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color = "black") +
  theme(legend.position = "none") + scale_fill_brewer(palette="Set3")+
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3.5,
               vjust = -0.7,fontface=2, na.rm= T)
dev.off()

jpeg( filename="yieldsoil.jpeg", width=5, height=5, units="in", res=500)
ggplot(data, aes(x=soil_type, y=yield, fill=soil_type)) +
  geom_boxplot()+ geom_point(color = "#696969", alpha = 1/2)+
  labs(title = "Yield (ton/ha) by soil type") +
  labs(x = "Soil Type", y= "Yield (ton/ha)" ) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color = "black") +
  theme(legend.position = "none", plot.title = element_text(size=20)) + 
  scale_fill_brewer(palette="Set3")+
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3.5,
               vjust = -0.7,fontface=2, na.rm= T)
dev.off()

## replace faulty data in irrigation system column
data$irrigation_system[which(data$irrigation_system == "Irrigation System")] = "NA"
data$irrigation_system[which(data$irrigation_system == "Irrigation")] = "NA"

jpeg( filename="yieldirri.jpeg", width=5, height=5, units="in", res=500)

ggplot(data, aes(x=irrigation_system, y=yield, fill=irrigation_system)) +
  geom_boxplot() + 
  geom_point(color = "#696969", alpha = 1/2)+
  labs(title = "Yield (ton/ha) by irrigation system") +
  labs(x = "Irrigation system", y= "Yield (ton/ha)" ) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color = "black") +
  theme(legend.position = "none", plot.title = element_text(size=20)) + 
  scale_fill_brewer(palette="Set3") + 
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3.5,
               vjust = -0.7,fontface=2, na.rm= T)
dev.off()

data$variety[which(data$variety == "Atlantic")] = "ATLANTIC"
jpeg( filename="yieldvar.jpeg", width=5, height=5, units="in", res=500)
ggplot(data, aes(x=variety, y=yield, fill=variety)) +
  geom_boxplot()+ geom_point(color = "#696969", alpha = 1/2)+
  labs(title = "Yield (ton/ha) by variety") +
  labs(x = "Variety", y= "Yield (ton/ha)" ) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color = "black") +
  theme(legend.position = "none", plot.title = element_text(size=20)) + 
  scale_fill_brewer(palette="Set3")+
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3.5,
               vjust = -0.7,fontface=2, na.rm= T)
dev.off()

jpeg( filename="yieldprice.jpeg", width=5, height=5, units="in", res=500)
ggplot(data, aes(x=factor(price_vnd), y=yield, fill=factor(price_vnd))) +
  geom_boxplot()+ geom_point(color = "#696969", alpha = 1/2)+
  labs(title = "Yield (ton/ha) by price") +
  labs(x = "Price (VND)", y= "Yield (ton/ha)" ) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color = "black") +
  theme(legend.position = "none", plot.title = element_text(size=20)) + 
  scale_fill_brewer(palette="Set3")+
  stat_summary(aes(label=round(..y..,2)), fun.y=mean, geom="text", size=3.5,
               vjust = -0.7,fontface=2, na.rm= T)
  
dev.off()

#plant date vs yield
quantile(data$planting_date)
segmentdata <- read_excel("D:/OneDrive - CGIAR/Pepsico/Pepsico exploratory Vie/Tracking DS crop 2018-2019_HangReuben 1.xlsx", 
                          sheet = "all (2)", col_types = c("numeric", 
                                                           "text", "text", "text", "text", "numeric", 
                                                           "text", "text", "numeric", "text", 
                                                           "text", "numeric", "numeric", "date", 
                                                           "date", "numeric", "date", "numeric", 
                                                           "numeric", "text", "text"))
segmentdata <- mutate(segmentdata, volume= PC_size_kg/1000)
jpeg( filename="segment1.jpeg", width=10, height=5, units="in", res=500)
ggplot(segmentdata, aes(x=segment, y=yield, fill= segment)) + 
  geom_boxplot()+ stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  scale_fill_brewer(palette="Blues")+theme_classic()+
  labs(y = "Yield (ton/ha)", x="Planting date", title="Vietnam-Yield by plant date 2019" ) 
dev.off()

jpeg( filename="segment11.jpeg", width=8, height=5, units="in", res=500)
ggplot(segmentdata, aes(x=yield, y=planting_date))+
  geom_boxplot(fill = "lightblue")+
  geom_point(color= "darkblue")+
  coord_flip()+theme_classic()+
  labs(x = "Yield (ton/ha)", y="Planting date", title="Vietnam-Yield by plant date 2019" ) 
dev.off()

jpeg( filename="segment111.jpeg", width=8, height=5, units="in", res=500)
ggplot(segmentdata, aes(x=potato_region, y=planting_date))+
  geom_boxplot(fill = "lightblue")+
  geom_point(color= "darkblue")+
  theme_classic()+coord_flip()+
  labs(x = "Region", y="Planting date", title="Vietnam-Planting date by regions 2019" )
dev.off()

## ductrong seed and fertilizer data
ductrong<- read_excel("D:/OneDrive - CGIAR/Pepsico/Pepsico exploratory Vie/Tracking DS crop 2018-2019_HangReuben 1.xlsx", 
                      sheet = "Ductrong", col_types = c("numeric", 
                                                        "text", "text", "text", "text", "numeric", 
                                                        "text", "text", "numeric", "text", 
                                                        "numeric", "numeric", "numeric", 
                                                        "text", "numeric", "numeric", "date", 
                                                        "date", "numeric", "date", "numeric", 
                                                        "numeric", "text"))
ductrong <- mutate(ductrong,volume = PC_size_kg/1000,
                   season_duration=harvesting_date-planting_date,
                   germination_duration = emergence_date-planting_date,
                   seed_rate=Seed/contract_area,
                   fertilizer_rate=Fertilizer/contract_area)
##Note: unit of seed and fertilizer is unknown
ductrong <- transform(ductrong, season_duration = as.numeric(season_duration),
                     germination_duration = as.numeric(germination_duration))
my_data1 <- ductrong[, c(6,9,15,22,25,26,27,28)]

jpeg( filename="matrixductrong.jpeg", width=10, height=5, units="in", res=500)
chart.Correlation(my_data1, histogram=F, pch=19)
dev.off()

### price yield variety scatter
ggplot(data, aes(color=price_vnd, y=yield, x = variety)) +geom_point()


