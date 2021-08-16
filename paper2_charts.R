setwd('C:/Users/dsmet/OneDrive/Desktop/MABA Program/Coursework/MABA6121 Practical Stats for Business Applications/data_files')

library(readxl)
library(pastecs)
library(ggplot2)
library(dplyr)

#Read in the data
library(readxl)
moose_associates <- read_excel("Paper2.xlsx", na="NA", col_names = TRUE)

#Convert Region to a factor variable
region <- factor(moose_associates$Region)

#Attach the dataframe for future use
attach(moose_associates)

#Region subset dataframes
moose_region1 <-subset(moose_associates, Region == 1)
moose_region2 <-subset(moose_associates, Region == 2)
moose_region3 <-subset(moose_associates, Region == 3)


linefit_offices <-lm(Revenue ~ Offices)
linefit_offices_poly <- lm(Revenue ~ Offices+ I(Offices^2))
linefit_partners <- lm(Revenue ~ Partners)
linefit_employees <- lm(Revenue ~ Employees)
linefit_mult_reg2 <- lm(Revenue ~ Offices + Partners + Employees + (region_factor ==1) +(region_factor==3))
linefit_multso_reg2 <- lm(Revenue ~ Offices + I(Offices^2) + Partners + Employees + (region_factor ==1) +(region_factor==3))



#Linear Models with fitted regression lines
# scatter plot w/ fitted linear regression line
#Revenue and Offices
ggplot(moose_associates, aes(x=Offices, y=Revenue)) +geom_point(size = 3, color = "#940303") +geom_smooth(method = lm, se=FALSE, color = "#940303") +
  geom_smooth(method = loess, se=FALSE, color = "#FADA0F") + ggtitle("Relationship between # of Offices and Revenue") + 
  xlab("# of Offices") + ylab("Revenue (US $million)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Revenue and Partners
ggplot(moose_associates, aes(x=Partners, y=Revenue)) +geom_point(size = 3, color = "#940303") +geom_smooth(method=lm, se=FALSE, color = "#940303") +
  ggtitle("Relationship between # of Partners and Revenue") +
  xlab("# of Partners") + ylab("Revenue (US $million)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#Revenue and Employees
ggplot(moose_associates, aes(x=Employees, y=Revenue)) +geom_point(size = 3, color = "#940303") +geom_smooth(method=lm, se=FALSE, color = "#940303") +
    ggtitle("Relationship between # of Employees and Revenue") +
  xlab("# of Employees") + ylab("Revenue (US $million)") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#aggregate Revenue, Offices, Employees and Partners trend data sorted on Region 
#Revenue
ggplot(moose_associates, aes(x = Revenue, color = region, fill=region)) + geom_histogram(aes(y=..density..), alpha=0.5, 
                                                                           position=position_dodge(), binwidth = 12)+ 
  scale_color_manual(values = c("#FADA0F", "#940303", "#AFAFAF")) +
  scale_fill_manual(values = c("#FADA0F","#940303", "#AFAFAF")) + ggtitle("Revenue Data Summary") +
  xlab("Revenue (US $million)") + ylab("Density") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#Offices
ggplot(moose_associates, aes(x = Offices, color = region, fill=region)) + geom_histogram(aes(y=..density..), alpha=0.5, 
                                                                                                       position=position_dodge(), binwidth = 3)+ 
  scale_color_manual(values = c("#FADA0F", "#940303", "#AFAFAF")) +
  scale_fill_manual(values = c("#FADA0F","#940303", "#AFAFAF")) + ggtitle("Offices Data Summary") +
  xlab("# of Offices") + ylab("Density") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

#Partners
ggplot(moose_associates, aes(x = Partners, color = region, fill=region)) + geom_histogram(aes(y=..density..), alpha=0.5, 
                                                                                                       position=position_dodge(), binwidth = 10)+ 
  scale_color_manual(values = c("#FADA0F", "#940303", "#AFAFAF")) +
  scale_fill_manual(values = c("#FADA0F","#940303", "#AFAFAF")) + ggtitle("Partners Data Summary") +
  xlab("# of Partners") + ylab("Density") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Employees
ggplot(moose_associates, aes(x = Employees, color = region, fill=region)) + geom_histogram(aes(y=..density..), alpha=0.5, 
                                                                                                        position=position_dodge(), binwidth = 75)+ 
  scale_color_manual(values = c("#FADA0F", "#940303", "#AFAFAF")) +
  scale_fill_manual(values = c("#FADA0F","#940303", "#AFAFAF")) + ggtitle("Employees Data Summary") +
  xlab("# of Employees") + ylab("Density") + guides(title = "Region") + theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))