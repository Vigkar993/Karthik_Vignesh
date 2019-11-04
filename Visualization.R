#Importing data
library(tidyverse)
#dat<-read_csv()
#to see an example of a full path on your system type the following
system.file(package = "dslabs")
wd<-getwd()
library(readr)
#to read excel files
library(readxl)
#introduction to visualization
#interactive learning https://shiny.rstudio.com/tutorial/

#GGplot2
library(dplyr)
library(ggplot2)
library(dslabs)
data(murders)
murders %>% ggplot() + geom_point(aes(x=population/10^6,y=total))
#or
p<-ggplot(data=murders)
p+geom_point(aes(population/10^6,total))
#layers
p+geom_point(aes(population/10^6,total))+
  geom_text(aes(population/10^6,total,label=state))
p+ geom_text(aes(population/10^6,total,label=state))
p+ geom_text(aes(population/10^6,total),label="state")
#here size is not a mapping:whereas mappings use data from specific observation and need to in aes()
#operations we want to affect all the points the same way do not need to be included in aes
p+geom_point(aes(population/10^6,total),size=5)+ geom_text(aes(population/10^6,total,label=abb),nudge_x = 1)
#nudge helps in moving x /y side ways

#global versus local aesthetic mapping
args(ggplot)
#mutilple mapping can be avoided by global mapping ie mapping in ggplot
p<-murders %>% ggplot(aes(population/10^6,total,label=abb))
p+geom_point(size=3)+geom_text(nudge_x = 1.5)

#Scales
p+geom_point(size=3)+geom_text(nudge_x = 0.5)+
  scale_x_continuous(trans = "log10")+scale_y_continuous(trans="log10")
#the particular transformation is so common that ggplot2 provides a specialized function
#scale_x_log10 and scale_y_log10
p<-p+geom_point(size=3)+geom_text(nudge_x = 0.5)+scale_x_log10()+scale_y_log10()
#titles
p+geom_point(size=3)+geom_text(nudge_x = 0.5)+scale_x_log10()+scale_y_log10()+
  xlab("x-axis title")+ylab("y axis title")+ggtitle("plot title")
#categories as colors
p+geom_point(size=3,color="Blue")+geom_text(nudge_x = 0.5)+scale_x_log10()+scale_y_log10()+
  xlab("x-axis title")+ylab("y axis title")+ggtitle("plot title")
p+geom_point(size=3,aes(color=region))+geom_text(nudge_x = 0.5)+scale_x_log10()+scale_y_log10()+
  xlab("x-axis title")+ylab("y axis title")+ggtitle("plot title")
#annotations ,shapes and adjustmnets '
#when we want to represent avg murder rate ,we calculate rate  then

r<-murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% pull(rate)
q<-p+geom_point(size=3)+geom_text(nudge_x = 0.5)+scale_x_log10()+scale_y_log10()
s<-p+geom_abline(intercept = log10(r),lty=2,color="darkgrey")+geom_point(aes(col=region),size=3)
#add on packages
# we use a function in dslabs that automatically sets a default themes
ds_theme_set()
#many other themes are added using ggthemes package 
install.packages("ggthemes")
library(ggthemes)
s+theme_economist()
#putting it all together 
library(ggthemes)
install.packages("ggrepel")
library(ggrepel)
r<-murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% pull(rate)
r
murders %>% ggplot(aes(population/10^6,total,label=abb))+
  geom_abline(intercept = log10(r),lty=2,color="darkgreen")+
  geom_point(aes(col=region),size=3)+geom_text_repel()+scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions(log scale)")+
  ylab("population in miilions(log scale)")+
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name="Region")+
  theme_light()
  #theme_economist()
?ggthemes
#quick plot with qplots()
#to make quick plots of a values in a vector ,
#a scatter plot of two values in two vectors
#a boxplot of categorial or numerical vectors
data("murders")
x<-log10(murders$population)
y<-murders$total
data.frame(X=x,Y=y) %>% ggplot(aes(x,y))+geom_point()
#with q plot its simple
qplot(x,y)
#Grid of plots
#to plot graphs next to each other gridExtra package permits us to do that
install.packages("gridExtra")
library(gridExtra)
p1<-murders %>% 
  mutate(rate=total/population*10^5) %>%
  filter(population<2*10^6) %>%
  ggplot(aes(population/10^6,rate,label=abb))+
  geom_text()+ ggtitle("Small States")
p2<-murders %>% 
  mutate(rate=total/population*10^5) %>%
  filter(population>10*10^6) %>%
  ggplot(aes(population/10^6,rate,label=abb))+
  geom_text()+ ggtitle("Large States")
grid.arrange(p1,p2,ncol=2)
#unit 9
#ordered categorical data is refered to as ordinal data
#numerical data can be further divided inyo continuous and discrete
# case study:describing student heights
library(tidyverse)
library(dslabs)
data(heights)
head(heights)
#similar to what a ffrequency table does for categorical data ,the cdf defines the distribution for numerical data
#for numerical data -histograms wih non overlapping bins
#smooth density plots are more appealing that histogram
#smooth density plots do not have sharp edges at interval boundaries and many local peaks have been removeds
#scale of y axis chaged from counts to density
#smooth density is basically the curve that goes through the top of histogram bars when the bins are very small
#histogram is assumption free, smooth density is based on assumptions
#another advantage of  smooth density plots over histograms is that densities make it easier to 
#compare two distributions
#eCDF
#Histograms 
index<-heights$sex=='Male'
index
x<-heights$height[index ]
m<-mean(x)
s<-sd(x)
c(average=m,sd=s)
#in r we can obtain standard units using function scale(x)
z<-scale(x)
#now to see how many men are within 2 sd's from average we simply type:
mean(abs(z)<2)

#Qunatile -Quantile plots
#qq plots answer data which distribution  is a better fit
# qq plots are used to check how similar is two dataset distributed 
 library(tidyverse)
heights %>% filter(sex=='Male') %>% ggplot(aes(sample=scale(height)))+
  geom_qq() +
  geom_abline()
#percentiles
 #quantiles and percentiles-youtube-statquest
#box plot-quartiles 
#stratification
#Barplots
library(dslabs)
murders %>% ggplot(aes(region))+geom_bar()

#default is to count the number of each and draw bar
data(murders)
tab<-murders %>% count(region) %>% mutate(proportions=n/sum(n))
tab
tab %>% ggplot(aes(region,proportions))+geom_bar(stat='Identity')
# we no lom=nger want geom_bar to count , but rather just plot to the height provided by the proportions variable 
#for this we need to provide x(the categories) and y (the values ) and use the stat='identity' option

#Histogram
heights %>% filter(sex=='Female') %>%
  ggplot(aes(height))+
  geom_histogram()
#adding binwidth #color #border
heights %>% filter(sex=='Female') %>%
  ggplot(aes(height))+
  geom_histogram(binwidth = 1,fill="blue",color="black")+
  xlab("Male heights in inches")+
  ggtitle("Histogram")
#Density Plots
#to create smooth density we use geom_density 
heights %>% filter(sex=='Female') %>%
  ggplot(aes(height))+
  geom_density()
#fill color
heights %>% filter(sex=='Female') %>%
  ggplot(aes(height))+
  geom_density(fill='blue')
# to change smoothness of the density , we use adjust '
#adjust arguemnt to multiple the default value
heights %>% filter(sex=='Female') %>%
  ggplot(aes(height))+
  geom_density(fill='blue',adjust=0.5)
#qq plots
heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=height))+
  geom_qq()
#by default ,the sample variable is compared toa normal distribution with average 0 and sd=1
#to change this we use dparams argument to help file. 

heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=height))+
  geom_qq(dparams ='params')+
  geom_abline()

#another option is to scale the data first and then make qqplot agaist standard normal 
heights %>% 
  filter(sex=='Male') %>%
  ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline()

#Images 
x<-expand.grid(x=1:12,y=1:10) %>% 
  mutate(z=1:120)
x %>% ggplot(aes(x,y,fill=z))+
  geom_raster()
#with these images you want to change color scale .This can be done through scale_fil_gradientn layer
x %>% ggplot(aes(x,y,fill=z)) +geom_raster()+
  scale_fill_gradientn(colors=terrain.colors(10))

#quick plots
x<-heights %>% 
  filter(sex=='Male') %>%
  pull(height)
qplot(x)
# to make a quick qq plot you have to use a sample argument 

qplot(sample=scale(x))+geom_abline()
#data frame is not the first argument in qplot so we use a dot operator to map data.see dot operator

heights %>% qplot(sex,height,data=.)

#adding geom()
heights %>% qplot(sex,height,data=.,geom='boxplot')
qplot(x,geom='density')
qplot(x,bins=15,color=I('black'),xlab="Population")
#the reason function I is used is beacause we want qplot to treat black as character rather than convert to factor
#which is default beg=havior in aes. 
#in r function i is used to to"keep as it is
#Chapter 10
#case studies : new insight on poverty
library(tidyverse)
library(dslabs)
library(gapminder)
gapminder %>% as_tibble()
gapminder %>% 
  filter(year==2015 & country %in% c('Sri Lanka','Turkey')) %>%
  select(country,infant_mortality) %>% arrange(desc(infant_mortality))
gapminder %>% 
  filter(year==2015,infant_mortality!="NA") %>%
  select(country,infant_mortality,continent) %>% arrange(desc(infant_mortality)) %>% head(10)
#scatterplots
#life expectancy vs fertility rates(average no of children per women) in 1962
filter(gapminder,year==1962) %>%
  ggplot(aes(fertility,life_expectancy))+
  geom_point()
#region
filter(gapminder,year==1962) %>%
  ggplot(aes(fertility,life_expectancy,color=continent))+
  geom_point()
#comparing 1962 and 2012
#Faceting
filter(gapminder,year %in% c(1962,2012)) %>%
  ggplot(aes(fertility,life_expectancy,color=continent))+
  geom_point()+ facet_grid(continent~year)
filter(gapminder,year %in% c(1962,2012)) %>%
  ggplot(aes(fertility,life_expectancy,color=continent))+
  geom_point()+ facet_grid(~year)
#facet wrap
years<-c(1962,1980,1990,2000,2012)
continents<-c("Europe","Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) +
  geom_point()+facet_wrap(~year)
#Time series plots 
gapminder %>% 
  filter(country=='United States') %>% 
  ggplot(aes(year,fertility))+geom_point()
gapminder %>% 
  filter(country=='United States') %>% 
  ggplot(aes(year,fertility))+geom_line()
countries<-c('India','Pakistan')
filter(gapminder,country %in% countries) %>%
  ggplot(aes(year,fertility))+
  geom_line()
#here both points for two countries were joined 
#to aviod
countries<-c('South Korea','Germany')
gapminder %>% filter(country %in% countries & !is.na(fertility)) %>%
  ggplot(aes(year,fertility,group= country))+
  geom_line()
filter(gapminder,country %in% countries & !is.na(fertility)) %>%
  ggplot(aes(year,fertility,group=country))+
  geom_line()
#useful side effect of using color is that the countries are automatically grouped
filter(gapminder,country %in% countries & !is.na(fertility)) %>%
  ggplot(aes(year,fertility,col=country))+
  geom_line()
#labels instead of legends 
#for trend plots its better to label the plots than using legends 
labels<-data.frame(country=countries,x=c(1975,1965),y=c(60,70))
gapminder %>% filter(country %in% countries) %>% 
  ggplot(aes(year,life_expectancy,col=country))+
  geom_line()+
  geom_text(data=labels,aes(x,y,label=country))+
  theme(legend.position = "none")
?geom_text
#Data Transformations
gapminder<-gapminder %>% mutate(dollars_per_day=gdp/population/365)
gapminder %>%  head()
#log transformation
past_year<-1970
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day))+ 
  geom_histogram(binwidth = 1,color='black')
past_year<-1970
#applying log2 transformations 
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1,color='black')
#which base 
#log 2
#base e(natural log) and base 10
#natural log is not recommended 
#log 2 or log 10 depending on the ranges
gapminder %>% filter(year==past_year) %>%
  summarise(min=min(population),max=max(population))
#log10 is better for ranges like this
filter(gapminder,year==past_year) %>%
  ggplot(aes(log10(dollars_per_day)))+
  geom_histogram(binwidth = 0.5,color='black')
#transform the values or scale 
#there are two ways we can use log transormation in plots
#1we cann log the values before plotting them'
#2 use log scales in the axis
#if we log the data we can more easily interpret immediate vales 
#the advantage of using logged scales is that we see original values on axes
#if we want to scale the axis we can use scale_x function for log 10 scale_x_log10()
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color='black')+
  scale_x_continuous(trans="log2")
#logging data
past_year<-1970
#applying log2 transformations 
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) + 
  geom_histogram(binwidth = 1,color='black')
#the above distribution has multiple modes at 4 and at 32,we call these local modes
# other argument are availabe through trans argument 
#sqrt transformation is useful when considering counts
#logistic transformation is useful when plotting proportions between 0 and 1
#reverse transformation is useful when we want smaller values to be on top
# visualizing multimodal distribution
# the above histogram shows the distribution has two modes,income values shows dichotomy 
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(region,dollars_per_day)) + 
  geom_point()
#rotating axis labels 
#we can rotate axis with coord_flip
#or we can roate labels by changing the theme thhrough element_text
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(region,dollars_per_day)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle=90,hjust=1))
# the graphs was reordered alphabetically .DO NOT DO THAT 
#reorder function helps to order by  some strata
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  mutate(region=reorder(region,dollars_per_day,FUN = median)) %>%
  ggplot(aes(region,dollars_per_day)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle=90,hjust=1))
#log transformation
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>%
  mutate(region=reorder(region,dollars_per_day,FUN = median)) %>%
  ggplot(aes(region,dollars_per_day)) + 
  geom_point()+
  theme(axis.text.x = element_text(angle=90,hjust=1))+
  scale_y_continuous(trans="log2")
#boxplots
?case_when()
gapminder<-gapminder %>%
  mutate(group=case_when(
    region %in% c('Western Europe','Northern Europe','Western Europe',
                  'Northern America','Australia and New Zealand') ~ 'West',
    region %in% c('Eastern Asia','South-Eastern Asia') ~'East Asia',
    region  %in% c('Caribbean','Central America','South America') ~ 'Latin America',
    continent == 'Africa' & region!='Northern Africa'~'Sub-Saharan-Africa',
    TRUE~'Others'))
gapminder %>% mutate(group=factor(group,
                                  levels=c('Others','Latin America','East Asia','West')))
summary(gapminder)
levels(gapminder$region)
#comparing multiple distribution
p<-gapminder %>%
  filter(year==past_year & !is.na(gdp)) %>%
  ggplot(aes(group,dollars_per_day))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 90,hjust = 1))+
  scale_y_continuous(trans='log2')
p  
p+geom_point(alpha=0.5)
head(gapminder)
#ridge plots
install.packages('ggridges')
library(ggridges)

gapminder %>%
  filter(year==past_year& !is.na(dollars_per_day))%>%
  ggplot(aes(dollars_per_day,group))+
  scale_x_continuous(trans ='log2')+
  geom_density_ridges(jittered_points=TRUE)
#rug representation of data
gapminder %>%
  filter(year==past_year& !is.na(dollars_per_day))%>%
  ggplot(aes(dollars_per_day,group))+
  scale_x_continuous(trans ='log2')+
  geom_density_ridges(jittered_points=TRUE,
                      position = position_points_jitter(width=0.05,height=0,),
                      point_shape='|',point_size=3,point_alpha=1,alpha=0.7)
#1970 versus 2010 income distribution
past_year<-1970
present_year<-2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(west_devolop=ifelse(group=='West','West','Developing')) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color='black')+
  scale_x_continuous(trans='log2')+
  facet_grid(year~west_devolop)
#####
gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(west_develop=ifelse(group=='West','West','Developing')) 
#we remake the plot using countries with data available for both years 
#using intersect function
country_list1<-gapminder %>%
  filter(year==past_year& !is.na(dollars_per_day))%>%
  pull(country)
country_list2<-gapminder %>%
  filter(year==present_year& !is.na(dollars_per_day))%>%
  pull(country)
country_list<-intersect(country_list1,country_list2)
#now we make a plot for coutries that exist in both years
past_year<-1970
present_year<-2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & country %in% country_list& !is.na(gdp)) %>%
  mutate(west_devolop=ifelse(group=='West','West','Developing')) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color='black')+
  scale_x_continuous(trans='log2')+
  facet_grid(year~west_devolop)
#boxplotgrid
past_year<-1970
present_year<-2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & country %in% country_list) %>%
  ggplot(aes(group,dollars_per_day))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  scale_y_continuous(trans='log2')+
  facet_grid(~year)
#1970 & 2010 side y side plot
past_year<-1970
present_year<-2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & country %in% country_list) %>%
  mutate(year=factor(year)) %>%
  ggplot(aes(group,dollars_per_day,fill=year))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle=90,hjust = 1))+
  scale_y_continuous(trans='log2')+
  xlab("")
#income distributio in 1970 and 2010
gapminder %>%
  filter(year %in% c(past_year,present_year) & country %in% country_list ) %>%
  ggplot(aes(dollars_per_day))+
  geom_density(fill='grey')+
  scale_x_continuous(trans='log2')+
  facet_grid(.~year)
#comaparing west vs developing countries
  gapminder %>% 
    filter(year %in% c(past_year,present_year) &country %in% country_list) %>%
    mutate(west_ddevlop=ifelse(group=='West','West','Developing')) %>%
    ggplot(aes(dollars_per_day,fill=west_ddevlop))+
    geom_density(alpha=0.2)+
    scale_x_continuous(trans='log2')+
    facet_grid(year~.)
#acceing computed variables with geom_density()
# we want the count to be on y axis . 
#this can be done by
#aes(x=dollars_per_day,y=..count..)
  gapminder %>% 
    filter(year %in% c(past_year,present_year) &country %in% country_list) %>%
    mutate(west_ddevlop=ifelse(group=='West','West','Developing')) %>%
    ggplot(aes(x=dollars_per_day,y=..count..,fill=west_ddevlop))+
    geom_density(alpha=0.2)+
    scale_x_continuous(trans='log2',limit=c(0.125,300))+
    facet_grid(year~.)
#to smoothen the desnity bw argument is used
  gapminder %>% 
    filter(year %in% c(past_year,present_year) &country %in% country_list) %>%
    mutate(west_ddevlop=ifelse(group=='West','West','Developing')) %>%
    ggplot(aes(x=dollars_per_day,y=..count..,fill=west_ddevlop))+
    geom_density(alpha=0.2,bw=0.75)+
    scale_x_continuous(trans='log2',limit=c(0.125,300))+
    facet_grid(year~.)
#we use ridge plot to visulaze by groups
  gapminder %>% 
    filter(year %in% c(past_year,present_year) &country %in% country_list & 
             !is.na(dollars_per_day)) %>%
    ggplot(aes(dollars_per_day,group))+
    geom_density_ridges(adjust=1.5)+
    scale_x_continuous(trans='log2')+
    facet_grid(.~year)
#another way to achive this by stacking densities on top of each other 
  gapminder %>%
    filter(year %in% c(past_year,present_year) &country %in% country_list) %>%
    group_by(year) %>%
    mutate(weight=population/sum(population)*2) %>%
    ungroup() %>%
    ggplot(aes(dollars_per_day,fill=group))+
    geom_density(alpha=0.2,bw=0.75,position = 'stack')+
    scale_x_continuous(trans='log2',limit=c(0.125,300))+
    facet_grid(year~.)
# weighted densities
#using weight mapping argument
#Importance of showing data
  gapminder<-gapminder %>%
    mutate(group=case_when(
      region %in% c('Western Europe','Northern Europe','Western Europe',
                    'Northern America','Australia and New Zealand') ~ 'West',
      region %in% c('Northern Africa')~'Northern Africa',
      region %in% c('Eastern Asia','South-Eastern Asia') ~'East Asia',
      region %in% c('Southern Asia') ~ 'Southern Asia',
      region  %in% c('Caribbean','Central America','South America') ~ 'Latin America',
      continent == 'Africa' & region!='Northern Africa'~'Sub-Saharan-Africa',
      region %in% c('Melanesia','Micronesia','Polynesia')~'Pacific Islands'))
#computing quntities for each region'
surv_income<-gapminder %>%
  filter(year %in% present_year & !is.na(gdp) &
           !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarise(income=sum(gdp/sum(population)/365),
            infant_survival_rate=
              1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)


