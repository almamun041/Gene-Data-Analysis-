#load_packages
library(tidyverse)
library(gtsummary)
library(gt)
library(easystats)
library(readr)
library(naniar)
library(ggplot2)
library(readxl)
library(haven)
library(sjPlot)
library(ggpubr)
library(colorspace)
library(viridis)
library(viridisLite)
library(ggthemes)
library(likert)
#1.import_data
library(readxl)
Data_Info_Antibiotics<- read_excel("D:/4-2/Pet,Zoo, Wild Animal Lab/Knowledge Attitude and Practice Survey in Kenya -  Baseline Data 1.xlsx")

view(Data_Info_Antibiotics)
Half-Info<- Data_Info_Antibiotics 
   
plot(likert(Data_Info_Antibiotics$County,Data_Info_Antibiotics$`How many years working in a hospital`))
#2.Data exploration
#Types of Data
class(Data_Info_Antibiotics)

#Number of Row
nrow(Data_Info_Antibiotics)

#Number of column
ncol(Data_Info_Antibiotics)
 
#Dimention 
dim(Data_Info_Antibiotics)

#Name of Data 
names(Data_Info_Antibiotics)

#First series of Row 
head(Data_Info_Antibiotics)

head(Data_Info_Antibiotics , 10 )

#Structure of Data
str(Data_Info_Antibiotics)
glimpse(Data_Info_Antibiotics)

#Visualization_of_data & missing data
vis_miss(Data_Info_Antibiotics)

#SELECT 
select(Data_Info_Antibiotics, Age )
select(Data_Info_Antibiotics ,5)
select(Data_Info_Antibiotics, 1:5)
select(Data_Info_Antibiotics, -c(Age))
rlang::last_trace()



#3.Data_Cleaning 
#Check the missing & duplicated value 
is.na(Data_Info_Antibiotics)
sum(is.na(Data_Info_Antibiotics))
duplicated(Data_Info_Antibiotics)
sum(duplicated(Data_Info_Antibiotics))
#remove missing data
na.omit(Data_Info_Antibiotics)


#Unique_elements
?unique
unique(Data_Info_Antibiotics)
class(Data_Info_Antibiotics$`Sex:`)
response_arry <-as.factor(Data_Info_Antibiotics$`Sex:`)
response_arry2 <- factor((Data_Info_Antibiotics$`Sex:`),
                         levels = c("male" , "Female ") )
levels(Data_Info_Antibiotics$`Sex:`)

#Removing_duplicate_value
unique_vector <- unique(res$Age)
na.omit(Data_Info_Antibiotics)
#convert data type
Smartphone_use_data <- read_excel(Data_Info_Antibiotics, stringAsFactor = TRUE)

New_data <- Data_Info_Antibiotics %>% 
  mutate_if(is.character , as.factor ) 
glimpse(New_data)

New_data <- Data_Info_Antibiotics %>%  
  mutate_if (is.character ,as.factor )

#4.Descriptive statistics
#Demographic characteristics of study participants
Data_Info_Antibiotics %>% 
  select(1:4) %>% 
  tbl_summary(statistic = all_continuous() ~ "{mean}± {sd}" ,
              missing ="no") %>% 
  as_gt() %>% 
  gtsave("Table1.docx")

#comparative analysis
New_data %>% 
  select(Habitat, 32:52) %>% 
  tbl_summary(statistic = all_continuous() ~ "{mean}±{sd}",
              missing = "no",
              by = Habitat) %>% 
  add_overall() %>% 
  add_p() %>% 
  bold_p(t=0.05) %>% 
  as_gt() %>% 
  gtsave("Table2.docx")

#5.Hypothesis testing
#multivariate analysis
model_data2 <- lm(New_data$`17. Scared`~ Age+Qualifications+Habitat,
   data= New_data)
summary(model_data) 



New_data %>% 
mod1 <- glm(New_data$`Does your facility have a frequently released antibiogram?`~ 

t1 <- tbl_regression(mod1, exponentiate = TRUE)


#Relationship between variables 
ggplot(data = New_data , mapping = aes (x=Qualifications , fill= Qualifications )) +
  geom_bar() + 
  labs( x= "DEGRES_Name " ,
        y= "Frequency" ,
        title = "Name of the Degrees",
        fill = "Qualifications" )
  
  #Visualize Statistical Data
fig <- plot_xtab (
  x=New_data$`Sex:`,
  grp = New_data$`17. Scared`,
  title = "Sex vs Scared" ,
  margin = "row" ,
  show.summary = T ,
  bar.pos = "stack"
)
  
save_plot(filename = "figures/SexvsScared.jpg",
          fig=fig ,width = 30 ,height =  15 ,dpi =300 )

dataarg <- cbind(c("A", "B", "C"), c(10, 20, 30))
dataarg
view(dataarg)


aeS->  = read.table('https://raw.githubusercontent.com/pine-bio-support/DataScience/main/Final_cell_lines_RNA-expression_FPKM_values_1000genes_with_NA.txt',row.names=1, header = TRUE, sep="\t")
Cleandata = na.exclude(aeS)
logdata = log(Cleandata+1) 
boxplot(logdata)

df = read.table('https://code.omicslogic.com/assets/datasets/15_genes/CellLines_15Genes.txt',header = TRUE, sep='\t')

#Remove 1st column
df=df[,-1]


#Descriptive statistics 
summary(df)


#Convert into matrix
df=as.matrix(df),

boxplot(df, col="red", las=2, main="Boxplot")