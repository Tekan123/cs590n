#Setting working directory-------
getwd() 
setwd('/Users/tekan.rana/Desktop/Peanut_data')
getwd()

# Installing packages for ggplot--------
install.packages("tidyverse")
library(tidyverse)
install.packages("ggplot2")
library(ggplot2)

#I have three data from June to August.
#My goal is to edit those data and combine them(three month's data).
# and create a bar plot in facets and show you how to customize the bar plot.
# The comparision will be how nutrients (NPK) content will differ according to tissue and position of tissue sampling. 

# FOR JUNE DATA, read and cleaning csv file---------------
data<- read.csv('June.csv') #Upload June data
View(data)
data_a<- data %>% select(SAMPLE.ID, N, P, K) # selecting specific columns
data_a
data_a1<- rename(data_a,id = 'SAMPLE.ID') # renaming the 'sample_id' column
data_a1
data_a2<- slice(data_a1, 2:57) # selecting rows from 2 to 57
data_a2

#Adding 'tissue' column in 'data_a2' by creating tissue() function
#1. making while loop--------
num = 1 # inital number
group = 4 # Skip in the iteration
leaf <- vector() # Empty vector
while (num <= nrow(data_a2)){  
    leaf <- c(leaf, num, num+1) # Append leaf with Num and Num+1
    num<-num+group # Add group to Num
}

# 2.Creating 'tissue()' function to add 'tissue' column in data_a2
tissue<-function(x){
    #Check If X (id) is in Leaf Vector
    if (x %in% leaf){
        #If True - Return Leaf
        return('leaf')
    }else{
        #If False - Return Petiole
        return('petiole')
    }
}
#3. Apply the 'tissue()' function to 'id' column of data_a2
data_a2['tissue'] <- sapply(data_a2[['id']], tissue)
#Print data
data_a2
# 4. Add the 'position' column to the 'data_a2' using 'ifelse()' function
data_a2['position'] <- ifelse(data_a2[['id']] %% 2 == 1, 'mature','cluster')
data_a2
data1<- data_a2 # reassigning the data_a2 as data1
data1

# Add 'month' column in data1 by creating 'month()' function
month<- function (x){
    for(x in 1:length(data1)){
        return('June')
    } 
}

month()
# Apply the 'month()' function to data1 with 'sapply()' function---
data1['month']<- sapply(data1[['id']],month)
data1


# FOR JULY DATA adding cloumns (Processes are similar to the previous processes for JUNE data)-------------------
data_b<- read.csv('July.csv')
View(data_b)
data_b1<- data_b %>% select(SAMPLE.ID, N, P, K) # selecting specific column
data_b1
data_b2<- rename(data_b1,id = 'SAMPLE.ID') # renaming the 'sample_id column
data_b2
data_b3<- slice(data_b2, 2:57) # selecting rows from 2 to 57
data_b3

#Adding 'tissue' column in 'data_b3' by calling 'tissue()' function
#1. making while loop--------
num = 1
group = 4
leaf <- vector()
while (num <= nrow(data_b3)){
    leaf <- c(leaf, num, num+1)
    num<-num+group
}

# 2.calling 'tissue()' function for 'tissue' column
tissue()
#3. Apply the ''tissue()' function to 'id' column of data_b3
data_b3['tissue'] <- sapply(data_b3[['id']], tissue)
#Print data
data_b3
# 4. Add the 'position' column to the data_b3 by using 'ifelse()' function
data_b3['position'] <- ifelse(data_b3[['id']] %% 2 == 1, 'mature','cluster')
data_b3
data2<- data_b3
data2

# add the 'month' column in data2 by creating 'month()' function and used by 'sapply()' function
month<- function (x){
    for(x in 1:length(data2)){
       return('July')
    } 
}

month()
# apply the function month()---
data2['month']<- sapply(data2[['id']],month)
data2



# FOR AUGUST data (SProcesses are similar to the processes used for the JUNE and JULY data)------------

data_c<- read.csv('August.csv')
View(data_c)
data_c1<- data_c %>% select(SAMPLE.ID, N, P, K) # selecting specific column
data_c1
data_c2<- rename(data_c1,id = 'SAMPLE.ID') # renaming the 'sample_id column
data_c2
data_c3<- slice(data_c2, 1:56) # selecting rows from 2 to 57
data_c3

#Adding columns 'tissue' by calling 'tissue() data in 'data_c3'
#1. making while loop--------
num = 1
group = 4
leaf <- vector()
while (num <= nrow(data_b3)){
    leaf <- c(leaf, num, num+1)
    num<-num+group
}

# 2.calling 'tissue()'function to add 'tissue' column in data_c3
tissue()
#3. Apply the function to 'id' column with 'sappply()' function
data_c3['tissue'] <- sapply(data_c3[['id']], tissue)
#Print data
data_c3
# 4. Add the 'position' column to the 'data_b3 by using 'ifelse()' function
data_c3['position'] <- ifelse(data_c3[['id']] %% 2 == 1, 'mature','cluster')
data_c3
data3<- data_c3
data3

# Add the 'month' column in data3 by creating 'month()' function
month<- function (x){
    for(x in 1:length(data3)){
        return('August')
    } 
}

month()
# Apply the 'month()' function with 'sapply()' function to data3
data3['month']<- sapply(data3[['id']],month)
data3

# Reshaping data into "long-form" from "Wide-form" using 'gather()' function in TIDYVERSE/Tidyr PACKAGE---------
data1_final <- data1 %>% gather(nutrient, concentration, N:K)
data2_final<- data2 %>% gather(nutrient, concentration, N:K)
data3_final<- data3 %>% gather(nutrient, concentration, N:K)

# combine all data using 'bind_row()' function
combined<- bind_rows(data1_final, data2_final, data3_final)
View(combined)
# To control the sequence for later on axis_text in figure
combined1<-combined %>% mutate(nutrient=factor(nutrient, levels=c("N", "P", "K")),
                               month=factor(month, levels= c("June", "July", "August")))
View(combined1)

##############BAR PLOT FORMATION WITH MEANS AND Standard Error (SE)###################################
fig<- ggplot(combined1, aes(tissue,concentration)) # To give the mapping of plot
fig
fig1<- fig+ facet_grid(position~month+nutrient) # To add facets on the plot
fig1
fig2<- fig1+ stat_summary(fun.y=mean,# To calculting the mean from data and show in y-axis of plot
                          geom="bar", # To add barplot
                          aes(fill=tissue)) # To define the color of bars
fig2
fig3<- fig2+stat_summary(fun.data=mean_se, # To calculate the SE
                         geom="errorbar", # To add errobar on barplot
                         width=.2) # To control the size of 'errorbar'
fig3
fig4<- fig3+labs(x="Tissue", # To add labels
         y="Concentration", 
         title="Nutrient status over season on different tissue")+
    theme_classic()+ # To create clear background of plot
    scale_fill_manual(name="Tissue Type", # To control legend and bar color
                      labels=c("leaf", "Petiole"),
                      values= c("Red", "Blue"))
fig4                   
#CUSTOMIZATION OF PLOT using 'theme()' function--------------------------
fig5<- fig4+theme(plot.title= element_text(hjust = 0.5, face="bold", colour= "blue")) # For the title
fig5
fig6<- fig5+theme(axis.title=element_text(face="bold",size=rel(1.5))) # For axis title
fig6
fig7<- fig6+ theme(axis.line= element_line(colour="black", size=rel(2))) # For axis lines
fig7
fig8<- fig7+theme(axis.text= element_text(face="bold", colour='blue', angle=90)) # For axis text
fig8

#CUSTOMIZATION OF PANELS using 'theme()' function--------------------------------
fig9<- fig8+theme(panel.background = element_rect(fill= "grey")) # For background color
fig9
fig10<- fig9+ theme(panel.spacing.x = unit(0.5, "lines")) # For panel spacing
fig10
fig11<- fig10 + theme(strip.background=element_rect(fill="green")) # For Strip background
fig11               
fig12<- fig11+ theme(strip.text=element_text(face='bold', family='Times New Roman', colour='blue')) # For strip texts
fig12
fig13<- fig12+theme(strip.text.y=element_text(angle=0)) # For the strip text of y axis
fig13

#CUSTOMIZATION of the legend using 'theme()' function------------------------------
fig14<- fig13+ theme(legend.background =element_rect(fill='orange', colour='grey'),
                     legend.key =element_rect(colour='blue'),
                     legend.text=element_text(face='bold', colour='black'),
                     legend.title=element_text(face='bold', size=rel(1.75)),
                     legend.position = 'right')
fig14

                        ### THIS CODE IS AVAILABLE IN 'github.com/Tekan123/cs590n'#####

