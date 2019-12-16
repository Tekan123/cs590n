###### Setting working directory-------
getwd() 
setwd('/Users/tekan.rana/Desktop/Peanut_data')
getwd()

###### FOR JUNE DATA, read and cleaning csv file---------------
data<- read.csv('June.csv') #June data
View(data)
data_a<- data %>% select(SAMPLE.ID, N, P, K) # selecting specific column
data_a
data_a1<- rename(data_a,id = 'SAMPLE.ID') # renaming the 'sample_id column
data_a1
data_a2<- slice(data_a1, 2:57) # selecting rows from 2 to 57
data_a2

##### adding columns ('tissue' & 'position') in 'data_a2'----------------------
#1. making while loop--------
num = 1
group = 4
leaf <- vector()
while (num <= nrow(data_a2)){
    leaf <- c(leaf, num, num+1)
    num<-num+group
}

# 2.Function for tissue Column
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
#3. Apply the function to 'id' Column
data_a2['tissue'] <- sapply(data_a2[['id']], tissue)
#Print df to see if it worked
data_a2
# 4. Add the position column to the 'data_a2'
data_a2['position'] <- ifelse(data_a2[['id']] %% 2 == 1, 'mature','cluster')
data_a2
data1<- data_a2
data1

# add month column in data1
month<- function (x){
    for(x in 1:length(data1)){
        return('June')
    } 
}

month()
# apply the function month()---
data1['month']<- sapply(data1[['id']],month)
data1


# FOR JULY DATA adding cloumns-------------------
data_b<- read.csv('July.csv')
View(data_b)
data_b1<- data_b %>% select(SAMPLE.ID, N, P, K) # selecting specific column
data_b1
data_b2<- rename(data_b1,id = 'SAMPLE.ID') # renaming the 'sample_id column
data_b2
data_b3<- slice(data_b2, 2:57) # selecting rows from 2 to 57
data_b3

##### ADDING columns ('tissue' & 'position') in 'data_a2'----------------------
#1. making while loop--------
num = 1
group = 4
leaf <- vector()
while (num <= nrow(data_b3)){
    leaf <- c(leaf, num, num+1)
    num<-num+group
}

# 2.calling Function  tissue for tissue Column
tissue()
#3. Apply the function to 'id' Column
data_b3['tissue'] <- sapply(data_b3[['id']], tissue)
#Print df to see if it worked
data_b3
# 4. Add the position column to the 'data_b3'
data_b3['position'] <- ifelse(data_b3[['id']] %% 2 == 1, 'mature','cluster')
data_b3
data2<- data_b3
data2

# add the month column in data2
month<- function (x){
    for(x in 1:length(data2)){
       return('July')
    } 
}

month()
# apply the function month()---
data2['month']<- sapply(data2[['id']],month)
data2



# FOR AUGUST data------------

data_c<- read.csv('August.csv')
View(data_c)
data_c1<- data_c %>% select(SAMPLE.ID, N, P, K) # selecting specific column
data_c1
data_c2<- rename(data_c1,id = 'SAMPLE.ID') # renaming the 'sample_id column
data_c2
data_c3<- slice(data_c2, 1:56) # selecting rows from 2 to 57
data_c3

##### adding columns ('tissue' & 'position') in 'data_a2'----------------------
#1. making while loop--------
num = 1
group = 4
leaf <- vector()
while (num <= nrow(data_b3)){
    leaf <- c(leaf, num, num+1)
    num<-num+group
}

# 2.calling Function  tissue for tissue Column
tissue()
#3. Apply the function to 'id' Column
data_c3['tissue'] <- sapply(data_c3[['id']], tissue)
#Print df to see if it worked
data_c3
# 4. Add the position column to the 'data_b3'
data_c3['position'] <- ifelse(data_c3[['id']] %% 2 == 1, 'mature','cluster')
data_c3
data3<- data_c3
data3

# add the month column with funtion month()
month<- function (x){
    for(x in 1:lengtht(data3)){
        return('August')
    } 
}

month()
# apply the function month()---
data3['month']<- sapply(data3[['id']],month)
data3

# reshaping data WITH TIDYVERSE PACKAGE---------
data1_final <- data1 %>% gather(nutrient, concentration, N:K)
data2_final<- data2 %>% gather(nutrient, concentration, N:K)
data3_final<- data3 %>% gather(nutrient, concentration, N:K)

combined<- bind_rows(data1_final, data2_final, data3_final)
View(combined)
# control the sequence for later on axis_text in figure
combined1<-combined %>% mutate(nutrient=factor(nutrient, levels=c("N", "P", "K")),
                               month=factor(month, levels= c("June", "July", "August")))
View(combined1)
####### BAR PLOT FORMATION WITH MEANS AND SE############
fig<- ggplot(combined1, aes(tissue,concentration))
fig
fig1<- fig+ facet_grid(position~month+nutrient)
fig1
fig2<- fig1+ stat_summary(fun.y=mean,
                          geom="bar",
                          aes(fill=tissue))
fig3<- fig2+stat_summary(fun.data=mean_se, 
                         geom="errorbar",
                         width=.1)+
    labs(x="Tissue", 
         y="Concentration", 
         title="Nutrient status over season on different tissue")+
    theme_classic()+
    scale_fill_manual(name="Tissue Type",
                      labels=c("leaf", "Petiole"),
                      values= c("Red", "Blue"))
fig3                    
############### CUSTOMIZATION OF PLOT#####################
fig4<- fig3+theme(plot.title= element_text(hjust = 0.5, face="bold", colour= "blue"))
fig4
fig5<- fig4+theme(axis.title=element_text(face="bold",size=rel(1.5)))
fig5
fig6<- fig5+ theme(axis.line= element_line(colour="black", size=rel(2)))
fig6
fig7<- fig6+theme(axis.text= element_text(face="bold", colour='blue', angle=90))
fig7

###### CUSTOMIZATION OF PANELS--------------------------------
fig8<- fig7+theme(panel.background = element_rect(fill= "grey"))
fig8
fig9<- fig8+ theme(panel.spacing.x = unit(1, "lines"))
fig9
fig10<- fig9 + theme(strip.background=element_rect(fill="green"))
fig10               
fig11<- fig10+ theme(strip.text=element_text(face='bold', family='Times New Roman', colour='blue'))
fig11
fig12<- fig11+theme(strip.text.y=element_text(angle=0))
fig12

###### For the legend------------------------------
fig13<- fig12+ theme(legend.background =element_rect(fill='orange', colour='grey'),
                     legend.key =element_rect(colour='blue'),
                     legend.text=element_text(face='bold', colour='black'),
                     legend.title=element_text(face='bold', size=rel(1.75)),
                     legend.position = 'right')
fig13



