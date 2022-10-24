
# Libraries
library(shiny)
library(dplyr)
library(networkD3)
library(reshape2)

# The work is realized with two datasets : in lockdown and post lockdown
#=========================================================================================
#================================= in lockdown ========================================
#=========================================================================================
# Import dataset 
df_test_covid = read.csv("donnees-tests-covid19-labo-quotidien-2020-05-29-19h00_00.csv", sep = ";")
#************ Preparation des donnees ************************************
# delete '0' from clage_covid column
df_test_covid = df_test_covid[which(df_test_covid$clage_covid != 0),]
# grouping the departments into the corresponding regions
r1 = c(62,59,80,"02",60) %>% as.character()# Haut de France : 62,59,80,2,60 
r2 = c(95,78,91,77, 75,93,94,92)%>% as.character()# Ile de france 
r3 = c(76,27,14,50,61)%>% as.character()# Normandie
r4 = c(68,67,88,54,57,52,55,10,51,"08")%>% as.character()# Grand EST 
r5 = c(29,22,56,35)%>% as.character()# Bretagne
r6 = c(53,72,49,44,85)%>% as.character()# Pays de la loire 
r7 = c(28,45,41,18,36,37)%>% as.character()# Centre de loire 
r8 = c(90,25,70,39,21,71,58,89)%>% as.character()# Bourgogne Franche-Comte  
r9 = c(74,73,38,"01",26,"07",69,42,43,15,63,"03")%>% as.character()# Auvergne Rhone Alpes  
r10 = c(23,87,86,79,17,16,19,24,33,47,40,64)%>% as.character()# Nouvelle Aquitaine 
r11 = c("04","05","06",13,84,83)%>% as.character()# Provence Alpes Cote d'Azur : 
r12 = c(30,48,12,46,82,34,81,31,32,65,"09",66,11)%>% as.character()# Occitanie 
r13 = c("2A" , "2B") # CORSE
df_test_covid=df_test_covid %>% mutate(
    reg = case_when(
        dep %in% r1 ==T ~ "Hauts-de-France",
        dep %in% r2 ==T ~ "Ile-de-france ",
        dep %in% r3 ==T ~ "Normandie", 
        dep %in% r4 ==T ~"Grand EST ",
        dep %in% r5 ==T ~ "Bretagne",
        dep %in% r6 ==T ~ "Pays de la loire ",
        dep %in% r7 ==T ~ "Centre-Val de loire ",
        dep %in% r8 ==T ~ "Bourgogne-Franche-Comte",
        dep %in% r9 ==T ~ "Auvergne Rhone-Alpes",
        dep %in% r10 ==T ~ "Nouvelle-Aquitaine",
        dep %in% r11 ==T ~"Provence-Alpes-Cote d'Azur" ,
        dep %in% r12 ==T ~ "Occitanie ",
        dep %in% r13 ==T ~ "CORSE ",
        dep == '971' ~ "Guadeloupe",
        dep == '972'~ "Martinique" ,
        dep == '973'~ "Guyane",
        dep == '974'~ "La Reunion", 
        dep == '976'~ "Mayotte",
        
        TRUE ~ "None"
    )
)
# Recode clage_covid column by their meanings
df_test_covid = df_test_covid %>% mutate(
    clage_covid = case_when(
        clage_covid == 'A'~ "moins de 15 ans",
        clage_covid == 'B'~ "15-44 ans",
        clage_covid == 'C'~ "45-64 ans",
        clage_covid == 'D'~ "65-74 ans",
        clage_covid == 'E'~ "75 et plus",
        TRUE ~ as.character(clage_covid)
        
    )
    
)

df_test_covid$jour = as.Date(df_test_covid$jour)
df_test_covid = data.frame(df_test_covid)
#=================================================================
# Create the Sankey diagram 
#*# Calculate the number of variables for each indicator 
#**************************************************
# Data preparation for Sankey diagram 
# with identification by sex
#**************************************************
#***************PFor the total number of tests 
# total number of tests per gender
h = sum(df_test_covid$nb_test_h)
f = sum(df_test_covid$nb_test_f)
ch = c("Homme", h)
cf = c("Femme", f)
df_test_sexe = rbind.data.frame(ch, cf)
df_test_sexe$from = "Test_total"
df_test_sexe$group = c(1,2)
colnames(df_test_sexe)[1] = 'to'
colnames(df_test_sexe)[2] = 'value'
df_test_sexe = df_test_sexe[, c(3,1,2,4)]
df_test_sexe$value=as.character(df_test_sexe$value)%>% as.numeric()

# gender by age group 
# for men
df_test_h_age = aggregate(df_test_covid[,c("nb_test_h")], by= list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_test_h_age$from = "Homme"
colnames(df_test_h_age)[1]='to'
colnames(df_test_h_age)[2] = "value"
df_test_h_age$group = 1
# for women
df_test_f_age = aggregate(df_test_covid[,c("nb_test_f")], by= list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_test_f_age$from = "Femme"
colnames(df_test_f_age)[1]='to'
colnames(df_test_f_age)[2] = "value"
df_test_f_age$group = 2
# For both
df_test_sexe_age = rbind(df_test_h_age, df_test_f_age)
df_test_sexe_age = df_test_sexe_age[,c(3,1,2,4)]

#*********** For positive tests
# age group by positive tests

# First: gender by age group 
# For men
df_testpos_h_age = aggregate(df_test_covid[,c("nb_pos_h")], by= list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_testpos_h_age$sexe = "Homme_pos"
colnames(df_testpos_h_age)[2] = "Test_positif"
df_testpos_h_age$group = 1
# for women
df_testpos_f_age = aggregate(df_test_covid[,c("nb_pos_f")], by= list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_testpos_f_age$sexe = "Femme_pos"
colnames(df_testpos_f_age)[2] = "Test_positif"
df_testpos_f_age$group = 2
# grouping to obtain age class per positive tests per gender
df_testpos_age = rbind(df_testpos_h_age, df_testpos_f_age)
df_testpos_age = df_testpos_age[-3]
df_testpos_age$to = "Test_positif"
colnames(df_testpos_age) = c("from", "value", "group", "to")
df_testpos_age = df_testpos_age[,c(1,4,2,3)]

# positive tests by gender
h_pos = sum(df_test_covid$nb_pos_h)
f_pos = sum(df_test_covid$nb_pos_f)
ch_pos= c("Homme_pos", h_pos)
cf_pos = c("Femme_pos", f_pos)
df_testpos_sexe = rbind.data.frame(ch_pos, cf_pos)
colnames(df_testpos_sexe) = c("sexe", "Test_positif")
df_testpos_sexe$group = c(1,2)
df_testpos_sexe$from = "Test_positif"
colnames(df_testpos_sexe)=c("to", "value", "group", "from")
df_testpos_sexe = df_testpos_sexe[,c(4,1,2,3)]
df_testpos_sexe$value=as.character(df_testpos_sexe$value)%>%as.numeric()

# Grouping all this dataframe
df_sankey_sexe = rbind.data.frame(df_test_sexe,df_test_sexe_age,df_testpos_age, df_testpos_sexe)
df_sankey_sexe$group = df_sankey_sexe$group%>%as.numeric()%>%as.character()
# create the data frame of sankey diagram
node1_sex = unique(df_sankey_sexe$from)
node2_sex = unique(df_testpos_sexe$to)
node1_sex = data_frame("node" = node1_sex, "id" = 0:(length(node1_sex)-1))
node2_sex = data.frame('node' = node2_sex, "id" = nrow(node1_sex): (nrow(node1_sex) + length(node2_sex)-1))
nodes_sex= rbind(node1_sex,node2_sex)
nodes_sex = data.frame(nodes_sex)

df_sankey_sexe = df_sankey_sexe %>% left_join(nodes_sex, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_sex, by = c("to" = "node")) %>% rename(To_id=id)

#**************************************************
# Data preparation for Sankey diagram 
# with identification by age class
#**************************************************
#***************For the total number of tests 

# nbr of total tests per age group
df_test_age = aggregate(df_test_covid[,c('nb_test')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_test_age$from = 'Test_total'
df_test_age$group_link = 0:(nrow(df_test_age)-1)
colnames(df_test_age)[1] = 'to'
colnames(df_test_age)[2]= 'value'
df_test_age = df_test_age[,c(3,1,2,4)]

# Age group by gender 
# for men
df_test_age_h = aggregate(df_test_covid[,c('nb_test_h')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_test_age_h$to = 'Homme'
df_test_age_h$group_link = 0:(nrow(df_test_age_h)-1)
colnames(df_test_age_h)[1]= 'from'
colnames(df_test_age_h)[2]='value'
# for women
df_test_age_f= aggregate(df_test_covid[,c('nb_test_f')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_test_age_f$to = 'Femme'
df_test_age_f$group_link = 0:(nrow(df_test_age_f)-1)
colnames(df_test_age_f)[1]= 'from'
colnames(df_test_age_f)[2]='value'
# for both
df_test_age_sex = rbind(df_test_age_h,df_test_age_f)
df_test_age_sex=df_test_age_sex[,c(1,3,2,4)]

#************** for positive tests 
# gender by pos test by age
# First of all: age class by gender 
# for men
df_testpos_age_h = aggregate(df_test_covid[,c('nb_pos_h')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_testpos_age_h$from = 'Homme'
df_testpos_age_h$group_link = 0:(nrow(df_testpos_age_h)-1)
colnames(df_testpos_age_h)[1]= 'to'
colnames(df_testpos_age_h)[2]='value'
# for women
df_testpos_age_f= aggregate(df_test_covid[,c('nb_pos_f')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_testpos_age_f$from = 'Femme'
df_testpos_age_f$group_link = 0:(nrow(df_test_age_f)-1)
colnames(df_testpos_age_f)[1]= 'to'
colnames(df_testpos_age_f)[2]='value'
# group to obtain gender per positive tests per age class
df_testpos_age_sex = rbind(df_testpos_age_h,df_testpos_age_f)
df_testpos_age_sex$to = 'Test_positif'
df_testpos_age_sex=df_testpos_age_sex[,c(3,1,2,4)]
# this data frame will be used later
df_testpos_age_sex0 = df_testpos_age_sex
df_testpos_age_sex0 = df_testpos_age_sex0[,c(3,1,2,4)]

#positive tests by age group
df_testpos_par_age = aggregate(df_test_covid[,c('nb_pos')], by = list(clage_covid = df_test_covid$clage_covid), FUN = sum)
df_testpos_par_age$from = 'Test_positif'
df_testpos_par_age$group_link = 0:(nrow(df_testpos_par_age)-1)
colnames(df_testpos_par_age)[1] = 'to'
colnames(df_testpos_par_age)[2]= 'value'
df_testpos_par_age = df_testpos_par_age[,c(3,1,2,4)]
df_testpos_par_age$to = paste(df_testpos_par_age$to, 'pos', sep='_' )
# grouping all this data frame
df_sankey_test_age = rbind.data.frame(df_test_age,df_test_age_sex,df_testpos_age_sex, df_testpos_par_age)
df_sankey_test_age$group_link = df_sankey_test_age$group_link%>% as.numeric()%>% as.character()
# create dataframe of sankey diagram
node1_age = unique(df_sankey_test_age$from)
node2_age = unique(df_testpos_par_age$to)
node1_age = data_frame("node" = node1_age, "id" = 0:(length(node1_age)-1))
node2_age = data.frame('node' = node2_age, "id" = nrow(node1_age): (nrow(node1_age) + length(node2_age)-1))
nodes_age= rbind(node1_age,node2_age)
nodes_age = data.frame(nodes_age)
df_sankey_test_age = df_sankey_test_age %>% left_join(nodes_age, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_age, by = c("to" = "node")) %>% rename(To_id=id)


#******************************************************
#* Sankey diagram for the tests performed 
#* by gender and age group
#*****************************************************
#*
# We use the previous data frames  
#total test by sex and age class
df_test_sexe_age0 = df_test_sexe_age[-4]
df_test_sexe0 = df_test_sexe[-4]
# sankey data frame
df_sankey_test = rbind(df_test_sexe0, df_test_sexe_age0)

node1_test = unique(df_sankey_test$from)
node2_test = unique(df_test_sexe_age$to)
node1_test = data_frame("node" = node1_test, "id" = 0:(length(node1_test)-1))
node2_test = data.frame('node' = node2_test, "id" = nrow(node1_test): (nrow(node1_test) + length(node2_test)-1))
nodes_test= rbind(node1_test,node2_test)
nodes_test = data.frame(nodes_test)
df_sankey_test = df_sankey_test %>% left_join(nodes_test, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_test, by = c("to" = "node")) %>% rename(To_id=id)

#******************************************************
#* Sankey diagram for positive tests 
#* by gender and age group
#********************************************************************************
df_testpos_sexe0 = df_testpos_sexe[-4]
df_testpos_sexe0$to = c('Homme', 'Femme')
df_testpos_age_sex0 = df_testpos_age_sex0[-4]
# data frame of sankey diagram 
# links
df_sankey_testpos = rbind(df_testpos_sexe0, df_testpos_age_sex0)
# nodes 
node1_testpos = unique(df_sankey_testpos$from)
node2_testpos = unique(df_testpos_age_sex0$to)
node1_testpos = data_frame("node" = node1_testpos, "id" = 0:(length(node1_testpos)-1))
node2_testpos = data.frame('node' = node2_testpos, "id" = nrow(node1_testpos): (nrow(node1_testpos) + length(node2_testpos)-1))
nodes_testpos= rbind(node1_testpos,node2_testpos)
nodes_testpos = data.frame(nodes_testpos)
df_sankey_testpos = df_sankey_testpos %>% left_join(nodes_testpos, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_testpos, by = c("to" = "node")) %>% rename(To_id=id)


#* Sankey diagram for total tests
#* by region, gender and age group
#*****************************************************

# region by sex
# for men
df_test_reg_sexe_h = aggregate(df_test_covid$nb_test_h, by = list(reg= df_test_covid$reg), FUN = sum)
df_test_reg_sexe_h$to = "Homme"
colnames(df_test_reg_sexe_h)= c("from", "value", "to")
df_test_reg_sexe_h= df_test_reg_sexe_h[,c(1,3,2)]
# for women
df_test_reg_sexe_f = aggregate(df_test_covid$nb_test_f, by = list(reg= df_test_covid$reg), FUN = sum)
df_test_reg_sexe_f$to = "Femme"
colnames(df_test_reg_sexe_f)= c("from", "value", "to")
df_test_reg_sexe_f= df_test_reg_sexe_f[,c(1,3,2)]
# reg per gender
df_test_reg_sexe = rbind.data.frame(df_test_reg_sexe_h, df_test_reg_sexe_f)

# gender per age group
# for men
df_test_reg_sexe_h = aggregate(df_test_covid$nb_test_h, by = list(reg= df_test_covid$reg), FUN = sum)
df_test_reg_sexe_h$to = "Homme"
colnames(df_test_reg_sexe_h)= c("from", "value", "to")
df_test_reg_sexe_h= df_test_reg_sexe_h[,c(1,3,2)]
# for women
df_test_reg_sexe_f = aggregate(df_test_covid$nb_test_f, by = list(reg= df_test_covid$reg), FUN = sum)
df_test_reg_sexe_f$to = "Femme"
colnames(df_test_reg_sexe_f)= c("from", "value", "to")
df_test_reg_sexe_f= df_test_reg_sexe_f[,c(1,3,2)]
#
df_test_reg_sexe = rbind.data.frame(df_test_reg_sexe_h, df_test_reg_sexe_f)

# age group age per test total
df_test_reg_age_total = aggregate(df_test_covid$nb_test, by = list(from= df_test_covid$clage_covid), FUN = sum)
df_test_reg_age_total$to = "Test_total"
colnames(df_test_reg_age_total)[2]= "value"
df_test_reg_age_total= df_test_reg_age_total[,c(1,3,2)]

# sankey dataframe
# Links
df_sankey_test_reg = rbind.data.frame(df_test_reg_sexe,df_test_sexe_age0,df_test_reg_age_total)
df_sankey_test_reg = df_sankey_test_reg[which(df_sankey_test_reg$value != 0),]
# Nodes
node1_test_reg = unique(df_sankey_test_reg$from)
node2_test_reg = unique(df_test_reg_age_total$to)
node1_test_reg = data_frame("node" = node1_test_reg, "id" = 0:(length(node1_test_reg)-1))
node2_test_reg = data.frame('node' = node2_test_reg, "id" = nrow(node1_test_reg): (nrow(node1_test_reg) + length(node2_test_reg)-1))
nodes_test_reg= rbind(node1_test_reg,node2_test_reg)
nodes_test_reg = data.frame(nodes_test_reg)
df_sankey_test_reg = df_sankey_test_reg %>% left_join(nodes_test_reg, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_test_reg, by = c("to" = "node")) %>% rename(To_id=id)

#******************************************************
#* Sankey diagram for positive tests
#* by region, gender and age group
#*****************************************************
# Region by gender
# for men
df_testpos_reg_sexe_h = aggregate(df_test_covid$nb_pos_h, by = list(reg= df_test_covid$reg), FUN = sum)
df_testpos_reg_sexe_h$to = "Homme"
colnames(df_testpos_reg_sexe_h)= c("from", "value", "to")
df_testpos_reg_sexe_h= df_testpos_reg_sexe_h[,c(1,3,2)]
# for women
df_testpos_reg_sexe_f = aggregate(df_test_covid$nb_pos_f, by = list(reg= df_test_covid$reg), FUN = sum)
df_testpos_reg_sexe_f$to = "Femme"
colnames(df_testpos_reg_sexe_f)= c("from", "value", "to")
df_testpos_reg_sexe_f= df_testpos_reg_sexe_f[,c(1,3,2)]
# region per gender 
df_testpos_reg_sexe = rbind.data.frame(df_testpos_reg_sexe_h, df_testpos_reg_sexe_f)

# gender per age group

# age group per positive tests
df_testpos_reg_age_total = aggregate(df_test_covid$nb_pos, by = list(from= df_test_covid$clage_covid), FUN = sum)
df_testpos_reg_age_total$to = "Test_positif"
colnames(df_testpos_reg_age_total)[2]= "value"
df_testpos_reg_age_total= df_testpos_reg_age_total[,c(1,3,2)]

# sankey dataframe
#Links
df_sankey_testpos_reg = rbind.data.frame(df_testpos_reg_sexe,df_testpos_age_sex0,df_testpos_reg_age_total)
df_sankey_testpos_reg = df_sankey_testpos_reg[which(df_sankey_testpos_reg$value != 0),]
#Nodes
node1_testpos_reg = unique(df_sankey_testpos_reg$from)
node2_testpos_reg = unique(df_testpos_reg_age_total$to)
node1_testpos_reg = data_frame("node" = node1_testpos_reg, "id" = 0:(length(node1_testpos_reg)-1))
node2_testpos_reg = data.frame('node' = node2_testpos_reg, "id" = nrow(node1_testpos_reg): (nrow(node1_testpos_reg) + length(node2_testpos_reg)-1))
nodes_testpos_reg= rbind(node1_testpos_reg,node2_testpos_reg)
nodes_testpos_reg = data.frame(nodes_testpos_reg)
df_sankey_testpos_reg = df_sankey_testpos_reg %>% left_join(nodes_testpos_reg, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_testpos_reg, by = c("to" = "node")) %>% rename(To_id=id)

#=========================================================================================
#================================= POST lockdown ========================================
#=========================================================================================
# import dataset
df_pc_test_covid = read.csv("sp-pos-quot-reg-2020-09-15-19h39.csv", sep = ";")
#***********************************************
#* data preparation
#*************************************************
#*
df_pc_test_covid = df_pc_test_covid[which(df_pc_test_covid$cl_age90!= 0),]
df_pc_test_covid$cl_age90 = as.factor(df_pc_test_covid$cl_age90)
# recode cl_age90 column
df_pc_test_covid = df_pc_test_covid %>% mutate(
    cl_age90 = case_when(
        cl_age90 == '9' ~ "moins de 19 ans",
        cl_age90  == '19' ~ "moins de 19 ans",
        cl_age90 == '29'  ~ "20-49 ans",
        cl_age90  == '39' ~ "20-49 ans",
        cl_age90  == '49' ~ "20-49 ans",
        cl_age90 == '59' ~ "50-69 ans",
        cl_age90  == '69' ~ "50-69 ans",
        cl_age90 == '79' ~ "70-79 ans",
        cl_age90 == '89' ~ "80 et plus",
        cl_age90  == '90' ~ "80 et plus",
        TRUE ~ as.character(cl_age90)
        
    )
    
)
# Recode region column
df_pc_test_covid = df_pc_test_covid[which(df_pc_test_covid$reg != 5 & df_pc_test_covid$reg != 7 & 
                                              df_pc_test_covid$reg != 8),]
df_pc_test_covid = df_pc_test_covid %>% mutate(
    reg = case_when(
        reg == 1 ~ 'Guadeloupe',
        reg == 2 ~ 'Martinique',
        reg == 3 ~ 'Guyane',
        reg == 4 ~ 'La Reunion',
        reg == 6 ~ 'Mayotte',
        reg == 11 ~ 'Ile-de-France',
        reg == 24 ~ 'Centre-Val de Loire',
        reg == 27 ~ 'Bourgogne-Franche-Comte',
        reg == 28 ~ 'Normandie',
        reg == 32 ~ 'Hauts-de-France',
        reg == 44 ~ 'Grand Est',
        reg == 52 ~ 'Pays de la Loire',
        reg == 53 ~ 'Bretagne',
        reg == 75 ~ ' Nouvelle-Aquitaine',
        reg == 76 ~ 'Occitanie', 
        reg == 84 ~ 'Auvergne Rhone Alpes',
        reg == 93 ~ "Provence-Alpes-Cote d'Azur",
        reg == 94 ~ 'Corse'
        
        
        
    )
)

df_pc_test_covid$jour = as.Date(df_pc_test_covid$jour)
df_pc_test_covid = data.frame(df_pc_test_covid)
#========================
# Creation of Sankey diagrams
# Data preparation for the different Sankey diagrams 
#**********************************************************
# Sankey diagram with flow identification
# by gender
#**********************************************************

#************* For total tests 
# nbr of total tests by gender
h_pc = sum(df_pc_test_covid$T_h)
f_pc = sum(df_pc_test_covid$T_f)
ch_pc = c("Homme", h_pc)
cf_pc = c("Femme", f_pc)
df_pc_test_sexe = rbind.data.frame(ch_pc, cf_pc)
df_pc_test_sexe$from = "Test_total"
df_pc_test_sexe$group = c(1,2)
colnames(df_pc_test_sexe)[1] = 'to'
colnames(df_pc_test_sexe)[2] = 'value'
df_pc_test_sexe = df_pc_test_sexe[, c(3,1,2,4)]
df_pc_test_sexe$value=as.character(df_pc_test_sexe$value)%>% as.numeric()

# gender per age group 
# for men
df_pc_test_h_age = aggregate(df_pc_test_covid[,c("T_h")], by= list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_h_age$from = "Homme"
colnames(df_pc_test_h_age)[1]='to'
colnames(df_pc_test_h_age)[2] = "value"
df_pc_test_h_age$group = 1
# for women
df_pc_test_f_age = aggregate(df_pc_test_covid[,c("T_f")], by= list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_f_age$from = "Femme"
colnames(df_pc_test_f_age)[1]='to'
colnames(df_pc_test_f_age)[2] = "value"
df_pc_test_f_age$group = 2
# grouping 
df_pc_test_sexe_age = rbind(df_pc_test_h_age, df_pc_test_f_age)
df_pc_test_sexe_age = df_pc_test_sexe_age[,c(3,1,2,4)]

#********* for positive tests
# age by positive tests
# First: gender by age group
# for men
df_pc_testpos_h_age = aggregate(df_pc_test_covid[,c("P_h")], by= list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_h_age$sexe = "Homme_pos"
colnames(df_pc_testpos_h_age)[2] = "Test_positif"
df_pc_testpos_h_age$group = 1
# for women
df_pc_testpos_f_age = aggregate(df_pc_test_covid[,c("P_f")], by= list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_f_age$sexe = "Femme_pos"
colnames(df_pc_testpos_f_age)[2] = "Test_positif"
df_pc_testpos_f_age$group = 2
# age group per positive tests
df_pc_testpos_age = rbind(df_pc_testpos_h_age, df_pc_testpos_f_age)
df_pc_testpos_age = df_pc_testpos_age[-3]
df_pc_testpos_age$to = "Test_positif"
colnames(df_pc_testpos_age) = c("from", "value", "group", "to")
df_pc_testpos_age = df_pc_testpos_age[,c(1,4,2,3)]

# positive tests per gender
h_pc_pos = sum(df_pc_test_covid$P_h)
f_pc_pos = sum(df_pc_test_covid$P_f)
ch_pc_pos= c("Homme_pos", h_pc_pos)
cf_pc_pos = c("Femme_pos", f_pc_pos)
df_pc_testpos_sexe = rbind.data.frame(ch_pc_pos, cf_pc_pos)
colnames(df_pc_testpos_sexe) = c("sexe", "Test_positif")
df_pc_testpos_sexe$group = c(1,2)
df_pc_testpos_sexe$from = "Test_positif"
colnames(df_pc_testpos_sexe)=c("to", "value", "group", "from")
df_pc_testpos_sexe = df_pc_testpos_sexe[,c(4,1,2,3)]
df_pc_testpos_sexe$value=as.character(df_pc_testpos_sexe$value)%>%as.numeric()

# sankey data frame
# Links
df_pc_sankey_sexe = rbind.data.frame(df_pc_test_sexe,df_pc_test_sexe_age,df_pc_testpos_age, df_pc_testpos_sexe)
df_pc_sankey_sexe$group = df_pc_sankey_sexe$group%>%as.numeric()%>%as.character()
# Nodes
node1_pc__sex = unique(df_pc_sankey_sexe$from)
node2_pc__sex = unique(df_pc_testpos_sexe$to)
node1_pc__sex = data_frame("node" = node1_pc__sex, "id" = 0:(length(node1_pc__sex)-1))
node2_pc__sex = data.frame('node' = node2_pc__sex, "id" = nrow(node1_pc__sex): (nrow(node1_pc__sex) + length(node2_pc__sex)-1))
nodes_pc__sex= rbind(node1_pc__sex,node2_pc__sex)
nodes_pc__sex = data.frame(nodes_pc__sex)
df_pc_sankey_sexe = df_pc_sankey_sexe %>% left_join(nodes_pc__sex, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__sex, by = c("to" = "node")) %>% rename(To_id=id)

#**********************************************************
# Sankey diagram with flow identification
# by age group
#**********************************************************

#*********** for total tests  
# total number of tests per age group
df_pc_test_age = aggregate(df_pc_test_covid[,c('T')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_age$from = 'Test_total'
df_pc_test_age$group_link = 0:(nrow(df_pc_test_age)-1)
colnames(df_pc_test_age)[1] = 'to'
colnames(df_pc_test_age)[2]= 'value'
df_pc_test_age = df_pc_test_age[,c(3,1,2,4)]

#age group per gender 
#for men
df_pc_test_age_h = aggregate(df_pc_test_covid[,c('T_h')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_age_h$to = 'Homme'
df_pc_test_age_h$group_link = 0:(nrow(df_pc_test_age_h)-1)
colnames(df_pc_test_age_h)[1]= 'from'
colnames(df_pc_test_age_h)[2]='value'
# for women
df_pc_test_age_f= aggregate(df_pc_test_covid[,c('T_f')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_age_f$to = 'Femme'
df_pc_test_age_f$group_link = 0:(nrow(df_pc_test_age_f)-1)
colnames(df_pc_test_age_f)[1]= 'from'
colnames(df_pc_test_age_f)[2]='value'
#
df_pc_test_age_sex = rbind(df_pc_test_age_h,df_pc_test_age_f)
df_pc_test_age_sex=df_pc_test_age_sex[,c(1,3,2,4)]

# gender per positive test per age
# first: age group by sex 
# for men
df_pc_testpos_age_h = aggregate(df_pc_test_covid[,c('P_h')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_age_h$from = 'Homme'
df_pc_testpos_age_h$group_link = 0:(nrow(df_pc_testpos_age_h)-1)
colnames(df_pc_testpos_age_h)[1]= 'to'
colnames(df_pc_testpos_age_h)[2]='value'
# for women
df_pc_testpos_age_f= aggregate(df_pc_test_covid[,c('P_f')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_age_f$from = 'Femme'
df_pc_testpos_age_f$group_link = 0:(nrow(df_pc_test_age_f)-1)
colnames(df_pc_testpos_age_f)[1]= 'to'
colnames(df_pc_testpos_age_f)[2]='value'

df_pc_testpos_age_sex = rbind(df_pc_testpos_age_h,df_pc_testpos_age_f)
df_pc_testpos_age_sex$to = 'Test_positif'
df_pc_testpos_age_sex=df_pc_testpos_age_sex[,c(3,1,2,4)]

# # we will use this data frame later
df_pc_testpos_age_sex0 = df_pc_testpos_age_sex
df_pc_testpos_age_sex0 = df_pc_testpos_age_sex0[,c(3,1,2,4)]

#positive tests per age group
df_pc_testpos_par_age = aggregate(df_pc_test_covid[,c('P')], by = list(cl_age90 = df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_par_age$from = 'Test_positif'
df_pc_testpos_par_age$group_link = 0:(nrow(df_pc_testpos_par_age)-1)
colnames(df_pc_testpos_par_age)[1] = 'to'
colnames(df_pc_testpos_par_age)[2]= 'value'
df_pc_testpos_par_age = df_pc_testpos_par_age[,c(3,1,2,4)]
df_pc_testpos_par_age$to = paste(df_pc_testpos_par_age$to, 'pos', sep='_' )

# sankey data frame
#Links
df_pc_sankey_test_age = rbind.data.frame(df_pc_test_age,df_pc_test_age_sex,df_pc_testpos_age_sex, df_pc_testpos_par_age)
df_pc_sankey_test_age$group_link = df_pc_sankey_test_age$group_link%>% as.numeric()%>% as.character()
node1_pc__age = unique(df_pc_sankey_test_age$from)
node2_pc__age = unique(df_pc_testpos_par_age$to)
node1_pc__age = data_frame("node" = node1_pc__age, "id" = 0:(length(node1_pc__age)-1))
node2_pc__age = data.frame('node' = node2_pc__age, "id" = nrow(node1_pc__age): (nrow(node1_pc__age) + length(node2_pc__age)-1))
nodes_pc__age= rbind(node1_pc__age,node2_pc__age)
nodes_pc__age = data.frame(nodes_pc__age)
#nodes
df_pc_sankey_test_age = df_pc_sankey_test_age %>% left_join(nodes_pc__age, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__age, by = c("to" = "node")) %>% rename(To_id=id)

#***********************************************************
#*Sankey chart for total tests
#*by gender and age group
#*************************************************************

df_pc_test_sexe_age0 = df_pc_test_sexe_age[-4]
df_pc_test_sexe0 = df_pc_test_sexe[-4]
# sankey data frame
#Links
df_pc_sankey_test = rbind(df_pc_test_sexe0, df_pc_test_sexe_age0)

# Nodes
node1_pc__test = unique(df_pc_sankey_test$from)
node2_pc__test = unique(df_pc_test_sexe_age$to)
node1_pc__test = data_frame("node" = node1_pc__test, "id" = 0:(length(node1_pc__test)-1))
node2_pc__test = data.frame('node' = node2_pc__test, "id" = nrow(node1_pc__test): (nrow(node1_pc__test) + length(node2_pc__test)-1))
nodes_pc__test= rbind(node1_pc__test,node2_pc__test)
nodes_pc__test = data.frame(nodes_pc__test)
df_pc_sankey_test = df_pc_sankey_test %>% left_join(nodes_pc__test, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__test, by = c("to" = "node")) %>% rename(To_id=id)

#***********************************************************
#*Sankey diagram for positive tests
#*per gender and age group
#************************************

df_pc_testpos_sexe0 = df_pc_testpos_sexe[-4]
df_pc_testpos_sexe0$to = c('Homme', 'Femme')
df_pc_testpos_age_sex0 = df_pc_testpos_age_sex0[-4]
# sankey data frame
# Links
df_pc_sankey_testpos = rbind(df_pc_testpos_sexe0, df_pc_testpos_age_sex0)
# nodes
node1_pc__testpos = unique(df_pc_sankey_testpos$from)
node2_pc__testpos = unique(df_pc_testpos_age_sex0$to)
node1_pc__testpos = data_frame("node" = node1_pc__testpos, "id" = 0:(length(node1_pc__testpos)-1))
node2_pc__testpos = data.frame('node' = node2_pc__testpos, "id" = nrow(node1_pc__testpos): (nrow(node1_pc__testpos) + length(node2_pc__testpos)-1))
nodes_pc__testpos= rbind(node1_pc__testpos,node2_pc__testpos)
nodes_pc__testpos = data.frame(nodes_pc__testpos)
df_pc_sankey_testpos = df_pc_sankey_testpos %>% left_join(nodes_pc__testpos, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__testpos, by = c("to" = "node")) %>% rename(To_id=id)

#***********************************************************
#*Sankey chart for total tests 
#*per region, gender and age group
#*************************************************************=

#region per gender
#for men
df_pc_test_reg_sexe_h = aggregate(df_pc_test_covid$T_h, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_test_reg_sexe_h$to = "Homme"
colnames(df_pc_test_reg_sexe_h)= c("from", "value", "to")
df_pc_test_reg_sexe_h= df_pc_test_reg_sexe_h[,c(1,3,2)]
# for women
df_pc_test_reg_sexe_f = aggregate(df_pc_test_covid$T_f, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_test_reg_sexe_f$to = "Femme"
colnames(df_pc_test_reg_sexe_f)= c("from", "value", "to")
df_pc_test_reg_sexe_f= df_pc_test_reg_sexe_f[,c(1,3,2)]
# 
df_pc_test_reg_sexe = rbind.data.frame(df_pc_test_reg_sexe_h, df_pc_test_reg_sexe_f)

#gender per age per region
#for men
df_pc_test_reg_sexe_h = aggregate(df_pc_test_covid$T_h, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_test_reg_sexe_h$to = "Homme"
colnames(df_pc_test_reg_sexe_h)= c("from", "value", "to")
df_pc_test_reg_sexe_h= df_pc_test_reg_sexe_h[,c(1,3,2)]
# for women
df_pc_test_reg_sexe_f = aggregate(df_pc_test_covid$T_f, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_test_reg_sexe_f$to = "Femme"
colnames(df_pc_test_reg_sexe_f)= c("from", "value", "to")
df_pc_test_reg_sexe_f= df_pc_test_reg_sexe_f[,c(1,3,2)]
# region per gender
df_pc_test_reg_sexe = rbind.data.frame(df_pc_test_reg_sexe_h, df_pc_test_reg_sexe_f)

# age group per total tests
df_pc_test_reg_age_total = aggregate(df_pc_test_covid$T, by = list(from= df_pc_test_covid$cl_age90), FUN = sum)
df_pc_test_reg_age_total$to = "Test_total"
colnames(df_pc_test_reg_age_total)[2]= "value"
df_pc_test_reg_age_total= df_pc_test_reg_age_total[,c(1,3,2)]

# sankey data frame
#Links
df_pc_sankey_test_reg = rbind.data.frame(df_pc_test_reg_sexe,df_pc_test_sexe_age0,df_pc_test_reg_age_total)
df_pc_sankey_test_reg = df_pc_sankey_test_reg[which(df_pc_sankey_test_reg$value != 0),]
# Nodes
node1_pc__test_reg = unique(df_pc_sankey_test_reg$from)
node2_pc__test_reg = unique(df_pc_test_reg_age_total$to)
node1_pc__test_reg = data_frame("node" = node1_pc__test_reg, "id" = 0:(length(node1_pc__test_reg)-1))
node2_pc__test_reg = data.frame('node' = node2_pc__test_reg, "id" = nrow(node1_pc__test_reg): (nrow(node1_pc__test_reg) + length(node2_pc__test_reg)-1))
nodes_pc__test_reg= rbind(node1_pc__test_reg,node2_pc__test_reg)
nodes_pc__test_reg = data.frame(nodes_pc__test_reg)
df_pc_sankey_test_reg = df_pc_sankey_test_reg %>% left_join(nodes_pc__test_reg, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__test_reg, by = c("to" = "node")) %>% rename(To_id=id)


#***********************************************************
#*Sankey diagram for positive tests
#*per region, gender and age group
#*************************************************************
#*
#region per region per gender
df_pc_testpos_reg_sexe_h = aggregate(df_pc_test_covid$P_h, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_testpos_reg_sexe_h$to = "Homme"
colnames(df_pc_testpos_reg_sexe_h)= c("from", "value", "to")
df_pc_testpos_reg_sexe_h= df_pc_testpos_reg_sexe_h[,c(1,3,2)]
# for women 
df_pc_testpos_reg_sexe_f = aggregate(df_pc_test_covid$P_f, by = list(reg= df_pc_test_covid$reg), FUN = sum)
df_pc_testpos_reg_sexe_f$to = "Femme"
colnames(df_pc_testpos_reg_sexe_f)= c("from", "value", "to")
df_pc_testpos_reg_sexe_f= df_pc_testpos_reg_sexe_f[,c(1,3,2)]
# region per gender
df_pc_testpos_reg_sexe = rbind.data.frame(df_pc_testpos_reg_sexe_h, df_pc_testpos_reg_sexe_f)

#gender per age group per reg

# age group per positive tests
df_pc_testpos_reg_age_total = aggregate(df_pc_test_covid$P, by = list(from= df_pc_test_covid$cl_age90), FUN = sum)
df_pc_testpos_reg_age_total$to = "Test_positif"
colnames(df_pc_testpos_reg_age_total)[2]= "value"
df_pc_testpos_reg_age_total= df_pc_testpos_reg_age_total[,c(1,3,2)]

# Sankey data frame
# Links
df_pc_sankey_testpos_reg = rbind.data.frame(df_pc_testpos_reg_sexe,df_pc_testpos_age_sex0,df_pc_testpos_reg_age_total)
df_pc_sankey_testpos_reg = df_pc_sankey_testpos_reg[which(df_pc_sankey_testpos_reg$value != 0),]

# Nodes
node1_pc__testpos_reg = unique(df_pc_sankey_testpos_reg$from)
node2_pc__testpos_reg = unique(df_pc_testpos_reg_age_total$to)
node1_pc__testpos_reg = data_frame("node" = node1_pc__testpos_reg, "id" = 0:(length(node1_pc__testpos_reg)-1))
node2_pc__testpos_reg = data.frame('node' = node2_pc__testpos_reg, "id" = nrow(node1_pc__testpos_reg): (nrow(node1_pc__testpos_reg) + length(node2_pc__testpos_reg)-1))
nodes_pc__testpos_reg= rbind(node1_pc__testpos_reg,node2_pc__testpos_reg)
nodes_pc__testpos_reg = data.frame(nodes_pc__testpos_reg)
df_pc_sankey_testpos_reg = df_pc_sankey_testpos_reg %>% left_join(nodes_pc__testpos_reg, by = c("from" = "node"))%>% rename(From_id = id) %>%
    left_join(nodes_pc__testpos_reg, by = c("to" = "node")) %>% rename(To_id=id)
#=================================================================================
#=================================================================================
#=================================================================================
# Create the user interface and et the server of the shiny app
#=================================================================================

# user interface
ui <- navbarPage('COVID-19 Screening test',
                 
    tabPanel('Sankey Diagram', 
             # The first page contains an overlay of Sankey diagrams for 
             # total tests and positive tests obtained 
             # during and after lockdown
             
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons('r2', 'During lockdown ', list('Total tests'=1, 'positive tests'=2)),
                         checkboxInput("ch", label = "Distribution by region", value = FALSE)),
                     mainPanel(
                         sankeyNetworkOutput('plot2'))
                 
             )),
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons('r_2', 'Post lockdown ', list('Total tests'=1, 'positive tests'=2)),
                         checkboxInput("ch_", label = "Distribution by region", value = FALSE)
                     ),
                     mainPanel(
                         sankeyNetworkOutput('plot_2')
                     ))
             
             )
             ), 
    tabPanel('Sankey Diagram by flow',
             # second page contains an overlay of Sankey diagrams  
             # with flows identification by gender and age group
             # during and after containment
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons('r1', 'During lockdown ', list('Identification by sexe'=1, 'Identification by age group'=2))
                 
             ),
             mainPanel(
                 sankeyNetworkOutput('plot1'))
                 )),
             fluidRow(
                 sidebarLayout(
                     sidebarPanel(
                         radioButtons('r_1', 'Post lockdown ', list('Identification by sexe'=1, 'Identification by age group'=2))
                         
                     ),
                     mainPanel(
                         sankeyNetworkOutput('plot_1')
                     )
                 ))
             
             
             
    )
    

    
)

# server
# we define the execution functions of different sankey digrams 
# according to buttons created in the user interface
server <- function(input, output) {
 output$plot1 = renderSankeyNetwork({
        r = input$r1 
        if (r==1){ sankeyNetwork(Links = df_sankey_sexe,  Nodes = nodes_sex , Source ="From_id", Target = "To_id", Value =  "value", 
                                         NodeID  = "node",LinkGroup = "group",fontSize = 11 , nodeWidth =40)
        }
        else {sankeyNetwork(Links = df_sankey_test_age,  Nodes = nodes_age , Source ="From_id", Target = "To_id", Value =  "value", 
                                     NodeID  = "node",LinkGroup = "group_link",fontSize = 11 , nodeWidth =40)
        }
    })    
    
 output$plot_1 = renderSankeyNetwork({
     r = input$r_1
     if (r==1){ sankeyNetwork(Links = df_pc_sankey_sexe,  Nodes = nodes_pc__sex , Source ="From_id", Target = "To_id", Value =  "value", 
                              NodeID  = "node",LinkGroup = "group",fontSize = 11 , nodeWidth =40)
         
         
     }
     else {sankeyNetwork(Links = df_pc_sankey_test_age,  Nodes = nodes_pc__age , Source ="From_id", Target = "To_id", Value =  "value", 
                         NodeID  = "node",LinkGroup = "group_link",fontSize = 11 , nodeWidth =40)
         
     }
 })
    
    output$plot2 = renderSankeyNetwork({
        r = input$r2
        c = input$ch
        if (c=='FALSE') {
            if (r==1) {
                sankeyNetwork(Links = df_sankey_test,  Nodes = nodes_test , Source ="From_id", 
                              Target = "To_id", Value =  "value", NodeID  = "node", fontSize = 11 , nodeWidth =40)
            }
            else {
                sankeyNetwork(Links = df_sankey_testpos,  Nodes = nodes_testpos , Source ="From_id", Target = "To_id", Value =  "value",
                              NodeID  = "node", fontSize = 11 , nodeWidth =40)
            }
            
        }
        else {
            if (r==1) {
                sankeyNetwork(Links = df_sankey_test_reg,  Nodes = nodes_test_reg , Source ="From_id", 
                              Target = "To_id", Value =  "value", NodeID  = "node", fontSize = 11 , nodeWidth =40)
                
            }
            else {
                sankeyNetwork(Links = df_sankey_testpos_reg,  Nodes = nodes_testpos_reg , Source ="From_id", Target = "To_id", Value =  "value", 
                              NodeID  = "node", fontSize = 11 , nodeWidth =40)
                
            }
            
        }
        
        
        
    })
    output$plot_2 = renderSankeyNetwork({
        r = input$r_2
        c = input$ch_
        if (c=='FALSE') {
            if (r==1) {
                sankeyNetwork(Links = df_pc_sankey_test,  Nodes = nodes_pc__test , Source ="From_id", 
                              Target = "To_id", Value =  "value", NodeID  = "node", fontSize = 11 , nodeWidth =40)
            }
            else {
                sankeyNetwork(Links = df_pc_sankey_testpos,  Nodes = nodes_pc__testpos , Source ="From_id", Target = "To_id", Value =  "value",
                              NodeID  = "node", fontSize = 11 , nodeWidth =40)
            }
            
        }
        else {
            if (r==1) {
                sankeyNetwork(Links = df_pc_sankey_test_reg,  Nodes = nodes_pc__test_reg , Source ="From_id", 
                              Target = "To_id", Value =  "value", NodeID  = "node", fontSize = 11 , nodeWidth =40)
            }
            else {
                sankeyNetwork(Links = df_pc_sankey_testpos_reg,  Nodes = nodes_pc__testpos_reg , Source ="From_id", Target = "To_id", Value =  "value", 
                              NodeID  = "node", fontSize = 11 , nodeWidth =40)
                
            }
            
        }
        
        
    })
        
    
}

# Run the application 
shinyApp(ui = ui, server = server)
