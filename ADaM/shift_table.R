#Author:Dominic Muia
#SHIFT TABLE
#02/03/2024
#------------------------------------------------------------------------------#
#---------------------------Install and Load R Packages------------------------#
#------------------------------------------------------------------------------#

#install.packages(c("haven", "sas7bdat","stringr","flextable")) 

library(haven,warn.conflicts = FALSE)

library(dplyr,warn.conflicts = FALSE)

library(tidyverse,warn.conflicts = FALSE)

library(flextable)


#------------------------------------------------------------------------------#
#----------------Importing req' SAS files:LB & ADSL----------------------------#
#------------------------------------------------------------------------------#

adeg <- read_sas("C:/Users/Dominic/Desktop/R/ADaM/adeg.sas7bdat")


adsl <- read_sas("C:/Users/Dominic/Desktop/R/ADaM/adsl.sas7bdat")

View(adeg)

View(adsl)

nrow(adeg)#original dataset

nrow(adsl)

colnames(adeg)

colnames(adsl)

#------------------------------------------------------------------------------#
#--------------------------------Get Big N-------------------------------------#
#------------------------------------------------------------------------------#
big_n <- rbind(adsl,transform(adsl,TRT01AN=3)) %>% #Output for treatment Total
         group_by(TRT01AN) %>% 
         filter  (SAFFL=="Y") %>%
         summarise(count=n())
    
View(big_n)

#------------------------------------------------------------------------------#
#------------------Replicate to get Treatment=3, i.e, Trt total----------------#
#------------------------------------------------------------------------------#

#Method 1:-ADEG dataset
adeg1 <- rbind(adeg, transform(adeg, TRT01AN = 3)) %>% 
         select  (USUBJID, AVISITN, AVISIT, PARAM, TRT01AN, AVALC, BASEC) 

# Print the updated data frame
View(adeg1) 

nrow(adeg1)

#Method 2:-ADSL dataset and assign "Missing" to AVALC and BASEC
# Create a new data frame with the combination of usubjid and week
adsl1 <- expand.grid(USUBJID =unique(adsl$USUBJID), AVISITN = c(8, 11, 12)) %>%
         distinct (USUBJID, AVISITN,.keep_all=TRUE) %>% #ensure no duplicates
         left_join(adsl,by="USUBJID") %>% #retain previous existing variables
         mutate(AVISIT=case_when(AVISITN == 8  ~ "Week 4",
                                 AVISITN == 11 ~ "Week 12",
                                 .default =      "Week 16"),
                AVALC="Missing", BASEC="Missing" 
                )   

nrow(adsl1)

View(adsl1)

#Treatment total = 3
missing_rows <- adsl1 %>%
                bind_rows (mutate(adsl1, TRT01AN=3)) %>%
                select(USUBJID, TRT01AN, BASEC ,AVALC, AVISITN, AVISIT, SAFFL) %>%
                arrange(USUBJID, TRT01AN) 

# Print the updated data frame
nrow(missing_rows)

View(missing_rows)
#------------------------------------------------------------------------------#
#---------------------Update Missing Rows------------------------------------#
#------------------------------------------------------------------------------#

adeg02<- missing_rows %>% 
         left_join (adeg1, by=c("USUBJID", "AVISITN", "AVISIT","TRT01AN")) %>%
         mutate (BASEC_=ifelse(!is.na(BASEC.y),BASEC.y,BASEC.x),
                 AVALC_=ifelse(!is.na(AVALC.y),AVALC.y,AVALC.x)
                 ) %>% filter(SAFFL=="Y" & AVISITN>6 & BASEC_!="" & AVALC_!="") %>%
        select (-starts_with(c("BASEC.","AVALC."))) 

View(adeg02)
#------------------------------------------------------------------------------#
#---------------User-Defined Summary Statistics function-----------------------#
#------------------------------------------------------------------------------#

f_summary <- function(group= c("AVISITN","AVISIT","TRT01AN","BASEC_","AVALC_")){
  adeg02 %>%
    group_by_at(group) %>% #Dynamic grouping- var's passed as arguments to a f()
    summarise(count=n(), .groups="drop")
  
}

#------------------------------------------------------------------------------#
#------------------Call the User-Defined summary Function----------------------#
#------------------------------------------------------------------------------#

result1<-f_summary()
View(result1)

#column total- ATOXGR
result2<-f_summary(c("AVISITN","AVISIT","TRT01AN","AVALC_"))
View(result2)

#subject at baseline.
result3<-f_summary(c("AVISITN","AVISIT","TRT01AN","BASEC_"))
View(result3)

#subject at baseline(Total).
result4<-f_summary(c("AVISITN","AVISIT","TRT01AN"))
View(result4)

#Append result1--result4
results<-result1 %>% 
         bind_rows(result2) %>%
         bind_rows(result3) %>%
         bind_rows(result4) %>%
         mutate(across(c(AVALC_,BASEC_),~ifelse(is.na(.),"Total",.)) 
               )

View(results)

#------------------------------------------------------------------------------#
#--------------Dummy Shell to Input 0's for missing Cases within a group-------#
#------------------------------------------------------------------------------#

#Method 1 using expand.grid funtion
shell<-expand.grid(AVISITN=unique(results$AVISITN),BASEC_=unique(results$BASEC_),
                   AVALC_=unique(results$AVALC_), TRT01AN=unique(results$TRT01AN)) %>% 
      arrange(AVISITN,TRT01AN,BASEC_) %>%
      distinct (AVISITN,TRT01AN,BASEC_,AVALC_,.keep_all=TRUE) %>%
      mutate(count = 0, AVISIT=case_when(AVISITN == 8  ~ "Week 4",
                                         AVISITN == 11 ~ "Week 12",
                                         .default =      "Week 16")
             )

View(shell)

#------------------------------------------------------------------------------#
#----------------------Merge Shell with RESULTS dataset------------------------#
#------------------------------------------------------------------------------#

reshell<-left_join(shell,results,by=c("AVISITN","AVISIT","BASEC_","AVALC_","TRT01AN")) %>%
         mutate(count=coalesce(count.y,count.x)) %>%
         select (-c(count.y,count.x)) %>%
         arrange(AVISITN, TRT01AN)

view(reshell)

#------------------------------------------------------------------------------#
#-----------------------Calculate Percentages----------------------------------#
#------------------------------------------------------------------------------#

reshell1<- reshell %>% 
           group_by (AVISITN,TRT01AN) %>%
           mutate   (den  = last(count),
                     count_c=sprintf("%3d",count),
                     perc_ = sprintf("%5.1f",round(count/den*100,1)),
                     count_per=paste(count_c," (", perc_, ")", sep=""),
                     order_avalc=match(AVALC_,c("Normal","Abnormal-NCS","Abnormal-CS","Missing","Total")),
                     order_basec=match(BASEC_,c("Normal","Abnormal-NCS","Abnormal-CS","Missing","Total"))
                    ) %>%
          arrange(AVISITN, TRT01AN, order_avalc,order_basec) %>% 
          ungroup %>% #to help me drop AVISITN 
          select(-c(den,count, count_c, perc_,order_avalc,order_basec,AVISITN))

View(reshell1)

#------------------------------------------------------------------------------#
#--------------------Transposing form Narrow to Wide---------------------------#
#------------------------------------------------------------------------------#

t_reshell<-reshell1 %>% 
           pivot_wider (names_from=c(BASEC_,TRT01AN), values_from=count_per) %>%
           select(AVISIT,everything())
           

View(t_reshell)

colnames(t_reshell)



#------------------------------------------------------------------------------#
#--------------------Report----------------------------------------------------#
#------------------------------------------------------------------------------#
ft <- qflextable(t_reshell)

ft
ft <- ft %>% 
  set_header_labels(AVISIT = "Timepoint", AVALC_ = "Post Baseline Result", Normal_1 = "Normal", 
                    "Abnormal-NCS_1"="Abnormal-NCS", "Abnormal-CS_1"="Abnormal-CS", 
                    "Missing_1"="Missing", "Total_1"="Total", "Normal_2"="Normal", 
                    "Abnormal-NCS_2"="Abnormal-NCS", "Abnormal-CS_2"="Abnormal-CS",
                    "Missing_2"="Missing","Total_2"="Total", "Normal_3"="Normal",
                    "Abnormal-NCS_3"="Abnormal-NCS", "Abnormal-CS_3"="Abnormal-CS",
                    "Missing_3"="Missing", "Total_3"="Total") %>%
  width(j=1,width = 0.4) %>%
  width(j=2,width = 0.5) %>%
  width(j=c(3:17),width = 0.4)

ft1<-add_header_row(ft,values=c("","","Placebo","Active","All ubjects"),
                    colwidths=c(1,1,5,5,5)) 


print(ft1, preview = "docx")








############################################################################################


%>% 
   %>%
  align(j = "center", part = 'all') %>%
  set_table_properties(layout = "autofit", width = .9) 



ft1 <- valign(ft1, valign = "bottom", part = "header")

dim(ft1)

ft1


