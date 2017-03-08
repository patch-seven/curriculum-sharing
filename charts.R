library(readr)
library(tidyverse)
library(knitr)
library(WriteXLS)
#install.packages("xlsx")
library(xlsx)

test

subject_foe <- read_csv("Z:/Business Intelligence/309 Projects/2016_Flexible Academic Programme (FlexAP)/Curriculum Sharing/2015_16 subjects by foe teaching owning splits.csv")

subject_foe %>% select(`Subject Study Pack Type Description`) %>% distinct()
subject_foe %>% select(`Subject Owning Fac`) %>% distinct()

# keep only the undergraduate and postgraduate subjects
subject_foe <- subject_foe %>%
  filter(`Subject Study Pack Type Description` %in% c("Undergraduate Subject", "Postgraduate Subject"))%>%
  filter(`Subject Owning Fac` != "SBFF - SUBSIDIARIES") %>%
  mutate(subject_o_fac = substr(`Subject Owning Fac`, 8, length(`Subject Owning Fac`))) %>%
  mutate(subject_t_fac = substr(`Subject Teaching Fac`, 8, length(`Subject Teaching Fac`)))

# Create a reference object with list of all faculties in order to automate charting for each faculty
faculties <- subject_foe %>% select(`subject_o_fac`) %>% distinct()

# Rework on 06/03/2017 Present teaching faculty rather than teaching divisions...

for (i in c(1:10)) {
  table <- subject_foe %>%
    filter(subject_o_fac == faculties$subject_o_fac[i]) %>%
    group_by(`Subject Availability Year`, `Subject Study Pack Type Description`, `Subject Owning Org Name`, "Teaching Faculty" = subject_t_fac)%>%
    summarise(Load = sum(`Nett Eftsl`))
  write.xlsx(as.data.frame(table), "table2a.xls", sheetName = faculties$subject_o_fac[i], append=T)
  chart <- table %>%  
    ggplot(aes(x=`Subject Owning Org Name`, y=Load, fill=`Teaching Faculty`)) +
    geom_bar(stat = "identity") +
    facet_wrap(`Subject Study Pack Type Description`~`Subject Availability Year`) +
    coord_flip() +
    ggtitle(paste0("2015 and 2016 Teaching Load for ", faculties$subject_o_fac[i], " by Subject Owning Department and Teaching Faculty"))+
    xlab("Subject Owning Department") +
    ylab("Teaching Load") +
    theme(text = element_text(size=5))+
    labs(fill= "Teaching Faculty")+
    guides(fill=guide_legend(ncol=1))
  print(chart)
  ggsave(filename = paste0(faculties$subject_o_fac[i], " chart 2a.pdf"), plot = chart, dpi = 600)
}

#End Rework 06/03/2017


for (i in c(1:10)) {
chart <- subject_foe %>%
  filter(subject_o_fac == faculties$subject_o_fac[i]) %>%
  group_by(`Subject Availability Year`, `Subject Study Pack Type Description`, `Subject Owning Org Name`, "Teaching Department" = `Level 3 Name`)%>%
  summarise(Load = sum(`Nett Eftsl`)) %>%
  ggplot(aes(x=`Subject Owning Org Name`, y=Load, fill=`Level 3 Name`)) +
  geom_bar(stat = "identity") +
  facet_wrap(`Subject Study Pack Type Description`~`Subject Availability Year`) +
  coord_flip() +
  ggtitle(paste0("2015 and 2016 Teaching Load for ", faculties$subject_o_fac[i], " by Subject Owning Department and FOE"))+
  xlab("Subject Owning Department") +
  ylab("Teaching Load") +
  theme(text = element_text(size=5))+
  labs(fill= "Field of Education")+
  guides(fill=guide_legend(ncol=1))
print(chart)
ggsave(filename = paste0(faculties$subject_o_fac[i], " chart 1.pdf"), plot = chart, dpi = 600)
}

for (i in c(1:10)) {
  chart <- subject_foe %>%
    filter(subject_o_fac == faculties$subject_o_fac[i]) %>%
    group_by(`Subject Availability Year`, `Subject Study Pack Type Description`, `Subject Owning Org Name`, `Subject Teaching Org Name`)%>%
    summarise(Load = sum(`Nett Eftsl`)) %>%
    ggplot(aes(x=`Subject Owning Org Name`, y=Load, fill=`Subject Teaching Org Name`)) +
    geom_bar(stat = "identity") +
    facet_wrap(`Subject Study Pack Type Description`~`Subject Availability Year`) +
    coord_flip() +
    ggtitle(paste0("2015 and 2016 Teaching Load for ", faculties$subject_o_fac[i], "Owning Department by Subject Teaching Department"))+
    xlab("Subject Owning Department") +
    ylab("Teaching Load") +
    theme(text = element_text(size=5))+
    labs(fill= "Subject Teaching Department")+
    guides(fill=guide_legend(ncol=1))
  print(chart)
  ggsave(filename = paste0(faculties$subject_o_fac[i], " chart 2.pdf"), plot = chart, dpi = 600)
}


### Working


for (i in c(1:10)) {
  chart <- subject_foe %>%
    filter(subject_o_fac == faculties$subject_o_fac[i]) %>%
    group_by(`Subject Availability Year`, `Subject Study Pack Type Description`, `Subject Owning Org Name`, "Teaching Department" = `Level 3 Name`)%>%
    summarise(Load = sum(`Nett Eftsl`)) %>%
    unite(Year_level, `Subject Availability Year`, `Subject Study Pack Type Description`) %>%
    spread(Year_level, Load) %>%
    kable()
  
  
  ggplot(aes(x=`Subject Owning Org Name`, y=Load, fill=`Level 3 Name`)) +
    geom_bar(stat = "identity") +
    facet_wrap(`Subject Study Pack Type Description`~`Subject Availability Year`) +
    coord_flip() +
    ggtitle(paste0("2015 and 2016 Teaching Load for ", faculties$subject_o_fac[i], " by Subject Owning Department and FOE"))+
    xlab("Subject Owning Department") +
    ylab("Teaching Load") +
    theme(text = element_text(size=5))+
    labs(fill= "Field of Education")+
    guides(fill=guide_legend(ncol=1))
  
  print(chart)
  ggsave(filename = paste0(faculties$subject_o_fac[i], " table 1.pdf"), plot = chart, dpi = 600)
}



subject_foe %>%
  filter(subject_t_fac == faculties$subject_o_fac[3])%>%
  group_by(`Subject Availability Year`, `Subject Study Pack Name`, `Subject Owning Org Name`, `Level 4 Name`, `Subject Study Pack Type Description`, subject_t_fac) %>%
  summarise(Load = sum(`Nett Eftsl`)) %>%
  ggplot(aes(x=`Subject Owning Org Name`, y=Load, fill=`Subject Teaching Org Name`)) +
  geom_bar(stat = "identity") +
  facet_wrap(`Subject Study Pack Type Description`~`Subject Availability Year`) +
  coord_flip() +
  ggtitle(paste0("2015 and 2016 Teaching Load for ", faculties$subject_o_fac[i], " by Subject Teaching Department"))+
  xlab("Subject Owning Department") +
  ylab("Teaching Load") +
  theme(text = element_text(size=5))+
  guides(fill=guide_legend(ncol=1))
print(chart)
ggsave(filename = paste0(faculties$subject_o_fac[i], " chart 2.pdf"), plot = chart, dpi = 600)
}
  




