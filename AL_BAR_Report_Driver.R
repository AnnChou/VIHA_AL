library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

rm(list = ls())                                                                        #clean all objec

#----------------------------
#       SET PARAMETERS
#----------------------------

path <- "/cloud/project"
ot_pth <- "/cloud/project/Output/"                      # Output path
fn<- "Book1"
# -- parameters for file name ---
b_date <- "Sept 01, 2019"                                                                #begin date of the report period
e_date <- "Sept 31, 2019"                                                                #end date of the report period

input_folder <- "Data"                                                                            #month of the report period and folder name 

#-- remove old reports in the Output directory
old_pdf<-dir(path=ot_pth, pattern = "pdf")                                              #prepare for the files to be delete          
old_html<-dir(path=ot_pth, pattern = "html")                                            #          
old_excel<-dir(path=ot_pth, pattern = "xlsx")                                           #          
oldfile_pdf<-paste(ot_pth, old_pdf, sep = "");                                          #prepare for the directory
oldfile_html<-paste(ot_pth, old_html, sep = "");                                        #
oldfile_excel<-paste(ot_pth, old_excel, sep = "");                                        #
file.remove(oldfile_pdf)
file.remove(oldfile_html)
file.remove(oldfile_excel)


#----------------------------
#       READ DATA 
#----------------------------
in_file <-paste(path,input_folder,fn, sep = "/")                                                   #prepare input file name
in_file <-paste(in_file,"csv",sep = ".")


dt <- read.csv(in_file)                                                                  #read data


#----------------------------
#       MANIPULATION 
#----------------------------



dt_no_neg<- dt %>% mutate(Temp.Clt.Rate = replace(Temp.Clt.Rate,                          #repace "-1" in column AC
                                                  as.numeric(Temp.Clt.Rate) == -1, NA))

ov <- dt_no_neg %>% group_by(Building.name) %>%                                          #tally records for each facility
  tally() 

rt <- dt_no_neg %>% group_by(Building.name) %>%                                          #subset for necessary report data
  select(Hospital.Service,Client.Name,MRN,Month.Start,Month.End,Client.Rate,Rate.type) %>%
  arrange(Building.name) %>% 
  mutate(MRN = as.character(MRN))

#rt <- rename(rt,)

rt$Client.Rate <- as.numeric(as.character(rt$Client.Rate))                          #convert factors to numeric



test <- rt %>% filter(Building.name %in% c("Ayre Manor-Sooke","Jesken Aerie-Langford","Braehaven-Salt Spring"))

#contractor_rt <- rt %>% filter(`Building name` %in% keep1)                            #subset data for contractors only
#care_type_summary <- rt %>% group_by(Building.name) %>%                  #summarize the number of care types for each facility
#summarize(care_cat=n_distinct(Hospital.Service))   

save.image(file = "my-dt.Rdata")

source('Excel.R')                                                                       # function to create multiple excel reports is in a sepreate file

for ( blt_nm in unique(test$Building.name)){
  #pdf report
  rmarkdown::render(input = "Pdf.Rmd",  # file 2
                    output_format = "pdf_document",
                    output_file = paste("AL_", gsub(" ","_",blt_nm), "_" ,Sys.Date(),".pdf", sep=''),
                    output_dir = ot_pth)
  
  # html reports
  #rmarkdown::render(input = "Html.Rmd",  # file 2
  #                  output_format = "html_document",
  #                  output_file = paste("AL_", gsub(" ","_",blt_nm), Sys.Date(), ".html", sep=''),
  #                  output_dir = ot_pth)
  # excel reports
  my_excel()
}
