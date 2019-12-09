library(stringr)
library(lubridate)
library(r2excel)

#------------------------------------------
# This file is called by the drive
# It creates excel Output for the BAR reports
#------------------------------------------

my_excel <- function()
{
  rpt_data <- rt[rt$Building.name==blt_nm,]
  #ot_dt <- rpt_data[,c(-1,-2)]
  ttl <- sum(rpt_data$Client.Rate)                                        # prepare summary table
  sum_table <- data.frame(name = "Total Amount: ",
                          amount = ttl)
  
  rpt_date <- gsub("-", " ", (format(Sys.Date(),"%b-%d-%Y")))             # prepare report date
  substr(rpt_date,7,7) <- ","
  #--create a wookbook
  wb<-createWorkbook((type ="xlsx"))
  sheet1 <- createSheet(wb,sheetName = paste("AL ",blt_nm, " BAR Report", sep = ""))
  #--titles
  xlsx.addHeader(wb, sheet1, value = "ASSISTED LIVING FACILITY BILLING ACTIVITY REPORT",
                 color = "red", underline =1, level = 1, startCol = 3)
  xlsx.addLineBreak(sheet = sheet1,numberOfLine = 1)
  
  xlsx.addHeader(wb, sheet1, value = "Vancouver Island Health Authority",
                 color = "red", level = 3, startRow = 3, startCol = 4)
  xlsx.addLineBreak(sheet = sheet1,numberOfLine = 1)
  
  
  xlsx.addHeader(wb, sheet1, value = paste("Report Period: ", b_date,"-", e_date,
                                           "      Facility: ", blt_nm,
                                           "      Report Date: ", 
                                           rpt_date,sep = ""),
                 color = "blue", level = 5, startRow = 5, startCol = 2)
  
  xlsx.addHeader(wb, sheet1, value = "Rate Types:  AL - assisted living;  TAL - Temporary Rate Reduction",
                 color = "blue", level = 5, startRow = 6, startCol = 2)
  #xlsx.addHeader(wb, sheet1, value = paste("Total Amount: $",ttl,
  #                                         sep = ""),
  #               color = "blue", level = 6, startRow = 7, startCol = 2)
  xlsx.addLineBreak(sheet = sheet1,numberOfLine = 3)
  
  #--tables 
    str_row <- 10                                                        #counter for the start row of tables
  
    for (care_type in unique(rpt_data$Hospital.Service)){
       
      print(paste("Start Row: ", str_row, sep = " ")) 
      xlsx.addHeader(wb, sheet1, value = paste("Hospital Service:", care_type,sep =" "),
                     color = "blue", level = 5, startRow = str_row, startCol = 3)
      str_row <- str_row + 1
      
      rpt_sub_data <-rpt_data[rpt_data$Hospital.Service==care_type, c(-1,-2)]
      
      names(rpt_sub_data) <- c("Client Name", "MRN", "Service From Date",
                        "Service To Date", "Client Rate (Monthly)","Rate Type")
      
      #print table 
      xlsx.addTable(wb,sheet1, data = as.data.frame(rpt_sub_data),startCol = 2, 
                    row.names = T, startRow = str_row, columnWidth = 18)
      
      str_row <- str_row + nrow(rpt_sub_data) + 6
    
    }
  #--summary table
    
    
  xlsx.addTable(wb,sheet1, data = sum_table,startCol = 6, 
                row.names = F, col.names = F, columnWidth = 18,
                fontColor = "blue", fontSize = 14,
                startRow = str_row-2)
  
  #--save workbook to excel            
  fname <- paste(ot_pth,"AL_",blt_nm,".xlsx",sep = "")
  saveWorkbook(wb,fname)
}

