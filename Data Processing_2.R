############ DATA CLEANING FOR MODELLING ############
################ CENTURY LINK GROUP ################
# ------------ WRITTEN BY:  M.CONDALARY -------------


#Notes:
#1. All files pertaining to the quarter we're predicting are name_yr.qtr
#Where qtr is the quarter to be predicted (data comes from prior quarter)
#2. All rows where we have no information for count of prods are dropped
#   And these rows indicate no prods at the end of previous qtr
#3. Change file paths (there are 2) under READ IN ALL FILES


#------------------ LOAD PACKAGES -------------------

library("plyr")
library("sqldf")


#---------------- READ IN ALL FILES ------------------

setwd(file.path("C:","Users","mallo","Documents","Century Link Proj","Data Prep")) #FILE PATH

#Original calls.txt
file1<-"Calls.txt"
calls<-read.delim(file = file1, sep = ",")

#Dhruv's cleaned up customer sample file
file2<-"Customer Sample.csv"
custsamp<-read.csv(file = file2)

#Regions By State csv
file3<-"Regions.csv"
region<-read.csv(file = file3)
colnames(region)[colnames(region) == "State"] <- "ADR_STATE_CODE"

#Rate Changes x.txt
setwd(file.path("C:","Users","mallo","Documents","Century Link Proj","Raw Files")) #FILE PATH
file4<-paste0('Rate Changes ',seq.int(1,12),'.txt')
rc1<-read.delim(file = file4[1], sep = ",")
rc2<-read.delim(file = file4[2], sep = ",")
rc3<-read.delim(file = file4[3], sep = ",")
rc4<-read.delim(file = file4[4], sep = ",")
rc5<-read.delim(file = file4[5], sep = ",")
rc6<-read.delim(file = file4[6], sep = ",")
rc7<-read.delim(file = file4[7], sep = ",")
rc8<-read.delim(file = file4[8], sep = ",")
rc9<-read.delim(file = file4[9], sep = ",")
rc10<-read.delim(file = file4[10], sep = ",")
rc11<-read.delim(file = file4[11], sep = ",")
rc12<-read.delim(file = file4[12], sep = ",")


# --------------- PROCESS TO MERGE FILES ---------------

#####Calls to Calls per Qtr:

calls_sub<-calls
calls_sub$date_posix<-as.POSIXct(x = substr(as.character(calls$DATE_TIME), 1, nchar(as.character(calls$DATE_TIME))-2),format = "%m/%d/%Y %H:%M:%S")
calls_sub$year<-format(calls_sub$date_posix, format='%Y')
calls_sub$month<-format(calls_sub$date_posix, format='%m')
calls_sub<-subset(calls_sub, year == "2015")                    #MODIFY YR
calls_sub1<-calls_sub[ which(calls_sub$month == "04"|
                                calls_sub$month == "05"|
                                calls_sub$month == "06"), ]     #MODIFY MONTHS FOR QTR TO PRED

calls_sub2<-calls_sub[ which(calls_sub$month == "01"|
                               calls_sub$month == "02"|
                               calls_sub$month == "03"), ]      #MODIFY MONTHS FOR QTR FROM PREV
calls_sub2<-count(calls_sub2, 'BAN_SEQ')
colnames(calls_sub2)[colnames(calls_sub2)=="freq"] <- "Num_Calls_Last"

totalcalls<-count(calls_sub1, 'BAN_SEQ')
colnames(totalcalls)[colnames(totalcalls)=="freq"] <- "Num_Calls"

totalcalls<-join(x = totalcalls, y = calls_sub2, 
                         by = "BAN_SEQ", type = "full") #MODIFICATION 3/22
totalcalls$Num_Calls<-replace(totalcalls$Num_Calls,
                                   which(is.na(totalcalls$Num_Calls)),0)
totalcalls$Num_Calls_Last<-replace(totalcalls$Num_Calls_Last,
                                   which(is.na(totalcalls$Num_Calls_Last)),0)

rm(calls_sub)
rm(calls_sub1)
rm(calls_sub2)
rm(calls)

#####Customer Sample to Custsamp_Clean (dedup):

custsamp_clean<-unique(custsamp)
custsamp_clean<-custsamp_clean[order(custsamp_clean$BAN_SEQ),]
custsamp_clean<-custsamp_clean[!duplicated(custsamp_clean$BAN_SEQ, fromLast=T),]
custsamp_clean<-sqldf('select * from custsamp_clean left join region USING (ADR_STATE_CODE)')
custsamp_clean$region<-replace(custsamp_clean$region,
                                   which(is.na(custsamp_clean$region)),"Other")

rm(custsamp)


#####Raw rcx files to prodsrcx_yr.qtr:

ratechanges<-rbind(rc1, rc2, rc3,
                   rc4, rc5, rc6,
                   rc7, rc8, rc9,
                   rc10, rc11, rc12)

rm(rc1)
rm(rc2)
rm(rc3)
rm(rc4)
rm(rc5)
rm(rc6)
rm(rc7)
rm(rc8)
rm(rc9)
rm(rc10)
rm(rc11)
rm(rc12)

rc_sub<-ratechanges
rc_sub$date_posix<-as.POSIXct(x = substr(x = as.character(rc_sub$SNAPSHOT_DATE), 
                                         start = 1, 
                                         stop = nchar(as.character(rc_sub$SNAPSHOT_DATE))), 
                              format = "%m/%d/%Y")
rc_sub$year<-format(rc_sub$date_posix, format='%Y')
rc_sub<-subset(rc_sub, year == "2015")                            #MODIFY YR
rc_sub$month<-format(rc_sub$date_posix, format='%m')
rc_sub<-rc_sub[ which(rc_sub$month == "03"), ]                    #MODIFY MONTH FOR QTR

rc_pr<-subset(x = rc_sub, select = c(BAN_SEQ, CURR_PAYING_RATE))
rc_pr$num_pr<-as.numeric(levels(rc_pr$CURR_PAYING_RATE)[rc_pr$CURR_PAYING_RATE])
rc_pr<-sqldf("select * , max(num_pr) as max_payrate from rc_pr group by BAN_SEQ")

rc_bc<-subset(x = rc_sub, select = c(BAN_SEQ, BUNDLE_CODE))
rc_bc<-sqldf("select distinct BAN_SEQ, case WHEN BUNDLE_CODE = '~' 
             THEN 0 ELSE 1 END as bun_ind FROM rc_bc")
rc_bc<-sqldf("select * FROM rc_bc where bun_ind = 1")

prods<-unique(subset(x = rc_sub, select = c(BAN_SEQ, PRODUCT_TYPE)))
prods<-count(df = prods, vars = "BAN_SEQ")
prods<-join(x = prods, y = rc_pr, by = "BAN_SEQ", type = "left")
prods<-join(x = prods, y = rc_bc, by = "BAN_SEQ", type = "left")
colnames(prods)[colnames(prods)=="freq"] <- "Num_Prods"

rm(rc_sub)
rm(rc_pr)
rm(rc_bc)


# ------------------- MERGE THE FILES --------------------

#1. Full join rcagg_year.qtr and totalcalls_year.qtr = callsprods_yr.qtr
callsprods<-join(x = prods, y = totalcalls, 
                         by = "BAN_SEQ", type = "full")

#2. Eliminate all rows where the #prods = NA, and then if #calls = NA put 0
callsprods<-subset(callsprods, Num_Prods != 'NA')
callsprods$Num_Calls<-replace(callsprods$Num_Calls, 
                                 which(is.na(callsprods$Num_Calls)),0)
callsprods$bun_ind<-replace(callsprods$bun_ind,
                                  which(is.na(callsprods$bun_ind)),0)

#3. Left join callsprods_yr.qtr with custsamp_clean for final file                (#CHANGE NAME)
final_2015.Q2<-join(x = callsprods, y = custsamp_clean, 
                    by = "BAN_SEQ", type = "left")
final_2015.Q2$OUTCOME <- factor(with(final_2015.Q2, ifelse((Num_Calls == 0), 0, 1)))
final_2015.Q2$num_pr <- NULL
final_2015.Q2$CURR_PAYING_RATE <- NULL

#4. Write the final file
setwd(file.path("C:","Users","mallo","Documents","Century Link Proj","Data Prep")) #FILE PATH
file6 <- 'Final_2015Q2.csv'
write.csv(x = final_2015.Q2, file = file6)
