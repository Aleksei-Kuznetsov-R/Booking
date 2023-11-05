#PART1 - LIBRARY ----
Load_Libraries <- c("rvest", "RSelenium","tidyverse","sys","dplyr","urltools","stringr","data.table",
                    "plyr","telegram.bot","tidyr","purrr","DescTools","vroom","httr","expss","KeyboardSimulator","netstat","RDCOMClient")
lapply(Load_Libraries, require, character.only = TRUE)

#PART2 - FUNCTIONS ----
#This feature is designed to allow you to build a beautiful table in Telegram. 
#Telegram does not accept any R table types, so you need to use the custom function below to get a table 

# function to translate data.frame to telegram table
to_tg_table <- function( table, align = NULL, indents = 3, parse_mode = 'Markdown' ) {
  
  # if alignment is not set, then align to the left edge
  if ( is.null(align) ) {
    col_num <- length(table)
    align   <- str_c( rep('l', col_num), collapse = '' )
  }
  
  # check if the alignment is correct
  if ( length(table) != nchar(align) ) {
    align <- NULL
  }
  
  # new column alignment 
  side <- sapply(1:nchar(align), 
                 function(x) { 
                   letter <- substr(align, x, x)
                   switch (letter,
                           'l' = 'right',
                           'r' = 'left',
                           'c' = 'both',
                           'left'
                   )
                 })
  
  # save the names
  t_names      <- names(table)
  
  # calculate column widths
  names_length <- sapply(t_names, nchar) 
  value_length <- sapply(table, function(x) max(nchar(as.character(x))))
  max_length   <- ifelse(value_length > names_length, value_length, names_length)
  
  # adjust the size of column names to their width + the number of spaces specified in indents 
  t_names <- mapply(str_pad, 
                    string = t_names, 
                    width  = max_length + indents, 
                    side   = side)
  
  # merge column names
  str_names <- str_c(t_names, collapse = '')
  
  # arguments for the str_pad function
  rules <- list(string = table, width = max_length + indents, side = side)
  
  # translate each column one by one to the desired form
  t_str <-   pmap_df( rules, str_pad )%>%
    unite("data", everything(), remove = TRUE, sep = '') %>%
    unlist(data) %>%
    str_c(collapse = '\n') 
  
  # If the table is more than 4096 characters long, trim it off
  if ( nchar(t_str) >= 4021 ) {
    warning('The table is over 4096 characters long!')
    t_str <- substr(t_str, 1, 4021)
  }
  
  # code block selection characters according to the selected layout
  code_block <- switch(parse_mode, 
                       'Markdown' = c('```', '```'),
                       'HTML' = c('<code>', '</code>'))
  
  # translate into code
  res <- str_c(code_block[1], str_names, t_str, code_block[2], sep = '\n')
  
  return(res)
}

#PART3 - METADATA ----
#Determine today's date - 1 day for our report
current_day <- format(Sys.Date() - 1, format = "%Y%m%d")

#Address for generating log files
log_con <- file(paste0("C:/Documents/LOGS/Booking/",current_day,"_LOG.txt"), open = "a")
                         
#Make a list of page addresses that the script needs to pass through
hotel_address <- c("https://www.booking.com/hotel/uz/samarkand-regency-amir-temur-samarkand.en-gb.html#tab-main",
                   "https://www.booking.com/hotel/uz/savitsky-plaza-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/silk-road-by-minyoun-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/stars-of-ulugbek-by-lia-minyoun-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/wellness-park-i-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/wellness-park-ii-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/wellness-park-iii-samarkand.en-gb.html",
                   "https://www.booking.com/hotel/uz/wellness-park-iv-samarkand.en-gb.html")

#Specify data for telegram bot
bot = Bot(token = bot_token('WS_bot'))
updates = bot$getUpdates()

#PART4 - PARSE ----
#I tried different parsing methods, but came to the conclusion that the most stable option is to use the RSelenium package.

#Call the browser session. 
#Use netstat::free_port() instead of specifying a specific port, because there are cases when the port is busy and the script does not work. 
#Also in case of errors, when you need to restart the script, the port will be busy, which requires a reboot of R
rD  <- rsDriver(browser = "firefox", netstat::free_port(), verbose = F,chromever = NULL)
remDr <- rD[["client"]]
remDr$open() 

Booking = NULL

#Create a loop that will perform the same actions for all the pages we have previously specified
for (o in 1:length(hotel_address)) { 
  
#Inside the loop we need to specify that we are substituting the website address as a new variable
  url <- paste0(hotel_address[o])
  remDr$navigate(url)
  
  # First wait for the DOM to load by pausing for a couple seconds.
  Sys.sleep(2)
  
  html <- remDr$getPageSource()[[1]]
  html <- read_html(html)
  
#Next, we create variables on all the data we need so that they are collected one by one.
  Score <- html %>% 
    html_elements(".daaa8ff09f > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(Score,validate = NULL, .name_repair = "unique") 
 
  Reviews <- html %>% 
    html_elements("span.a3b8729ab1:nth-child(2)") %>% 
    html_text() %>% 
    as_tibble(Reviews,validate = NULL, .name_repair = "unique") 
  
  Staff <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(Staff,validate = NULL, .name_repair = "unique") 
  
  Facilities <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(2)") %>% 
    html_text() %>% 
    as_tibble(Facilities,validate = NULL, .name_repair = "unique") 
  
  Cleanliness <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(3) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(Cleanliness,validate = NULL, .name_repair = "unique") 
  
  Comfort <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(4) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(Comfort,validate = NULL, .name_repair = "unique") 
  
  ValueForMoney <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(5) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(ValueForMoney,validate = NULL, .name_repair = "unique") 
  
  Location <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(6) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(Location,validate = NULL, .name_repair = "unique") 
  
  FreeWiFi <- html %>% 
    html_elements("div.bui-spacer--larger:nth-child(3) > div:nth-child(1) > div:nth-child(2) > div:nth-child(7) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1) > div:nth-child(1)") %>% 
    html_text() %>% 
    as_tibble(FreeWiFi,validate = NULL, .name_repair = "unique") 

#In order to determine after the gathering what data belong to what, we add a parameter - the address of the page from which they are gathered. 
#This also helps to understand what happened and where in case of errors
  hotel_name <- as_tibble(hotel_address[o])

#We add the date of data gathering, but in the format the shareholders want to see it in
  date <- as_tibble(paste0(str_sub(current_day,-2,-1),".",
                           str_sub(current_day,-4,-3),".",
                           str_sub(current_day,1,4)))
#Merge all data into one vector
  Results <- bind_cols(date,hotel_name,Score, Reviews,Staff,Facilities,Cleanliness,Comfort,ValueForMoney,Location,FreeWiFi)
#Then add it all to table 
  Booking <- rbind.fill(Booking, Results)
}

#PART5 - DATA CLEANING ----

Hotel_scores <- Booking

colnames(Hotel_scores)[1]  <- "Date"
colnames(Hotel_scores)[2]  <- "Property"
colnames(Hotel_scores)[3]  <- "Score"
colnames(Hotel_scores)[4]  <- "Reviews"
colnames(Hotel_scores)[5]  <- "Staff"
colnames(Hotel_scores)[6]  <- "Facilities"
colnames(Hotel_scores)[7]  <- "Cleanliness"
colnames(Hotel_scores)[8]  <- "Comfort"
colnames(Hotel_scores)[9]  <- "Value.for.money"
colnames(Hotel_scores)[10] <- "Location"
colnames(Hotel_scores)[11] <- "Free.WiFi"

#*Property ----

Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/samarkand-regency-amir-temur-samarkand.en-gb.html#tab-main"] <- "5.1. Regency" #Regency
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/savitsky-plaza-samarkand.en-gb.html"]                        <- "4.1. Savitsky" #Savitsky
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/silk-road-by-minyoun-samarkand.en-gb.html"]                  <- "5.2. Silk Road" #Minyoun
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/stars-of-ulugbek-by-lia-minyoun-samarkand.en-gb.html"]       <- "4.2. Lia" #Lia
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/wellness-park-i-samarkand.en-gb.html"]                       <- "3.1. Afrosiyob" #Wellness I
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/wellness-park-ii-samarkand.en-gb.html"]                      <- "3.2. Sogd" #Wellness II
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/wellness-park-iii-samarkand.en-gb.html"]                     <- "3.3. Bactria" #Wellness III
Hotel_scores[Hotel_scores == "https://www.booking.com/hotel/uz/wellness-park-iv-samarkand.en-gb.html"]                      <- "3.4. Turon" #Wellness IV

#*Overall Score ----

Scores_format = subset(Hotel_scores, select = c(Date,Property,Score))

Scores_format$Score <- gsub(',','.',Scores_format$Score)  
Scores_format$Score <- str_trim(Scores_format$Score)
Scores_format$Desc = "Overall Score"
colnames(Scores_format)[3]  <- "Total"
#*Overall Score ----

Reviews_format = subset(Hotel_scores, select = c(Date,Property,Reviews))

Reviews_format$Reviews <- gsub(' reviews','',Reviews_format$Reviews)  
Reviews_format$Reviews <- gsub(' review','',Reviews_format$Reviews)  
Reviews_format$Reviews <- gsub(' îòçûâà','',Reviews_format$Reviews)
Reviews_format$Reviews <- gsub(' îòçûâîâ','',Reviews_format$Reviews) 
Reviews_format$Reviews <- gsub(' îòçûâ','',Reviews_format$Reviews) 
Reviews_format$Reviews <- gsub('·','',Reviews_format$Reviews)
Reviews_format$Reviews <- str_trim(Reviews_format$Reviews)
Reviews_format$Desc = "Overall Reviews"
colnames(Reviews_format)[3]  <- "Total"
Reviews_format$Total <- as.numeric(Reviews_format$Total)

#*Staff ----
Staff_format = subset(Hotel_scores, select = c(Date,Property,Staff))

Staff_format$Staff <- gsub('Staff ','',Staff_format$Staff) 
Staff_format$Staff <- str_trim(Staff_format$Staff)
colnames(Staff_format)[3]  <- "Total"
Staff_format$Desc = "Staff"
Staff_format$Total <- as.numeric(Staff_format$Total)

#*Facilities ----
Facilities_format = subset(Hotel_scores, select = c(Date,Property,Facilities))

Facilities_format$Facilities <- gsub('Facilities ','',Facilities_format$Facilities) 
Facilities_format$Facilities <- str_trim(Facilities_format$Facilities)
colnames(Facilities_format)[3]  <- "Total"
Facilities_format$Desc = "Facilities"
Facilities_format$Total <- as.numeric(Facilities_format$Total)

#*Cleanliness ----
Cleanliness_format = subset(Hotel_scores, select = c(Date,Property,Cleanliness))

Cleanliness_format$Cleanliness <- gsub('Cleanliness ','',Cleanliness_format$Cleanliness) 
Cleanliness_format$Cleanliness <- str_trim(Cleanliness_format$Cleanliness)
colnames(Cleanliness_format)[3]  <- "Total"
Cleanliness_format$Desc = "Cleanliness"
Cleanliness_format$Total <- as.numeric(Cleanliness_format$Total)

#*Comfort ----
Comfort_format = subset(Hotel_scores, select = c(Date,Property,Comfort))

Comfort_format$Comfort <- gsub('Comfort ','',Comfort_format$Comfort) 
Comfort_format$Comfort <- str_trim(Comfort_format$Comfort)
colnames(Comfort_format)[3]  <- "Total"
Comfort_format$Desc = "Comfort"
Comfort_format$Total <- as.numeric(Comfort_format$Total)

#*Value for money ----
Value_for_money_format = subset(Hotel_scores, select = c(Date,Property,Value.for.money))

Value_for_money_format$Value.for.money <- gsub('Value for money ','',Value_for_money_format$Value.for.money) 
Value_for_money_format$Value.for.money <- str_trim(Value_for_money_format$Value.for.money)
colnames(Value_for_money_format)[3]  <- "Total"
Value_for_money_format$Desc = "Value for money"
Value_for_money_format$Total <- as.numeric(Value_for_money_format$Total)

#*Location ----
Location_format = subset(Hotel_scores, select = c(Date,Property,Location))

Location_format$Location <- gsub('Location ','',Location_format$Location) 
Location_format$Location <- str_trim(Location_format$Location)
colnames(Location_format)[3]  <- "Total"
Location_format$Desc = "Location"
Location_format$Total <- as.numeric(Location_format$Total)

#*Free WiFi ----
Free_WiFi_format = subset(Hotel_scores, select = c(Date,Property,Free.WiFi))

Free_WiFi_format$Free.WiFi <- gsub('Free WiFi ','',Free_WiFi_format$Free.WiFi) 
Free_WiFi_format$Free.WiFi <- str_trim(Free_WiFi_format$Free.WiFi)
colnames(Free_WiFi_format)[3]  <- "Total"
Free_WiFi_format$Desc = "Free WiFi"
Free_WiFi_format$Total <- as.numeric(Free_WiFi_format$Total)
#*Closing Browser ----
remDr$closeWindow()
rD <- rD$server$stop()
#Booking.com CSV template ----
Hotel_scores = NULL
Hotel_scores <- rbind.fill(Hotel_scores,Scores_format,Reviews_format,Staff_format,
                           Facilities_format,Cleanliness_format,Comfort_format,Value_for_money_format,
                           Location_format,Free_WiFi_format)

Hotel_scores = Hotel_scores[,c("Property","Desc","Total")] 

Hotel_scores$Property[Hotel_scores$Property == "5.1. Regency"]   <- "11" #Regency
Hotel_scores$Property[Hotel_scores$Property == "4.1. Savitsky"]  <- "12" #Savitsky
Hotel_scores$Property[Hotel_scores$Property == "5.2. Silk Road"] <- "13" #Minyoun
Hotel_scores$Property[Hotel_scores$Property == "4.2. Lia"]       <- "14" #Lia
Hotel_scores$Property[Hotel_scores$Property == "3.1. Afrosiyob"] <- "15" #Wellness I
Hotel_scores$Property[Hotel_scores$Property == "3.2. Sogd"]      <- "16" #Wellness II
Hotel_scores$Property[Hotel_scores$Property == "3.3. Bactria"]   <- "17" #Wellness III
Hotel_scores$Property[Hotel_scores$Property == "3.4. Turon"]     <- "18" #Wellness IV

Hotel_scores$Desc[Hotel_scores$Desc == "Overall Score"]   <- "31" #Ðåçóëüòàò
Hotel_scores$Desc[Hotel_scores$Desc == "Overall Reviews"] <- "40" #Overall Reviews
Hotel_scores$Desc[Hotel_scores$Desc == "Comfort"]         <- "41" #Comfort
Hotel_scores$Desc[Hotel_scores$Desc == "Cleanliness"]     <- "42" #Cleanliness
Hotel_scores$Desc[Hotel_scores$Desc == "Value for money"] <- "43" #Value for money
Hotel_scores$Desc[Hotel_scores$Desc == "Staff"]           <- "44" #Staff
Hotel_scores$Desc[Hotel_scores$Desc == "Facilities"]      <- "45" #Facilities
Hotel_scores$Desc[Hotel_scores$Desc == "Location"]        <- "46" #Location
Hotel_scores$Desc[Hotel_scores$Desc == "Free WiFi"]       <- "47" #Free WiFi

Hotel_scores$Total[is.na(Hotel_scores$Total) ] <- "0.0"

Only_scores <- Hotel_scores %>%
  filter(Hotel_scores$Desc != "40") #Overall Reviews
Only_scores$Total <- paste0(format(Only_scores$Total, trim = TRUE), "/10")

Only_reviews <- Hotel_scores %>%
  filter(Hotel_scores$Desc == "40") #Overall Reviews
Only_reviews$Total <- paste0(format(as.character(Only_reviews$Total), trim = TRUE))

Hotel_scores = NULL
Hotel_scores = rbind.fill(Only_scores,Only_reviews)

Hotel_scores[nrow(Hotel_scores) + 1,] <- c(0, 0, paste0(str_sub(current_day,-2,-1),".",
                                                        str_sub(current_day,-4,-3),".",
                                                        str_sub(current_day,1,4))) #Report date

Hotel_scores[nrow(Hotel_scores) + 1,] <- c(11, 30, "Regency" )      #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(12, 30, "Savitsky" )     #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(13, 30, "Minyoun" )      #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(14, 30, "Lia" )          #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(15, 30, "Wellness I" )   #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(16, 30, "Wellness II" )  #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(17, 30, "Wellness III" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(18, 30, "Wellness IV" )  #Blank

Hotel_scores[nrow(Hotel_scores) + 1,] <- c(11, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(12, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(13, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(14, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(15, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(16, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(17, 10, "" ) #Blank
Hotel_scores[nrow(Hotel_scores) + 1,] <- c(18, 10, "" ) #Blank

Hotel_scores$M <- str_c(Hotel_scores$Property,"-",Hotel_scores$Desc)

Hotel_scores <- Hotel_scores[order(Hotel_scores$M),]

Hotel_scores <- Hotel_scores[-1]
Hotel_scores <- Hotel_scores[-3]

Hotel_scores$Desc[Hotel_scores$Desc == "0"]  <- "Äàòà îò÷åòà - "
Hotel_scores$Desc[Hotel_scores$Desc == "1"]  <- ""
Hotel_scores$Desc[Hotel_scores$Desc == "10"] <- ""
Hotel_scores$Desc[Hotel_scores$Desc == "30"] <- "----------------"

Hotel_scores$Desc[Hotel_scores$Desc == "31"] <- "Ðåçóëüòàò:"      #Exceptional
Hotel_scores$Desc[Hotel_scores$Desc == "40"] <- "Êîë-âî îòçûâîâ:" #Overall Reviews
Hotel_scores$Desc[Hotel_scores$Desc == "41"] <- "Óäîáñòâî"        #Comfort
Hotel_scores$Desc[Hotel_scores$Desc == "42"] <- "×èñòîòà"         #Cleanliness
Hotel_scores$Desc[Hotel_scores$Desc == "43"] <- "Öåíà/Êà÷åñòâî"   #Value for money
Hotel_scores$Desc[Hotel_scores$Desc == "44"] <- "Ïåðñîíàë"        #Staff
Hotel_scores$Desc[Hotel_scores$Desc == "45"] <- "Îáúåêò"          #Facilities
Hotel_scores$Desc[Hotel_scores$Desc == "46"] <- "Ðàñïîëîæåíèå"    #Location
Hotel_scores$Desc[Hotel_scores$Desc == "47"] <- "Free WiFi"       #Free WiFi

colnames(Hotel_scores)[1]  <- "________________" 
colnames(Hotel_scores)[2]  <- "____________" 


#SEND MESSAGE ----

tg_table <- to_tg_table(head(Hotel_scores, 100) )

bot$sendMessage(chat_id = "-1001898659039",text = "Booking.com Scores", parse_mode = "Markdown")
bot$sendMessage(chat_id = "-1001898659039",text = tg_table, parse_mode = "Markdown")

# 5259026233 AK chat id
#-1001898659039 STC-Analytics Insight chat id
close(log_con)
