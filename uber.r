##################################################################################
#                               UBER CASE STUDY
##################################################################################

#Loading the uber data
uber_data_set <- read.csv('Uber Request Data.csv', stringsAsFactors = F);

##################
# Data Cleaning
##################

#Add 00 as seconds if seconds is missing in request timestamp.
uber_data_set$Request.timestamp <- sapply(uber_data_set$Request.timestamp,
                                      function (x) {
                                        ifelse(length(grep('.*\\:.*:', x)),
                                          x,
                                          gsub('/', '-', paste(x, ':00', sep = "")) 
                                        )
                                      }
                                );

# Convert Request Timestamp from string to date format

uber_data_set$Request.timestamp <- strptime(
                                      uber_data_set$Request.timestamp,
                                      format="%d-%m-%Y %H:%M:%S"
                                );

###############################################################
#                Deriving Data from existing columns
###############################################################


# Extracting the day on which request was made. eg: Monday, Tueday...
uber_data_set$weekday <- weekdays(uber_data_set$Request.timestamp)

distinctdays <- unique(uber_data_set$weekday)
#As Saturday and Sunday are not part of the data, there will no no analysis
#comparing weekdays and weekends.

#Extracting the hour when the request was made.
uber_data_set$Hour <- format(uber_data_set$Request.timestamp, "%H");


#Creating 5 categories combining all time slots.

# Categories are divided as follows:
#12:00 AM to 4:00 AM - Pre-Morning
#4:00 AM  to 8:00 AM  - Early Morning
#8:00 AM  to 12:00 PM - Morning
#12:00 PM to 4:00 PM - AfterNoon
#4:00 PM to 8:00 PM - Evening
#8:00 PM to 12:00 AM - Night

uber_data_set$categories <- sapply(
                              as.numeric(format(uber_data_set$Request.timestamp, "%H")),
                              function(x) {
                                if (x<=4) {'Pre-Morning'}
                                else if(x<=8){'Early Morning'}
                                else if (x<=12) {'Morning'}
                                else if (x<=16) {'AfterNoon'}
                                else if (x<=20) {'Evening'}
                                else {'Night'}}
                          );


#Analysis

# Getting total number of requests
total_no_requests <- nrow(uber_data_set);

# The frequency of requests across different status
freq_by_status <- table(uber_data_set$Status);


# The percentage across status where the requests end up
percentage_by_status <- prop.table(freq_by_status) * 100;

#Cancelled and No Cars Available makeup 58% of the requests and 
#therefore there is a supply - demand gap.


# No of requests across each hour of the day.
freq_hour <- table(uber_data_set$Hour)

# As it is difficult to find the time at which requests peak
#(which could be easy via visualization)
# We introduce analysis by time slots prepared above.
freq_time_slots <- table(uber_data_set$categories)

# Requests go up at early Morning and Evening


# Finding at which time slots when cabs are getting cancelled
# or there are no cabs available

#Categorizing by Status
status_by_categories <- table(uber_data_set$categories, uber_data_set$Status)

status_by_categories <- as.data.frame(status_by_categories, stringsAsFactors = F);
names(status_by_categories) <- c("Time Slot", "Status", "Count");

#Finding the time slot where cancellation occurs the most.
cancelled_subset <- status_by_categories[which(status_by_categories$Status == 'Cancelled'),];
cancelled_time_slot <- cancelled_subset[which.max(cancelled_subset$Count), 1]

#This shows that the cancellation is highest at Early Morning


#Finding the time slot where No cabs are available mostly.
No_cabs_subset <- status_by_categories[which(status_by_categories$Status == 'No Cars Available'),];
No_cabs_time_slot <- cancelled_subset[which.max(No_cabs_subset$Count), 1];

#This shows that the cabs are not available mostly at Evening.



# Problems at Airport and City

# Finding pressing problem at Airport and filtering out Trips Completed rows.

cancelled_subset_airport <- uber_data_set[which(uber_data_set$Pickup.point == 'Airport'
                                                 & uber_data_set$Status != 'Trip Completed'
                                                 & uber_data_set$Status == 'Cancelled')
                                          ,];

No_cabs_subset_airport <- uber_data_set[which(uber_data_set$Pickup.point == 'Airport'
                                               & uber_data_set$Status != 'Trip Completed'
                                               & uber_data_set$Status == 'No Cars Available')
                                        ,];


# Summary by time slots and status at Airport

# For status= 'Cancelled'
cancelled_airport_summary <- table(cancelled_subset_airport$categories);
#For status = 'No Cars Available'
no_cabs_airport_summary <- table(No_cabs_subset_airport$categories);

# By the above 2 summaries we can see that No cabs Available
#is the pressing problem at Airport During Evening


#Finding pressing problem at City and filtering out Trips Completed rows.
cancelled_subset_city <- uber_data_set[which(uber_data_set$Pickup.point == 'City'
                                                & uber_data_set$Status != 'Trip Completed'
                                                & uber_data_set$Status == 'Cancelled')
                                          ,];

No_cabs_subset_city <- uber_data_set[which(uber_data_set$Pickup.point == 'City'
                                              & uber_data_set$Status != 'Trip Completed'
                                              & uber_data_set$Status == 'No Cars Available'),];


# Summary by time slots and status at City

# For status= 'Cancelled'
cancelled_city_summary <- table(cancelled_subset_city$categories);
#For status = 'No Cars Available'
no_cabs_city_summary <- table(No_cabs_subset_city$categories);

# By the above 2 summaries we can see that Cancellation
#is the pressing problem at City During Early Morning.
 
# Writing the modified table as csv to use it in Tableau for Data Visualization.
write.csv(uber_data_set, "Uber Data Set Copy.csv", row.names = F)
















