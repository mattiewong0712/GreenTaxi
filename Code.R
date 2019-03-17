require(data.table)

# load data from the website
green_taxi = fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')

#1.1 How many data points
print("Task 1.1:")
print(sprintf("There are %d data points, thus we would make analysis on these data.", nrow(green_taxi)))

#1.2 Histogram of the number of the trip distance
jpeg("Hist_Trip_Dist.jpg")
hist(green_taxi$Trip_distance, prob = T, main = "Histogram of Trip Distance")

# the 3rd quantile of the distance is only 3.74, and the maximum is 603.1
# and among 1494926 trips, there are only 68 trips larger than 50 miles
# 78727 trips less than 0.5 miles,
# so remove these trips to see the histogram of most trips
green_taxi_1 = green_taxi[green_taxi$Trip_distance < 50 & green_taxi$Trip_distance > 0.5]
jpeg("Hist_Trip_Dist_modified.jpg")
hist(green_taxi_1$Trip_distance, prob = T, main = "Histogram of Trip Distance (Modified trips)")

print("Task 1.2:")
print("The histograms are saved in the same folder as the code files.")
print("Through the histogram of all the trips, we find most of the trips are very close to 0, it may because of the incorectly record, or lots of person took the short distance trip. Besides, there is an outlier with the trip distance very large, we need to pay attention to that.")
print("We assume that the trip with distance larger than 50 miles should be an occasional case and seldom occurs, the trip with distance less than 0.5 miles should be wrongly recorded since it against the common sense that person would take taxi with such a short distance trip, thus we remove these kind of trips to make analysis on the other trips.")
print("So through the modified histogram of the trips (removing trips with distance less than 0.5 and larger than 50), we see most of the trips are still short distance trips. We can infer that most of persons in NY would like to take taxi even the trip distance is short.")

#1.3 Find daily mean and median trip distance grouped by hour

# based on the remaining trips in previous part,
# remove trips with pick up time exactly the same as drop off time
# OR the trip period is less than 1 miniute

ts_pickup = as.POSIXlt(green_taxi_1$lpep_pickup_datetime)
green_taxi_1$pickup_hr = ts_pickup$hour
green_taxi_1$pickup_min = ts_pickup$min
green_taxi_1$hour_idx_pickup = green_taxi_1$pickup_hr + green_taxi_1$pickup_min/60 # add varaible of hour index

ts_dropoff = as.POSIXlt(green_taxi_1$Lpep_dropoff_datetime)
green_taxi_1$dropoff_hr = ts_dropoff$hour
green_taxi_1$dropoff_min = ts_dropoff$min
green_taxi_1$hour_idx_dropoff = green_taxi_1$dropoff_hr + green_taxi_1$dropoff_min/60

idc = green_taxi_1$hour_idx_pickup != green_taxi_1$hour_idx_dropoff # keep the trip period is more than 1 minute
green_taxi_2 = green_taxi_1[idc]

# some trip may occurs more than 1 hour, thus split the distance into two hour groups by the ratio of time cost
# then, for each trip, it has 24 weight values
weight_mat <- matrix(0, nrow = nrow(green_taxi_2), ncol = 24)

for (i in 1:nrow(green_taxi_2)) {
  start = green_taxi_2$hour_idx_pickup[i]
  end = green_taxi_2$hour_idx_dropoff[i]
  
  if (start < end) {
    weight_mat[i, floor(start) + 1] = 1 - (start - floor(start))
    weight_mat[i, floor(end) + 1] = end - floor(end)
    
    if ((floor(end) - floor(start + 1) + 1) != 0) { 
      weight_mat[i, (floor(start) + 1 + 1):(floor(start) + floor(end) - floor(start + 1) + 1)] = 1
    }
  }
  
  if (start > end) { # the trip period cross the 00:00am
    weight_mat[i, floor(start) + 1] = 1 - (start - floor(start))
    weight_mat[i, floor(end) + 1] = end - floor(end)
    if ((floor(start) + 1) != 24) {weight_mat[i, (floor(start) + 1 + 1):24] = 1}
    if (floor(end) != 0) {weight_mat[i, 1:floor(end)] = 1}
    
  }
  
  weight_mat[i, ] = weight_mat[i, ]/sum(weight_mat[i, ]) # make the sum of weights for each trip is 1
}

# it is a n*24 matrix, where n is the total number of trips
# then for each row (each hour group), the non-zero values are the trip distance in this group
dist_hour = weight_mat * matrix(rep(green_taxi_2$Trip_distance, 24), ncol = 24)

fun_mean <- function(x) {mean(x[x != 0])}
fun_median <- function(x) {median(x[x != 0])}
mean_dist_hour = apply(dist_hour, 2, fun_mean) # mean of each hour
median_dist_hour = apply(dist_hour, 2, fun_median) # median of each hour

print("Task 1.3:")
print(sprintf("The daily mean of trip distance (in miles) grouped by hour are %s, from the 1st to 24th hour of the day", paste(round(mean_dist_hour,4), collapse=",")))
print(sprintf("The daily median of trip distance (in miles) grouped by hour are %s, from the 1st to 24th hour of the day", paste(round(median_dist_hour,4), collapse=",")))
print("Through the mean and median result, we find the same thing as we find through the histogram, that is, people prefer taking taxi for short distance trip, like 1 miles.")

fun_ct <- function(x) {sum(x > 0)}
ct_dist_hour = apply(dist_hour, 2, fun_ct)

print(sprintf("We also compute the count of trips for each hour, they are %s, from the 1st to 24th hour of the day", paste(ct_dist_hour, collapse=",")))
print("We can see that the traffic is high in the afternoon and evening, and the trip distance mean is relatively small during these hours. Thus to decrease the load of traffic and benefit the environment protection, it is suggested that the city could provide public bicycles to help people with the short trips.")

#2 build model of the ratio of tip and type of trips

# Cash tips are not included in the tip_amount and total_amount,
# so when tip_amount is 0, it is not sure whether the customers didn't pay the tip
# OR they paid the cash tip, and the cash tip information is not provided.
# Then, we assume customers pay the trip fee and tip using the same payment type.

# Based on the modfied data previously, also remove the trip with total_amount 0 (or less than 0)
# OR payment type is not credit card
green_taxi_3 = green_taxi_2[green_taxi_2$Total_amount > 0 & green_taxi_2$Payment_type == 1]

# compute tip_rate = tip_amount/trip_amount
green_taxi_3$tip_rate = green_taxi_3$Tip_amount / green_taxi_3$Total_amount

# remove the trips with small trip time but relative large trip distance
# we compute the speed as miles per hour for each trip, 
# and remove the trips with speed larger than 80 mph OR less than 1 mph
green_taxi_3$period = green_taxi_3$hour_idx_dropoff - green_taxi_3$hour_idx_pickup
green_taxi_3$speed = green_taxi_3$Trip_distance / green_taxi_3$period # miles per hour
green_taxi_3 = green_taxi_3[green_taxi_3$speed < 80 & green_taxi_3$speed > 1]

# remove the trips with large tip rate, like more than 50%
green_taxi_3 = green_taxi_3[green_taxi_3$tip_rate < 0.5 & green_taxi_3$tip_rate >= 0]

# categorize pick up time into 4 groups, 0-5 is dawn, 6-9 is morning, 10-15 is afternoon, 16-23 is evening
green_taxi_3$pickup_tm = 0
green_taxi_3$pickup_tm[green_taxi_3$pickup_hr <= 5 & green_taxi_3$pickup_hr >= 0] = 1
green_taxi_3$pickup_tm[green_taxi_3$pickup_hr <= 9 & green_taxi_3$pickup_hr >= 6] = 2
green_taxi_3$pickup_tm[green_taxi_3$pickup_hr <= 15 & green_taxi_3$pickup_hr >= 10] = 3
green_taxi_3$pickup_tm[green_taxi_3$pickup_hr <= 23 & green_taxi_3$pickup_hr >= 16] = 4

green_taxi_3$pickup_tm = as.character(green_taxi_3$pickup_tm)
green_taxi_3$Passenger_count = as.character(green_taxi_3$Passenger_count)
green_taxi_3$Trip_type = as.character(green_taxi_3$Trip_type)
green_taxi_3$Extra = as.character(green_taxi_3$Extra)

# split data as train and test
num = nrow(green_taxi_3)
set.seed(1234)
train_id = sample(seq(1,num,1), floor(num*0.8))
train = green_taxi_3[train_id, ]
test = green_taxi_3[-train_id,]

# build model with possible variables of trip kind
mod = lm(tip_rate ~ Passenger_count + Trip_distance + Fare_amount + 
            Extra + Tolls_amount + Trip_type + pickup_tm, train)
mse = mean((predict.lm(mod, test) - test$tip_rate)^2) # test the model, mse = 0.00473

print("Task 2:")
print(sprintf("The model explaining the relationship between the tip rate and kind of trip is built as Tip_rate = %.6f * Fare_amount + %.6f * Extra_overnight_charge + %.6f * Tolls_amount + %.6f * Trip_type_dispatch + %.6f * pickup_time_zone_morning + %.6f * pickup_time_zone_evening",
              mod$coefficients["Fare_amount"], mod$coefficients["Extra1"],
              mod$coefficients["Tolls_amount"], mod$coefficients["Trip_type2"],
              mod$coefficients["pickup_tm2"], mod$coefficients["pickup_tm4"]))
print("This is model is reliable since it can explain the relationship significantly, by using the test data to predict the tip rate, we find the discrepancy between the true and estimated values is very small as 0.4%.")
print("From the model, we can infer that the tip rate is highly related with the fare amount, overnight charge or not, tolls amount, whether dispatch trip or not, pick up time zone (especially for morning and evening).")
print("To get better tip rate, the trip should be a low fare amount, not overnight charge, high tolls amount, not dispatch trip type, occurs in the evening.")

#3 Find anomaly from the data

print("Task 3 (option 1):")
print("From previous analysis, we find there are some abnormal observations from the data:")
print("1. trip_distance is short, it against the common sense that one person would not take taxi for a trip less than 0.5 mile")
print("2. pick up time is exactly the same as drop off time, or the difference are less than 1 minute")
print("3. total amount is quite low or negative, it should be unusual that taking taxi cost almost nothing")
print("4. trip period is small while the trip distance is relative large or the opposite situation, for example, one trip has a distance as 603.1 mile while the time cost is only about 11 minutes")
print("5. tip rate is very large, for example, one trip has $300 tip while the total amount is 303.3")
print("And before build model, we already remove these abonormal trips to get good model fit.")







