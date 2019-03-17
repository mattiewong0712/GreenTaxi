require(data.table)

# load data from the website
green_taxi = fread('https://s3.amazonaws.com/nyc-tlc/trip+data/green_tripdata_2015-09.csv')

#1.1 How many data points
print(sprintf("There are %d data points", nrow(green_taxi)))

#1.2 Histogram of the number of the trip distance
jpeg("Hist_Trip_Dist.jpg")
hist(green_taxi$Trip_distance, prob = T, main = "Histogram of Trip Distance")

# the 3rd quantile of the distance is only 3.74, and the maximum is 603.1
# so remove the trips with large distance to see the histogram of most trips
green_taxi_1 = green_taxi[green_taxi$Trip_distance < 20]
jpeg("Hist_Trip_Dist_less_20mils.jpg")
hist(green_taxi_1$Trip_distance, prob = T, main = "Histogram of Trip Distance (Less than 20 Miles)")

#1.3 Find daily mean and median trip distance grouped by hour

# find abnormal rows with pick up time exactly the same as drop off time
# OR the trip period is less than 1 miniute
# OR the trip distance less than 0.1 mile
green_taxi_2 = green_taxi[green_taxi$Trip_distance >= 0.1]

ts_pickup = as.POSIXlt(green_taxi_2$lpep_pickup_datetime)
hour_idx_pickup = ts_pickup$hour + ts_pickup$min/60 # add varaible of hour index

ts_dropoff = as.POSIXlt(green_taxi_2$Lpep_dropoff_datetime)
hour_idx_dropoff = ts_dropoff$hour + ts_dropoff$min/60

idc = hour_idx_pickup != hour_idx_dropoff # keep the trip period is more than 1 minute
green_taxi_2 = green_taxi_2[idc]
hour_idx_pickup = hour_idx_pickup[idc]
hour_idx_dropoff = hour_idx_dropoff[idc]

# some trip may occurs more than 1 hour, thus split the distance into two hour groups by the ratio of time cost
# then, for each trip, it has 24 weight values
weight_mat <- matrix(0, nrow = nrow(green_taxi_2), ncol = 24)

for (i in 1:nrow(green_taxi_2)) {
  start = hour_idx_pickup[i]
  end = hour_idx_dropoff[i]
  
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

#fun_ct <- function(x) {sum(x > 0)}
#apply(dist_hour, 2, fun_ct)

print(sprintf("The daily mean of trip distance (in miles) grouped by hour are %s, from the 1st to 24th hour of the day", paste(round(mean_dist_hour,4), collapse=",")))
print(sprintf("The daily median of trip distance (in miles) grouped by hour are %s, from the 1st to 24th hour of the day", paste(round(median_dist_hour,4), collapse=",")))


#2 build model of the ratio of tip and type of trips

# Cash tips are not included in the tip_amount and total_amount,
# so when tip_amount is 0, it is not sure whether the customers didn't pay the tip
# OR they paid the cash tip, and the cash tip information is not provided.
# Then, we assume customers pay the trip fee and tip using the same payment type.

# Based on the modfied data in part 1, also remove the trip with total_amount 0
# OR payment type is not credit card
green_taxi_3 = green_taxi_2[green_taxi_2$Total_amount != 0 & green_taxi_2$Payment_type == 1]

# compute tip_rate = tip_amount/trip_amount
green_taxi_3$tip_rate = green_taxi_3$Tip_amount / green_taxi_3$Total_amount

# categorize pick up time into 4 groups, 0-5 is dawn, 6-9 is morning, 10-15 is afternoon, 16-23 is evening
green_taxi_3$pickup_hr = as.POSIXlt(green_taxi_3$lpep_pickup_datetime)$hour
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
train_id = sample(seq(1,num,1), floor(num*0.8))
train = green_taxi_3[train_id, ]
test = green_taxi_3[-train_id,]

# build model with possible variables of trip kind
mod = lm(tip_rate ~ Passenger_count + Trip_distance + Fare_amount + 
            Extra + Tolls_amount + Trip_type + pickup_tm, train)
mse = mean((predict.lm(mod, test) - test$tip_rate)^2) # test the model, mse = 0.005

print(sprintf("The model explaining the relationship between the tip rate and kind of trip is built as Tip_rate = %.6f * Trip_distance + %.6f * Fare_amount + %.6f * Tolls_amount + %.6f * Trip_type_dispatch + %.6f * pickup_time_zone_morning + %.6f * pickup_time_zone_evening",
              mod$coefficients["Trip_distance"], mod$coefficients["Fare_amount"],
              mod$coefficients["Tolls_amount"], mod$coefficients["Trip_type2"],
              mod$coefficients["pickup_tm2"], mod$coefficients["pickup_tm4"]))
print("From the model, we can infer that to get better tip rate, the trip should be a long distance trip, low fare amount, high tolls amount, dispatch trip type, occurs in the evening.")

#3 Find anomaly from the data

print("From previous analysis, we find there are some abnormal observations from the data:")
print("1. pick up time is exactly the same as drop off time, or the difference are less than 1 minute")
print("2. trip_distance is short, it against the common sense that one person would not take taxi for a trip less than 0.5 mile")
print("3. trip period is small while the trip distance is relative large or the opposite situation, for example, one trip has a distance as 603.1 mile while the time cost is only about 11 minutes")
print("4. tip rate is very large, for example, one trip has $300 tip while the total amount is 303.3")
print("5. total amount is quite low or negative, it should be unusual that taking taxi cost almost nothing")








