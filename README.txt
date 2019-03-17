{\rtf1\ansi\ansicpg1252\cocoartf1671\cocoasubrtf200
{\fonttbl\f0\fnil\fcharset0 AndaleMono;}
{\colortbl;\red255\green255\blue255;\red47\green255\blue18;\red0\green0\blue0;}
{\*\expandedcolortbl;;\cssrgb\c15686\c99608\c7843;\csgray\c0\c90000;}
\margl1440\margr1440\vieww10800\viewh8400\viewkind0
\pard\tx560\tx1120\tx1680\tx2240\tx2800\tx3360\tx3920\tx4480\tx5040\tx5600\tx6160\tx6720\pardirnatural\partightenfactor0

\f0\fs24 \cf2 \cb3 \CocoaLigature0 [1] "There are 1494926 data points"\
[1] "The daily mean of trip distance (in miles) grouped by hour are 2.4963,2.3834,2.351,2.3485,2.5048,2.673,2.685,2.3817,2.2369,2.2667,2.2364,2.1791,2.1933,2.1662,2.1451,2.1425,2.116,2.0598,2.0831,2.1285,2.1867,2.3174,2.4661,2.5176, from the 1st to 24th hour of the day"\
[1] "The daily median of trip distance (in miles) grouped by hour are 1.75,1.71,1.67,1.6,1.655,1.67,1.74,1.57,1.53,1.57,1.545,1.5,1.5,1.48,1.46,1.48,1.48,1.46,1.49,1.51,1.565,1.64,1.75,1.7542, from the 1st to 24th hour of the day"\
[1] "The model explaining the relationship between the tip rate and kind of trip is built as Tip_rate = 0.000690 * Trip_distance + -0.000646 * Fare_amount + 0.000958 * Tolls_amount + -0.033611 * Trip_type_dispatch + -0.002883 * pickup_time_zone_morning + 0.002668 * pickup_time_zone_evening"\
[1] "From the model, we can infer that to get better tip rate, the trip should be a long distance trip, low fare amount, high tolls amount, dispatch trip type, occurs in the evening."\
[1] "From previous analysis, we find there are some abnormal observations from the data:"\
[1] "1. pick up time is exactly the same as drop off time, or the difference are less than 1 minute"\
[1] "2. trip_distance is short, it against the common sense that one person would not take taxi for a trip less than 0.5 mile"\
[1] "3. trip period is small while the trip distance is relative large or the opposite situation, for example, one trip has a distance as 603.1 mile while the time cost is only about 11 minutes"\
[1] "4. tip rate is very large, for example, one trip has $300 tip while the total amount is 303.3"\
[1] "5. total amount is quite low or negative, it should be unusual that taking taxi cost almost nothing"\
}