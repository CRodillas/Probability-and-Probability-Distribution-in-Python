
================
2024-03-02

``` r
Sensor1_Isup <- 0.15
Sensor1_Irev <- 0.5
Sensor2_Isup <- 0.2
Sensor2_Irev <- 0.6
Sensor3_Isup <- 0.25
Sensor3_Irev <- 0.8
Sensor4_Isup <- 0.4
Sensor4_Irev <- 0.85

Sensor1_Wrev <- (Sensor1_Isup*Sensor1_Irev)*100
Sensor2_Wrev <- (Sensor2_Isup*Sensor2_Irev)*100
Sensor3_Wrev <- (Sensor3_Isup*Sensor3_Irev)*100
Sensor4_Wrev <- (Sensor4_Isup*Sensor4_Irev)*100

OverallSensor_Wrev <- Sensor1_Wrev+Sensor2_Wrev+Sensor3_Wrev+Sensor4_Wrev

cat("Overall percentage of relevant images:", OverallSensor_Wrev, "%")
```

    ## Overall percentage of relevant images: 73.5 %

To find the overall relevant images. Finding the weighted images per
sensor. With this, the percentage of relevancy in each sensor can be
known if multiplied by the percentage of the supplied images. Adding all
weighted percentage on each sensors together will result to the overall
relevant images

``` r
#Probs in a fair coin
p_head <- 0.5  
p_tail <- 0.5  

# prob pe event
p_Esub1 <- p_head * p_head + p_tail * p_tail  #hedhed + taltail 
p_Esub2 <- p_head * p_head + p_head * p_tail  #headfirst
p_Esub3 <- p_head * p_tail + p_head * p_head  #tailfirst


# intersection
#since only {HH} is the intersection for e1,e2,e3
p_Esub1_Esub2 <- p_head * p_head  
p_Esub1_Esub3 <- p_head * p_head  
p_Esub2_Esub3 <- p_head * p_head

# pairwise independent each
pairind_E1_E2 <- p_Esub1_Esub2 == p_Esub1 * p_Esub2
pairind_E1_E3 <- p_Esub1_Esub3 == p_Esub1 * p_Esub3
pairind_E2_E3 <- p_Esub2_Esub3 == p_Esub2 * p_Esub3


mutually_independent <- (p_Esub1 * p_Esub2 * p_Esub3) == (p_Esub1_Esub2 * p_Esub1_Esub3 * p_Esub2_Esub3)

cat("Pairwise Independence of E1 and E2: ", pairind_E1_E2, "\n")
```

    ## Pairwise Independence of E1 and E2:  TRUE

``` r
cat("Pairwise Independence of E1 and E3: ", pairind_E1_E3, "\n")
```

    ## Pairwise Independence of E1 and E3:  TRUE

``` r
cat("Pairwise Independence of E2 and E3: ", pairind_E2_E3, "\n")
```

    ## Pairwise Independence of E2 and E3:  TRUE

``` r
cat("Mutual Independence of E1, E2, and E3: ", mutually_independent, "\n")
```

    ## Mutual Independence of E1, E2, and E3:  FALSE

Since it shows TRUE in all three pairwise independence, this concludes
that E1, E2, and E3 are pairwise independent onto each other, but is not
mutually dependent. By this, it could be inferred that random variable/s
can be independent on each other, such as A and B or B and C, but all
three of them is not a subset of every other subset.
