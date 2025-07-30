FA8_RODILLAS
================
2024-04-11

``` r
mean <- 200
variance <- 256

#to find std, we can squareroot the variance

std<- variance**(1/2)

std
```

    ## [1] 16

``` r
#a
a_over224 <- 1 -pnorm(224, mean, std)

cat("a. Probability of signal over 224 µV = ", a_over224 * 100, "% \n")
```

    ## a. Probability of signal over 224 µV =  6.68072 %

``` r
#b. 
b_186to224 <-  pnorm(224, mean, std) - pnorm(186, mean, std)

cat("b. Probability of signal between 186 µV to 224 µV = ", b_186to224 * 100, "% \n")
```

    ## b. Probability of signal between 186 µV to 224 µV =  74.24058 %

``` r
#c. 
c_below25per <- qnorm(0.25, mean, std)
cat("c. Micro voltage below 25% = ", c_below25per, " \n")
```

    ## c. Micro voltage below 25% =  189.2082

``` r
#d
d_less240_more210 <- pnorm(240, mean, std, lower.tail = TRUE) - pnorm(210, mean, std, lower.tail = TRUE)
cat("d. Probability of less than 240 μV, given that it is larger than 210 μV = ",d_less240_more210 *100, "% \n")
```

    ## d. Probability of less than 240 μV, given that it is larger than 210 μV =  25.97759 %

``` r
#e
interquartile_range <- qnorm(0.75, mean, std) - qnorm(0.25, mean, std)
cat("d. Estimated interquartile range = ",interquartile_range, " \n")
```

    ## d. Estimated interquartile range =  21.58367

``` r
#f
f_less220_more210 <- pnorm(220, mean, std, lower.tail = TRUE) - pnorm(210, mean, std, lower.tail = TRUE)
cat("d. Probability of less than 220 μV, given that it is larger than 210 μV = ",f_less220_more210 *100, "% \n")
```

    ## d. Probability of less than 220 μV, given that it is larger than 210 μV =  16.03358 %

``` r
#g
g_more200_more220 <- (1 - pnorm(220, mean, std))/(1 - pnorm(200, mean, std))
cat("d. Probability of more than 220 μV, given that it is larger than 200 μV = ",g_more200_more220 *100, "% \n")
```

    ## d. Probability of more than 220 μV, given that it is larger than 200 μV =  21.12995 %

``` r
#n2
ave_dt <- 25
two_variance <- 144
#to find std, we can squareroot the variance

two_std <- two_variance **(1/2)
a_bound <- qnorm(0.95, ave_dt, two_std)
cat("a.bounds which includes 95% of the downtime = ", a_bound, "\n")
```

    ## a.bounds which includes 95% of the downtime =  44.73824

``` r
b_bound <-qnorm(0.10, ave_dt, two_std, lower.tail = FALSE)

cat("b.bound above which includes 10% of the downtime = ", b_bound, "\n")
```

    ## b.bound above which includes 10% of the downtime =  40.37862
