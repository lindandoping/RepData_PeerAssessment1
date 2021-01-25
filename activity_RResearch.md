Activity Research Analysis
--------------------------

The response to the following questions are as follows

### Loading and Processing the data

    library(readr);library(dplyr);library(lubridate);library("ggplot2")
    library("Hmisc");library(reshape)
    adata<-read_csv("/Users/mndoping/Desktop/DataScience/JHopkins Data Science/Reproducible Research/activity.csv")
    head(adata)

    ## # A tibble: 6 x 3
    ##   steps date       interval
    ##   <dbl> <date>        <dbl>
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### Average Number of Steps Taken per day

    #Group data by date
    adata_s<- adata %>% group_by(date) %>% 
    summarise(Totalsteps=sum(steps,na.rm = TRUE))

    #Plot of the histogram total steps per day
    ggplot(adata_s, aes(x=Totalsteps))+geom_histogram(binwidth =5000, fill="lightcoral",
                                                   color="black")

<img src="Figs/total steps-1.png" style="display: block; margin: auto;" />

    #Mean and median of total steps taken per day
    adata_smean<-round(mean(adata_s$Totalsteps),2)
    adata_smedian<-round(median(adata_s$Totalsteps),2)

The mean of the total steps per day is 9354.23 and the median of the
total steps per day is 1.039510^{4}.

### Average daily Activity pattern

    #Time series plot- Average daily pattern
    adata_int<- adata %>% group_by(interval) %>% 
      summarise(steps_mean=mean(steps, na.rm = TRUE))
                
    g2<-ggplot(adata_int, aes(x=interval, y=steps_mean))+geom_line(color="salmon2")+
      ggtitle("Time series plot of Average Daily Pattern ")
    g2

<img src="Figs/daily activity pattern-1.png" style="display: block; margin: auto;" />

    #Determine 5min interval with maximum number of steps
    max<-max(adata_int$steps_mean)
    adata_max<-subset(adata_int, steps_mean==max)$interval

The 5-minute interval that contains maximum number of steps is 835.

### Imputing Missing Values

    library(knitr);library(kableExtra)
    #Determining the total number of missing values data 
    na_sum<-sum(is.na(adata))
    c<-subset(adata, is.na(steps))
    na_rows<- c %>% group_by(date) %>% 
      summarise(missing_sum=length(steps))
    na_rows %>%
      kbl(caption = "Total missing rows per day") %>%
      kable_classic_2(full_width = F)

<table class=" lightable-classic-2" style='font-family: "Arial Narrow", "Source Sans Pro", sans-serif; width: auto !important; margin-left: auto; margin-right: auto;'>
<caption>
Total missing rows per day
</caption>
<thead>
<tr>
<th style="text-align:left;">
date
</th>
<th style="text-align:right;">
missing\_sum
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
2012-10-01
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-10-08
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-01
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-04
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-09
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-10
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-14
</td>
<td style="text-align:right;">
288
</td>
</tr>
<tr>
<td style="text-align:left;">
2012-11-30
</td>
<td style="text-align:right;">
288
</td>
</tr>
</tbody>
</table>

    #Imputing mean using impute function
    adata_imp<-impute(adata$steps,fun=mean)
    adata$impute<-adata_imp

The total number of missing values is 2304. A break down of the missing
values showed that these values were specific to entire days as seen
above. The impute function from Hmisc package, using mean was used to
replace missing values.

    #Plot histogram of total steps taken per day including imputed values
    adata_new<- adata %>% group_by(date) %>% 
      summarise(Totalsteps=sum(impute))
    g3<-ggplot(adata_new, aes(x=Totalsteps))+geom_histogram(binwidth =5000, fill="cyan1",color="black")

    g3

<img src="Figs/hist_impute-1.png" style="display: block; margin: auto;" />

    #Mean and median of total steps taken per day of new data with imputed values
    adata_nmean<-round(mean(adata_new$Totalsteps),2)
    adata_nmedian<-round(median(adata_new$Totalsteps),2)

    #Impact of imputing values
    mean_dif<-round((adata_nmean-adata_smean),2)
    median_dif<-round((adata_nmedian-adata_smedian),2)
    total_dif<-round((sum(adata_new$Totalsteps) - sum(adata_s$Totalsteps)),2)

The mean difference is 1411.96, the median difference is 371.19, and the
total difference is 8.61295110^{4}. Overall, as seen from values,
imputed data shows higher values as expected.

### Difference in Activity Patterns Between Weekdays and Weekends

The difference in weekday and weekend activity patterns can be seen
below

    #Identifying Weekdays 
    adata$wday<-wday(adata$date, label=TRUE)
    for (i in 1:length(adata$wday)){
      if(adata$wday[i]=="Sat"|adata$wday[i]=="Sun"){
        adata$wdayL[i]<-1
      } else {adata$wdayL[i]<-2}
    }

    # Group  Data for plots
    wkdy<-subset(adata, adata$wdayL==2)
    wknd<-subset(adata, adata$wdayL==1)

    wkdyi<- wkdy %>% group_by(interval) %>% 
      summarise(imp_wkdy=mean(impute))
    wkndi<- wknd %>% group_by(interval) %>% 
      summarise(imp_wknd=mean(impute))
    wkdyi$imp_wknd<-wkndi$imp_wknd
    id<-data.frame("id"=c(1:288))
    wkdyi<-cbind(id, wkdyi)

    #Reshape Data for Plots
    wkdyi<-melt(wkdyi, c("id","interval"))

    g<-ggplot(wkdyi, aes(x=interval, y=value, color=variable))+geom_line()+
      facet_wrap(~variable, ncol=1)
    g

<img src="Figs/difference patterns-1.png" style="display: block; margin: auto;" />
