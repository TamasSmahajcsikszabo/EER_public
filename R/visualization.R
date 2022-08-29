library(readr)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)

#` Boxplot
boxplot <- function(data, x = "day", y = "rpa_daily_tot_wpmc", xlab = "Day", ylab = "RPA Daily Total WPMC") {
    ggplot(data, aes_string(x = x, y = y, group = x)) +
        geom_boxplot() +
        theme_bw() +
        scale_x_continuous(breaks = seq(1, max(data[x], na.rm = TRUE))) +
        labs(
            x = xlab,
            y = ylab
        )
}

#` Frequency plot
frequency_plot <- function(data, x="rpa_daily_tot", labels=c("daily positive rumination", "frequencies")){
    freqs <- data %>%
    group_by_at(x) %>%
    summarize(N=n())
    
    #extracting missing data
    missing <- freqs[is.na(freqs[x]),]$N[[1]]
    
    # Y axis labels
    breaks <- freqs[x][!is.na(freqs[x])]
    print(length(breaks))
    if (length(breaks)>10){
        breaks[seq(1,length(breaks), by=2)] <- ""
    }

    ggplot(data=freqs)+ 
    geom_col(aes_string(x = x, y="N"), color="black", fill="grey70")+
    labs(x = labels[1], y= labels[2], caption=paste0("missing: ",missing, " daily observations")) +
    theme_light()+
    scale_x_continuous(breaks=)
}

#` Hexbin plot
hexplot <- function(data, x = "daily_posaff", y = "rpa_daily_tot_wpmc", xlab = "Daily Positive Aff.", ylab = "RPA Daily Total WMPC") {
    range1 <- range(data[x], na.rm=TRUE)
    range2 <- range(data[y], na.rm=TRUE)

    range1_width <- range1[2]-range1[1]
    range2_width <- range2[2]-range2[1]

    w2 <- range2_width/range1_width * .1

    ggplot(data, aes_string(x = x, y = y)) +
        geom_hex(binwidth = c(.1, w2), color="gray50", show.legend=FALSE)+
        geom_smooth(color='black')+
        theme_bw() +
        scale_fill_gradient(low = "white", high = "grey20") +
        scale_x_continuous(breaks = round(seq(min(data[x], na.rm=TRUE), max(data[x], na.rm = TRUE)))) +
        labs(
            x = xlab,
            y = ylab
        )
}
