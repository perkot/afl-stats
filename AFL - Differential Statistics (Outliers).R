# Outliers
# http://r-statistics.co/Outlier-Treatment-With-R.html

OUT_MG <- boxplot.stats(Matches_Numeric$MG_Diff)$out  # outlier values.
boxplot(Matches_Numeric$MG_Diff, 
        main="Metres Gained Outliers", 
        boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)


# For continuous variable (convert to categorical if needed.)
boxplot(MG_Diff ~ margin, 
        data = Matches_Numeric, 
        main = "Boxplot for metres gained & winning margin")

# cooks distance
mod <- lm(margin ~ ., 
          data = Matches_Numeric)
cooksd <- cooks.distance(mod)
# plot cooks distance
plot(cooksd, 
     pch="*", 
     cex=2,
     main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, 
     y=cooksd, 
     labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),
                   names(cooksd),""), 
     col="red")  # add labels


# join back 
Matches_Numeric[,"cooks_distance"] <- cooksd
# identify 4-times 
Matches_Numeric$Outlier <- ifelse(cooksd > 4*mean(cooksd), "Yes", "No")

# join back 
Matches[,"cooks_distance"] <- cooksd
# identify 4-times 
Matches$Outlier <- ifelse(cooksd > 4*mean(cooksd), "Yes", "No")
