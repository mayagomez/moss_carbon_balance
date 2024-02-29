


#### #### FIGURES & STATISTICAL TESTS #### #### #### #### #### #### #### #### #### #### #### #### 

#### COMPOSITE CURVE ####
#See seperate composite curve script

#### BOXPLOTS ####
#Import Dataset
summary_stats_full <- readxl::read_excel("/Users/migomez/Documents/MIDD/4 - Fourth Year (2019-2020)/MOSS Dr. Coe (BIOL 0700)/2019 Sheep Creek/summary_stats_full.xlsx")
summary_stats_full$Macrosite <- factor(summary_stats_full$Macrosite, levels = c('Low', 'Mid', 'High'))


#### Macrosite ####

## TOTAL CBAL
figure2 <- ggplot(summary_stats_full, aes(x=Macrosite, y=total_cbal, fill=Macrosite)) + 
  geom_boxplot(alpha=1) + 
  labs(x="Macrosite",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
  theme_classic() +
  ggpubr::fill_palette(palette = 'npg')+
  ggpubr::color_palette(palette = 'npg')
#Significant difference in the means? One-way anova: No
aov(total_cbal~Macrosite, data = summary_stats_full) %>% summary()
means <- aggregate(total_cbal ~  Macrosite, summary_stats_full, mean)




## A PHASE by Macrosite
figure2_a <- ggplot(summary_stats_full, aes(x=Macrosite, y=aphase_area, fill=Macrosite)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Macrosite",y= bquote ('aphase_area' ~ (mu~mol ~ m^-2)))+
  coord_cartesian(ylim = c(-0.5, 0.3)) +
  theme_classic() +
  ggpubr::fill_palette(palette = 'npg')+
  ggpubr::color_palette(palette = 'npg')
#Significant difference in the means? Yes... but then NO...
aov(aphase_area~Macrosite, data = summary_stats_full) %>% summary()
aov(aphase_area~Macrosite, data = summary_stats_full) %>% TukeyHSD()

## B PHASE by Macrosite
figure2_b <- ggplot(summary_stats_full, aes(x=Macrosite, y=bphase_area, fill=Macrosite)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Macrosite",y= bquote ('bphase_area' ~ (mu~mol ~ m^-2)))+
  coord_cartesian(ylim = c(-0.5, 0.3)) +
  theme_classic() +
  ggpubr::fill_palette(palette = 'npg')+
  ggpubr::color_palette(palette = 'npg')
#Significant difference in the means? No
aov(bphase_area~Macrosite, data = summary_stats_full) %>% summary()
aov(bphase_area~Macrosite, data = summary_stats_full) %>% TukeyHSD()

## C PHASE by Macrosite
figure2_c <- ggplot(summary_stats_full, aes(x=Macrosite, y=cphase_area, fill=Macrosite)) + 
  geom_boxplot(alpha=1) + 
  labs(x="Macrosite",y= bquote ('cphase_area' ~ (mu~mol ~ m^-2)))+
  coord_cartesian(ylim = c(-0.5, 0.3)) +
  theme_classic() + 
  ggpubr::fill_palette(palette = 'npg')+
  ggpubr::color_palette(palette = 'npg')
#Significant difference in the means? No
aov(cphase_area~Macrosite, data = summary_stats_full) %>% summary()
aov(cphase_area~Macrosite, data = summary_stats_full) %>% TukeyHSD()

# library(grid)
# library(gridExtra)
grid.arrange(figure2_a, figure2_b, figure2_c, ncol=3)

## ALL Macrosites: PHASE Comparisons
#Significant difference? Yes
aov(cphase_area~aphase_area, data = summary_stats_full) %>% summary()
# aov(aphase_area~bphase_area, data = summary_stats_full) %>% summary()
# aov(cphase_area~aphase_area, data = summary_stats_full) %>% summary()


#### TOPOGRAPHY ####
figure3 <- ggplot(summary_stats_full, aes(x=Topography, y=total_cbal, fill=Topography)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Topography",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
  theme_classic() + scale_fill_brewer(palette="Set2") +
  facet_wrap(~Macrosite, ncol = 3)

#Significant difference in the means? No (Two-way anova with an interaction term)
#aov(total_cbal~Macrosite+Topography+Macrosite:Topography, data = summary_stats_full) %>% summary()
aov(total_cbal~Macrosite*Topography, data = summary_stats_full) %>% summary()
aov(aphase_area~Macrosite*Topography, data = summary_stats_full) %>% summary()
aov(bphase_area~Macrosite*Topography, data = summary_stats_full) %>% summary()
aov(cphase_area~Macrosite*Topography, data = summary_stats_full) %>% summary()

## this nests microhabitat into macrosite (macro is dep, micro is not)
# summary(aov(total_cbal~Macrosite/Microhabitat, data = summary_stats_full))
## this is an interaction 
# summary(aov(total_cbal~Macrosite*Microhabitat, data = summary_stats_full))

figure3_a <- ggplot(summary_stats_full, aes(x=Topography, y=aphase_area, fill=Topography)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  theme_classic() + 
  labs(x="Topography",y= bquote ('aphase_area' ~ (mu~mol ~ m^-2)))+
  scale_fill_brewer(palette="Reds") +
  facet_wrap(~Macrosite, ncol = 3)
figure3_b <- ggplot(summary_stats_full, aes(x=Topography, y=bphase_area, fill=Topography)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Topography",y= bquote ('bphase_area' ~ (mu~mol ~ m^-2)))+
  theme_classic() +
  scale_fill_brewer(palette="Blues") +
  facet_wrap(~Macrosite, ncol = 3)
figure3_c <- ggplot(summary_stats_full, aes(x=Topography, y=cphase_area, fill=Topography)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Macrosite",y= bquote ('cphase_area' ~ (mu~mol ~ m^-2)))+
  theme_classic() + scale_fill_brewer(palette="Greens") +
  facet_wrap(~Macrosite, ncol = 3)



#### MICROHABITAT #### 
#Serious gaps in data... not reliable.
figure4 <- ggplot(summary_stats_full, aes(x=Microhabitat, y=total_cbal, fill=Topography)) + 
  geom_boxplot(alpha=1,show.legend = FALSE) + 
  labs(x="Microhabitat",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
  theme_classic() + scale_fill_brewer(palette="Set2") +
  facet_wrap(~Macrosite, ncol = 3)

#Significant difference in the means? No
aov(total_cbal~Macrosite*Microhabitat, data = summary_stats_full) %>% summary()
aov(aphase_area~Macrosite*Microhabitat, data = summary_stats_full) %>% summary()
aov(bphase_area~Macrosite*Microhabitat, data = summary_stats_full) %>% summary()
aov(cphase_area~Macrosite*Microhabitat, data = summary_stats_full) %>% summary()

# figure4a <- ggplot(summary_stats_full, aes(x=Microhabitat, y=aphase_area, fill=Topography)) + 
#   geom_boxplot(alpha=1,show.legend = FALSE) + 
#   labs(x="Microhabitat",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
#   theme_classic() + scale_fill_brewer(palette="Set2") +
#   facet_wrap(~Macrosite, ncol = 3)
# figure4b <- ggplot(summary_stats_full, aes(x=Microhabitat, y=bphase_area, fill=Topography)) + 
#   geom_boxplot(alpha=1,show.legend = FALSE) + 
#   labs(x="Microhabitat",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
#   theme_classic() + scale_fill_brewer(palette="Set2") +
#   facet_wrap(~Macrosite, ncol = 3)
# figure4c <- ggplot(summary_stats_full, aes(x=Microhabitat, y=cphase_area, fill=Topography)) + 
#   geom_boxplot(alpha=1,show.legend = FALSE) + 
#   labs(x="Microhabitat",y= bquote ('total_cbal' ~ (mu~mol ~ m^-2)))+
#   theme_classic() + scale_fill_brewer(palette="Set2") +
#   facet_wrap(~Macrosite, ncol = 3)


#### MAX Respiration & Photosynthesis ####
aov(R_maxval_A~Macrosite, data = summary_stats_full) %>% summary()
aov(R_maxtime_A~Macrosite, data = summary_stats_full) %>% summary()
aov(P_maxval~Macrosite, data = summary_stats_full) %>% summary()
aov(P_maxtime~Macrosite, data = summary_stats_full) %>% summary()
aov(R_maxval_C~Macrosite, data = summary_stats_full) %>% summary()
aov(R_maxtime_C~Macrosite, data = summary_stats_full) %>% summary()

aov(CO2comp1~Macrosite, data = summary_stats_full) %>% summary()
aov(CO2comp2~Macrosite, data = summary_stats_full) %>% summary()

# A vs. C Between Sites
aov(R_maxval_A~R_maxval_C, data = summary_stats_full) %>% summary()

aov(R_maxval_A~R_maxval_C + Macrosite, data = summary_stats_full) %>% summary()
aov(R_maxval_A~R_maxval_C + Macrosite, data = summary_stats_full) %>% TukeyHSD()
#Yes!!

#### CURVE END TIME ####
aov(End_Time~Macrosite, data = summary_stats_full) %>% summary()
aov(End_Time~Macrosite, data = summary_stats_full) %>% TukeyHSD()



#### HISTOGRAM of Wet SA ####

#Preliminary Tests
#Yes: Wet SA has an sig effect on MacrositeHigh
lm(formula = aphase_area~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()
# lm(formula = bphase_area~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()
# lm(formula = cphase_area~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()
# lm(formula = total_cbal~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()
lm(formula = End_Time~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()
lm(formula = End_Time~`Wet SA` + Macrosite, data = summary_stats_full) %>% summary()



# Histogram of Wet SA 
ggplot(summary_stats_full, aes(x=`Wet SA`,fill=Macrosite))+
  geom_histogram(position=position_dodge2(), binwidth = 0.5)+
  theme_classic()+
  ggpubr::fill_palette(palette = 'npg')

ggplot(summary_stats_full, aes(x=`Wet SA`,fill=Macrosite))+
  geom_density(color = 'transparent', alpha = .5)+
  geom_rug(aes(color = Macrosite))+
  theme_classic()+
  ggpubr::fill_palette(palette = 'npg')+
  ggpubr::color_palette(palette = 'npg')


#### WET SA by Macrosite ####
aov(`Wet SA`~Macrosite, data = summary_stats_full) %>% summary()
aov(`Wet SA`~Macrosite, data = summary_stats_full) %>% TukeyHSD()



# Different Visual
# install.packages('ggpubr')
# library(ggpubr)
# symnum.args <- list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), symbols = c("****", "***", "**", "*", "ns"))
# ggboxplot(data = summary_stats_full, x='Topography', y='total_cbal', fill='Topography',
#           palette = 'npg', facet.by = 'Macrosite')+
#   stat_compare_means(data = summary_stats_full,
#                      symnum.args = symnum.args, method = "t.test", paired = FALSE,
#                 ref.group = 'F') +
#   stat_compare_means(method = 'anova')


# mvclm <- lm(data = summary_stats_full, formula = total_cbal ~ factor(Topography) + factor(Macrosite) + factor(Topography):factor(Macrosite) + factor(Topography)*factor(Macrosite) )
# mvclm <- lm(data = summary_stats_full, formula = total_cbal ~ relevel(factor(Topography), ref = 'N'))
# mvclm %>% summary



#### CORRELOGRAM ####
#install.packages("corrplot")
library(corrplot)

summary_stats_part <- as.data.frame(summary_stats_full[,6:24], drop=TRUE)
cor1 <- cor(summary_stats_part, method = c("pearson"), use = "complete.obs")

#?corrplot # for info
figure3 <- corrplot(cor1, type = "upper", order = "hclust", # use "hclust" for hier clustering
                    tl.col = "black", tl.srt = 45) # use this one

# matrix of the p-value of the correlation
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
p.mat <- cor.mtest(cor1)

# Leave blank on no significant coefficient
figure3b <- corrplot(cor1, type="upper", order="hclust", tl.col = "black", tl.srt = 45,
                     p.mat = p.mat, sig.level = 0.05, insig = "blank")

# To correlation coefficients
figure3c <- corrplot(cor1, method="color",  
                     type="upper", order="hclust", 
                     addCoef.col = "black", # Add coefficient of correlation
                     tl.col="black", tl.srt=45, #Text label color and rotation
                     # Combine with significance
                     p.mat = p.mat, sig.level = 0.05, insig = "blank", 
                     # hide correlation coefficient on the principal diagonal
                     diag=TRUE, number.cex=0.5
)
# This is the ONE!
figure3d <- corrplot(cor1, type = "upper", order = "hclust", 
                     p.mat = p.mat, sig.level = 0.05, tl.col = "black", tl.srt = 45) # use this one


# Make a table w/ Correlation Coefficients & P Values
# install.packages("Hmisc")
# library("Hmisc")
cor_df <- rcorr(as.matrix(summary_stats_part))
# Extract correlation coefficients
cormat2 <- cor_df$r
# Extract p values 
pmat2 <- cor_df$P
flattenCorrMatrix <- function(cormat2, pmat2) {
  ut <- upper.tri(cormat2)
  data.frame(
    row = rownames(cormat2)[row(cormat2)[ut]],
    column = rownames(cormat2)[col(cormat2)[ut]],
    cor  =(cormat2)[ut],
    p = pmat2[ut]
  )
}
cor_df2 <- rcorr(as.matrix(summary_stats_part[,1:20]))
final_cor_df <- flattenCorrMatrix(cor_df2$r, cor_df2$P)
