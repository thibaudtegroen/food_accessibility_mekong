# get fishbase data for IUCN hydrobasins referenced data

# Thibaud te Groen 

# oktober 2022

#code adapted from Tamara Keijzer IUCN_hybas_fishbase.R 

# Import & cleaning Data -----------------------------------------------------------



#set directory
setwd("C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase")
install.packages('gridExtra')

install.packages("sf")
library(sf)
library(dplyr);library(foreach);library(rfishbase);library(ggplot2);library(grid); library(gridExtra);
library(ggpubr)

options(FISHBASE_VERSION="21.06")


# load species data

#sp <-  read_sf("species_ranges_mekong.gpkg")
sp_1 <- read.csv("Mekong_fbnames_IUCNhybas.csv")

nutr <- read.csv("fish_protein_github.csv")

tam_species <- read.csv("Mekong_fbnames_IUCNhybas.csv")

nutr <- read.csv("fish_protein_github.csv")

val_species <- read_sf('species_ranges_mekong.gpkg')

#rename column 'scientific name' to 'fb_name'
colnames(nutr)[1] ="fb_name"




# Synonyms

fb_names=array()

your_list=unique(nutr$species) 

for(i in 1:length(your_list)) {
  
  fb_names[i]=as.character(validate_names(your_list[i])[1])
  
}
match_species=data.frame(binomial=your_list,fb_names=fb_names)

write.csv(match_species,'C:/Industrial Ecology/Jaar 3/Masterscriptie/Databases/Fishbase/match_nutr.csv', row.names = F)

match_nutr <- read.csv("match_nutr.csv")

# Create species and nutrient dataframe ------------------------------------------------


species_fb <- match_nutr$fb_name



# from species table + traits.sp is nieuwe dataframe met values zoals species, brack per soort

traits.sp <- species(species_fb, # vector of species names
                     
                     fields = c("Species", "Brack","Saltwater","Fresh","AnaCat","Length", "Importance"))

colnames(traits.sp) = c("fb_name", "Brackish","Saltwater","Freshwater","Migration","Length", "Commercial")

traits.sp$Migration[traits.sp$Migration == " "] <- NA


sp_3 <- left_join(traits.sp, nutr, by = "fb_name")
sp_2 <- sp_3[!is.na(sp_3$Selenium_mu),] #exlude species with NA values
sp_4 <- sp_2[!duplicated(sp_2$fb_name),] #unique values


# Barplot Calcium ---------------------------------------------------------


#calcium barplot

calcium_barplot   <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name, Calcium_mu),
                         y = Calcium_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+
  
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, 
                              y = Calcium_mu, 
                              ymin= Calcium_l95, 
                              ymax= Calcium_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 2000, 
                                  by = 100))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Calcium (mg/100g)")

ggsave(filename = "calcium_barplot.png", plot = calcium_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(calcium_barplot )



# Barplot Iron ------------------------------------------------------------


#Iron barplot

iron_barplot  <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name,Iron_mu),
                         y = Iron_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+
  
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Iron_mu, 
                              ymin= Iron_l95, 
                              ymax= Iron_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 15, 
                                  by = 0.5))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Iron (mg/100g)")

ggsave(filename = "iron_barplot.png", plot = iron_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(iron_barplot)


# Barplot Omega 3 PUFA ----------------------------------------------------


#Omega 3 PUFA barplot

omega_3_barplot <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name,Omega_3_mu),
                         y = Omega_3_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+
  
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Omega_3_mu, 
                              ymin= Omega_3_l95, 
                              ymax= Omega_3_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 4, 
                                  by = 0.1))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Omega 3 PUFA (g/100g)")

ggsave(filename = "omega_3_barplot.png", plot = omega_3_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(omega_3_barplot)

# Barplot Protein ---------------------------------------------------------

#Protein barplot

protein_barplot <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name,Protein_mu),
                         y = Protein_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+
  
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Protein_mu, 
                              ymin= Protein_l95, 
                              ymax= Protein_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 22, 
                                  by = 2))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Protein (g/100g)")

ggsave(filename = "protein_barplot.png", plot = protein_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(protein_barplot)

# Barplot Selenium --------------------------------------------------------


#Selenium


selenium_barplot <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name, Selenium_mu),
                         y = Selenium_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+
  
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Selenium_mu, 
                              ymin=Selenium_l95, 
                              ymax=Selenium_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 800, 
                                  by = 50))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Selenium (µg/100g)")

ggsave(filename = "selenium_barplot.png", plot = selenium_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)


print(selenium_barplot)


# Barplot Vitamin A -------------------------------------------------------


#Vitamim A


vit_A_barplot <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name,Vitamin_A_mu),
                         y = Vitamin_A_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
  )+

  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Vitamin_A_mu, 
                              ymin=Vitamin_A_l95, 
                              ymax=Vitamin_A_u95), 
                width=.3,
  )+
  
  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 2000, 
                                  by = 50))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
        ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
  ) +
  
  labs(x= "Scientific name", y= "Vitamin A (µg/100g)")  
  
ggsave(filename = "vitamin_A_barplot.png", plot = vit_A_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(vit_A_barplot)

# Barplot Zinc ------------------------------------------------------------

zinc_barplot <- 
  
  ggplot (sp_4) +
  geom_col(mapping = aes(x = reorder(fb_name,Zinc_mu),
                         y = Zinc_mu),
           fill = "grey", 
           width = 0.7,
           color = "black"
           )+
  #error bars
  geom_errorbar(mapping = aes(x = fb_name, y = Zinc_mu, 
                              ymin=Zinc_l95, 
                              ymax=Zinc_u95), 
                width=.3,
                position = position_dodge(.8))+

  scale_y_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                        to = 15, 
                                        by = 0.4))+
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        axis.line = element_line(size = 0.5, 
                                 colour = "black", 
                                 ),
        axis.title =  element_text (size = 15, color = "black",
                                    face = "bold"),
        panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        axis.text.y = element_text (margin = margin(t = 10)),
        axis.ticks.x = element_blank()
        ) +
  
  labs(x= "Scientific name", 
       y= "Zinc (mg/100g)")  
  

ggsave(filename = "Zinc_barplot.png", plot = zinc_barplot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

print(zinc_barplot)



# Max and Min values ------------------------------------------------------




#max values index
print(paste("highest index Calcium is : ",which.max(sp_4$Calcium_mu)))
print(paste("highest index Iron is : ",which.max(sp_4$Iron_mu)))
print(paste("highest index Omega 3 is : ",which.max(sp_4$Omega_3_mu)))
print(paste("highest index Protein is : ",which.max(sp_4$Protein_mu)))
print(paste("highest index Sel is : ",which.max(sp_4$Selenium_mu)))
print(paste("highest index Vit A is : ",which.max(sp_4$Vitamin_A_mu)))
print(paste("highest index Zinc is : ",which.max(sp_4$Zinc_mu)))

#min value index
print(paste("highest index Calcium is : ",which.min(sp_4$Calcium_mu)))
print(paste("highest index Iron is : ",which.min(sp_4$Iron_mu)))
print(paste("highest index Omega 3 is : ",which.min(sp_4$Omega_3_mu)))
print(paste("highest index Protein is : ",which.min(sp_4$Protein_mu)))
print(paste("highest index Sel is : ",which.min(sp_4$Selenium_mu)))
print(paste("highest index Vit A is : ",which.min(sp_4$Vitamin_A_mu)))
print(paste("highest index Zinc is : ",which.min(sp_4$Zinc_mu)))

#max value
print(paste("highest value Calcium is : ",max(sp_4$Calcium_mu)))
print(paste("highest value Iron is : ",max(sp_4$Iron_mu)))
print(paste("highest value Omega 3 is : ",max(sp_4$Omega_3_mu)))
print(paste("highest value Protein is : ",max(sp_4$Protein_mu)))
print(paste("highest value Sel is : ",max(sp_4$Selenium_mu)))
print(paste("highest value Vit A is : ",max(sp_4$Vitamin_A_mu)))
print(paste("highest value Zinc is : ",max(sp_4$Zinc_mu)))

#min value
print(paste("highest value Calcium is : ",min(sp_4$Calcium_mu)))
print(paste("highest value Iron is : ",min(sp_4$Iron_mu)))
print(paste("highest value Omega 3 is : ",min(sp_4$Omega_3_mu)))
print(paste("highest value Protein is : ",min(sp_4$Protein_mu)))
print(paste("highest value Sel is : ",min(sp_4$Selenium_mu)))
print(paste("highest value Vit A is : ",min(sp_4$Vitamin_A_mu)))
print(paste("highest value Zinc is : ",min(sp_4$Zinc_mu)))


# Variability -------------------------------------------------------------


#coefficient of variation



cv_calcium <- sd(sp_4$Calcium_mu)/ mean(sp_4$Calcium_mu)
cv_Iron <- sd(sp_4$Iron_mu)/ mean(sp_4$Iron_mu) 
cv_Omega_3 <- sd(sp_4$Omega_3_mu)/ mean(sp_4$Omega_3_mu) 
cv_Protein <- sd(sp_4$Protein_mu)/ mean(sp_4$Protein_mu) 
cv_Selenium <- sd(sp_4$Selenium_mu)/ mean(sp_4$Selenium_mu) 
cv_Vitamin_A <- sd(sp_4$Vitamin_A_mu)/ mean(sp_4$Vitamin_A_mu) 
cv_Zinc <- sd(sp_4$Zinc_mu)/ mean(sp_4$Zinc_mu) 

var1 <- data.frame(cv_calcium, cv_Iron, cv_Omega_3, cv_Protein, cv_Selenium,cv_Vitamin_A, cv_Zinc)
var2 <- data.frame(sd(sp_4$Calcium_mu), sd(sp_4$Iron_mu), sd(sp_4$Omega_3_mu), 
                 sd(sp_4$Protein_mu), sd(sp_4$Selenium_mu), sd(sp_4$Vitamin_A_mu), 
                 sd(sp_4$Zinc_mu)) 
var3 <- data.frame(mean(sp_4$Calcium_mu), mean(sp_4$Iron_mu), mean(sp_4$Protein_mu),mean(sp_4$Omega_3_mu),
                   mean(sp_4$Selenium_mu),
                   mean(sp_4$Vitamin_A_mu), mean(sp_4$Zinc_mu))

cv_df <- data.frame(var1)





write.csv(cv_df,"C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken\\cv.csv", row.names = FALSE)

edcdf_cal <- ecdf(sp_4$Calcium_mu) 
edcdf_iron <- ecdf(sp_4$Iron_mu)
edcdf_omega_3 <- ecdf(sp_4$Omega_3_mu)
edcdf_prot <- ecdf(sp_4$Protein_mu)
edcdf_sel <- ecdf(sp_4$Selenium_mu)
edcdf_vit_A <- ecdf(sp_4$Vitamin_A_mu)
edcdf_zinc <- ecdf(sp_4$Zinc_mu)

#plot_cal <- ggplot(edcdf_cal, verticals=TRUE, do.points=FALSE)
#plot_iron <- plot(edcdf_iron, verticals=TRUE, do.points=FALSE, add=TRUE, col='red')
#plot_omega_3 <- plot(edcdf_omega_3, verticals=TRUE, do.points=FALSE, add=TRUE, col='pink')
#plot_prot <- plot(edcdf_prot, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
#plot_sel <- plot(edcdf_sel, verticals=TRUE, do.points=FALSE, add=TRUE, col='brown')
#plot_vit_A <- plot(edcdf_vit_A, verticals=TRUE, do.points=FALSE, add=TRUE, col='orange')
#plot_zinc <- plot(edcdf_zinc, verticals=TRUE, do.points=FALSE, add=TRUE, col= 'grey')

#par(mfrow = c(3,3))
#par(mfrow = c(1,1))

#install.packages('ggpubr')
##ggarrange(plot_cal, plot_iron, plot_omega_3 + rremove("x.text"), 
          #labels = c("A", "B", "C"),
          #ncol = 2, nrow = 2)

plot_cal_1 <- ggplot(sp_4, aes(x = Calcium_mu)) + 
  stat_ecdf(mapping = aes(x = Calcium_mu), 
            geom = "line", 
            color = 'black',
            ) +
  labs(y = "f(calcium)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 900, 
                                  by = 100))
plot_cal_1

plot_cal_2 <- ggplot(sp_4, aes(x = Iron_mu)) + 
  stat_ecdf(mapping = aes(), geom = "line") +
  labs(y = "f(iron)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 10, 
                                  by = 1))
plot_cal_2

plot_cal_3 <- ggplot(sp_4, aes(x = Omega_3_mu )) + 
  stat_ecdf(mapping = aes(), geom = "line" ) +
  labs(y = "f(Omega 3)")+
  scale_x_continuous(expand = expansion (0),
                   breaks = seq(from=0, 
                                to = 1.5, 
                                by = 0.3))
plot_cal_3

plot_cal_4 <- ggplot(sp_4, aes(x = Protein_mu )) + 
  stat_ecdf(mapping = aes(), geom = "line") +
  labs(y = "f(Protein)")+
  scale_x_continuous(expand = expansion (0),
                   breaks = seq(from=15, 
                                to = 20, 
                                by = 1))
plot_cal_4

plot_cal_5 <- ggplot(sp_4, aes(x = Selenium_mu)) + 
  stat_ecdf(mapping = aes(), geom = "line") +
  labs(y = "f(Selenium)")+
  scale_x_continuous(expand = expansion (0),
                   breaks = seq(from=0, 
                                to = 400, 
                                by = 50))
plot_cal_5

plot_cal_6 <- ggplot(sp_4, aes(x = Vitamin_A_mu )) + 
  stat_ecdf(mapping = aes(), geom = "line") +
  labs(y = "f(Vitamin A)") +
  scale_x_continuous(expand = expansion (0),
                     breaks = seq(from=0, 
                                  to = 600, 
                                  by = 50))
plot_cal_6

plot_cal_7 <- ggplot(sp_4, aes(x = Zinc_mu)) + 
  stat_ecdf(mapping = aes(), geom = "line") +
  labs(y = "f(Zinc)")+
  scale_x_continuous(expand = expansion (0),
                   breaks = seq(from=0, 
                                to = 5, 
                                by = 1))
plot_cal_7


ecdf_plot = ggarrange(plot_cal_1,plot_cal_2, plot_cal_3, plot_cal_4, plot_cal_5, plot_cal_6, plot_cal_7,
          labels = c("A","B","C","D","E","F","G"),
         ncol = 3, nrow =3)

ggsave(filename = "ecdf_plot.png", plot = ecdf_plot,
       path = "C:/Industrial Ecology/Jaar 3/Masterscriptie/grafieken",
       device = png, width = 10, height = 7, dpi = 300)

# Outliers ----------------------------------------------------------------


#outliers calcium
lower_bound <- quantile(sp_4$Calcium_mu, 0.05)
upper_bound <- quantile(sp_4$Calcium_mu, 0.95)
outlier_ind_cal <- which(sp_4$Calcium_mu< lower_bound | sp_4$Calcium_mu > upper_bound)
sp_4[outlier_ind_cal,]

outliers_calcium <- boxplot.stats(sp_4$Calcium_mu)$out
boxplot(sp_4$Calcium_mu,
        ylab = "Calcium"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))


#outliers Iron
lower_bound <- quantile(sp_4$Iron_mu, 0.025)
upper_bound <- quantile(sp_4$Iron_mu, 0.975)
outlier_ind_iron <- which(sp_4$Iron_mu< lower_bound | sp_4$Iron_mu > upper_bound)
sp_4[outlier_ind_iron,]

outliers_Iron <- boxplot.stats(sp_4$Omega_3_mu)$out
boxplot(sp_4$Omega_3_mu,
        ylab = "Iron"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

#Outliers Omega_3


#Outliers Protein


#Outliers_Selenium

#Outliers_Vitamin A


out <- boxplot.stats(sp_4$Omega_3_mu)$out
boxplot(sp_4$Omega_3_mu,
        ylab = "Omega_3"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))
