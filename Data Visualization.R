library(ggplot2)
library(cowplot)

age_dat <- read.csv("https://raw.githubusercontent.com/Tanvi-Jindal/Vaccination-Mapping/master/age_data.csv")
age_dat$Age <- as.factor(age_dat$Age)
age_dat$PopLog <- log(age_dat$Population)
age_dat$sqrtVac <- sqrt(age_dat$Vaccine)
age_dat$Month <- factor(age_dat$Month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))

race_dat <- read.csv("https://raw.githubusercontent.com/Tanvi-Jindal/Vaccination-Mapping/master/race_data.csv")
race_dat$Race <- relevel(race_dat$Race, "White")
race_dat$PopLog <- log(race_dat$Population)
race_dat$sqrtVac <- sqrt(race_dat$Vaccine)
race_dat$Month <- factor(race_dat$Month, levels = c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr"))

#Visualizing variables - age
##Vaccine
qqnorm(age_dat$Vaccine)
qqline(age_dat$Vaccine)
ggplot(age_dat, aes(Vaccine))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate")
ggplot(age_dat, aes(Vaccine, color = Season))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate \n by Season")
ggplot(age_dat, aes(Vaccine, color = Age))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate \n by Age Group")
#ggplot(age_dat, aes(Vaccine, color = Age))+geom_density()+ facet_grid(age_dat$Season) + labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate \n by Age Group")
ggplot(age_dat, aes(sqrtVac), color = Season))+geom_density()+ labs(x="Vaccine Coverage (square rooted)", title="Density plot of Vaccination Rate \n by Season")
qqnorm(age_dat$sqrtVac)
qqline(age_dat$sqrtVac)
##Population
qqnorm(age_dat$Population)
qqline(age_dat$Population)
ggplot(age_dat, aes(Population))+geom_density()
ggplot(age_dat, aes(PopLog))+geom_density()
qqnorm(age_dat$PopLog)
qqline(age_dat$PopLog)
age_dat <- na.omit(age_dat)
Age <- age_dat$Age
Vaccine <- age_dat$Vaccine
Month <- age_dat$Month
Rep <- age_dat$Republican.Rep.
interaction.plot(Rep, Age, Vaccine, type = "l", lty = 1, lwd = 2, col = c("blue4", "green4", "red4", "yellow4"), xpd = FALSE)

#Visualizing variables - race
##Vaccine
r1 <- ggplot(race_dat, aes(Vaccine))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate")
r2 <- ggplot(race_dat, aes(sqrtVac))+geom_density()+ labs(x="Vaccine Coverage (square root)", title="Density plot of Vaccination Rate (sq. rooted)")
plot_grid(r1, r2, labels = list("Fig. 1 (a)", "(b)"))
##Population
r3 <- ggplot(race_dat, aes(Population))+geom_density() + labs(x = "Population Density", title = "Density Plot of Population Density")
r4 <- ggplot(race_dat, aes(PopLog))+geom_density()+ labs(x = "Population Density (logged)", title = "Density Plot of Population Density (logged)")
plot_grid(r3, r4, labels = list("Fig. 2 (a)", "(b)"))
##State/ Region
race_dat %>% 
  arrange(Region) %>% 
  ggplot(aes(y = sqrtVac, x = State, color = Region))+geom_point() + labs(x = "States", y = "Vaccine Coverage (square root)", title = "Fig. 3. Vaccine Coverage by State")+theme(axis.text.x = element_text(angle = 90))
##Race and Month
r5 <- ggplot(race_dat, aes(sqrtVac, color = Season))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate \n by Season")
r6 <- ggplot(race_dat, aes(sqrtVac, color = Race))+geom_density()+ labs(x="Vaccine Coverage", title="Density plot of Vaccination Rate \n by Race")
plot_grid(r5, r6, labels = list("Fig4(a)", "(b)"))

race_dat <- na.omit(race_dat)
Month <- race_dat$Month
Race <- race_dat$Race
Vaccine <- race_dat$Vaccine
interaction.plot(Month, Race, Vaccine, type = "l", lty = 1, lwd = 2, col = c("red1", "green1", "blue1", "purple1"), main = "Fig. 5. Interaction between Month and Race", frame.plot = FALSE)
