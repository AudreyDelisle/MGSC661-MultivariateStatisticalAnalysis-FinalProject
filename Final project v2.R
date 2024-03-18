
###################################
# Final Project : Audrey Delisle###
###################################



#########################
# Step 1: Preprocss Data#
#########################

# Load necessary libraries
library(dplyr)
library(fastDummies)

# Import Dataset
setwd("C:/Users/aadel/OneDrive/Documents/Mcgill Masters/2. Fall Term/MGSC 661 - Data Analytics and AI for Business")
olympic <- read.csv("Olympic.csv")

# Mapping NOC codes to their respective subregions
noc_to_continent <- c(
  RUS = "Eastern Europe", # Russia
  USA = "Northern America", # United States
  ESP = "Southern Europe", # Spain
  IRI = "Western Asia", # Iran
  CAN = "Northern America", # Canada
  AZE = "Western Asia", # Azerbaijan
  TJK = "Central Asia", # Tajikistan
  EUN = "Eastern Europe", # Unified Team (primarily Russia)
  ETH = "Sub-Saharan Africa", # Ethiopia
  TUR = "Western Asia", # Turkey
  BUL = "Eastern Europe", # Bulgaria
  AUS = "Australia and New Zealand", # Australia
  GBR = "Northern Europe", # Great Britain
  SWE = "Northern Europe", # Sweden
  URS = "Eastern Europe", # Soviet Union
  ROU = "Eastern Europe", # Romania
  MEX = "Latin America and the Caribbean", # Mexico
  NZL = "Australia and New Zealand", # New Zealand
  NGR = "Sub-Saharan Africa", # Nigeria
  LTU = "Northern Europe", # Lithuania
  BRA = "Latin America and the Caribbean", # Brazil
  FRA = "Western Europe", # France
  CUB = "Latin America and the Caribbean", # Cuba
  JPN = "Eastern Asia", # Japan
  KSA = "Western Asia", # Saudi Arabia
  CRO = "Southern Europe", # Croatia
  ARM = "Western Asia", # Armenia
  TTO = "Latin America and the Caribbean", # Trinidad and Tobago
  BOT = "Sub-Saharan Africa", # Botswana
  TUN = "Northern Africa", # Tunisia
  KOR = "Eastern Asia", # South Korea
  NOR = "Northern Europe", # Norway
  ITA = "Southern Europe", # Italy
  UKR = "Eastern Europe", # Ukraine
  MAR = "Northern Africa", # Morocco
  YUG = "Southern Europe", # Yugoslavia
  FIN = "Northern Europe", # Finland
  JAM = "Latin America and the Caribbean", # Jamaica
  BLR = "Eastern Europe", # Belarus
  GDR = "Eastern Europe", # East Germany
  CHN = "Eastern Asia", # China
  CZE = "Eastern Europe", # Czech Republic
  BAH = "Latin America and the Caribbean", # Bahamas
  GRE = "Southern Europe", # Greece
  KAZ = "Central Asia", # Kazakhstan
  HUN = "Eastern Europe", # Hungary
  GER = "Western Europe", # Germany
  ARG = "Latin America and the Caribbean", # Argentina
  GUA = "Latin America and the Caribbean", # Guatemala
  QAT = "Western Asia", # Qatar
  FRG = "Western Europe", # West Germany
  TAN = "Sub-Saharan Africa", # Tanzania
  LIB = "Northern Africa", # Lebanon
  KGZ = "Central Asia", # Kyrgyzstan
  SCG = "Southern Europe", # Serbia and Montenegro
  POL = "Eastern Europe", # Poland
  SRB = "Southern Europe", # Serbia
  KEN = "Sub-Saharan Africa", # Kenya
  DEN = "Northern Europe", # Denmark
  GEO = "Western Asia", # Georgia
  BEL = "Western Europe", # Belgium
  ALG = "Northern Africa", # Algeria
  NED = "Western Europe", # Netherlands
  TCH = "Eastern Europe", # Czechoslovakia
  SLO = "Southern Europe", # Slovenia
  ZIM = "Sub-Saharan Africa", # Zimbabwe
  PUR = "Latin America and the Caribbean", # Puerto Rico
  SUI = "Western Europe", # Switzerland
  SEN = "Sub-Saharan Africa", # Senegal
  UZB = "Central Asia", # Uzbekistan
  MGL = "Eastern Asia", # Mongolia
  IND = "Southern Asia", # India
  POR = "Southern Europe", # Portugal
  LAT = "Northern Europe", # Latvia
  RSA = "Sub-Saharan Africa", # South Africa
  ISL = "Northern Europe", # Iceland
  NAM = "Sub-Saharan Africa", # Namibia
  EGY = "Northern Africa", # Egypt
  AUT = "Western Europe", # Austria
  PRK = "Eastern Asia", # North Korea
  COL = "Latin America and the Caribbean", # Colombia
  MKD = "Southern Europe", # North Macedonia
  SUD = "Sub-Saharan Africa", # Sudan
  BRN = "Western Asia", # Bahrain
  GRN = "Latin America and the Caribbean", # Grenada
  SRI = "Southern Asia", # Sri Lanka
  UGA = "Sub-Saharan Africa", # Uganda
  EST = "Northern Europe", # Estonia
  ZAM = "Sub-Saharan Africa", # Zambia
  CMR = "Sub-Saharan Africa", # Cameroon
  SVK = "Eastern Europe", # Slovakia
  MDA = "Eastern Europe", # Moldova
  MOZ = "Sub-Saharan Africa", # Mozambique
  SUR = "Latin America and the Caribbean", # Suriname
  BDI = "Sub-Saharan Africa", # Burundi
  IRL = "Northern Europe", # Ireland
  ECU = "Latin America and the Caribbean", # Ecuador
  CRC = "Latin America and the Caribbean", # Costa Rica
  VEN = "Latin America and the Caribbean", # Venezuela
  PAN = "Latin America and the Caribbean", # Panama
  DJI = "Sub-Saharan Africa", # Djibouti
  DOM = "Latin America and the Caribbean", # Dominican Republic
  SGP = "South-eastern Asia", # Singapore
  SYR = "Western Asia", # Syria
  ERI = "Sub-Saharan Africa", # Eritrea
  BAR = "Latin America and the Caribbean", # Barbados
  CIV = "Sub-Saharan Africa" # Côte d'Ivoire
)


# Function to map NOC to continent
map_noc_to_continent <- function(noc_code) {
  noc_to_continent[noc_code] <- ifelse(is.na(noc_to_continent[noc_code]), "Other", noc_to_continent[noc_code])
}

# Add the new 'continent' column to olympic_filtered
olympic$continent <- sapply(olympic$NOC, map_noc_to_continent)


# Pre-processing and filtering
olympic_filtered <- olympic %>%
  filter(Year >= 1980, !is.na(Medal)) %>%
  mutate(
    Age = as.numeric(Age),
    Height = as.numeric(Height),
    Weight = as.numeric(Weight),
    BMI = Weight / (Height/100)^2
  ) %>%
  na.omit() %>%
  select(-c(ID, Name, Team, Games, Medal, Year, Season, City, Event, NOC))


# Dummify gender and continent
olympic_filtered <- olympic_filtered %>%
  dummy_cols(select_columns = c("Sex", "continent"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)


# dummify 5 sports
olympic_filtered <- olympic_filtered %>%
  mutate(
    BB = ifelse(Sport == "Basketball", 1, 0),
    Gym = ifelse(Sport == "Gymnastics", 1, 0),
    MMA = ifelse(Sport == "Wrestling", 1, 0),
    Run = ifelse(Sport == "Athletics", 1, 0),
    Swim = ifelse(Sport == "Swimming", 1, 0)
  )

# Filter for the five sports and dummify them
olympic_filtered_5S <- olympic_filtered %>%
  filter(Sport %in% c("Basketball", "Gymnastics", "Wrestling", "Athletics", "Swimming")) %>%


#############################################
#Step 2: Probability of an athelete playing a sport##
##############################################

total_athletes <- nrow(olympic_filtered)

probabilities <- sapply(c("Basketball", "Gymnastics", "Wrestling", "Athletics", "Swimming"), function(sport) {
  sport_count <- sum(olympic_filtered$Sport == sport)
  sport_count / total_athletes
})

names(probabilities) <- c("Basketball", "Gymnastics", "Wrestling", "Athletics", "Swimming")
probabilities



##################################
#Step 3: Sports Summary & Graphs###
#################################

sports_summary <- lapply(split(olympic_filtered_5S, olympic_filtered_5S$Sport), summary)
sports_summary


library(ggplot2)

olympic_filtered_5S_long <- olympic_filtered_5S %>%
  select(Sport, starts_with("continent_"), BB, Gym, MMA, Run, Swim) %>%
  pivot_longer(cols = starts_with("continent_"), names_to = "Continent", values_to = "Value") %>%
  filter(Value == 1) %>%
  mutate(Continent = sub("continent_", "", Continent)) # Cleaning up column names for the plot


# Create a Set3 color palette with maximum available colors
set3_palette <- brewer.pal(12, "Set3")

# Manually add an additional distinct color to the palette
extended_palette <- c(set3_palette, "#A0A0A0") # Adding a grey color as an example

# Now create a named vector with colors for each continent
# Ensure the order of continents matches with the data
continent_names <- unique(olympic_filtered_5S_long$Continent)
names(extended_palette) <- continent_names

# Now, create the stacked bar chart with the extended palette
ggplot(olympic_filtered_5S_long, aes(x = Sport, fill = Continent)) +
  geom_bar(position = "fill") + # 'fill' position will create a stacked bar chart with proportions
  scale_fill_manual(values = extended_palette) +
  labs(title = "Continent Distribution within Sports", x = "Sport", y = "Proportion of Athletes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels if they overlap




library(ggplot2)
library(RColorBrewer)

# Box plot for Age Distribution with color
ggplot(olympic_filtered_5S, aes(x = Sport, y = Age, fill = Sport)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Sport", x = "Sport", y = "Age") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend if not needed

# Box plot for Height Distribution with color
ggplot(olympic_filtered_5S, aes(x = Sport, y = Height, fill = Sport)) +
  geom_boxplot() +
  labs(title = "Height Distribution by Sport", x = "Sport", y = "Height (cm)") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend if not needed

# Box plot for Weight Distribution with color
ggplot(olympic_filtered_5S, aes(x = Sport, y = Weight, fill = Sport)) +
  geom_boxplot() +
  labs(title = "Weight Distribution by Sport", x = "Sport", y = "Weight (kg)") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend if not needed

# Box plot for BMI Distribution with color
ggplot(olympic_filtered_5S, aes(x = Sport, y = BMI, fill = Sport)) +
  geom_boxplot() +
  labs(title = "BMI Distribution by Sport", x = "Sport", y = "BMI") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "none") # Hide the legend if not needed


# Assuming the column is "Sex_M" and needs to be converted back to a factor for plotting
olympic_filtered_5S$Gender <- factor(olympic_filtered_5S$Sex_M, labels = c("Female", "Male"))

# Now plot the gender distribution by sport
ggplot(olympic_filtered_5S, aes(x = Sport, fill = Gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Distribution by Sport", x = "Sport", y = "Count") +
  scale_fill_brewer(palette = "Set1") + # Use a color palette suitable for gender
  theme_minimal()


library(ggplot2)
library(dplyr)

# Calculate the proportions for each sport
sport_proportions <- olympic_filtered_5S %>%
  summarize(
    Basketball = mean(BB),
    Gymnastics = mean(Gym),
    Wrestling = mean(MMA),
    Athletics = mean(Run),
    Swimming = mean(Swim)
  ) %>%
  gather(key = "Sport", value = "Probability")

# Plot the proportions
ggplot(sport_proportions, aes(x = Sport, y = Probability)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Probability of Participation by Sport", x = "Sport", y = "Probability") +
  theme_minimal()



#######################################
##### Check for Multicolineairty######
######################################

install.packages("ggplot2")
install.packages("reshape2")
library(ggplot2)
library(reshape2)

# Select only numeric columns
olympic_filtered_numeric <- olympic_filtered[sapply(olympic_filtered, is.numeric)]

# Calculate correlation matrix
correlation_matrix <- cor(olympic_filtered_numeric)

# Melt the correlation matrix
library(reshape2)
melted_correlation <- melt(correlation_matrix)

# Create the heatmap
library(ggplot2)
ggplot(data = melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap')




###################################
#Plotting the models##############
##################################

lr_model_BB <- glm(BB ~ Age + Height + Weight + `continent_Northern America` + `continent_Eastern Europe` + `continent_Eastern Asia` + `continent_Sub-Saharan Africa` + `continent_Southern Europe`, data = olympic_filtered, family = "binomial")
summary(lr_model_BB)

lr_model_Gym <- glm(Gym ~ Age + Height + Weight + `continent_Northern America` + `continent_Eastern Europe` + `continent_Eastern Asia` + `continent_Sub-Saharan Africa` + `continent_Southern Europe`, data = olympic_filtered, family = "binomial")
summary(lr_model_Gym)

lr_model_Swim <- glm(Swim ~ Age + Height + Weight + `continent_Northern America` + `continent_Eastern Europe` + `continent_Eastern Asia` + `continent_Sub-Saharan Africa` + `continent_Southern Europe`, data = olympic_filtered, family = "binomial")
summary(lr_model_Swim)

lr_model_MMA <- glm(MMA ~ Age + Height + Weight + `continent_Northern America` + `continent_Eastern Europe` + `continent_Eastern Asia` + `continent_Sub-Saharan Africa` + `continent_Southern Europe`, data = olympic_filtered, family = "binomial")
summary(lr_model_MMA)

lr_model_Run <- glm(Run ~ Age + Height + Weight + `continent_Northern America` + `continent_Eastern Europe` + `continent_Eastern Asia` + `continent_Sub-Saharan Africa` + `continent_Southern Europe`, data = olympic_filtered, family = "binomial")
summary(lr_model_Run)


library(stargazer)
stargazer(lr_model_BB, lr_model_Gym, lr_model_Swim, lr_model_MMA, lr_model_Run, type = "html")
stargazer(lr_model_BB, lr_model_Gym, lr_model_Swim, lr_model_MMA, type = "html")


stargazer(lr_model_Gym, lr_model_Swim, lr_model_MMA, lr_model_Run,
          type = "html", 
          title = "Regression Results")



########################
# Classification#######
#######################

####Estimating LDA
install.packages("MASS")
install.packages("klaR")
library(MASS)
library(klaR)

olympic_filtered_5S$Sport <- as.factor(olympic_filtered_5S$Sport)

attach(olympic_filtered_5S)
lda1<- lda(Sport ~ Weight + Height)
lda1

partimat(Sport~Weight+Height, method="lda",image.colors=c("grey", "light green", "white", "lightblue","pink")) 


attach(olympic_filtered_5S)
qda1=qda(Sport ~ Weight + Height + Age)
qda1

partimat(Sport~Weight+Height+Age, method="qda",image.colors=c("grey", "light green", "white", "lightblue","pink")) 


#############################################
# Running PCA and Creating Graphs ############
#############################################


# Load necessary libraries
library(ggplot2)
install.packages("GGally")
library(GGally)
library(ggfortify)

# Assuming your dataset is named olympic_filtered
attach(olympic_filtered)

# Load the necessary library
library(dplyr)

# Split data into labels and variables using the dplyr::select function
olympic_labels = dplyr::select(olympic_filtered, BB, Gym, MMA, Run, Swim)
olympic_vars = dplyr::select(olympic_filtered, Age, Height, Weight, `continent_Northern America`,`continent_Eastern Europe`,`continent_Eastern Asia`,`continent_Sub-Saharan Africa`,`continent_Southern Europe`)


library(GGally)
# Create scatter plots
ggpairs(olympic_vars)

#### Running principal component analysis (PCA) ######
pca <- prcomp(na.omit(olympic_vars), scale = TRUE)
print(pca)

#### Visualizing principal components
autoplot(pca, data = na.omit(olympic_vars), loadings = TRUE, loadings.label = TRUE)

###### Percentage of variance explained (PVE) plot ######
pve <- (pca$sdev^2) / sum(pca$sdev^2)
par(mfrow = c(1, 2))
plot(pve, ylim = c(0, 1))
plot(cumsum(pve), ylim = c(0, 1))



