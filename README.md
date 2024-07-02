# Hunting Seizure Scientific Paper
R Codes and public dataset used in the scientific paper

# DADOS

setwd("set_your_path")

cov<-read.csv("data_municipios.csv",h=T,row.names=1,sep=";")
cac<-read.csv("dados_caca.csv",h=T,sep=";")
df_complete <- merge(cac, cov, by = "ID", all.x = TRUE)
head(df_complete)


load.lib<-c("viridis", "dplyr", "data.table","tidyr","readr","devtools",
           "skimr","dlookr","chron","tidyverse","network","igraph","sna","ggnet",
           "ergm","intergraph","RColorBrewer","hrbrthemes","ggalt","ggExtra","patchwork",
           "circlize","networkD3","bipartite","ggalluvial","htmlwidgets","htmltools")

# Then we select only the packages that aren't currently installed.

install.lib <- load.lib[!load.lib %in% installed.packages()]

# And finally we install the missing packages, including their dependency.
for(lib in install.lib) install.packages(lib,dependencies=TRUE)
# After the installation process completes, we load all packages.
sapply(load.lib,require,character=TRUE)

## covariables_brazilian_municipalities

dados <- read.csv("covariables_brazilian_municipalities.csv", header=T,sep=";", encoding = "UTF-8", dec=",")
glimpse(dados)


dados$Biome_name  = factor(dados$Biome_name ,
                           levels = c("Amazonia", "Mata_Atlantica", "Cerrado", "Caatinga","Pampa","Pantanal"),
                           labels = c("Amazônia", "Mata Atlântica", "Cerrado", "Caatinga", "Pampa", "Pantanal"))
glimpse(dados)

caça <- read.csv("Dados CAÇA PRF 2017-2022_2.csv", header=T, sep=";", dec=',',encoding = "UTF-8",)

head(caça)

colnames(caça) <- c('Crime type','Age','Sex', 'State of birth','Education','Profission', 'State residence') 


caça <- caça %>% mutate(Age_group_WHO = case_when(
  #  +   Idade < 18 ~"Menor idade" ,
  +   Age < 25 & Age >= 18 ~ "Young 18-24" ,
  +   Age < 45 & Age >= 25 ~ "Young adult 25-44"  ,
  +   Age < 60 & Age >= 45 ~ "Half age 45-59"  ,
  +   Age < 75 & Age >= 60 ~ "Elderly 60-74"  ,
  +   Age < 95 & Age >= 75 ~ "Elder 75-95"))



caça$Education  = factor(caça$Education ,
                         levels = c("não informad", "ensino fundamental",
                                    "ensino médi","Não escolarizad", "superior"),
                         labels = c("Not declared", "Elementary School",
                                    "High School", "Unschooled", "Higher Education"))

glimpse(caça)

caça_2 <- read.csv("Dados CAÇA PRF 2017-2022.csv", header=T, sep=";", encoding = "UTF-8", dec=',')


glimpse(caça_2)
colnames(caça_2) <- c('Crime_ID','Aprehension','Locality','On_transit','Operation',
                      'Colective_or_not', 'Error','City','State','Region','Biome','Amount',
                      'Group','Family','Species','Dead_or_not','IUCN','IBAMA','CITES_SP','CITES_FA'
) 
teste <- cbind(caça,caça_2)
View(caça)
View(caça_2)
View(dados)

#### veja quais dados seguem distribuição normal
library(ggpubr)
library(ggtext)

ggqqplot(caça$Age)


#### Amazon Alluvial

#glimpse(dados)

total_all_biomes <- sum(caça_2$Amount)
  View(caça_2)
  
cities <- caça_2 %>%
  filter(City)

cities <- caça_2 %>%
group_by(City) %>%
  summarize(n=n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))
  View(cities)
  
  family <- caça_2 %>%
    group_by(Family, Biome, Species) %>%
    summarize(n=n()) %>%
    mutate(pct = n/sum(n),
           lbl = scales::percent(pct))
  View(family)

    Biome <- caça_2 %>%
    group_by(Biome, Operation) %>%
    summarize(n=n()) %>%
    mutate(pct = n/sum(n),
           lbl = scales::percent(pct))
    View(Biome)
  
    graff6 <- ggplot(Biome, 
                     aes(x = factor(Biome,
                                    levels = c("Amazônia", "Mata Atlântica","Caatinga",
                                               "Cerrado", "Pampa"),
                                    labels = c("Amazon", "Atlantic Forest", "Caatinga",
                                               "Cerrado","Pampa")),
                         y = pct,
                         fill = factor(Operation,
                                       levels = c("Acionamento por outro órgão",
                                                  "Denúncia/informação",
                                                  "Fiscalização de rotina",
                                                  "Motivada por outro crime",
                                                  "Não informado",
                                                  "Operação de fiscalização ambiental"),
                                       labels = c("Activation/Initiation by\nanother agency N=1",
                                                  "Report/criminal complaint\nN=3",
                                                  "Routine inspection/\nsurveillance N=231",
                                                  "Motivated by another crime\nN=3",
                                                  "NA=3",
                                                  "Environmental inspection\noperation N=30")))) + 
      geom_bar(stat="identity", color="black", 
               position = "fill") +
      geom_text(aes(label = lbl), 
                size = 5, 
                position = position_stack(vjust = 0.5)) +
      scale_fill_brewer(palette = "PiYG") +
      labs(y = "Percentage of data", 
           fill = "Types of PRF Operations",
           title = "Main PRF operations according to Brazilian Biomes",
           x = "Brazilian biomes") +
      theme_bw(base_size = 15)
  
    ggsave(file=paste0("./outputs/PRFops.pdf"),
           plot=graff6,width=24,height=16,dpi="print") 

    #### Alluvial
    
zep <- caça_2 %>%
  filter(Biome=="Amazônia") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(State,City,Family,Species, Amount, Aprehension)

floyd2 <- zep %>%
  group_by(State,City,Family,Species, Amount, Aprehension) %>%
  summarize(freq = n())

total_ind_mor1 <- sum(floyd2$Amount) # 344

floyd2$Species[is.na(floyd2$Species)] <- "Non-identified"
floyd2$Family[is.na(floyd2$Family)] <- "Non-identified"

# remove NA for better visualization

floyd3 <- na.omit(floyd2)
summarise(floyd3)
View(floyd3)
total_ind_mor <- sum(floyd2$Amount) # 266
344-266
20200/344
2900/344
floyd2$Species[is.na(floyd2$Species)] <- "Non-identified"

doors <- ggplot(floyd3,
                aes(axis5 = Family,
                    axis4 = Amount,
                    axis3 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities", "Species\nPodocnemis unifilis = 202 (58%)\nCuniculus paca = 29 (8.5%)",
                              "Specimens killed N=344\n78 non-identified species\n Panthera onca = 1 (Macapá - AP)","Taxonomic Family"),expand = c(.05, .05)) +
  scale_fill_viridis(discrete = T, option="rocket", alpha = 0.7) +
  labs(title = "(A) Amazon biome hunting seizure between 2017-2022",
       #subtitle = "Stratified by seizure apprehension (2017-2022)",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando.png"),
       plot=doors,width=18,height=18,dpi=1100)

####################

## Atlantic Forest

glimpse(dados)
zep2 <- caça_2 %>%
  filter(Biome=="Mata Atlântica") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(State,City,Family,Species, Amount, Aprehension)

zep2 <- zep2 %>%
  group_by(State,City,Family,Species, Amount, Aprehension) %>%
  summarize(freq = n())


# remove NA for better visualization
total_ind_mor2 <- sum(zep2$Amount) # 209

zep3 <- na.omit(zep2) # 13 complete data
summary(zep2)

total_ind_mor <- sum(zep2$Amount) # 70

5200/209
2100/209
View(zep2)
zep2$Species[is.na(zep2$Species)] <- "Non-identified"
zep2$Family[is.na(zep2$Family)] <- "Non-identified"

zeppelin <- ggplot(zep2,
                aes(axis5 = Family,
                    axis4 = Amount,
                    axis3 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Aprehension)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities","Species\nDendrocygna autumnalis = 52 (24.9%)\nDasypodidae family = 21 (10%)",
                              "Specimens killed N=209\n139 non-identified species",
                              "Taxonomic Family"),expand = c(.05, .05)) +
  scale_fill_viridis(discrete = T, option="turbo", alpha = 0.9) +
  labs(title = "(B) Atlantic Forest biome hunting seizure between 2017-2022",
      # subtitle = "Stratified by seizure aprehension (2017-2022)",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando_atlanticforest.png"),
       plot=zeppelin,width=18,height=10,dpi=640)


################# CAATINGA


funk <- caça_2 %>%
  filter(Biome=="Caatinga") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(State,City,Family,Species, Amount, Aprehension)

funk2 <- funk %>%
  group_by(State,City,Family,Species, Amount, Aprehension) %>%
  summarize(freq = n())
View(funk2)
total_ind_mor3 <- sum(funk2$Amount) # 7124

funk2$Species[is.na(funk2$Species)] <- "Non-identified"
funk2$Family[is.na(funk2$Family)] <- "Non-identified"

# remove NA for better visualization

funk3 <- na.omit(funk2) # 36 complete data

View(funk3)
total_ind_mor <- sum(funk3$Amount) # 684
7124-684
40100/7124
696100/7124
total_ind_mor_na <- sum(funk2$Species=="Non-identified") # 684

grandfunk <- ggplot(funk2,
                aes(axis5 = Family,
                    axis4 = Amount,
                    axis3 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities", "Species\nColumbidea Family = 6961 (97.7%)\nZenaida auriculata = 401 (5.62%)",
                              "Specimens killed N=7124\n6440 non-identified species","Taxonomic Family"),expand = c(.05, .05)) +
  scale_fill_viridis(discrete = T, option="plasma", alpha = 0.5) +
  labs(title = "(C) Caatinga biome hunting seizure between 2017-2022",
      # subtitle = "Stratified by taxonomic family groups",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando_caatinga.png"),
       plot=grandfunk,width=18,height=10,dpi=640)


################# PAMPA

glimpse(dados)
purple <- caça_2 %>%
  filter(Biome=="Pampa") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(State,City,Family,Species, Amount, Aprehension)

purple2 <- purple %>%
  group_by(State,City,Family,Species, Amount, Aprehension) %>%
  summarize(freq = n()) # 25 incomplete data
View(purple2)
total_ind_mor4 <- sum(purple2$Amount) # 605

purple2$Species[is.na(purple2$Species)] <- "Non-identified"
purple2$Family[is.na(purple2$Family)] <- "Non-identified"
# remove NA for better visualization

purple3 <- na.omit(purple2) # 13 complete data
total_ind_mor4.1 <- sum(purple3$Amount) # 70
605-70
View(purple2)
2900/605
2100/605
25000/605
deeppurple <- ggplot(purple2,
                aes(axis5 = Family,
                    axis4 = Amount,
                    axis3 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities", "Species\nHydrochoerus hydrochaeris = 29 (4.8%)\nColumbidae family = 250 (41%)",
                              "Specimens killed N=605\n535 non-identified species",
                              "Taxonomic Family"),expand = c(.05, .05)) +
  scale_fill_viridis(discrete = T, option="viridis", alpha = 0.8) +
  labs(title = "(E) Pampa biome hunting seizure between 2017-2022",
      # subtitle = "Stratified by taxonomic family group",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando_pampa.png"),
       plot=deeppurple,width=18,height=10,dpi=640)



################# CERRADO

glimpse(dados)
bowie <- caça_2 %>%
  filter(Biome=="Cerrado") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(State,City,Family,Species, Amount, Aprehension)

bowie2 <- bowie %>%
  group_by(State,City,Family,Species, Amount, Aprehension) %>%
  summarize(freq = n()) # 38 incomplete data
bowie2$Species <- sub('Species', 'italic(Species)~', bowie2$Species)

# remove NA for better visualization
total_ind_mor5 <- sum(bowie2$Amount) # 82 with uncompleted data

bowie2$Species[is.na(bowie2$Species)] <- "Non-identified"
bowie2$Family[is.na(bowie2$Family)] <- "Non-identified"

bowie3 <- na.omit(bowie2) # 12 complete data

View(bowie2)
total_ind_mor5.1 <- sum(bowie3$Amount) # 70
500/82
2500/82
library(ggtext)
davidbowie <- ggplot(bowie2,
                aes(axis5 = Family,
                    axis4 = Amount,
                    axis3 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities", 
                              "Dasypodidae/Chlamyphoridae = 25 (30.5%)\nCuniculus paca = 5 (6%)",
                              "Specimens killed N=82\n68 non-identified species\nAlligatoridae = 10 Psittacidae = 21",
                              "Taxonomic Family"),expand = c(.05, .05)) +
    scale_fill_viridis(discrete = T, option="magma", alpha = 0.8) +
  labs(title = "(D) Cerrado biome hunting seizure between 2017-2022",
       #subtitle = "Stratified by species taxonomic family",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando_cerrado.png"),
       plot=davidbowie,width=18,height=10,dpi=640)


###################################################
italicize_labels <- function(labels) {
  sapply(labels, function(label) {
    parse(text = paste("italic('", label, "')", sep = ""))
  })
}

# Extract unique species names
species_labels <- unique(bowie2$Species)

# Apply the custom labeling function to species labels
italic_species_labels <- italicize_labels(species_labels)

# Create a named vector to map original species names to italicized names
names(italic_species_labels) <- species_labels

# Function to label with italics
label_with_italics <- function(label) {
  if (label %in% species_labels) {
    return(italic_species_labels[[label]])
  } else {
    return(label)
  }
}

# Extract unique species names
species_labels <- unique(bowie2$Species)

# Function to create italic expressions
italicize <- function(label) {
  parse(text = paste0("italic('", label, "')"))
}

# Create a named vector for axis labels with italic expressions
axis_labels <- c(
  "Brazilian States",
  "Cities",
  setNames(lapply(species_labels, italicize), species_labels),
  "Specimens killed N=82\n68 non-identified species\nAlligatoridae = 10 Psittacidae = 21",
  "Taxonomic Family"
)

# Update the plot
davidbowie <- ggplot(bowie2,
                     aes(axis5 = Family,
                         axis4 = Amount,
                         axis3 = Species,
                         axis1 = State,
                         axis2 = City,
                         y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha = 0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(
    limits = c("Brazilian States", "Cities", "Species", 
               "Specimens killed N=82\n68 non-identified species\nAlligatoridae = 10 Psittacidae = 21",
               "Taxonomic Family"),
    expand = c(.05, .05),
    labels = axis_labels
  ) +
  scale_fill_viridis(discrete = TRUE, option = "magma", alpha = 0.8) +
  labs(title = "(D) Cerrado biome hunting seizure between 2017-2022",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none")

###################################################

library(gridExtra)

g2 <- grid.arrange(doors, zeppelin, grandfunk,davidbowie, deeppurple,nrow=5, ncol=1)

ggsave(file=paste0("./outputs/5_biomes.pdf"),
       plot=g2,width=15,height=45,dpi="print", limitsize = F)

gogogo <- rbind(zep2, floyd2, funk2, bowie2, purple2)
write.csv(gogogo, file = "./outputs/foo.csv")

View(gogogo)

############ daods Bogoni

morais <- data %>%
  filter(Bioma=="Amaz\xf4nia") %>%
  #filter(Qualidade.do.sono== "Ruim") %>%
  dplyr::select(UF,City,Family,Species, Qtd, Date)

morais2 <- morais %>%
  group_by(UF,City,Family,Species, Qtd, Date) %>%
  summarize(freq = n())

total_ind_mor1 <- sum(morais2$Qtd) # 344

# remove NA for better visualization

floyd3 <- na.omit(floyd2)
summarise(floyd3)
View(floyd3)
total_ind_mor <- sum(floyd3$Amount) # 266

20200/266
2900/266
doors <- ggplot(floyd3,
                aes(axis5 = Aprehension,
                    axis3 = Amount,
                    axis4 = Species,
                    axis1 = State,
                    axis2 = City,
                    y = freq)) +
  geom_alluvium(aes(fill = Family)) +
  geom_stratum(alpha=0.9) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Brazilian States","Cities", "Amount individuals killed N=266",
                              "Species\nPodocnemis unifilis = 202 (76%)\nCuniculus paca = 29 (11%)",
                              "Aprehension date"),expand = c(.05, .05)) +
  scale_fill_viridis(discrete = T, option="rocket", alpha = 0.7) +
  labs(title = "(A) Amazon biome hunting pressure between 2017-2022",
       subtitle = "Stratified by taxonomic family group",
       y = "Frequency") +
  theme_minimal() +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 

ggsave(file=paste0("./outputs/grafico_testando.png"),
       plot=doors,width=18,height=18,dpi=1100)


# MAPA

require(raster)
library(sf)
biomas<-st_read("./Biomas_250mil/lm_bioma_250.shp")
biomas<-readOGR("./Biomas.shp")
library(viridis)
str(biomas)
biomas <- biomas[, -2]
x11()
plot(biomas,las=1,axes=T,col=plasma(6,alpha=0.5))

#mascara <- biomas[biomas$Bioma,]

data=df_complete
names(data)
library(dplyr)
plot.new()
result <- data %>%
  group_by(Lat, Long) %>%
  summarise(Total_Qtd = sum(Qtd, na.rm = TRUE), .groups = 'drop')
points(x=result$Long,y=result$Lat,cex=log(result$Total_Qtd),col="black",pch=21,bg=adjustcolor("cyan",0.5))
library(prettymapr)  
addnortharrow()
addscalebar()

# HISTOGRAM

head(data)
cores<-inferno(6,alpha=0.5)
cores=c(cores[1],cores[3],cores[4],cores[2],cores[6])
x11()
boxplot(log(data$Qtd)~data$Biome,col=cores,xlab="Biomes",ylab="log(quantitiy)")


# BARPLOT


names(data)
head(data)
library(ggplot2)
cores<-inferno(6,alpha=1)
cores=c(cores[1],cores[3],cores[4],cores[2],cores[6])
data$Biome <- as.factor(data$Biome)
result <- data %>%
  group_by(Biome,Year) %>%
  summarise(Total = sum(Qtd, na.rm = TRUE), .groups = 'drop')
result<-as.data.frame(result)
result
g1<-ggplot(result, aes(x = factor(Year), y = log(Total), fill = Biome)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = cores) +  # Especifica as cores manualmente
    labs(title = "",
       x = "Year",
       y = "Log(Quantity)",
       fill = "Biome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
g1
library(ggplot2)
library(dplyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
plot_final<-ggarrange(g1,
          font.label = list(size = 5, color = "black", face = "bold", family = NULL),
          labels = c(""),
          ncol = 1, nrow = 1, common.legend = TRUE)
plot_final
fig<-grid.arrange(plot_final)
setwd("set_your_path")
ggsave(plot=fig,height=6,width=7,dpi=600, filename="Figure2.pdf", useDingbats=FALSE)

# GLM AND BIVARIATE
library(lme4)
model_global<-glmer(Qtd ~ log1p(HPD)+log1p(roads)+log1p(rivers)+log1p(Native_cover_2018)+(1|Biome),
              data = data, family = poisson)
summary(model_global)
library(sjPlot)
pred_plot <- plot_model(model_global, type = "pred", show.legend = FALSE)
re_plot <- plot_model(model_global, type = "re", show.legend = FALSE)
g1<-pred_plot[[1]]
g2<-pred_plot[[2]]
g3<-pred_plot[[3]]
g4<-pred_plot[[4]]
g5<-re_plot
plot_final<-ggarrange(g1,g2,g3,g4,g5,
          font.label = list(size = 5, color = "black", face = "bold", family = NULL),
          labels = c(""),
          ncol = 5, nrow = 1, common.legend = FALSE)
plot_final


model_global2<-glm(Qtd ~ log1p(HPD)+log1p(roads)+log1p(rivers)+log1p(Native_cover_2018),
              data = data, family = poisson)
summary(model_global2)

gogogo <- cbind(doors2)




names(data)
g1<-ggplot(data, aes(x=log(HPD), y=log(Qtd),colour=Biome)) +
geom_point(shape=16, size=3)+
scale_color_manual(values = cores) +
  #geom_smooth(method = "glm",aes(colour = Biome),
            #  linetype = 1, fill = "gray80", alpha = 0.5,method.args = list(family = poisson))+
 geom_smooth(method = "glm", colour = "cyan", 
              linetype = 1, fill = "cyan", alpha = 0.2, method.args = list(family = poisson)) +
labs(color = "Biome")+
theme_bw() +
  labs(x = "HPD", y = "log(Quantity per event)", 
       title = "")
g1


names(data)
g2<-ggplot(data, aes(x=log(roads), y=log(Qtd),colour=Biome)) +
geom_point(shape=16, size=3)+
scale_color_manual(values = cores) +
 # geom_smooth(method = "glm", aes(colour = Biome),
        #      linetype = 1, fill = "gray80", alpha = 0.5,method.args = list(family = poisson))+
 geom_smooth(method = "glm", colour = "cyan", 
              linetype = 1, fill = "cyan", alpha = 0.2, method.args = list(family = poisson)) +
labs(color = "Biome")+
theme_bw() +
  labs(x = "Roads", y = "log(Quantity per event)", 
       title = "")
g2



g3<-ggplot(data, aes(x=log(rivers), y=log(Qtd),colour=Biome)) +
geom_point(shape=16, size=3)+
scale_color_manual(values = cores) +
  #geom_smooth(method = "glm", aes(colour = Biome),
         #     linetype = 1, fill = "gray80", alpha = 0.5,method.args = list(family = poisson))+
 geom_smooth(method = "glm", colour = "cyan", 
              linetype = 1, fill = "cyan", alpha = 0.2, method.args = list(family = poisson)) +
labs(color = "Biome")+
theme_bw() +
  labs(x = "Rivers", y = "log(Quantity per event)", 
       title = "")
g3



names(data)
g4<-ggplot(data, aes(x=log(Native_cover_2018), y=log(Qtd),colour=Biome)) +
geom_point(shape=16, size=3)+
scale_color_manual(values = cores) +
 # geom_smooth(method = "glm", aes(colour = Biome),
    #          linetype = 1, fill = "gray80", alpha = 0.5,method.args = list(family = poisson))+
 geom_smooth(method = "glm", colour = "cyan", 
              linetype = 1, fill = "cyan", alpha = 0.2, method.args = list(family = poisson)) +
labs(color = "Biome")+
theme_bw() +
  labs(x = "Natural habitat", y = "log(Quantity per event)", 
       title = "")
g4


plot_final<-ggarrange(g1,g2,g3,g4,
          font.label = list(size = 5, color = "black", face = "bold", family = NULL),
          labels = c(""),
          ncol = 4, nrow = 1, common.legend = TRUE)
plot_final
fig<-grid.arrange(plot_final)
setwd("set_your_path")
ggsave(plot=fig,height=6,width=7,dpi=600, filename="Figure3.pdf", useDingbats=FALSE)
