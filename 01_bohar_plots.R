##################################
## Making figures for publication
#################################

## 01 L. bohar graphs


## Load libraries

library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyverse)
library(MetBrewer)

# clear workspace ----
rm(list = ls())

# set working directories ----

w.dir <- "~/Repositories/Isla_MSc_Meso_Isotopes"  # Isla change this to where your directory is located

# Set data directory - to read the data from
d.dir <- (paste(w.dir, "Data", sep='/'))
dr.dir <- (paste(w.dir, "Data/Raw", sep='/'))

# Set graph directory - to save plots
p.dir <- paste(w.dir, "Plots", sep='/')
#r.dir <- paste(w.dir, "rasters", sep='/')


# Load data ----

study <- "Meso_isotopes"

dir()


# Plot for L. Bohar

#palette ----

pal <- met.brewer(name="Renoir", n=3, type="discrete")
pal
  
clrs <- c("#FF6F00B2","#C71000B2")

df_LB <- read.csv(paste(dr.dir,  ("Bohar2.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  mutate_at(vars(Location, Sample.ID), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

#clrs <- c("Chagos" = "#FF6F00B2","SR" = "#C71000B2") Em updated colour palette 

LB_means <- df_LB %>%
  group_by(Location) %>% 
  dplyr::summarise(Nm = mean(N), 
            Nsd = sd(N), 
            Cm = mean(C), 
            Csd = sd(C))

# Set a theme

Theme1 <- theme_minimal()+
  theme(
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=14),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line= element_line(),
    #panel.grid=element_blank(),
    axis.ticks = element_line(),
    axis.text.x=element_text(size=12),
    axis.text.y=element_text(size=12),
    plot.title=element_text(size=14, face="italic"),
    legend.title = element_text(size=14, face="bold"),
    legend.position = "right",
    legend.text= element_text(size=13)
  )


# CN plot ----
# LB Plot A ----

LBplotA <- ggplot()+ 
  geom_point(data=LB_means, aes(x=Cm, y=Nm, colour=Location, fill=Location))+
  geom_errorbar(data = LB_means, 
                aes(x = Cm, 
                    ymin = Nm - Nsd, ymax = Nm + Nsd,  colour = Location), width = 0.2, cex = 1)+ 
  geom_errorbarh(data = LB_means, 
                 aes(y = Nm, 
                     xmin = Cm - Csd, xmax = Cm + Csd,  colour = Location), cex = 1)+ 
  scale_x_continuous(limits = c(-18, -8),breaks=seq(-18, -8, 2))+ 
  scale_y_continuous(limits = c(8,16),breaks=seq(8, 16, 2))+
  scale_color_manual(values =clrs, 
                     labels = c("Chagos", "Scott Reefs"))+
  scale_fill_manual(values =pal, 
                     labels = c("Chagos", "Scott Reefs"))+
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB")))))+ 
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air")))))+
  ggtitle("Lutjanus bohar")+
  geom_errorbar(aes(x = -14.513, 
                         ymin = 13.434 - 1, ymax = 13.434 + 1), cex = 1, lty = 2, colour = "#C71000B2")+
  geom_errorbarh(aes(y = 13.434, 
                     xmin = -14.513 - -2.045, xmax = -14.513 + -2.045), cex =1, lty = 2, colour = "#C71000B2")+
  Theme1

LBplotA <- LBplotA + theme(legend.position = "none")
LBplotA

# niche space plot ----


LB2 <- read.csv(paste(dr.dir,  ("Boharadj.csv"), sep = '/'))%>%
  mutate_at(vars(Location), list(as.factor)) %>% # make these columns as factors
  glimpse()

# create convex hull function from this website:
#https://cran.r-project.org/web/packages/ggplot2/vignettes/extending-ggplot2.html

StatChull <- ggproto("StatChull", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     
                     required_aes = c("x", "y")
)

stat_chull <- function(mapping = NULL, data = NULL, geom = "polygon",
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    stat = StatChull, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

# plot out convex hulls for niche space of each species

# LB PLot B ----

LBplotB <- ggplot(LB2,aes(x = C, y = N, colour = Location, fill = Location))+
  # geom_point(size = 3.5)+
  stat_chull(size = 0.5, alpha = 0.3)+
  scale_x_continuous(limits = c(-18, -8),breaks=seq(-18, -8, 2))+ 
  scale_y_continuous(limits = c(8,16),breaks=seq(8, 16, 2))+
  scale_color_manual(values =clrs, 
                     labels = c("Chagos", "Scott Reefs"))+
  scale_fill_manual(values =clrs, 
                    labels = c("Chagos", "Scott Reefs"))+
  # facet_wrap( ~ Trip, ncol = 1)+
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air"))))) + 
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB"))))) +
  Theme1

LBplotB


# niche size box plot ----


library(siar) #package is not working any more. use install.packages("SIBER") instead

library(nicheROVER)
library(mvtnorm)
#install.packages("mvtnorm")
library(siar)
#install.packages("siar")
dir()


lb <- read.csv(paste(dr.dir, ("Boharadj.csv"),sep = '/'),stringsAsFactors = FALSE)

lb$Location <- as.factor(lb$Location)
summary(lb$Location)
glimpse(lb)



attach(lb)
stderr <- function(x) sd(x)/sqrt(length(na.omit(x))) #standard error function
n_mean=tapply(N,list(Location), mean) #group fish into means by year
n_meanD=as.data.frame(n_mean)
c_mean=tapply(C,list(Location), mean) #group fish into means by year
c_meanD=as.data.frame(c_mean)
fish=data.frame(n_meanD,c_meanD)
fish$Location=rownames(fish)#create dataframe so ggplot can read data
detach(lb)
fish <-lb
aggregate(fish[5:6], fish[1], mean)

# generate parameter draws from the 'default' posteriors of each fish
nsamples <- 10000
system.time({
  fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                        X = fish[ii, 5:6]))
})

# mu1 (del15N), mu2 (del13C), and Sigma12
#clrs <- c("#d8d97a","#95c36e")
par(mar = c(4, 4, 0.5, 0.1) + 0.1, mfrow = c(1, 3))
niche.par.plot(fish.par, col = clrs, plot.index = 1)
niche.par.plot(fish.par, col = clrs, plot.index = 2)
niche.par.plot(fish.par, col = clrs, plot.index = 1:2)
legend("topleft", legend = names(fish.par), fill = clrs)

# all mu (del15N, del13C)
niche.par.plot(fish.par, col = clrs, plot.mu = TRUE, plot.Sigma = FALSE)
legend("topleft", legend = names(fish.par), fill = clrs)

# all mu and Sigma
par(mar = c(4.2, 4.2, 2, 1) + 0.1)
niche.par.plot(fish.par, col = clrs, plot.mu = TRUE, plot.Sigma = TRUE)
legend("topright", legend = names(fish.par), fill = clrs)

# 2-d projections of 10 niche regions
nsamples <- 10
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# format data for plotting function
fish.data <- tapply(1:nrow(fish), fish$Location, function(ii) X = fish[ii, 5:6])

niche.plot(niche.par = fish.par, niche.data = fish.data, pfrac = 0.05, iso.names = expression(delta^{
  15
} * N, delta^{
  13
} * C), col = clrs, xlab = expression("Isotope Ratio (per mil)"))

# niche overlap plots for 95% niche region sizes
nsamples <- 10000
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# Overlap calculation.  use nsamples = nprob = 10000 (1e4) for higher
# accuracy.  the variable over.stat can be supplied directly to the
# overlap.plot function

over.stat <- overlap(fish.par, nreps = nsamples, nprob = 10000, alpha = c(0.95,0.99))

# The mean overlap metrics calculated across iteratations for both niche
# region sizes (alpha = .95 and alpha = .99) can be calculated and displayed
# in an array.
over.mean <- apply(over.stat, c(1:2, 4), mean) * 100
round(over.mean, 2)

over.cred <- apply(over.stat * 100, c(1:2, 4), quantile, prob = c(0.025, 0.975), 
                   na.rm = TRUE)
round(over.cred[, , , 1])  # display alpha = .95 niche region

# Overlap plot.Before you run this, make sure that you have chosen your
# alpha level.
over.stat <- overlap(fish.par, nreps = nsamples, nprob = 10000, alpha = 0.95)
overlap.plot(over.stat, col = clrs, mean.cred.col = "#3D3B25B2", equal.axis = TRUE, 
             xlab = "Overlap probability (%) -- Niche region size: 95%")

tiff("lb.overlap.tiff", width = 9, height = 6.5, units = 'in', res = 300)
overlap.plot(over.stat, col = clrs, mean.cred.col = "#3D3B25B2", equal.axis = TRUE,
             xlab = "Overlap probability (%) -- Niche region size: 95%")
dev.off()

# calculation of niche size
# posterior distribution of (mu, Sigma) for each species
nsamples <- 10000
fish.par <- tapply(1:nrow(fish), fish$Location, function(ii) niw.post(nsamples = nsamples, 
                                                                      X = fish[ii, 5:6]))
# https://rdrr.io/github/mlysy/nicheROVER/src/R/niche.size.R
niche.size <- function(Sigma, alpha = .95) {
  Sigma <- as.matrix(Sigma)
  n <- nrow(Sigma)
  sz <- as.numeric(determinant(Sigma, logarithm = TRUE)$modulus)
  sz <- .5 * (sz + n * (log(pi) + log(qchisq(alpha, df = n)))) - lgamma(.5*n+1)
  exp(sz)
}

# posterior distribution of niche size by species
fish.size <- sapply(fish.par, function(spec) {
  apply(spec$Sigma, 3, niche.size, alpha = .95)
})
fish.size

# point estimate and standard error
rbind(est = colMeans(fish.size),
      se = apply(fish.size, 2, sd))

# E. areolatus E. multinotatus L. punctulatus L. sebae P. maculatus
# est    4.9574689       10.864757      10.001607 6.643900     9.442132
# se     0.9335154        2.057572       1.601639 1.058486     2.602830

par(mfrow = c(1,1))

fish.size # to make this a ggplot, let's change this from long to wide format because this is pretty unusable
fish.size.df <- as.data.frame(fish.size)
colnames(fish.size.df) # there is a space after Chagos, which is an easy fix and makes everything a little bit annoying to work with
summary(fish.size.df) 

#install.packages("janitor")
library(janitor)

#can be done by simply
fish.size.df <- clean_names(fish.size.df)
colnames(fish.size.df)

# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
# - factor_key: Treat the new key column as a factor (instead of character vector)

data_long <- gather(fish.size.df, Location, nichesize, chagos:sr, factor_key=TRUE)
glimpse(data_long)

# Now we can use ggboxplot

library(tidyverse)
library(ggplot2)

#Plot C ----

LBplotC <- ggplot(data_long, aes(x=Location, y=nichesize,  fill=Location))+
  geom_boxplot(notch=TRUE, show.legend = FALSE)+
  ylab("Niche Size")+
  #ggtitle("Lutjanus bohar")+
  scale_x_discrete(labels=c("chagos" = "Chagos", "sr" = "Scott Reefs"))+
  scale_fill_manual(values =clrs, 
                     labels = c("Chagos", "Scott Reefs"))+
  ylim(0, 35)+
  Theme1

LBplotC


# combining plots ----

library(patchwork)


bohar_plots <- LBplotA + LBplotB+ LBplotC + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect')
bohar_plots

?ggsave

setwd(p.dir)

ggsave("Lbohar.tiff", plot=bohar_plots, width=13, height=4.5, dpi=300)

# Fin