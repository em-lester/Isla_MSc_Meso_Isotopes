##################################
## Making figures for publication
#################################

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

pal <- met.brewer(name="Hokusai3", n=5, type="discrete")
pal
  


df_LB <- read.csv(paste(dr.dir,  ("Bohar2.csv"), sep = '/'), fileEncoding="UTF-8-BOM")%>% #Table 2 from paper
  mutate_at(vars(Location, Sample.ID), list(as.factor)) %>% # make these columns as factors
  glimpse()
head(df)
str(df)

#clrs <- c("Chagos" = "#FF6F00B2","SR" = "#C71000B2") Em updated colour palette 

LB_means <- df_LB %>%
  group_by(Location) %>% 
  summarise(Nm = mean(N), 
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



LBplotA <- ggplot()+ 
  geom_errorbar(data = LB_means, 
                aes(x = Cm, 
                    ymin = Nm - Nsd, ymax = Nm + Nsd, colour = Location), width = 0.2, cex = 1)+ 
  geom_errorbarh(data = LB_means, 
                 aes(y = Nm, 
                     xmin = Cm - Csd, xmax = Cm + Csd, colour = Location), cex = 1)+ 
  scale_x_continuous(limits = c(-18, -8),breaks=seq(-18, -8, 2))+ 
  scale_y_continuous(limits = c(8,16),breaks=seq(8, 16, 2))+
  scale_color_manual(values =pal, 
                     labels = c("Chagos", "Scott Reefs"))+
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB")))))+ 
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air")))))+
  ggtitle("Lutjanus bohar")+
  geom_errorbar(aes(x = -14.513, 
                         ymin = 13.434 - 1, ymax = 13.434 + 1), cex = 1, lty = 2, colour = "#85C660")+
  geom_errorbarh(aes(y = 13.434, 
                     xmin = -14.513 - -2.045, xmax = -14.513 + -2.045), cex =1, lty = 2, colour = "#85C660")+
  Theme1

LBplotA

# niche space plot


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

LBplotB <- ggplot(LB2,aes(x = C, y = N, colour = Location, fill = Location))+
  # geom_point(size = 3.5)+
  stat_chull(size = 0.5, alpha = 0.3)+
  scale_x_continuous(limits = c(-16, -8),breaks=seq(-16, -8, 2))+ 
  scale_y_continuous(limits = c(12,16),breaks=seq(12, 16, 2))+
  scale_color_manual(values =pal, 
                     labels = c("Chagos", "Scott Reefs"))+
  scale_fill_manual(values =pal, 
                    labels = c("Chagos", "Scott Reefs"))+
  # facet_wrap( ~ Trip, ncol = 1)+
  ylab(expression(atop(bold(~delta^15~"N " ("\u2030 " [vs]~"air"))))) + 
  xlab(expression(atop(bold(~delta^13~"C " ("\u2030 " [vs]~"VPDB"))))) +
  Theme1

LBplotB


# next plot