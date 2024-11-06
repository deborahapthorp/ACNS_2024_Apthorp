library(afex)
library(emmeans)
library(readr)
library(tidyr)
library(reshape2)
library(tidyverse)
library(dplyr)
library (patchwork)
library(papaja)
library(knitr)
library(ggrain)

## Recognition times 

Mean_Frames <- read_csv("Mean_Frames_Familiar_Faces_Filtered.csv")
Mean_Frames_long <- melt(Mean_Frames, 
                         # ID variables - all the variables to keep but not split apart on
                         id.vars=c("Participant", "Age"),
                         # The source columns
                         measure.vars=c("Unfamiliar_Upright", "Unfamiliar_Inverted", 
                                        "Familiar_Upright", "Familiar_Inverted", 
                                        "Self_Upright", "Self_Inverted"),
                         # Name of the destination column that will identify the original
                         # column that the measurement came from
                         variable.name="Condition",
                         value.name="Mean_N_Frames"               
                         
)

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Orientation = case_when(Condition == 'Unfamiliar_Upright' ~ 'Upright',
                                 Condition == 'Familiar_Upright' ~ 'Upright',
                                 Condition == 'Self_Upright' ~ 'Upright',
                                 TRUE ~ 'Inverted'))

Mean_Frames_long <- Mean_Frames_long %>%
  mutate(Familiarity = case_when(Condition == 'Unfamiliar_Upright' ~ 'Unfamiliar',
                                 Condition == 'Unfamiliar_Inverted' ~ 'Unfamiliar',
                                 Condition == 'Familiar_Upright' ~ 'Familiar',
                                 Condition == 'Familiar_Inverted' ~ 'Familiar',
                                 TRUE ~ 'Self'))

Mean_Frames_long$Mean_time <- Mean_Frames_long$Mean_N_Frames*(1000/120)
Mean_Frames_long$Age_c <- Mean_Frames_long$Age - mean(Mean_Frames_long$Age)

## Reaction times 

Mean_RTs <- read_csv("Mean_Reaction_Times_Familiar_Faces_Filtered.csv")
Mean_RTs_long <- melt(Mean_RTs, 
                      # ID variables - all the variables to keep but not split apart on
                      id.vars=c("Participant", "Age"),
                      # The source columns
                      measure.vars=c("Unfamiliar_Upright", "Unfamiliar_Inverted", 
                                     "Familiar_Upright", "Familiar_Inverted", 
                                     "Self_Upright", "Self_Inverted"),
                      # Name of the destination column that will identify the original
                      # column that the measurement came from
                      variable.name="Condition",
                      value.name="Mean_RT_secs" )

Mean_RTs_long <- Mean_RTs_long %>%
  mutate(Orientation = case_when(Condition == 'Unfamiliar_Upright' ~ 'Upright',
                                 Condition == 'Familiar_Upright' ~ 'Upright',
                                 Condition == 'Self_Upright' ~ 'Upright',
                                 TRUE ~ 'Inverted'))

Mean_RTs_long <- Mean_RTs_long %>%
  mutate(Familiarity = case_when(Condition == 'Unfamiliar_Upright' ~ 'Unfamiliar',
                                 Condition == 'Unfamiliar_Inverted' ~ 'Unfamiliar',
                                 Condition == 'Familiar_Upright' ~ 'Familiar',
                                 Condition == 'Familiar_Inverted' ~ 'Familiar',
                                 TRUE ~ 'Self'))

Mean_RTs_long$Mean_RT_ms <- Mean_RTs_long$Mean_RT_secs*(1000)
Mean_RTs_long$Age_c <- Mean_RTs_long$Age - mean(Mean_RTs_long$Age)




ggplot(Mean_Frames_long, aes(Familiarity, Mean_time, fill = Orientation)) +
  geom_rain(alpha = .5, rain.side = 'f', cov = "Age",
            boxplot.args = list(outlier.shape = NA, alpha = .8),
            violin.args = list(alpha = .8, color = NA),
            boxplot.args.pos = list(width = .1,
                                    position = ggpp::position_dodgenudge(width = .1,
                                                                         x = c(-.13, -.13, # t1 old, t1 young
                                                                               -.13, .13, 
                                                                               .13, .13))),
            violin.args.pos = list(width = .7,
                                   position = position_nudge(x = c(rep(-.2, 256*2), rep(-.2, 256*2),# t1
                                                                   rep(-.2, 256*2), rep(.2, 256*2), # t2
                                                                   rep(.2, 256*2), rep(.2, 256*2))))) +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  scale_color_viridis_c(option =  "A", direction = -1) +
  guides(fill = 'none', color = 'none')