#### INTRODUCTION ####

# Welcome. For the description of the project please visit: https://github.com/diekei/2023_EBR_coastal_beetles_dispersal
# Article is available at: 


#### LIBRARY ####

library(survival)
library(survminer)


#### DATA ####

# data available at 2023_EBR_data_survival
# abbreviations explanation:
# sp -> species name code
# id -> individual id
# dur -> survival duration in the experiment
# status -> survival status in the experiment
# stc -> survival status code
data.surv <- read.csv(file.choose())
data.surv$sp <- as.factor(data.surv$sp)


#### ANALYSIS - SURVIVAL CURVE ANALYSIS ####

curve.surv <- survfit(Surv(dur, stc) ~ sp, data = data.surv)

print(curve.surv)
summary(curve.surv)
summary(curve.surv)$table

df.surv <- data.frame(time = curve.surv$time,
                n.risk = curve.surv$n.risk,
                n.event = curve.surv$n.event,
                n.censor = curve.surv$n.censor,
                surv = curve.surv$surv,
                upper = curve.surv$upper,
                lower = curve.surv$lower)

head(df.surv)


#### ANALYSIS - LOG-RANK TEST ####

diff.surv <- pairwise_survdiff(Surv(dur, stc) ~ sp, data = data.surv)
diff.surv


#### VISUALISATION ####

plot.surv <- ggsurvplot(curve.surv, pval = FALSE, conf.int = TRUE, conf.int.alpha = 0.1,
                risk.table = FALSE, size = 1.5, 
                ncensor.plot = FALSE, surv.median.line = "hv", 
                ggtheme = theme_survminer(), 
                palette = c("simpsons"), 
                xlab = "\nTime in days", 
                ylab = "Survival probability\n",
                break.time.by = 1, 
                legend.labs = c("Aegialia arenaria (19)",
                                "Aphodius contaminatus (12)",
                                "Bledius subniger (9)",
                                "Notiophilus germinyi (4)"),
                legend = c(0.75,0.85), 
                legend.title = "Species", 
                font.legend = c(10, "italic"))

plot.surv

ggsave(filename = "Figure_3.png", 
       width = 5, height = 5, device='png', dpi=1200)

plot.surv$plot + theme_bw() + 
  theme (legend.position = "right") +
  facet_grid(sp ~ .)


