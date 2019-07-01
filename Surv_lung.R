library(survival)
data(package="survival")
data(lung)
head(lung)

lung$SurvivalObj <- with(lung, Surv(time, status==2))

km.as.one <- survfit(SurvivalObj ~1, data=lung, conf.type="log-log")
km.by.sex <- survfit(SurvivalObj ~sex, data=lung, conf.type="log-log")

plot(km.as.one)
plot(km.by.sex)

install.packages("survminer")
library(survminer)
s_plot <- ggsurvplot(km.by.sex, data=lung, size=1, #line size
                     palette=c("#177e89", "#eb5e55"), #colors
                     conf.int = T,
                     pval = T,
                     risk.table = T,
                     risk.table.col="strata",
                     legend.labs=c("Male", "Female"),
                     risk.table.height=0.5,
                     ggtheme = theme_bw())
s_plot
