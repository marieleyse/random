setwd("/Users/Marie-Elyse/Downloads")
#NEW = read.csv("MAVAN_48M_and_up_april2020.csv")
NEW <- read.csv("MAVAN_48M_and_up_jun2020.csv")
t1 = c(6,12,24,36,48, 60, 72)
c(6,12,24,36,48, 60, 72)
y = NEW[,c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]

pdf("simple_plot_depression.pdf")
x = c(6,12,24,36,48, 60, 72)
y = NEW[c("HWB6_CESD","HWB12_CESD","HWB24_CESD","HWB36_CESD","HWB48_CESD", "HWB60_CESD", "HWB72_CESD")]
plot(x, y)
dev.off()