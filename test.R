library(devtools)
library(nnet)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')
iris2 <- iris
RN <- nnet(iris2$Species ~ iris2$Petal.Width+iris2$Petal.Length, Data = iris2, size = 3, rang = 0.1, decay =0.01, maxit = 20)
#print(RN["wts"])
plot.nnet(RN)