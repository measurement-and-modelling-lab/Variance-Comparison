data <- read.csv("example1_data1.csv")

sample1 <- data[ , 1]
sample2 <- data[ , 2]

n <- length(sample1)
r <- cor(sample1, sample2)
sd1 <- sd(sample1)
sd2 <- sd(sample2)
v1 <- var(sample1)
v2 <- var(sample2)

t <- (v1 - v2) * sqrt(n - 2) / 2 * sd1 * sd2 * sqrt(1 - r^2)

2*pt(t, n - 2, lower = F)

t.test(sample1, sample2, paired = T)

a <- data$groups[1:14]
b <- c(0, 2.36, 0.82, 5.73, 2.66, 0.27, 0.83, 0.87, 0.80, 0.28, 0.42, 1.41, 2.72, 2.09)
groups <- factor(a)
ranked <- rank(b)
print(ranked)
print(b)

newL <- leveneTest(ranked, groups, center = mean)
oldL <- leveneTest(data$a, groups)

one <- ranked[1:8]
two <- ranked[9:14]
var(one)
var(two)
sd(one)
sd(two)
