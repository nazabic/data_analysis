data(iris)
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

ir.pca <- prcomp(log.ir, center = TRUE, scale =TRUE)
print(ir.pca)
plot(ir.pca, type= "l")


library(devtools)
install_github("vqv/ggbiplot", ref = "experimental")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
ggbiplot(ir.pca, obs.scale = 1, var.scale = 1,
         groups = log.ir, ellipse = TRUE, circle = TRUE) +
  theme(legend.direction = 'horizontal', legend.position = 'top')


iris_z <- lapply(iris, function(x) if (is.numeric(x)) scale(x) else x)
m <- lda(Species ~ ., data = iris_z)
df <- fortify(m, iris_z)

g <- ggplot(df, aes(x = LD1, y = LD2)) +
  geom_point(aes(color = Species)) + 
  stat_ellipse(aes(group = Species, color = Species)) +
  geom_axis(data = attr(df, "basis"), aes(label = abbreviate(.name))) + 
  ylim(-4, 4) + coord_equal()

