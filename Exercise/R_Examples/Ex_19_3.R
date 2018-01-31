# Example 19-3: CHAID for iris data

data(iris)

SepL = cut(iris$Sepal.Length,breaks = 10)
SepW = cut(iris$Sepal.Width, breaks = 10)
PetL = cut(iris$Petal.Length,breaks = 10)
PetW = cut(iris$Petal.Width, breaks = 10)
SepL = ordered(SepL)
SepW = ordered(SepW)
PetL = ordered(PetL)
PetW = ordered(PetW)

iris2=data.frame(SepL,SepW,PetL,PetW,Species=iris$Species)
iris2

library(CHAID)
iris.chaid = chaid(Species ~ ., data = iris2)

plot(iris.chaid)
print(iris.chaid)

pred = predict(iris.chaid, newdata=iris2)
pred

table.chaid = table(Species=iris2$Species, Prediction=pred)
table.chaid
sum(diag(table.chaid))/sum(table.chaid)





