library(readr)
score <- read_csv("~/Codes/R/学生成绩数据.csv")
names(score) <- c("Math", "Physics", "Chemistry", 
                  "Chinese", "History", "English")

# 绘制相关系数矩阵
library(corrplot)
cormat <- cor(score, method = "pearson")              # 计算相关系数矩阵
corrplot(cormat, method = "circle", type = "upper",   # 绘制相关系数矩阵图
         mar = rep(1, 4), tl.pos = "d", tl.cex = 1)
corrplot(cormat, add = TRUE, type = "lower", method = "number",
         col = "black", diag = FALSE, tl.pos = "n", cl.pos = "n")
#Classical FA 
score_FCA <- factanal(score, 2, scores = "regression");score_FCA 

#Robust FA
