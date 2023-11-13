data <- c("East","West","East","North","North","East","West","West","West","East","North")
print(data)
factor_data <- factor(data)
print(factor_data)

blood <- factor(c("O", "AB", "A"),levels = c("A", "B", "AB", "O"))
blood[1:2]
