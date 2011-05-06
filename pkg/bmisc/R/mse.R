mse <- function (model) UseMethod("mse")

mse.default <- function(model){
	mean(residuals(model,type ="response")^2)/df.residual(model)
}

