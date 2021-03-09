####################################################################
###############  INIT                  #############################
####################################################################

library("data.table")
library("dplyr")
library("ggplot2")
library("gridExtra")
library("stringr")
library("glmnet")

#Load file
if(!file.exists("downloadeddata.csv")){

	path <- "https://instruction.bus.wisc.edu/jfrees/jfreesbooks/regression%20modeling/bookwebdec2010/CSVData/HealthExpend.csv"

	download.file(path, "downloadeddata.csv")
	MEPS <- read.csv("downloadeddata.csv")

} else {

	MEPS <- fread("downloadeddata.csv")

}

####################################################################

#Drop trash, format factors
#Remove duplicates
#Replace NA values with factor "Missing"
names(MEPS) <- toupper(names(MEPS))
duplicates <- names(MEPS)[which(grepl("1", names(MEPS)))]
f <- function(x){if(any(x == "")){x[which(x == "")] <- "MISSING"}
					return(x)}


MEPS %>%
	.[, (c("EXPENDIP", "EXPENDOP")) := NULL] %>%
	.[, (duplicates) := NULL] %>%
	.[, (names(MEPS)[which(sapply(MEPS, is.character))]) := lapply(.SD, f), .SDcols = names(MEPS)[which(sapply(MEPS, is.character))]]

#Organize the frame
#Identify the binary variables
is_binary <- function(x){

	vals <- sort(unique(x))
	if(length(vals) == 2){

		return(all.equal(sort(unique(x)), c(0,1)))

	} else {

		return(FALSE)

	}
	
}

binary_variales <- names(MEPS)[which(sapply(MEPS, is_binary))]

convert_binary_to_factor <- function(x){

	out <- rep("Yes", length(x))
	out[which(x == 0)] <- "No"
	return(as.factor(out))

}

blank_string_to_NA <- function(x){

	x[which(x == "")] <- NA 
	return(x)

}


#Convert characters to factor
factor_variables <- names(MEPS)[which(sapply(MEPS, is.character))]
MEPS %>% 
	.[, (factor_variables) := lapply(.SD, blank_string_to_NA), .SDcols = factor_variables] %>%
	.[, (factor_variables) := lapply(.SD, as.factor), .SDcols = factor_variables] %>%
	.[, (binary_variales) := lapply(.SD, convert_binary_to_factor), .SDcols = binary_variales] 


#Save backupframe for Q10
backup <- MEPS[, lapply(.SD, function(x){x}), .SDcols = names(MEPS)]


#Q1
print("Table of COUNTIP:", quote = FALSE)
print(table(MEPS$COUNTIP))

COUNTIP_histogram_frame <- MEPS[, c("COUNTIP"), with = FALSE]
COUNTIP_histogram <- ggplot(COUNTIP_histogram_frame, aes(x = COUNTIP)) + geom_histogram(color="grey", 
																						fill="black",
																						binwidth = 1) +
																		ggtitle("Histogram of COUNTIP") +
																		xlab("Value") +
																		ylab("Count")

print(COUNTIP_histogram)																		


COUNTIP_stats <- c(mean(MEPS$COUNTIP),
					var(MEPS$COUNTIP))
COUNTIP_stats <- as.data.table(t(COUNTIP_stats))
colnames(COUNTIP_stats) <- c("Mean", "Variance")
COUNTIP_stats[, ("Variance/Mean") := Variance / Mean]
print(COUNTIP_stats)


#Q2
props <- MEPS[, lapply(.SD, function(x){sum(x == "Yes") / length(x)}), .SDcols = binary_variales]

props_plot_frame <- as.data.frame(t(props))
colnames(props_plot_frame) <- "Proportion"
props_plot_frame$Variable <- rownames(props_plot_frame)

props_plot <- ggplot(props_plot_frame, aes(x = Variable, y = Proportion)) + geom_bar(stat="identity") +
																			theme(axis.text.x = element_text(angle = 90)) +
																			ggtitle("Proportions of 1s within the sample") +
																			geom_hline(yintercept = 0.5, color = "red", linetype = "dashed")

print(props_plot)																			

#Q3
pie_chart <- function(x){

	if(any(is.na(x))){

		x <- as.character(x)
		x[which(is.na(x))] <- "MISSING"
		x <- as.factor(x)

	}
	values <- sort(unique(x))
	counts <- lapply(values, function(a){length(which(x == a))})


	pie_frame <- as.data.frame(values)
	colnames(pie_frame) <- "Variable"
	pie_frame$Value <- counts

	chart <- ggplot(pie_frame, aes(x="", y = Value, fill = Variable))+
			geom_bar(width = 1, stat = "identity") + 
			coord_polar("y", start=0) 

	return(chart)

}

pie_plots <- lapply(MEPS[, factor_variables, with = FALSE], pie_chart)
for(i in 1:length(pie_plots)){pie_plots[[i]] <- pie_plots[[i]] + ggtitle(factor_variables[i])}

gridExtra::grid.arrange(grobs = pie_plots[1:4], ncol=2, nrow=2)
gridExtra::grid.arrange(grobs = pie_plots[5:length(pie_plots)], ncol = 2, nrow=2)


#Q4

#RACE, GENDER, PHSTAT ANYLIMIT and INSURE seem to influence COUNTOP

mean_by <- function(x){

	out <- MEPS[, lapply(.SD, mean), .SDcols = "COUNTOP", by = x]
	names(out)[2] <- paste(names(out)[2], "_Mean", sep = "")
	return(out)

}
variables <- c("GENDER", "RACE", "REGION", "EDUC", "COLLEGE", "HIGHSCH", "PHSTAT", "ANYLIMIT", "INCOME", "INSURE")
tables_of_means <- lapply(as.list(variables), mean_by)

tables_of_means_max <- max(unlist(lapply(tables_of_means, function(x){max(x$COUNTOP_Mean)})))

bar_plots <- function(x){

	title <- paste("COUNTOP means given", names(x)[1])
	names(x)[1] <- "Variable"
	names(x)[2] <- "Mean"

	out <- ggplot(x, aes(x = Variable, y = Mean)) + geom_bar(stat="identity") +
																		ggtitle(title) +
																		geom_hline(yintercept = mean(MEPS$COUNTOP), color = "red", linetype = "dashed") +
																		ylim(0, tables_of_means_max + 2)

	return(out)

}

tables_of_means_barplots <- lapply(tables_of_means, bar_plots)
gridExtra::grid.arrange(grobs = tables_of_means_barplots[1:4], ncol=2, nrow=2)
gridExtra::grid.arrange(grobs = tables_of_means_barplots[5:8], ncol=2, nrow=2)
gridExtra::grid.arrange(grobs = tables_of_means_barplots[9:length(tables_of_means_barplots)], ncol=2, nrow=2)


#Q5
numerical_vars <- MEPS[, lapply(.SD, as.numeric), .SDcols = c("EDUC", "COLLEGE", "HIGHSCH")]
numerical_vars_cor <- cor(numerical_vars)

print(round(numerical_vars_cor, 2))

tile <- expand.grid(numerical_vars_cor)
names <- expand.grid(x = rownames(numerical_vars_cor), y = colnames(numerical_vars_cor))
tile <- as.data.table(cbind(tile, names))
names(tile) <- c("Correlation", "Var1", "Var2")

cor_plot <- ggplot(tile, aes(x = Var1, y = Var2, fill = Correlation)) + geom_tile() +
																		ggtitle("Correlation Heatmap") +
																		scale_fill_gradientn(colours = c("blue", "white", "red"),
															  						breaks = seq(-1, 1, 0.25))

cor_plot

#Drop EDUC
MEPS[, EDUC := NULL]
variables <- variables[-which(variables == "EDUC")]


#Q6
#Set COUNTOP aside
COUNTOP <- MEPS[, c("COUNTOP"), with = FALSE]
MEPS[, COUNTOP := NULL]

variables <- names(MEPS)
poisson_model_Q6 <- glm(COUNTIP ~., MEPS, family = poisson())
poisson_model_Q6_summary <- summary(poisson_model_Q6)

aic <- poisson_model_Q6_summary$aic

answer_table <- as.data.frame(aic)
colnames(answer_table) <- c("AIC")

p <- nrow(MEPS) - poisson_model_Q6_summary$df.residual
LL <- (aic - 2*p) / (-2)

answer_table$BIC <- p*log(nrow(MEPS)) - 2*LL
print(answer_table)


#Q7
model_Q6_coefs <- as.data.table(poisson_model_Q6_summary$coefficients)


split_name <- function(x){

	matches <- sapply(variables, function(a){grepl(a, x, fixed = TRUE)})
	matches <- apply(matches, 1, function(a){
												k <- which(a)
												if(length(k) == 0){return(NA)} else {return(k[length(k)])}
											})

	work_with <- which(!is.na(matches))
	matches <- matches[work_with]
	var_names <- variables[matches]

	x[work_with] <- sapply(c(1:length(var_names)), function(a){

														suffix <- str_remove(x[work_with][a], var_names[a])
														return(paste(var_names[a], "_=_", suffix, sep = ""))

		})

	return(x)

}

#Format the names of the coefficients, and print the rounded p-values
model_Q6_coefs %>%
	.[, Variable := rownames(poisson_model_Q6_summary$coefficients)] %>%
	.[, Variable := lapply(.SD, split_name), .SDcols = c("Variable")] %>%
	.[, (names(model_Q6_coefs)[1:3]) := NULL] %>%
	.[, P_val := lapply(.SD, function(x){x}), .SDcols = c("Pr(>|z|)")] %>%
	.[, ("Pr(>|z|)") := NULL] %>%
	.[, P_val := round(P_val, 4)] %>%
	.[, Coef := coef(poisson_model_Q6)] 

print(model_Q6_coefs)

#Filter dummy variables with p_val > 0.1


model_Q6_coefs_low_pval <- model_Q6_coefs[P_val > 0.1] %>%
												.[, P_val := round(P_val, 4)]




#Get p-values for factors as a whole
model_Q6_pval <- as.data.table(drop1(poisson_model_Q6))[-1, ] %>%
					.[, Chi_Score := Deviance - poisson_model_Q6$deviance] %>%
					.[, P_val := 1 - pchisq(Chi_Score, Df)] 

model_Q6_pval[, names(model_Q6_pval) := lapply(.SD, function(x){round(x, 6)}), .SDcols = names(model_Q6_pval)]					

model_Q6_pval <- cbind(all.vars(formula(poisson_model_Q6)[-2]), model_Q6_pval)
names(model_Q6_pval)[1] <- "Variable"

print(model_Q6_pval)


#Drop the levels with low p-values
#I will set dropped variables to their reference (i.e.: the first factor of their class)
#This ammounts to dropping their one-hot-encoded column
#(Dropped variable, and reference level, are tagged as "A_Base")
to_trim <- unique(str_split(model_Q6_coefs_low_pval$Variable, "_", simplify = TRUE)[, 1])
f <- function(x){

	index <- which(str_split(model_Q6_coefs_low_pval$Variable, "_", simplify = TRUE)[, 1] == x)
	base_level <- levels(MEPS[, x, with = FALSE][[1]])[1]
	base_level <- unique(c(base_level, str_split(model_Q6_coefs_low_pval$Variable, "_", simplify = TRUE)[index, 3]))

	return(base_level)

}
to_remove <- lapply(as.list(to_trim), f)

for(i in 1:length(to_trim)){

	if("" %in% to_remove[[i]]){next}
	setkeyv(MEPS, (to_trim[i]))
	MEPS %>%
		.[to_remove[[i]], (to_trim[i]) := "A_Base"] %>%
		.[, (to_trim[i]) := lapply(.SD, function(x){as.factor(as.character(x))}), .SDcols = to_trim[i]]

}

#Drop the variables with low p-values
rmv <- model_Q6_pval[P_val > 0.1, Variable]
MEPS[, (rmv) := NULL]




#Fit the new Poisson model
variables <- names(MEPS)
poisson_model_Q7 <- glm(COUNTIP ~., MEPS, family = poisson())
poisson_model_Q7_summary <- summary(poisson_model_Q7)

aic <- poisson_model_Q7_summary$aic

answer_table_Q7 <- as.data.frame(aic)
colnames(answer_table_Q7) <- c("AIC")

p <- nrow(MEPS) - poisson_model_Q7_summary$df.residual
LL <- (aic - 2*p) / (-2)

answer_table_Q7$BIC <- p*log(nrow(MEPS)) - 2*LL
answer_table_Q7 <- rbind(answer_table, answer_table_Q7)
rownames(answer_table_Q7) <- c("Full Model", "Reduced Model")
print(answer_table_Q7)


#Q8

#The Gender variable multiplies the mean COUNTIP by a factor of 1.493726

model_Q7_coefs <- as.data.table(poisson_model_Q7_summary$coefficients)
model_Q7_coefs %>%
	.[, Variable := rownames(poisson_model_Q7_summary$coefficients)] %>%
	.[, Variable := lapply(.SD, split_name), .SDcols = c("Variable")] %>%
	.[, (names(model_Q7_coefs)[1:3]) := NULL] %>%
	.[, P_val := lapply(.SD, function(x){x}), .SDcols = c("Pr(>|z|)")] %>%
	.[, ("Pr(>|z|)") := NULL] %>%
	.[, P_val := round(P_val, 10)] %>%
	.[, Coef := coef(poisson_model_Q7)] 

model_Q7_coefs_groupnames <- str_split(model_Q7_coefs$Variable, "_", simplify = TRUE)[, 1]

IsMale <- model_Q7_coefs[which(model_Q7_coefs_groupnames == "GENDER")] %>%
			.[, Multiplicative_Factor := exp(Coef)]

print(IsMale)


#Q9

individual <- MEPS[1]
individual$MARISTAT <- "A_Base"
individual$INSURE <- "Yes"
individual$AGE <- 30
individual$ANYLIMIT <- "No"
individual$PHSTAT <- "A_Base"
individual$REGION <- "WEST"
individual$INDUSCLASS <- "TRANSINFO"
individual$UNEMPLOY <- "No"

answer <- predict(poisson_model_Q7, individual, type = "response")
print(paste("Predicted mean IP:", round(answer, 4)), quote = FALSE)


#Q10

#Using L1 penalty for variable selection (family = poisson), then a quasi-poisson glm,
#we find that AGE, ANYLIMIT, COLLEGE, GENDER, LNHPOOR, INSURE, ESC, FAMSIZE and PHSTAT
#all seem to have an effect on COUNTOP. The over-dispertion parameter is almost 20.


#Use the original frame, since the MEPS one has been altered
#Remove COUNTIP
backup[, COUNTIP := NULL]

#Plot density
ggplot(backup, aes(x = COUNTOP)) + geom_histogram(color="grey", 
													fill="black",
													binwidth = 3) +
									ggtitle("Histogram of COUNTOP") +
									xlab("Value") +
									ylab("Count")


COUNTOP_stats <- c(mean(backup$COUNTOP),
					var(backup$COUNTOP))
COUNTOP_stats <- as.data.table(t(COUNTOP_stats))
colnames(COUNTOP_stats) <- c("Mean", "Variance")
COUNTOP_stats[, ("Variance/Mean") := Variance / Mean]
print(COUNTOP_stats)

#The variable is over-dispersed
#Use glmnet with L1 penalty for variable selection (1se rule)

X <- model.matrix(COUNTOP ~. , backup)
y <- as.matrix(backup$COUNTOP)
colnames(y) <- "COUNTOP"

lambda <- cv.glmnet(X, y, family = "poisson")

#Fit a regular GLM with the selected variable, but with quasi-poisson because of over-dispersion
#glmnet was only poisson, but the estimate for the mean is the same in both case, so it should be fine
X <- X[, which(coef(lambda)[-1] != 0)]
X <- as.data.table(cbind(X, y))

COUNTOP_model <- glm(COUNTOP ~., X, family = quasipoisson())
print(summary(COUNTOP_model))


