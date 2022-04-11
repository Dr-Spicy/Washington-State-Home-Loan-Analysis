library(randomForest)


## remove denials, county_code, county_name
# relocate
data = data.1 %>% select(!c(denials, county_code, county_name)) %>% relocate(loan_approved)

## recode the class label
data = data %>% mutate(loan_approved = if_else(loan_approved == 'Yes','1','0'))

## convert all chr to factor
chr.col = names(select_if(data,is.character))
data[,chr.col] = lapply(data[,chr.col], factor)
summary(data)

