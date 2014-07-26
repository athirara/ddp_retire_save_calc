# investment allocation by age 
# write file
# allocation based on Vanguard glide path
# http://www.wealthadvisors.com/assets/vanguard%20allocation%20updatetrf%20changes.pdf

# for age 
# note that the presumed retirement age is 65 in the glide path
invest <- data.frame(age=0:110, us_stocks=NA, intl_stocks=NA, us_bonds=NA, intl_bonds = NA, st_TIPS = NA)
invest[invest$age < 40, 'us_stocks'] <- 63
invest[invest$age < 40, 'intl_stocks'] <- 27
invest[invest$age < 40, 'us_bonds'] <- 8
invest[invest$age < 40, 'intl_bonds'] <- 2
invest[invest$age < 40, 'st_TIPS'] <- 0

invest[invest$age >= 40 & invest$age < 45, 'us_stocks'] <- 61
invest[invest$age >= 40 & invest$age < 45, 'intl_stocks'] <- 26
invest[invest$age >= 40 & invest$age < 45, 'us_bonds'] <- 10.5
invest[invest$age >= 40 & invest$age < 45, 'intl_bonds'] <- 2.5
invest[invest$age >= 40 & invest$age < 45, 'st_TIPS'] <- 0

invest[invest$age >= 45 & invest$age < 50, 'us_stocks'] <- 60.1
invest[invest$age >= 45 & invest$age < 50, 'intl_stocks'] <- 25.8
invest[invest$age >= 45 & invest$age < 50, 'us_bonds'] <- 11.3
invest[invest$age >= 45 & invest$age < 50, 'intl_bonds'] <- 2.8
invest[invest$age >= 45 & invest$age < 50, 'st_TIPS'] <- 0

invest[invest$age >= 50 & invest$age < 55, 'us_stocks'] <- 54.9
invest[invest$age >= 50 & invest$age < 55, 'intl_stocks'] <- 23.5
invest[invest$age >= 50 & invest$age < 55, 'us_bonds'] <- 17.3
invest[invest$age >= 50 & invest$age < 55, 'intl_bonds'] <- 4.3
invest[invest$age >= 50 & invest$age < 55, 'st_TIPS'] <- 0

invest[invest$age >= 55 & invest$age < 60, 'us_stocks'] <- 49.6
invest[invest$age >= 55 & invest$age < 60, 'intl_stocks'] <- 21.3
invest[invest$age >= 55 & invest$age < 60, 'us_bonds'] <- 23.3
invest[invest$age >= 55 & invest$age < 60, 'intl_bonds'] <- 5.8
invest[invest$age >= 55 & invest$age < 60, 'st_TIPS'] <- 0

invest[invest$age >= 60 & invest$age < 65, 'us_stocks'] <- 44.4
invest[invest$age >= 60 & invest$age < 65, 'intl_stocks'] <- 19.0
invest[invest$age >= 60 & invest$age < 65, 'us_bonds'] <- 29.3
invest[invest$age >= 60 & invest$age < 65, 'intl_bonds'] <- 7.3
invest[invest$age >= 60 & invest$age < 65, 'st_TIPS'] <- 0

invest[invest$age >= 65 & invest$age < 70, 'us_stocks'] <- 38.2
invest[invest$age >= 65 & invest$age < 70, 'intl_stocks'] <- 16.3
invest[invest$age >= 65 & invest$age < 70, 'us_bonds'] <- 32.0
invest[invest$age >= 65 & invest$age < 70, 'intl_bonds'] <- 9.1
invest[invest$age >= 65 & invest$age < 70, 'st_TIPS'] <- 4.4

invest[invest$age >= 70, 'us_stocks'] <- 29.5
invest[invest$age >= 70, 'intl_stocks'] <- 12.6
invest[invest$age >= 70, 'us_bonds'] <- 34.8
invest[invest$age >= 70, 'intl_bonds'] <- 11.6
invest[invest$age >= 70, 'st_TIPS'] <- 11.5

head(invest)
library(reshape2)
i <- melt(invest, id='age')

library(data.table)
i2 <- data.table(i, key = 'age')
i2
setnames(i2, 2:3, c('type','percent'))

levels(i2$type)[levels(i2$type)=="us_stocks"] <- "US Stocks"
levels(i2$type)[levels(i2$type)=="intl_stocks"] <- "International Stocks"
levels(i2$type)[levels(i2$type)=="us_bonds"] <- "US Bonds"
levels(i2$type)[levels(i2$type)=="intl_bonds"] <- "International Bonds"
levels(i2$type)[levels(i2$type)=="st_TIPS"] <- "Short Term TIPS"

i2 <- i2[age >= 10 &age<=100,]

i2$years_till_retirement <- 65 - i2$age

write.table(i2, 'invest_allocation_2.csv', sep = ',', row.names = F)

#======================================================
# for time till retirment

i3 <- copy(i2[i2$years_till_retirement>=0, 2:4, with=F]) # make i2 and i3 indepedent data tables

tmp <- data.frame(type = rep(unique(i3$type), length(56:90)),
                  percent = rep(unique(i3$percent[i3$years_till_retirement == 55]), length(56:90)),
                  years_till_retirement = rep(56:90, each=5))

i4 <- rbindlist(list(i3, tmp))

setkey(i4, 'years_till_retirement')

write.table(i4, 'invest_allocation_year.csv', sep = ',', row.names = F)
