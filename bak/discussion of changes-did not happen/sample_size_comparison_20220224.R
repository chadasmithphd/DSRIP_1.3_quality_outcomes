## SUMMARY OF CHANGES TABLE
################################################################################
# DATE       BY     WHAT
# 24FEB22    TCL    -- script created.
# 
#                      
################################################################################

## DESCRIPTION
## This script looks at the sample sizes of clients (numerator and denominator)
## from the RHP summary sheet provided by HHSC. Comparing sample sizes will
## provide some context about possible claims analysis introduced in DSRIP 1.3
## that is already provided in DSRIP 1.2

dta_path <- '/Users/toddleroux/Texas A&M University/Team - waiver2_final - Documents/DSRIP/DSRIP_1.3_quality_outcomes/'

library(tidyverse)
library(readxl)

source('/Users/toddleroux/Stonewall Analytics/ward_viking - Documents/ward_viking/functions.R')
'%notin%' <- Negate('%in%')

dta <- read_excel(paste0(dta_path, 
                         'rhp_summary_tcl.xlsx'), 
                  sheet = 'filter') %>% clean_names()

## getting the proper names for the facilities not found in the claims
## crosswalk data (these were identified in work on vidal)
tpi_exclude <- dta$tpi[grep(x = dta$provider, pattern = paste(
                                        'fannin county hosp', 
                                        'community care collaborative',
                                        'uhs texoma',
                                        'laredo reg med ctr',
                                        sep = '|'), ignore.case = T, value = F)]


dta2 <- dta %>% filter(measure_id == 'A1-508',
                       `%_of_am-7_1_goal_achieved_in_py1` %in% c('0', '0.75', '1'),
                       tpi %notin% tpi_exclude)

## make from wide to long
dta3 <- dta2 %>% select(provider, tpi, contains('numer'), contains('denom')) %>%
                 select(provider, tpi, matches(paste('baseline', 'py1', 'py2', 'py3', sep = '|')))
dta4 <- dta3 %>% select(-19:-50, -67:-98)

dta4_long <- dta4 %>% 
               pivot_longer(cols = ! c(provider, tpi)) %>% 
                  mutate(name2 = paste0('_', name),
                         what = ifelse(grepl('numer', name2),
                                       'numerator', 'denominator'),
                         when = ifelse(grepl('baseline', name2),
                                       'baseline',
                                       ifelse(grepl('py1', name2),
                                              'year_1',
                                              ifelse(grepl('py2', name2),
                                                     'year_2',
                                                     'year_3'))),
                         who = ifelse(grepl('all_payer', name2),
                               'all_payer',
                               ifelse(grepl('_liu_', name2),
                                      'liu',
                                      ifelse(grepl('_mliu_', name2),
                                             'mliu', 'medicaid'))),
                         value = as.integer(value)) %>%
                    select(-name2) %>% rename(orig = name)


## some plots
d4la <- dta4_long %>% 
         filter(what == 'denominator', 
                when == 'baseline') %>% 
            group_by(who) %>% 
              summarise(n = sum(value))

study <- data.frame(who = 'treatment_clients', n = 1240)
d4la2 <- bind_rows(d4la, study)
d4la2$who <- factor(x = d4la2$who,
                    levels = c('all_payer', 'mliu', 'liu', 'medicaid', 'treatment_clients'),
                    labels = c('All Payers', 'MLIU', 'LIU', 'Medicaid', 'Treatment Sample'))


g <- ggplot(data = d4la2, aes(x = reorder(who, -n), y = n)) + geom_bar(stat = 'identity', fill = '#045a8d')
g <- g + geom_text(data = subset(x = d4la2, subset = d4la2$who != 'Treatment Sample'), 
                   aes(x = reorder(who, -n), 
                       y = n, 
                       label = scales::comma(n)), 
                   size = 3,
                   color = 'white',
                   position = position_stack(vjust = .85))
g <- g + geom_text(data = subset(x = d4la2, subset = d4la2$who == 'Treatment Sample'), 
                   aes(x = reorder(who, -n), 
                       y = n, 
                       label = scales::comma(n)), 
                   size = 3,
                   color = 'black',
                   position = position_stack(vjust = 8))
g <- g + theme_minimal() + labs(title = 'Sample Size Comparison of 18 Treatment DSRIP 1.3 Providers',
                                subtitle = 'Baseline Denominators',
                                x = 'Population', y = 'Denominator')
g <- g + scale_y_continuous(labels = scales::comma)
g
