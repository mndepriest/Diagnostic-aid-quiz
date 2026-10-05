# Shiny setup #### 

# August 10 2026
# R 4.5.2


install.packages('rsconnect')

rsconnect::setAccountInfo(name='esker-lab',
                          token='493402474BAE5EE08E2E7A29CDE9DAE8',
                          secret='YRJXdFfIyQgeB7mBYsbskcmc8aiRLg+aP0SYOaUa')

library(rsconnect)
rsconnect::deployApp("C:/Users/miran/Desktop/Disease Dashboard")
