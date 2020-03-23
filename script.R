require(tidyverse)
n <- 1000
reporting.rate <- .5
pop.size <- 9000000

prob.group <- function(group.size,cases.rep,n,reporting.rate,pop.size) {
p.case.pop <- rpois(n,cases.rep/reporting.rate)/pop.size
p.no.case.group <- (1-p.case.pop)^group.size
p.case.group <- 1-p.no.case.group
return(p.case.group)
}

group.size <- c(50,100,500,1000,5000,10000)
cases.rep <- seq(0,10000,by = 200)
input<- expand.grid(group.size=group.size,cases.rep=cases.rep)
res <- data.table::data.table(n = rep(1:n,nrow(input)),
                              cases.rep = rep(input$cases.rep,each=n),
                              group.size = rep(input$group.size,each=n))
res$prob <- c(mapply(prob.group,group.size=input$group.size,cases.rep=input$cases.rep,
       MoreArgs=list(n=n,reporting.rate=reporting.rate,pop.size=pop.size)))

res %>% 
  group_by(cases.rep,group.size) %>% 
  summarise(lwr=quantile(prob,.025),
            mean=mean(prob),
            upr=quantile(prob,.975)) %>% 
  ggplot(aes(x=cases.rep,y=mean,ymin=lwr,ymax=upr,fill=factor(group.size)))+
  geom_ribbon(alpha=.2)+geom_line(aes(color=factor(group.size)))


  
