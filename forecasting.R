
<!-- 
  Next, I plotted the observed vs. predicted adults. 
```{r}
data.out <- dataForPlotting %>% 
  dplyr::rename(time = day) %>%
  dplyr::left_join(sum.data,by="time")

ggplot() +
  geom_pointrange(data=data.out,aes(x=totalAdult, y = X50., ymin=X2.5.,ymax=X97.5.)) +
  geom_abline(intercept=0,slope=1) +
  theme_classic() + xlim(c(0,15)) + ylim(c(0,15)) +
  xlab("Observed Adults (observed counts)") + ylab("Predicted Adults (model mean)") 
```

Now forecasting. 
```{r}
# define a function

fitRandomWalk <- function(data = adultDataWithMet, siteName = "SERC", jagsModel = RandomWalk,  forecast=FALSE){
  
  tmp <- data %>% 
    dplyr::filter(Site==siteName)
  
  if (forecast) {tmp$totalAdult[tmp$trainValidate=="Validate"] <- NA} else {tmp <- tmp[tmp$trainValidate=="Train",]}
  
  time = as.Date(tmp$Day)
  y = tmp$totalAdult
  
  dataJags <- list(y=y,
                   n=length(y),
                   x_ic=log(y[1]),
                   a_add=1,
                   r_add=1)
  
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    y.samp = sample(y,length(y),replace=TRUE)
    init[[i]] <- list(tau_add=1/var(y.samp))
  }
  
  j.model <- jags.model (file = textConnection(jagsModel),
                         data = dataJags,
                         inits = init,
                         n.chains = 3)
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c("x","mu","tau_add"),
                              n.iter = 10000)
}

```
```{r}
jags.out <- fitRandomWalk(data = adultDataWithMet, siteName = "ORNL", jagsModel = RandomWalk, forecast=FALSE)

out <- as.matrix(jags.out)
x.cols <- grep("^x",colnames(out)) ## grab all columns that start with the letter x
## remove exp() because model not on log scale
ci <- apply(out[,x.cols],2,quantile,c(0.025,0.5,0.975)) ## model was NOT fit on log scale

time = as.Date(adultDataWithMet[adultDataWithMet$Site=="ORNL"&adultDataWithMet$trainValidate=="Train",]$Day)

sum.data=data.frame(time,t(ci))

dataForPlotting = adultDataWithMet %>% 
  dplyr::filter(Site=="ORNL") %>%
  dplyr::filter(trainValidate=="Train")

ggplot() +
  geom_ribbon(data=sum.data,aes(x=time,ymin=X2.5.,ymax=X97.5.),
              color="cornflowerblue",fill="cornflowerblue",alpha=0.5) +
  geom_line(data=sum.data,aes(x=time,y=X50.),color="black") +
  geom_point(data=dataForPlotting,aes(x=day,y=totalAdult)) +
  theme_classic() + 
  xlab("Time") + ylab("Total Adults (counts)")
```
Note that this model misses all 0s.

## Second goal: could then set up a workflow to run the other sites with the same model
-->