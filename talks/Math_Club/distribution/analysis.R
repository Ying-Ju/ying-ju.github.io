# Packages: install.packages(c("ggplot2", "fitdistrplus"))

pacman::p_load(fitdistrplus, tidyverse)

dir.create("figures", showWarnings = FALSE)

cols <- c(Normal = "#287c8e", t = "#d97938", Weibull = "#7963a6",
          Gamma = "#287c8e", Lognormal = "#d97938")

theme_set(theme_minimal(base_size = 17) + theme(
  plot.background = element_rect(fill = "#faf9f5", colour = NA),
  panel.background = element_rect(fill = "#faf9f5", colour = NA),
  panel.grid.minor = element_blank(), legend.position = "bottom",
  legend.title = element_blank(), strip.text = element_text(face = "bold"),
  text = element_text(colour = "#183b49")))

save_plot <- function(p, name, w = 12, h = 4.8) {
  ggsave(file.path("figures", paste0(name, ".png")), p, width=w, height=h,
         dpi=180, bg="#faf9f5")
}

# Positive two-parameter families have a fixed lower endpoint of zero.
fit_positive <- function(x, family) fitdist(x, family)

functions_for <- function(fit) {
  e <- as.list(fit$estimate); fam <- fit$distname
  list(d=function(x) do.call(paste0("d",fam),c(list(x=x),e)),
       p=function(q) do.call(paste0("p",fam),c(list(q=q),e)),
       q=function(p) do.call(paste0("q",fam),c(list(p=p),e)))
}

baseball <- read.csv("data/fastballs.csv")

baseball$player <- factor(baseball$player_name,
  levels=c("Abbott, Andrew","Bibee, Tanner","Yamamoto, Yoshinobu"),
  labels=c("Andrew Abbott","Tanner Bibee","Yoshinobu Yamamoto"))

bcurves <- bqq <- bresults <- list()

for (name in levels(baseball$player)) {
  x <- baseball$release_speed[baseball$player==name]
  mu <- mean(x); sig <- sqrt(mean((x-mu)^2))
  # Location-scale Student t: joint MLE for location, scale, and df.
  nllt <- function(theta) {
    s <- exp(theta[2]); df <- exp(theta[3])
    value <- -sum(dt((x-theta[1])/s,df,log=TRUE)-log(s))
    if (is.finite(value)) value else 1e100
  }
  starts <- c(10,100,1000)
  opts <- lapply(starts,function(df) optim(c(mu,log(sig),log(df)),nllt,
    method="Nelder-Mead",control=list(maxit=10000,reltol=1e-11)))
  opt <- opts[[which.min(vapply(opts,function(o)o$value,numeric(1)))]]
  ts <- exp(opt$par[2]); td <- exp(opt$par[3]); tm <- opt$par[1]
  cat(name, ": t df=", td, "location=",tm,"scale=",ts," convergence=",opt$convergence,"\n")
  wf <- functions_for(fit_positive(x,"weibull"))
  fs <- list(Normal=list(d=function(z)dnorm(z,mu,sig),q=function(p)qnorm(p,mu,sig)),
    t=list(d=function(z)dt((z-tm)/ts,td)/ts,q=function(p)tm+ts*qt(p,td)),Weibull=wf)
  grid <- seq(89,100,length.out=1000); prob <- ppoints(length(x),a=.5)

    for (model in names(fs)) {
    fun <- fs[[model]]
    bcurves[[length(bcurves)+1]] <- data.frame(player=name,model=model,x=grid,y=fun$d(grid))
    bqq[[length(bqq)+1]] <- data.frame(player=name,model=model,theory=fun$q(prob),observed=sort(x))
    bresults[[length(bresults)+1]] <- data.frame(player=name,model=model,n=length(x),
      AIC=2*ifelse(model=="t",3,2)-2*sum(log(fun$d(x))),
      t_df=ifelse(model=="t",td,NA_real_))
  }
}

bc <- do.call(rbind,bcurves); bq <- do.call(rbind,bqq)

for (obj in c("bc","bq")) {
  tmp <- get(obj); tmp$player <- factor(tmp$player,levels=levels(baseball$player)); assign(obj,tmp)
}

write.csv(do.call(rbind,bresults),"data/baseball_fit_results_R.csv",row.names=FALSE)

p <- ggplot(baseball,aes(release_speed)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=.25,boundary=89,fill="#d6e6e9",colour="white") +
  geom_line(data=bc,aes(x,y,colour=model),linewidth=.9) +
  facet_wrap(~player,nrow=1) + scale_colour_manual(values=cols) +
  coord_cartesian(xlim=c(89,100)) + labs(x="Four-seam speed (mph)",y="Density")

save_plot(p,"baseball_fits",14,4.7)

p <- ggplot(bq,aes(theory,observed,colour=model)) +
  geom_abline(slope=1,intercept=0,linetype=2,colour="grey50") +
  geom_point(size=.8,alpha=.55) + facet_wrap(~player,nrow=1) +
  scale_colour_manual(values=cols) + coord_fixed(ratio=1,xlim=c(88,101),ylim=c(88,101)) +
  labs(x="Model quantiles (mph)",y="Observed quantiles (mph)")

save_plot(p,"baseball_qq",14,5.2)


pitches <- read.csv("data/fastballs.csv")
pitches$game_date <- as.Date(pitches$game_date)

game_speed <- aggregate(
  release_speed ~ player_name + game_date + game_pk,
  data = pitches,
  FUN = mean,
  na.rm = TRUE
)

game_speed$player <- factor(
  game_speed$player_name,
  levels = c(
    "Abbott, Andrew",
    "Bibee, Tanner",
    "Yamamoto, Yoshinobu"
  ),
  labels = c(
    "Andrew Abbott · Reds",
    "Tanner Bibee · Guardians",
    "Yoshinobu Yamamoto · Dodgers"
  )
)

p <- ggplot(
  game_speed,
  aes(x = game_date, y = release_speed)
) +
  geom_line(color = "#287c8e", linewidth = 0.7) +
  geom_point(color = "#d97938", size = 2.5) +
  facet_wrap(~ player, ncol = 1, scales = "fixed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "",
    subtitle = "2026 regular season · Four-seam fastballs",
    x = NULL,
    y = "Mean velocity per game (mph)",
    caption = "Each point represents one game. Source: Baseball Savant."
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    plot.title = element_text(face = "bold"),
    plot.background = element_rect(fill = "#faf9f5", color = NA)
  )

p

dir.create("figures", showWarnings = FALSE)

ggsave(
  "figures/baseball_time_series.png",
  plot = p,
  width = 11,
  height = 7,
  dpi = 300
)



fleet <- read.csv("data/police_repairs_2022.csv")

x <- fleet$labor_hours[fleet$labor_hours>0]

rcurves <- rqq <- rresults <- list()

for (model in c("Gamma","Lognormal","Weibull")) {
  fam <- c(Gamma="gamma",Lognormal="lnorm",Weibull="weibull")[[model]]
  fit <- fit_positive(x,fam); fun <- functions_for(fit)
  grid <- seq(.001,88,length.out=3500)
  rcurves[[model]] <- data.frame(model=model,x=grid,y=fun$d(grid))
  rqq[[model]] <- data.frame(model=model,theory=fun$q(ppoints(length(x),a=.5)),observed=sort(x))
  rresults[[model]] <- data.frame(model=model,AIC=fit$aic,over8=1-fun$p(8))
}

write.csv(do.call(rbind,rresults),"data/repair_fit_results_R.csv",row.names=FALSE)

p <- ggplot(data.frame(x=x),aes(x)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=.5,boundary=0,fill="#d6e6e9",colour="white") +
  geom_line(data=do.call(rbind,rcurves),aes(x,y,colour=model),linewidth=1) +
  scale_colour_manual(values=cols) + coord_cartesian(xlim=c(0,20)) +
  labs(x="Positive recorded labor hours",y="Density")

save_plot(p,"repair_fits")

p <- ggplot(do.call(rbind,rqq),aes(theory,observed,colour=model)) +
  geom_abline(slope=1,intercept=0,linetype=2,colour="grey50") + geom_point(size=1,alpha=.55) +
  scale_colour_manual(values=cols) + coord_fixed(ratio=1,xlim=c(0,90),ylim=c(0,90)) +
  labs(x="Model quantiles (labor hours)",y="Observed quantiles (labor hours)")

save_plot(p,"repair_qq",6.5,6.5)

claims <- read.csv("data/helene_claims.csv")

y <- claims$netBuildingPaymentAmount; cap <- 250000

stopifnot(all(!is.na(y)),all(y>=0 & y<=cap))

p <- ggplot(data.frame(y=y),aes(y)) +
  geom_histogram(binwidth=10000,boundary=0,closed="left",fill="#287c8e",colour="white") +
  annotate("text",x=240000,y=Inf,label=sprintf("%d zero payments\n%d payments at $250,000",sum(y==0),sum(y==cap)),
           hjust=1,vjust=1.6,size=5,colour="#183b49") +
  scale_x_continuous(labels=scales::label_dollar()) +
  labs(x="Net building payment",y="Number of claims")

save_plot(p,"insurance_hist")

inside <- y[y>0 & y<cap]; z <- inside/cap

icurves <- iresults <- list()

for (model in c("Gamma","Lognormal")) {
  if (model=="Gamma") {
    fit <- fitdist(z,"gamma"); start <- log(c(fit$estimate[["shape"]],1/fit$estimate[["rate"]]))
    ld <- function(x,t) dgamma(x,shape=exp(t[1]),scale=exp(t[2]),log=TRUE)
    lF <- function(t) pgamma(1,shape=exp(t[1]),scale=exp(t[2]),log.p=TRUE)
  } else {
    fit <- fitdist(z,"lnorm"); start <- c(fit$estimate[["meanlog"]],log(fit$estimate[["sdlog"]]))
    ld <- function(x,t) dlnorm(x,meanlog=t[1],sdlog=exp(t[2]),log=TRUE)
    lF <- function(t) plnorm(1,meanlog=t[1],sdlog=exp(t[2]),log.p=TRUE)
  }
  nll <- function(t) {
    value <- -sum(ld(z,t)-lF(t))
    if(is.finite(value)) value else 1e100
  }
  opt <- optim(start,nll,method="Nelder-Mead",control=list(maxit=10000,reltol=1e-12))
  stopifnot(opt$convergence==0)
  grid <- seq(.0001,.9999,length.out=1500)
  icurves[[model]] <- data.frame(model=model,x=grid*cap,y=exp(ld(grid,opt$par)-lF(opt$par))/cap)
  # AIC on the scaled interior values; raw-dollar AIC has a common additive constant.
  iresults[[model]] <- data.frame(model=model,AIC_scaled=4+2*opt$value)
}

write.csv(do.call(rbind,iresults),"data/insurance_fit_results_R.csv",row.names=FALSE)

p <- ggplot(data.frame(x=inside),aes(x)) +
  geom_histogram(aes(y=after_stat(density)),binwidth=10000,boundary=0,fill="#d6e6e9",colour="white") +
  geom_line(data=do.call(rbind,icurves),aes(x,y,colour=model),linewidth=1) +
  scale_colour_manual(values=cols,labels=function(x)paste(x,"(truncated)")) +
  scale_x_continuous(labels=scales::label_dollar()) +
  labs(x="Interior payment",y="Conditional density")

save_plot(p,"insurance_body")

cat("Baseball AIC:\n"); print(do.call(rbind,bresults))
cat("Repair AIC and P(X>8):\n"); print(do.call(rbind,rresults))
cat("Insurance interior AIC (scaled):\n"); print(do.call(rbind,iresults))
cat("All six ggplot figures written.\n")
