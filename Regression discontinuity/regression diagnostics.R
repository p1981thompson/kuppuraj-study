plot(mod1d,type=c("p","smooth"))

plot(mod1d,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))

plot(mod1d,resid(.,type="pearson")~SetInd, type=c("p","smooth"))

qqnorm(mod1d)

hist(resid(mod1d),100)

#############################

plot(mod1d2,type=c("p","smooth"))

plot(mod1d2,sqrt(abs(resid(.)))~fitted(.),
                  type=c("p","smooth"),ylab=expression(sqrt(abs(resid))))

plot(mod1d2,resid(.,type="pearson")~SetInd, type=c("p","smooth"))

qqnorm(resid(mod1d2))
qqline(resid(mod1d2))

hist(resid(mod1d2),100)
