power.prop.test(n=NULL,p1=NULL,p2=NULL,sig.level = 0.05,power=NULL,
                alternative = c("two.sided","one.sided")),
                strict=FALSE,
                tol=.Machine$double.eps^0.25)

power.prop.test(p1=0.03,p2=0.033,sig.level = 0.05,power = 0.8)

power.prop.test(p1=0.05, p2=0.051,sig.level = 0.05,power = 0.8)
