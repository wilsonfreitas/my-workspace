
library(rb3)
library(fixedincome)

df_yc <- yc_get("2022-05-20")

crv <- spotratecurve(
  df_yc$r_252, df_yc$biz_days, "discrete", "business/252", "Brazil/ANBIMA",
  refdate = df_yc$refdate[1]
)

forwardrate(crv, term(10, "days"), term(20, "days"))

t1 <- term(10, "days")
t2 <- term(20, "days")
fact1 <- compound(crv[[t1]])
fact2 <- compound(crv[[t2]])
fwd_term <- t2 - t1
tf <- toyears(crv@daycount, fwd_term)
rates(crv@compounding, tf, fact2 / fact1)