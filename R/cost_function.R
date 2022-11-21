

cost_function <- function(params, time, m, n, Nm, Nd) {
  #message(params["P"], ",", params["k"])
  pred <- ap_prediction(time = time,
                        m0 = m[1],
                        n0 = n[1],
                        P = params["P"],
                        k = params["k"])
  #vFH eq 14
  sum((abs(m - pred$mt)/sd(m))*Nm + (abs(n - pred$nt)/sd(n))*Nd)
}
