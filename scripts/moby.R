library("poweRlaw")
data("moby")
m_pl= displ$new(moby)
est=estimate_xmin(m_pl)
m_pl$setXmin(est)
plot(m_pl)
lines(m_pl, col=2)

m_ln =dislnorm$new(moby)
est=estimate_xmin(m_ln)
m_ln$setXmin(est)
lines(m_ln, col=3)
bs=bootstrap(m_pl,no_of_sims=5000,threads=2)
  