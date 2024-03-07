a1 <- 25
a2 <- 6
a3 <- 18
b1 <- 16
b2 <- 13
b3 <- 6
c1 <- 3
c2 <- 13
c3 <- 1

# Packages:
install.packages('GA')
library(GA)
install.packages('GenSA')
library('GenSA')
install.packages('alabama')
library('alabama')
library(ggplot2)

# Task 1

t0 <- 0 
tT <- 2 
dt <- 0.01 
n_time_moments <- (tT - t0) / dt + 1 
t <- seq(from = t0,
         to = tT, 
         by = dt) 

y_t0 <- -b2 
y_tT <- b1 

# min:

functional <- function(y_optim)
{
  y <- c(y_t0, y_optim, y_tT)
  dy <- (y - c(y_t0, y[-n_time_moments]))
  f <- (dy/dt)^2 + b1*(y)^2 + c1*(dy/dt)*exp(4*t)
  return(sum(f * dt))
} 

ga_functional <- GenSA(par = rep((y_t0 + y_tT) / 2, n_time_moments - 2),
                       fn = functional,
                       lower = rep(-10^4, n_time_moments - 2),
                       upper = rep(10^4, n_time_moments - 2)) 

y_optim <- ga_functional$par
optim_functional <- ga_functional$value
optim_functional

ggplot() + 
  geom_line(mapping = aes(x=t, y=c(y_t0, y_optim, y_tT)), color='red') +
  labs(title="Estimated y trajectory",
       x='t',
       y='y') + theme_bw()

ggsave('task1_min.png', width = 8, height = 4)

# max:

functional <- function(y_optim)
{
  y <- c(y_t0, y_optim, y_tT)
  dy <- (y - c(y_t0, y[-n_time_moments]))
  f <- (dy/dt)^2 + b1*(y)^2 + c1*(dy/dt)*exp(4*t)
  return(-sum(f * dt))
} 

ga_functional <- GenSA(par = rep((y_t0 + y_tT) / 2, n_time_moments - 2),
                       fn = functional,
                       lower = rep(-10^100, n_time_moments - 2),
                       upper = rep(10^100, n_time_moments - 2)) 

y_optim <- ga_functional$par
optim_functional <- -ga_functional$value
optim_functional

ggplot() + geom_line(aes(x=t, y=c(y_t0, y_optim, y_tT), color='red')) +
  labs(title="Estimated y trajectory",
       x="t", y="y") + theme_bw()
ggsave('task2_max.png', width = 8, height = 4)

# Task 2

t0 <- 0 
tT <- 3 
dt <- 0.1 
n <- (tT - t0) / dt + 1 
t <- seq(from = t0,
         to = tT, 
         by = dt) 

y_t0 <- a1 

# max:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- a3*y[i - 1] + u[i - 1]
    y[i] <- y[i - 1] + dy[i - 1] * dt 
  }
  return(-sum(b1*y[-1] - b2*(u^2)))
} 

ga_functional <- GenSA(fn = functional, lower = rep(-c1, n - 1),
                       upper = rep(c2, n - 1)) 

u_optim <- ga_functional$par
functional_optim <- -ga_functional$value
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- a3*y_optim[i - 1] + u_optim[i - 1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u trajectory",
                                x='t',
                                y='u') + theme_bw()
ggsave('u_task2_max.png', width = 8, height = 4)

ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) +
  geom_line(color='blue') +
  labs(title="Estimated y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task2_max.png', width = 8, height = 4)

# min:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- a3*y[i - 1] + u[i - 1]
    y[i] <- y[i - 1] + dy[i - 1] * dt # y(t) = y(t-1) + y'(t-1) * dt <=> y'(t-1) = (y(t) - y(t-1)) / dt
  }
  return(-sum(b1*y[-1] - b2*(u^2)))
} 

ga_functional <- ga("real-valued", functional, 
                    lower = rep(-c1, n - 1), 
                    upper = rep(c2, n - 1),
                    seed=1, optim=TRUE) 

u_optim <- c(ga_functional@solution[1,])
functional_optim <- ga_functional@fitnessValue
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- a3*y_optim[i - 1] + u_optim[i - 1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u trajectory",
       x='t',
       y='u') + theme_bw()
ggsave('u_task2_min.png', width = 8, height = 4)


ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) + 
  geom_line(color='blue') +
  labs(title="Estimated y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task2_min.png', width = 8, height = 4)

# Task 7

t0 <- 0 
tT <- 2
dt <- 0.1 
n <- (tT - t0) / dt + 1
t <- seq(from = t0,
         to = tT, 
         by = dt) 

y_t0 <- 0

# пункт a:

# max:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- u[i - 1] - t[i - 1]
    y[i] <- y[i - 1] + dy[i - 1] * dt # y(t) = y(t-1) + y'(t-1) * dt <=> y'(t-1) = (y(t) - y(t-1)) / dt
  }
  return(-sum((u^2)/2-t[-1]*y[-1]+y[-1]))
}

ga_functional <- GenSA(fn = functional,
                       lower = rep(-3/8, n - 1),
                       upper = rep(3/8, n - 1)) # genetic algorithm

u_optim <- ga_functional$par
functional_optim <- -ga_functional$value
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- u_optim[i - 1] - t[i - 1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='u') + theme_bw()
ggsave('u_task7_punkt_a_max.png', width = 8, height = 4)

ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) + 
  geom_line(color='blue') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task7_punkt_a_max.png', width = 8, height = 4)

# min:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- u[i - 1] - t[i - 1]
    y[i] <- y[i - 1] + dy[i - 1] * dt 
  }
  return(sum((u^2)/2-t[-1]*y[-1]+y[-1]))
}

ga_functional <- GenSA(fn = functional,
                       lower = rep(-3/8, n - 1),
                       upper = rep(3/8, n - 1)) 

u_optim <- ga_functional$par
functional_optim <- ga_functional$value
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- u_optim[i - 1] - t[i - 1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='u') + theme_bw()
ggsave('u_task7_punkt_a_min.png', width = 8, height = 4)

ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) + 
  geom_line(color='blue') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task7_punkt_a_min.png', width = 8, height = 4)


# пункт b:

# max:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- u[i - 1] + (u[i - 1])^2 - t[i-1]
    y[i] <- y[i - 1] + dy[i - 1] * dt 
  }
  return(-sum((u^2)/2-t[-1]*y[-1]+y[-1]))
} 

ga_functional <- GenSA(fn = functional,
                       lower = rep(-3/8, n - 1),
                       upper = rep(3/8, n - 1)) 

u_optim <- ga_functional$par
functional_optim <- -ga_functional$value
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- u_optim[i - 1] + (u_optim[i - 1])^2 - t[i-1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='u') + theme_bw()
ggsave('u_task7_punkt_b_max.png', width = 8, height = 4)

ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) + 
  geom_line(color='blue') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task7_punkt_b_max.png', width = 8, height = 4)

# min:

functional <- function(u)
{
  y <- c(y_t0, rep(0, n - 1))
  dy <- rep(0, n - 1)
  for (i in 2:n)
  {
    dy[i - 1] <- u[i - 1] + (u[i - 1])^2 - t[i-1]
    y[i] <- y[i - 1] + dy[i - 1] * dt 
  }
  return(sum((u^2)/2-t[-1]*y[-1]+y[-1]))
} 

ga_functional <- GenSA(fn = functional,
                       lower = rep(-3/8, n - 1),
                       upper = rep(3/8, n - 1)) 

u_optim <- ga_functional$par
functional_optim <- ga_functional$value
functional_optim

y_optim <- c(y_t0, rep(0, n - 1))
dy <- rep(0, n - 1)
for (i in 2:n)
{
  dy[i - 1] <- u_optim[i - 1] + (u_optim[i - 1])^2 - t[i-1]
  y_optim[i] <- y_optim[i - 1] + dy[i - 1] * dt 
}

ggplot(mapping = aes(x=t[-1], y=u_optim)) + 
  geom_line(color='red') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='u') + theme_bw()
ggsave('u_task7_punkt_b_min.png', width = 8, height = 4)

ggplot(mapping = aes(x=t[-1], y=y_optim[-1])) + 
  geom_line(color='blue') +
  labs(title="Estimated u and y trajectory",
       x='t',
       y='y') + theme_bw()
ggsave('y_task7_punkt_b_min.png', width = 8, height = 4)
