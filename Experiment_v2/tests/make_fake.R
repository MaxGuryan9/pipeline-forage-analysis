# Writes fake EthoVision-style raw exports: make_fake_file(path, seed)
# 23 h at one sample every 2 s, zone visits as random runs.
make_fake_file <- function(path, seed, start = "11/09/2026 07:00:00.000") {
  set.seed(seed)
  dt <- 2
  n  <- 23 * 3600 / dt
  t  <- (0:(n - 1)) * dt

  zones <- c("wheel", "cup", "shelter", "hopper", "pellet")
  runs  <- sample(zones, 3000, replace = TRUE)
  lens  <- rpois(3000, 12) + 1
  state <- rep(runs, lens)[1:n]

  z <- function(nm) as.integer(state == nm)
  df <- data.frame(
    `Trial time` = t, `Recording time` = t,
    `X center` = round(runif(n, 0, 30), 2), `Y center` = round(runif(n, 0, 30), 2),
    `X nose` = round(runif(n, 0, 30), 2), `Y nose` = round(runif(n, 0, 30), 2),
    `X tail` = round(runif(n, 0, 30), 2), `Y tail` = round(runif(n, 0, 30), 2),
    Area = round(runif(n, 5, 9), 2), Areachange = round(runif(n, 0, 1), 2),
    Elongation = round(runif(n, 0, 1), 2), Direction = round(runif(n, -180, 180), 1),
    `Distance moved` = round(rexp(n, 2), 3),
    `pellet earned` = rbinom(n, 1, 0.003) * z("pellet"),
    licks = rpois(n, 0.5) * z("cup"),
    wheel = rpois(n, 1.5) * z("wheel"),
    `nose pokes` = rbinom(n, 1, 0.05) * z("pellet"),
    `In zone wheel` = z("wheel"), `In zone cup` = z("cup"), `In zone shelter` = z("shelter"),
    `In zone food hopper` = z("hopper"), `In zone to earn pellet` = z("pellet"),
    `Result 1` = 1,
    check.names = FALSE
  )
  df$`X nose`[1:3] <- NA   # EthoVision writes "-" for missing values

  n_hdr <- 38
  header <- c(
    sprintf('"Number of header lines:","%d"', n_hdr),
    '"Experiment","Fake forage v2"',
    sprintf('"Start time","%s"', start),
    '"Arena name","Arena 1"',
    rep('""', n_hdr - 6),
    paste0('"', names(df), '"', collapse = ","),
    paste0('"', c("s", "s", rep("cm", 6), "cm2", "cm2/s", "", "deg", "cm", rep("", 10)), '"', collapse = ",")
  )
  stopifnot(length(header) == n_hdr)
  writeLines(header, path)
  write.table(df, path, sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE, na = '"-"')
}
