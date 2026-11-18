# ==============================================================================
# Rescorla-Wagner Model Simulation of US Signal Renewal (Goddard, 1999)
# ==============================================================================
# This R script simulates trial-by-trial associative strength (V) across 4 groups
# (AAA, ABA, AAB, ABB) and creates a live animation using gganimate and ggplot2.
# ==============================================================================

# --- Step 1: Install & Load Required Packages ---
# Uncomment the line below if you have not installed these packages yet:
# install.packages(c("tidyverse", "gganimate", "gifski"))

library(tidyverse)
library(gganimate)
library(gifski)

# --- Step 2: Define Rescorla-Wagner Compound Renewal Model Function ---
simulate_renewal <- function(n_acq = 15, n_ext = 10, n_test = 5,
                             alpha_S = 0.25, alpha_C = 0.15, beta = 0.5) {
  groups <- c("AAA", "ABA", "AAB", "ABB")
  results <- list()
  
  for (g in groups) {
    v_S <- 0.0  # Signal strength
    v_A <- 0.0  # Context A strength
    v_B <- 0.0  # Context B strength
    records <- data.frame()
    
    # Phase 1: Acquisition (Context A for all groups)
    for (t in 1:n_acq) {
      v_total <- v_S + v_A
      records <- rbind(records, data.frame(
        trial = t, 
        Phase = "Acquisition", 
        V = v_total, 
        Group = g
      ))
      err <- 1.0 - v_total
      v_S <- v_S + alpha_S * beta * err
      v_A <- v_A + alpha_C * beta * err
    }
    
    # Phase 2: Extinction (Context A for AAA/AAB, Context B for ABA/ABB)
    ext_ctx <- if (g %in% c("AAA", "AAB")) "A" else "B"
    for (t in (n_acq + 1):(n_acq + n_ext)) {
      v_total <- if (ext_ctx == "A") v_S + v_A else v_S + v_B
      records <- rbind(records, data.frame(
        trial = t, 
        Phase = "Extinction", 
        V = v_total, 
        Group = g
      ))
      err <- 0.0 - v_total
      v_S <- v_S + alpha_S * beta * err
      if (ext_ctx == "A") {
        v_A <- v_A + alpha_C * beta * err
      } else {
        v_B <- v_B + alpha_C * beta * err
      }
    }
    
    # Phase 3: Testing (Context A for AAA/ABA, Context B for AAB/ABB)
    test_ctx <- if (g %in% c("AAA", "ABA")) "A" else "B"
    for (t in (n_acq + n_ext + 1):(n_acq + n_ext + n_test)) {
      v_total <- if (test_ctx == "A") v_S + v_A else v_S + v_B
      records <- rbind(records, data.frame(
        trial = t, 
        Phase = "Test", 
        V = v_total, 
        Group = g
      ))
    }
    
    results[[g]] <- records
  }
  
  bind_rows(results)
}

# --- Step 3: Run Simulation ---
df <- simulate_renewal()

# --- Step 4: Build Animated Graph using ggplot2 & gganimate ---
p <- ggplot(df, aes(x = trial, y = V, color = Group, linetype = Group, group = Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = c(15.5, 25.5), linetype = "dashed", color = "gray50") +
  annotate("text", x = 8, y = 1.02, label = "Acquisition (Ctx A)", fontface = "bold", color = "gray30") +
  annotate("text", x = 20.5, y = 1.02, label = "Extinction (Ctx A/B)", fontface = "bold", color = "gray30") +
  annotate("text", x = 28, y = 1.02, label = "Test (A/B)", fontface = "bold", color = "gray30") +
  labs(
    title = "Rescorla-Wagner Model: US Signal Renewal",
    subtitle = "Trial: {frame_along} of 30",
    x = "Trial Number",
    y = "Associative Strength (V)",
    color = "Group",
    linetype = "Group"
  ) +
  scale_color_manual(values = c("AAA" = "#0072B2", "ABA" = "#D55E00", "AAB" = "#009E73", "ABB" = "#CC79A7")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  ) +
  transition_reveal(trial)

# --- Step 5: Render Animation in RStudio ---
anim <- animate(
  p, 
  nframes = 90, 
  fps = 10, 
  width = 800, 
  height = 500, 
  res = 100, 
  renderer = gifski_renderer()
)

# Display live animation in RStudio Viewer Pane
print(anim)

# --- Step 6: Optionally Save Animation GIF to File ---
# anim_save("rescorla_wagner_renewal.gif", animation = anim)
