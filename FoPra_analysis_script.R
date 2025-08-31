library(openxlsx)
library(here)
library(tidyverse) 
library(ordinal)
library(performance)
library(emmeans)
library(sysfonts)
library(showtext)
source("make_blockorder_variable.R")


#### dataframe preprocessing ---- 
# read raw data 
raw_df <- read.xlsx(here("still_face_data", "Phone_face.xlsx"), sheet = 1) 

# exclude irrelevant columns 
df <- raw_df %>%
  select(c(1:10, 
         "18.1_smartphone")) %>% 
  as_tibble()

# get assessment notes 
df %>%
  filter(!is.na(notes)) %>%
  mutate(notes = gsub("[\r\n\t]+", " ", notes)) %>%
  transmute(line = sprintf("%s\t%s", id, notes)) %>%
  pull(line) %>%
  writeLines()

# check for ids with wrong condition names 
df %>% 
  filter(condition %in% c("phone_1", "still_1")) %>% 
  pull(id) 

# check for ids with missing data 
df %>% 
  filter(is.na(affect_categories)) %>%
  pull(id)

df <- df %>%
  mutate(across(c(mutual_regulation, condition, `18.1_smartphone`, id), as.factor), # factorize
         affect_categories = as.ordered(affect_categories)) %>%
  mutate(mutual_regulation = relevel(mutual_regulation, ref = "4")) %>%
  mutate(condition = str_trim(as.character(condition))) %>% 
  filter(is.na(condition) | !condition %in% c("phone_1", "still_1")) %>% # exclude unfinished blocks
  mutate(condition = factor(condition)) %>% # rename second blocks as original condition
  mutate(condition = fct_recode(condition,
                                phone = "phone_2",
                                still = "still_2")) %>% 
  mutate(smartphone_usage = `18.1_smartphone`) %>%
  filter(!(id %in% c(3,4,14)))  %>% 
  mutate(
    `1` = if_else(is.na(affect_categories), NA_integer_, as.integer(affect_categories == "1")),
    `2` = if_else(is.na(affect_categories), NA_integer_, as.integer(affect_categories == "2")),
    `3` = if_else(is.na(affect_categories), NA_integer_, as.integer(affect_categories == "3")),
    `4` = if_else(is.na(affect_categories), NA_integer_, as.integer(affect_categories == "4"))
  ) %>% 
  mutate(
    `book` = if_else(is.na(condition), NA_integer_, as.integer(condition == "book")),
    `phone` = if_else(is.na(condition), NA_integer_, as.integer(condition == "phone")),
    `still` = if_else(is.na(condition), NA_integer_, as.integer(condition == "still")),
  ) %>% 
  filter(!is.na(affect_categories)) %>% 
  make_blockorder_variable() %>% 
  mutate(block_order = as.factor(block_order))

summary(df)

#### Descriptive Plot ---- 
df_trials <- df %>%
  mutate(affect_num = as.numeric(as.character(affect_categories)))

# Averages for each participant separated after condition 
df_idavg <- df %>%
  mutate(affect_num = as.numeric(as.character(affect_categories))) %>%
  group_by(id, condition) %>%
  summarise(mean_affect = mean(affect_num, na.rm = TRUE), .groups = "drop")

# overall mean per condition 
df_condavg <- df_idavg %>%
  group_by(condition) %>%
  summarise(overall_mean = mean(mean_affect, na.rm = TRUE), .groups = "drop")

cond_lvls <- c("still","phone","book")
cond_labs <- c("Still Face","Phone Face","Book Face")
df_trials$condition  <- factor(df_trials$condition,  levels = cond_lvls)
df_idavg$condition   <- factor(df_idavg$condition,   levels = cond_lvls)
df_condavg$condition <- factor(df_condavg$condition, levels = cond_lvls)

font_dir <- file.path("fonts", "CMU-Serif")
font_add(family = "cmu",
         regular = file.path(font_dir, "cmunrm.ttf"),
         bold    = file.path(font_dir, "cmunbx.ttf"),
         italic  = file.path(font_dir, "cmunti.ttf"),
         bolditalic = file.path(font_dir, "cmunbi.ttf"))
showtext_auto()  # turn on

cond_cols <- c(still = "#66C2A5", phone = "#FC8D62", book = "#8DA0CB")

ggplot() +
  geom_jitter(data = df_trials,
              aes(x = condition, y = affect_num),
              color = "grey60", alpha = 0.4, width = 0.2, size = 1, height = 0) +
  geom_jitter(data = df_idavg,
             aes(x = condition, y = mean_affect, color = condition),
             size = 3, alpha = 0.8, width = 0.1) +
  geom_crossbar(data = df_condavg,
                aes(x = condition, y = overall_mean,
                    ymin = overall_mean, ymax = overall_mean),
                inherit.aes = FALSE, width = 0.4,
                color = "black", size = 0.5, alpha = 0.6) +
  theme_minimal(base_size = 28, base_family = "cmu") +
  scale_color_manual(values = cond_cols, breaks = cond_lvls, labels = cond_labs, drop = FALSE) +
  theme(
    legend.position = "none",
    panel.spacing = unit(2, "lines"),
    strip.text    = element_text(size = 28, face = "plain"),
    axis.text     = element_text(size = 26),
    axis.title    = element_text(size = 28),
    axis.title.x = element_text(margin = margin(t = 12)),  # move x title down
    axis.title.y = element_text(margin = margin(r = 12))   # move y title left
  ) +
  scale_y_continuous(limits = c(1, 4), breaks = seq(1, 4, 0.5)) +
  scale_x_discrete(limits = c("still", "phone", "book"),
                   labels = c("Still Face", "Phone Face", "Book Face")) +
  labs(y = "Affect Category", x = "Condition") + 
  guides(color = "none") 
  
# combine level 3 and level 4 of affect categories to one level for a more robust model 
df$affect <- df$affect_categories
df$affect[df$affect==4] <- 3

#### clmm model ----
mod <- clmm(
  affect ~ condition * mutual_regulation + 
    r1 + r2 + r3 + 
    block_order + 
    smartphone_usage +
    (1 + condition|id), 
  data = df, 
  link = "logit", 
  Hess = TRUE  
)
summary(mod)
r2(mod)
icc(mod)

emm_mod <- emmeans(mod, ~ condition*mutual_regulation)
summary(emm_mod)

# Turn emmeans result into a data frame
em_df <- as.data.frame(emm_mod)
em_df$condition      <- factor(em_df$condition,      levels = cond_lvls)

# Reorder and relabel condition
em_df <- em_df %>%
  mutate(
    condition = factor(condition, levels = c("still","phone","book")),
    condition = factor(condition, labels = c("Still Face","Phone Face","Book Face")), 
    mutual_regulation = factor(mutual_regulation, levels = c("1", "2", "3", "4")),
    mutual_regulation = factor(mutual_regulation, labels = c("MR1 - Engagement Behaviors", 
                                                             "MR2 - Disengagement Behaviors", 
                                                             "MR3 - Ambivalent/ Conflicted Behaviors", 
                                                             "MR4 - No Clear (Dis-)Engagement"))
  )

ggplot(em_df,
       aes(x = condition, y = emmean, color = condition)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.2,
                position = position_dodge(width = 0.5),
                size = 1.2) +   
  facet_wrap(~ mutual_regulation, ncol = 2) +  
  labs(x = "Condition",
       y = "Latent logit ") +
  scale_color_brewer(palette = "Set2") +
  theme(legend.position = "none", 
        panel.spacing = unit(2, "lines")) + 
  theme_minimal(base_size = 28, base_family = "cmu") +
  theme(
    legend.position = "none",
    panel.spacing = unit(2, "lines"),
    strip.text    = element_text(size = 28, face = "plain"),
    axis.text     = element_text(size = 26),
    axis.title    = element_text(size = 28)
  )

#### CLMM Model for Appendix ---- 
model <- clmm(
  affect_categories ~ condition * mutual_regulation + 
    r1 + r2 + r3 + 
    block_order + 
    smartphone_usage +
    (1 + condition|id), 
  data = df, 
  link = "logit", 
  Hess = TRUE  
)
summary(model)
r2(model)
icc(model)