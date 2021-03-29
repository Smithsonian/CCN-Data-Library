## CCRCN Database Paper ####

## Analyze data latency from time of sampling to time of publishing
## contact: Jaxine Wolfe, wolfejax@si.edu


library(tidyverse)

# Prepare Workspace ####

cores <- read_csv("data/CCRCN_V2/cores.csv", guess_max = 7000)
citations <- read_csv("data/CCRCN_synthesis/CCRCN_study_citations.csv")

# Create a table of core sampling date and data publishing date

pub_dates <- citations %>% 
  filter(bibtype == "Misc") %>% 
  select(study_id, year) %>% 
  filter(study_id != "Windham_Meyers_et_al_2010" | year == "2010") %>%
  filter(study_id != "Piazza_et_al_2011" | year == "2011") %>%
  rename(pub_year = year)

core_dates <- cores %>% 
  select(study_id, site_id, core_id, year) %>%
  drop_na(year) %>%
  rename(sampled_year = year)
# over 2000 cores with no sampling year

# which cores have no associated date
na_core_date <- cores %>%
  filter(is.na(year))
sort(unique(na_core_date$study_id))

# determine the number of sampled cores per year
sampled_cores <- core_dates %>%
  add_count(sampled_year) %>% 
  select(sampled_year, n) %>% distinct() %>%
  arrange(sampled_year)

# plot the number of cores collected for each sampling year 
# (that were published in our database)
ggplot(sampled_cores, aes(sampled_year, n)) +
  geom_col(fill = "darkgreen") + theme_bw() +
  ggtitle("Number of cores collected per year")
ggsave("database_paper/figures/cores_sampled_per_year.jpg")

# join tables
dates <- left_join(core_dates, pub_dates) %>%
  drop_na(pub_year) # a few citations aren't aligning
  
# Compute cumulative counts ####
cumu_sampled <- dates %>% select(sampled_year) %>%
  rename(year = sampled_year) %>% 
  arrange(year) %>% add_count(year) %>% distinct() %>%
  mutate(cumulative_sampled = cumsum(n)) %>% select(-n)

cumu_pub <- dates %>% select(pub_year) %>%
  rename(year = pub_year) %>% 
  arrange(year) %>% add_count(year) %>%  distinct() %>%
  mutate(cumulative_published = cumsum(n)) %>% select(-n)

counts <- full_join(cumu_sampled, cumu_pub) %>%
  pivot_longer(cols = contains("cumulative"), 
               names_to = "category",
               names_prefix = "cumulative_", 
               values_to = "cumulative_count") %>%
  drop_na(cumulative_count)

# plot cumulative counts for sampled and published cores per year
ggplot(counts, aes(year, cumulative_count, col = category)) +
  geom_line() + 
  xlab("Year") + ylab("Cumulative Count") +
  theme_bw() +
  ggtitle("Cumulative number of cores sampled and published per year")
ggsave("database_paper/figures/cores_sampled_v_published.jpg")

## Data Latency ####

# compute latency of data
data_latency <- dates %>%  
  mutate(latency = pub_year - sampled_year) %>%
  select(sampled_year, pub_year, latency) %>% distinct() %>%
  # join core count per year to normalize latency by the number of cores collected per year
  left_join(sampled_cores) %>%
  mutate(norm_latency = latency/n) %>%
  arrange(sampled_year)

# plot relationship between year of sampling and the time it took to publish
ggplot(data_latency, aes(sampled_year, norm_latency)) +
  geom_point() + 
  # geom_smooth(method=lm, col = "black", lty = 2, alpha = 0.3) +
  theme_bw() +
  ggtitle("Relationship between sampling year and latency")
ggsave("database_paper/figures/data_latency.jpg")
