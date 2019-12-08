data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Height, fill = DM_Gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Height (cm)", fill = "Gender") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Weight)) +
  geom_boxplot() +
  facet_grid( ~ DM_Education)


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Weight, fill = DM_Gender)) +
  geom_boxplot() +
  labs(x = "Gender", y = "Weight (kg)", fill = "Gender") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender, DM_Education) %>%
  ggplot(aes(DM_Gender, DM_Age)) +
  geom_boxplot()


data %>%
  select(DM_Age, DM_Height, DM_Weight, DM_Gender) %>%
  ggpairs

# Age NA aldatu medianagaittik



data %>%
  select_if(is.ordered) %>%
  gather %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap( ~ key, scales = 'free_x')


# Hirutan banatu biharrekuak:

data %>%
  select_if(is.ordered) %>%
  gather %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_wrap( ~ key, scales = 'free_x')


# 1. Aging phobiak ze zerikusi dauka with healthy habits. Emakume eta gizonen arteko alderatzia.
# 2. Pisua eta pelikulen preferentzian lotura.
# 3. Zertan gastatzen daben aurreikusi interes eta musika/film gustuen arabera. Sailkatzaile bat sortu (BN)
