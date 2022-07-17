#Melhor visualização dos interceptos e das inclinações aleatórios por cidade,
#para o modelo final HLM
v_final <- 
  data.frame(hlm_final[["coefficients"]][["random"]][["cidade"]]) %>%
  rename(v00 = 1,
         v10 = 2)

v_final$cidade <- c(1:17)

v_final$cidade <- as.factor(v_final$cidade)

v_final %>% 
  select(cidade, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 25)

#Para observarmos graficamente o comportamento dos valores de v0j, ou seja,
#dos interceptos aleatórios por cidade, podemos comandar
random.effects(hlm_final) %>% 
  rename(v0j = 1) %>% 
  rownames_to_column("cidade") %>% 
  mutate(color_v0j = ifelse(v0j < 0, "A", "B"),
         hjust_v0j = ifelse(v0j > 0, 1.15, -0.15)) %>% 
  arrange(cidade) %>% 
  ggplot(aes(label = format(v0j, digits = 2), 
             hjust = hjust_v0j)) +
  geom_bar(aes(x = fct_rev(cidade), y = v0j, fill = color_v0j),
           stat = "identity", color = "black") +
  geom_text(aes(x = cidade, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "cidade",
       y = expression(nu[0][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")


#Para observarmos graficamente o comportamento dos valores de v1j, ou seja
#das inclinações aleatórias por cidade
random.effects(hlm_final) %>% 
  rename(v1j = 2) %>% 
  rownames_to_column("cidade") %>% 
  mutate(color_v1j = ifelse(v1j < 0, "A", "B"),
         hjust_v1j = ifelse(v1j > 0, 1.15, -0.15)) %>% 
  arrange(cidade) %>% 
  ggplot(aes(label = format(v1j, digits = 2), 
             hjust = hjust_v1j)) +
  geom_bar(aes(x = fct_rev(cidade), y = v1j, fill = color_v1j),
           stat = "identity", color = "black") +
  geom_text(aes(x = cidade, y = 0), size = 4.1, color = "black") +
  coord_flip() +
  labs(x = "cidade",
       y = expression(nu[1][j])) +
  scale_fill_manual("oi", values = c("firebrick1","green1")) +
  theme(panel.background = element_rect("white"),
        panel.border = element_rect(NA),
        panel.grid = element_line("grey95"),
        legend.position = "none")

