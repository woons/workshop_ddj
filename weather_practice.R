#-----------------------------------------------------
# 최고, 평균, 최저 한눈에 차트
#-----------------------------------------------------
df_final2$category <- factor(df_final2$category, levels=c('max_temp','avg_temp','min_temp', 'humidity', 'angry_index'))

ggplot(df_final2 %>% filter(category == "avg_temp" | category == "max_temp" | category == "min_temp"), 
       aes(x=dates, y=as.numeric(value), color = category)) +
  geom_jitter(alpha = .4) + geom_smooth(method = "lm", color = "white") + theme_woons() +
  facet_grid(.~category) + geom_hline(yintercept = 33, color = "grey")

ggplot(df_final2 %>% filter(category == "avg_temp" | category == "max_temp" | category == "min_temp"), 
       aes(x=dates, y=as.numeric(value), color = category)) +
  geom_jitter(alpha = .4) + geom_smooth(method = "lm", color = "white") + theme_woons() +
  geom_hline(yintercept = 33, color = "grey")


ggplot(df_final2 %>% filter(category == "humidity"), aes(x=dates, y=value, color=category2)) +
  geom_point() + theme_woons() + geom_smooth(method = "lm")

#-----------------------------------------------------
# 폭염 일수 시각화
#-----------------------------------------------------
vis_heat <- df_final2 %>% 
  filter(category == "max_temp") %>% 
  group_by(year, month, heat) %>% 
  count() %>% filter(heat == "폭염" & month == 7) %>% 
  mutate(category = if_else(n >= 15, "15일이상", "15일미만")) %>% 
  ungroup()

ggplot(vis_heat, aes(x=year, y=n, fill=category)) + 
  geom_bar(stat = "identity", width = .9) +
  theme_woons() +
  ggtitle(label = "서울 역대 7월 폭염일수", 
          subtitle = "최고기온 33도 이상 기록된 날 빈도수") +
  ylim(0, 20)

#-----------------------------------------------------
# 1994년 기준 폭염 일수 비교
#-----------------------------------------------------
vis_heat2 <- df_final2 %>% 
  filter(category == "max_temp" & month == 7) %>% 
  group_by(category2, heat) %>% 
  count() %>%
  ungroup() %>% 
  drop_na(heat) %>% 
  group_by(category2) %>% 
  mutate(avg = round(n / sum(n), 2)) %>% ungroup()

#-----------------------------------------------------
# 연도별 최고기온, 최저기온, 평균기온, 상대습도 평균값
#-----------------------------------------------------
unique(df_final2$category)

# 최고기온 평균
df_final2 %>% 
  filter(category == "max_temp" & month == 7) %>% 
  group_by(year) %>% summarise(avg = mean(value, na.rm = T)) %>% 
  arrange(desc(avg)) %>% View()

# 평균기온 평균
df_final2 %>% 
  filter(category == "avg_temp" & month == 7) %>% 
  group_by(year) %>% summarise(avg = mean(value, na.rm = T)) %>% 
  arrange(desc(avg))

# 최저기온 평균
df_final2 %>% 
  filter(category == "min_temp") %>% 
  group_by(year) %>% summarise(avg = mean(value, na.rm = T)) %>%
  arrange(desc(avg)) %>% ggplot(aes(x=year, y=avg, group=1)) + geom_line()

# 상대습도 평균
df_final2 %>% 
  filter(category == "humidity" & month == 7) %>% 
  group_by(year) %>% summarise(avg = mean(value, na.rm = T)) %>% 
  arrange(desc(avg)) %>% 
  ggplot(aes(x=as.character(year), y=avg, group=1)) + 
  geom_line() +
  theme_woons() +
  ggtitle("상대습도 변화 그래프") +
  geom_smooth(method = "lm") + ylim(50, 100)


# https://goo.gl/x7ye2z
# 1994년 7월 평균기온 28.5도 , 강수량 139.5mm , 상대습도 78.9%, 최고평균 32.64도
# 2018년 7월 평균기온 27.8도 , 강수량 185.6mm , 상대습도 67.5%  최고평균 32.13도

185.6 - 139.5

#-----------------------------------------------------
# 불쾌지수 비교
#-----------------------------------------------------
df_final2 %>% 
  filter(category == "angry_index") %>% 
  group_by(year) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% View()

#-----------------------------------------------------
# 1994년 기점 더위 추이
#-----------------------------------------------------
head(df_final2)

ggplot(df_final2 %>% filter(category == "avg_temp"), aes(x=dates, y=value, color=category2, group=category2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_woons() +
  ylim(10, 35) +
  ggtitle("94년 기점으로 평균온도 변화", "94년을 기점으로 추이가 달라짐")

ggplot(df_final2 %>% filter(category == "max_temp"), aes(x=dates, y=value, color=category2, group=category2)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_woons() +
  ggtitle("94년 기점으로 최고온도 변화", "94년을 기점으로 추이가 달라짐")

#-----------------------------------------------------
# 최고기온 평균 추이
#-----------------------------------------------------
max_stat <- df_final2 %>% 
  filter(category == "max_temp") %>% 
  group_by(year) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=avg, group=1)) + geom_line() + theme_woons()

# 연도별 평균온도는 2018년 28.15도 / 1994년 28.08도
avg_stat <- df_final2 %>% 
  filter(category == "avg_temp") %>% 
  group_by(year) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=avg, group=1)) + geom_line() + theme_woons()

df_final2 %>% 
  filter(category == "max_temp" | category == "min_temp") %>% 
  group_by(year, category) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(aes(x=year, y=avg, group=category, color=category)) + 
  geom_line() + 
  theme_woons() +
  ylim(18, 33) +
  geom_smooth(method = "lm", se = FALSE) + 
  ggtitle("점점 뜨거워지는 서울", "1960년 이후 최고 및 최저온도 평균 추이")

#-----------------------------------------------------
# 열대야 발생 비율
#-----------------------------------------------------
df_final2 %>% 
  filter(category == "min_temp") %>% 
  group_by(category2) %>% 
  summarise(avg = mean(value, na.rm = T))

df_final2 %>% 
  filter(category == "min_temp") %>% 
  group_by(year) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% View()

ggplot(df_final2 %>% filter(category == "min_temp" & value >= 25), aes(x=dates, y=value)) +
  geom_point() +
  theme_woons() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("최저온도 25도 이상 기록한 열대야", "25도 기준으로 찍어보자")

#-----------------------------------------------------
# 상대습도
#-----------------------------------------------------

df_humidity <- df_final2 %>% filter(category == "humidity")
ggplot(df_humidity, aes(x=dates, y=value)) + geom_point()

stat_humidity <- df_humidity %>% 
  filter(month == 7) %>% 
  group_by(year) %>% 
  summarise(avg = mean(value, na.rm = T)) %>% ungroup() %>% View()


ggplot(stat_humidity, aes(x=year, y=avg, group=1)) + 
  geom_line() + 
  theme_woons()
