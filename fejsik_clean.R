
library(memoise)
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)

library(Rfacebook)

load("d:/kaggle/fb_oauth")
setwd("d:/kaggle/fejsik/")


return_likes = function(page_name, n_posts) {
  print(page_name)
  pg <- getPage(page_name, n = n_posts, token = fb_oauth)
  dfc = data_frame(post = NA,
                   userid = NA)
  pg = filter(pg, likes_count > 0)
  for(p in pg$id) {
    fb_post = getPost(post = p, fb_oauth, n = 2000, likes = TRUE, comments = FALSE)
    id_comments = unique(fb_post$likes$from_id)
    dfc = rbind(dfc,
                data_frame(post = rep(p, length(id_comments)), 
                           userid = id_comments) )
    #print(length(id_comments))
  }
  dfc[, "fb"] = page_name
  return(dfc)
}


fb_names = c("Onet", "WirtualnaPolska", "wPolityce", "gazetapl", "dziennikrzeczpospolita", "interia",
             "TygodnikDoRzeczy", "KrytykaPolityczna", "PolskaAgencjaPrasowa", "nczas",
             "tvn24pl", "tvp.info", "radiozet", "rmf24", "natematpl", "TygodnikPolityka",
             "NewsweekPolska", "tygodnikwprost", "gazetaprawnapl", "forsalpl", "Bankierpl",
             "goscniedzielny", "RadioMaryja", "frondaPL", "NiezaleznaPL")

m_return_likes = memoise(return_likes)
z = map(fb_names, function(df) m_return_likes(df, 200))

df = do.call("rbind", z)
df = filter(df, !is.na(userid))

group_by(df, fb) %>%
  summarise(users = n_distinct(userid)) %>%
  arrange(desc(users)) %>%
  ggplot(aes(x = reorder(fb, users),
             y = users / 1000)) +
  geom_bar(stat = "identity") +
  geom_text(aes(y = 0.5 + users/1000,
                label = round(users/1000,1)  ), 
            size = 3) +
  coord_flip() +
  labs(y = "Aktywni uzytkownicy (tys.)",
       x = "Portal") +
  theme_minimal()

#write.csv(df, "df.csv")


#compare two FB webpages and return % of users that liked at least 1 post on both pages
fb_compare = function(df, fb_1, fb_2) {
  N_users = filter(df, fb %in% c(fb_1, fb_2)) %>%
    select(userid) %>%
    unique() %>%
    nrow()
  N_both = filter(df, fb %in% c(fb_1, fb_2)) %>%
    group_by(userid) %>%
    summarise(fb_count = n_distinct(fb)) %>%
    arrange(desc(fb_count)) %>%
    filter(fb_count > 1) %>%
    nrow()
  return(N_both / N_users)
  
}



fb_d = matrix(data = NA,
              nrow = length(fb_names),
              ncol = length(fb_names) )

for(i in 1:length(fb_names)) {
  for(j in 1:length(fb_names)) {
    if (i < j) {
      fb_d[i, j] = fb_compare(df, fb_names[i], fb_names[j])
      fb_d[j, i] = fb_d[i, j]
    } else if (i == j) {
      fb_d[i, j] = 1
    } else { }
  }
}


row.names(fb_d) = fb_names
colnames(fb_d) = fb_names
fb_d



#MDS

f1 = cmdscale(as.dist(1-fb_d), k=2, eig =TRUE)
df_mds = data_frame(xx = f1$points[,1],
                    yy = f1$points[,2],
                    fb = fb_names)

library(ggplot2)

ggplot(data = df_mds,
       aes(x = xx,
           y = yy,
           label = fb)) +
  geom_text(size = 3) +
  theme_bw()




#profile vs inne strony

test_sites = c("pisorgpl", "Nowoczesna.oficjalnie", "PlatformaObywatelska",
               "sojusz", "KlubPoselskiKukiz15")



ts = map(test_sites, function(df) m_return_likes(df, 100))

dts = do.call("rbind", ts)
dts = filter(dts, !is.na(userid))
dts = rbind(dts, df)

dm = matrix(data = NA,
              nrow = length(fb_names),
              ncol = length(test_sites) )

for(i in 1:length(fb_names)) {
  for(j in 1:length(test_sites)) {
    dm[i, j] <- fb_compare(dts, fb_names[i], test_sites[j])
  }
}


row.names(dm) = fb_names
colnames(dm) = test_sites
dm = as.data.frame(scale(dm))
dm$fb = row.names(dm)


write.csv(dm, "dm.csv")

ggplot(data = mutate(dm,
                     distm = PlatformaObywatelska - pisorgpl),
       aes(x = reorder(fb, distm),
           y = distm)) +
  geom_point() +
  coord_flip()

melt(dm, id.vars = "fb") %>%
  filter(variable %in% c("pisorgpl", "KlubPoselskiKukiz15")) %>%
  ggplot(aes(x = fb,
           y = value,
           colour = variable)) +
  geom_line(aes(group = variable), size = 1) +
  coord_flip() +
  theme_minimal()


# Voronoi diagram

library(deldir)

dvor = deldir(df_mds$xx, df_mds$yy)
ggplot(data=df_mds, aes(x=xx,y=yy)) +
  geom_segment(
    aes(x = x1, y = y1, xend = x2, yend = y2),
    size = 2,
    data = dvor$dirsgs,
    linetype = 1,
    color= "#FFB958") + 
  #Plot the points
  geom_point(
    fill=rgb(70,130,180,255,maxColorValue=255),
    pch=21,
    size = 4,
    color="#333333")


#PCA

p1 = prcomp(fb_d)
p1
plot(p1)
p1$x[, 1:2]
df_p = data_frame(xx = p1$x[,1],
                    yy = p1$x[,2],
                    fb = fb_names)

ggplot(data = df_p,
       aes(x = xx,
           y = yy,
           label = fb)) +
  geom_text(size = 3) +
  theme_bw()


#heatmap
library(reshape2)

head(fb_d)
fb_d[upper.tri(fb_d)] <- NA
dfa = as.data.frame(fb_d)
head(dfa)
dfa$fb = row.names(dfa)

dfa_long = melt(dfa, id.vars = "fb")
dfa_long$variable = as.character(dfa_long$variable)

dfa_long$fb = factor(dfa_long$fb, levels = fb_names)
dfa_long$variable = factor(dfa_long$variable, levels = fb_names)


dfa_long %>%
  filter(value <1) %>%
  arrange(fb) %>%
  ggplot(aes(x = fb,
             y = variable,
             fill = value)) +
  geom_tile()
