
library(lubridate)
library(ggplot2)
library(dplyr)

library(Rfacebook)

fb_oauth <- fbOAuth(app_id="826783820748299", app_secret="51d3c6151a6af3e5d00c37d2b61263b4",extended_permissions = TRUE)


#save(fb_oauth, file="d:/kaggle/fb_oauth")

#load("d:/kaggle/fb_oauth")


getFriends(token=fb_oauth, simplify=TRUE)
getNetwork(fb_oauth, format = "edgelist", verbose = TRUE)
// 
me <- getUsers("me",token=fb_oauth)

me
my_likes <- getLikes(user="me", token=fb_oauth)
my_likes[1]
str(my_likes)


pg <- getPage("interia", n = 100, token = fb_oauth)



pg$date = as.POSIXct(pg$created_time, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")


ggplot(data = pg,
       aes(x = date,
           y = likes_count)) +
  geom_point(aes(colour = type))

pg = mutate(pg, hr = hour(date))

ggplot(data = filter(pg, type == "link"),
       aes(x = hr,
           y = likes_count)) +
  geom_jitter()


pg %>%
  arrange(desc(likes_count)) %>%
  head(5)


pg_post = getPost(post=pg$id[5], fb_oauth, n=1000, likes=TRUE, comments=TRUE)
pg_post
pg_post
pg_post$comments$from_id

pg_post_users = getUsers(pg_post$likes$from_id, token = fb_oauth, private_info = FALSE)
pg_post_users[10]

?getUsers

pg[1]

dfc = data_frame(post = NA,
                 userid = NA)
for(p in pg$id) {
  fb_post = getPost(post = p, fb_oauth, likes = TRUE, comments = FALSE)
  id_comments = unique(fb_post$likes$from_id)
  dfc = rbind(dfc,
              data_frame(post = rep(p, length(id_comments)), 
                         userid = id_comments) )
  print(length(id_comments))
}


group_by(dfc, userid) %>%
  summarise(likes_count = n()) %>%
  arrange(desc(likes_count))


#get a df with IDs of users liking N last posts on a given page
return_likes = function(page_name, n_posts) {
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
    print(length(id_comments))
  }
  dfc[, "fb"] = page_name
  return(dfc)
}

?getPost
fb_names = c("Onet", "WirtualnaPolska", "wPolityce", "gazetapl", "dziennikrzeczpospolita", "wwwsepl",
             "TygodnikDoRzeczy", "Plotek",
             "healthyplanbyann", "pudelek", "pomponik" , "kwestiasmaku", "maczoofficial", "discoipolo",
             "goscniedzielny", "RadioMaryja", "frondaPL", "NiezaleznaPL")
df1 = return_likes("Onet", 100)
df2 = return_likes("WirtualnaPolska", 100)
df3 = return_likes("wPolityce", 100)
df4 = return_likes("gazetapl", 100)
df5 = return_likes("dziennikrzeczpospolita", 100)
df6 = return_likes("wwwsepl", 100)
df7 = return_likes("TygodnikDoRzeczy", 100)
df8 = return_likes("Plotek", 100)

df9 = return_likes("healthyplanbyann", 100)
df10 = return_likes("pudelek", 100)
df11 = return_likes("pomponik", 100)
df12 = return_likes("kwestiasmaku", 100)
df13 = return_likes("maczoofficial", 100)
df14 = return_likes("discoipolo", 100)
df15 = return_likes("goscniedzielny", 100)
df16 = return_likes("RadioMaryja", 100)
df17 = return_likes("frondaPL", 100)
df18 = return_likes("NiezaleznaPL", 100)

df = rbind(df1, df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13, df14, df15,
           df16, df17, df18) %>%
  filter(!is.na(userid))


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
    if (i != j) {
      fb_d[i, j] = fb_compare(df, fb_names[i], fb_names[j])
    } else {
      fb_d[i, j] = 1
    }
  }
}
row.names(fb_d) = fb_names
colnames(fb_d) = fb_names
fb_d


f1 = cmdscale(as.dist(1-fb_d), k=2, eig =TRUE)
x <- f1$points[,1]
y <- f1$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric	MDS",	type="n")
text(x, y, labels = row.names(fb_d), cex=.7)



group_by(df, userid) %>%
  summarise(fb_count = n_distinct(fb)) %>%
  arrange(desc(fb_count))


group_by(df, fb) %>%
  summarise(users = n_distinct(userid))


ggplot(data = fb_d,
       aes(x =)




group_by(df, fb, userid) %>%
  summarise(likes_count = n()) %>%
  arrange(desc(likes_count))

filter(df, fb %in% c("Onet", "WirtualnaPolska")) %>%
  select(userid) %>%
  unique() %>%
  nrow()




fb_compare(df, "WirtualnaPolska", "Onet")  

group_by(df, userid) %>%
  summarise(fb_count = n_distinct(fb)) %>%
  arrange(desc(fb_count)) %>%
  filter(fb_count > 1) %>%
  nrow()

inner_join(filter(df, fb == "Onet"),
           filter(df, fb == "WirtualnaPolska"),
           by = "userid") %>%
  
  unique()

#####txt


library(tidytext)


pgu = select(pg, id, message, likes_count) %>%
  unnest_tokens(word, message)

pgu_words = pgu %>%
  count(word, sort = TRUE) 
