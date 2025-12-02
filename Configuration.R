library(usethis)
use_package("ggplot2")
library(usethis)

# 请把下面的内容换成你自己的名字和邮箱
use_git_config(user.name = "xiangancheng", user.email = "2874486249@qq.com")
gitcreds::gitcreds_set()



library(usethis) # 确保 usethis 已加载
use_git()

use_github()
library(usethis)
usethis::use_r("bio_plots")
