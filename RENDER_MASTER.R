library(quarto) 
quarto:::quarto_render("index.qmd")
# render lại lần lượt các file để đảm bảo mọi thứ working good.

list.files(".",
           recursive = T,
           pattern = "\\.qmd",
           full.names = T) -> render_qmd

for(i in 1:length(render_qmd)){
  
  quarto:::quarto_render(render_qmd[i])
  
}


# list.files("posts/",
#            recursive = T,
#            pattern = "render.R",
#            full.names = T) -> render_lock
# 
# 
# for(j in 1:length(render_lock)){
#   
#   source(render_lock[j])
#   
# }
