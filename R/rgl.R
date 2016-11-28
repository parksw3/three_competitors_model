## http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
rgl_init <- function(new.device = FALSE, bg = "white", width = 1280) { 
    if( new.device | rgl.cur() == 0 ) {
        rgl.open()
        par3d(windowRect = 50 + c( 0, 0, width, width ) )
        rgl.bg(color = bg )
    }
    rgl.clear(type = c("shapes", "bboxdeco"))
    rgl.viewpoint(theta = 27.5, phi = 21, zoom = 0.8)
}

rgl_init()

rgl.df %>%
    filter(y == "y2") %>%
    with(plot3d(N1, N2, N3, type = "l", box = FALSE,
        xlim = c(0, 1.2),
        ylim = c(0, 1.2),
        zlim = c(0, 1.2),
        col = "red"
    ))

if(rgl.df$beta[1] == 0.5){
    rgl.df %>%
        filter(y == "y1") %>%
        with(plot3d(N1, N2, N3, type = "l", box = FALSE,
            xlim = c(0, 1.2),
            ylim = c(0, 1.2),
            zlim = c(0, 1.2),
            col = "blue"
        ))
}

segments3d(x = c(0, 0, 1, 0, 0, 1),
           y = c(1, 0, 0, 0, 1, 0),
           z= c(0, 1, 0, 1, 0, 0))
