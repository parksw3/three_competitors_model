triangle <- data.frame(x = c(0, 2, 0), y = c(0, 0, 2))
square <- data.frame(x = c(1, 3, 3, 1), y = c(1, 1, 3, 3))

g.parameter <- ggplot(NULL, aes(x, y)) +
    geom_polygon(data = triangle, fill = "red", alpha = 0.1, colour = "black") +
    geom_polygon(data = square, fill = "blue", alpha = 0.1) +
    scale_x_continuous(expand = c(0,0), name = expression(alpha)) +
    scale_y_continuous(expand = c(0,0), name = expression(beta)) +
    geom_segment(aes(x = 1, y = 1, xend = 1, yend = 3)) +
    geom_segment(aes(x = 1, y = 1, xend = 3, yend = 1)) +
    geom_text(aes(x = 0.6, y = 0.6), label = "(a)") +
    geom_text(aes(x = 0.6, y = 2), label = "(c)") +
    geom_text(aes(x = 2, y = 0.6), label = "(c)") +
    geom_text(aes(x = 2, y = 2), label = "(b)") +
    coord_fixed(ratio = 1) +
    theme(panel.grid = element_blank(),
        axis.title.y = element_text(angle=0),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank())