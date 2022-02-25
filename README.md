# ggarchery: Flexible segment geoms with arrows for ggplot2


ggarchery is intended to extend [ggplot2](https://github.com/tidyverse/ggplot2)'s handling of segments with arrowheads. At present it contains one geom and one position adjustment.

## `geom_arrowsegment()` allows placement of one or more arrowheads at any point on a segment

First, let's generate some data that would be understood by [ggplot2](https://github.com/tidyverse/ggplot2)'s normal `geom_segment()`:

```
library(tidyverse)
library(ggarchery)

tbl <- tibble(x = c(0.1, 0.2), xend = c(0.1, 0.8), y = c(0.1, 0.5), yend = c(0.7, 0.9))
```

The default behaviour of `geom_arrowsegment()` mimics that of `geom_arrowsegment(arrow = arrow())`

```
ggplot(tbl) + 
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow()) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_segment_example.png" width="400"/>

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend)) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example1.png" width="400"/>

The `arrows` parameter of `geom_arrowsegment()` also behaves exactly like the `arrow` parameter of `geom_segment`, as a call to `grid::arrow()`:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrows = arrow(type = 'closed')) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example2.png" width="400"/>

Now for the interesting bit. Suppose that we would like the arrowhead to appear at the midpoint of the segment, rather than the end. This can be done by specifying `arrow_positions = 0.5`.

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = 0.5) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example3.png" width="400"/>

Control of the arrow segment works as before:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = 0.5, arrows = arrow(type = 'closed')) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example4.png" width="400"/>

Other aesthetics also work as you would hope:

```
tbl <- tbl %>% mutate(col = c("A", "B"))

ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend, col = col), arrow_positions = 0.5)  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example5.png" width="400"/>

You can also define multiple arrowheads by making `arrow_positions` a vector of length greater than 1. All values are expected to fall between 0 and 1, and not be exactly 0:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = c(0.25, 0.75))  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example6.png" width="400"/>

If one value is 1, then the arrowhead appears at the end:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = c(0.25, 1))  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example7.png" width="400"/>

The look of each arrow can also be controlled separately by making `arrows` a list:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = c(0.25, 1), arrows = list(arrow(angle = 10), arrow(type = 'closed')))  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example8.png" width="400"/>

The `arrow_fills` option also mimics `arrow.fill` of `geom_segment()` but can be a vector.

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), 
                    arrow_positions = c(0.25, 1), 
                    arrow_fills = c("indianred3", "dodgerblue3"), 
                    arrows = arrow(type = "closed"))  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example9.png" width="400"/>

Finally, the geom can be used as an annotation:

```
ggplot(mtcars) + 
  geom_point(aes(x = disp, y=hp)) + 
  annotate(geom = "arrowsegment", 
           x = 170, 
           y=200, 
           xend = 145, 
           yend = 175, 
           arrow_positions = 0.6, 
           arrows = arrow(type = "closed", length = unit(0.1, "inches")))
```
<img src="man/figures/annotate_example.png" width="800"/>


## `position_attractsegment()` allows you to automatically shave the ends of arrow segments

`position_attractsegment()` is intended to solve the following problem. Suppose you have nicely laid out a set of labelled points:

```
pt.tbl <- tibble(x = c(0.25, 0.5, 0.75), y = c(0.25, 0.5, 0.75), labels = c("A", "B", "C"))

ggplot(pt.tbl) + 
  geom_point(aes(x,y, fill = labels), size =6, shape = 21) +
  geom_text(aes(x,y, label = labels)) +
  xlim(c(0, 1)) +
  ylim(c(0, 1)) +
  scale_fill_discrete(guide = "none")
```

<img src="man/figures/position_attractsegment_example1.png", width="400"/>

