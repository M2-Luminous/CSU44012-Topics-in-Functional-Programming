# ShapeExample1

This is a simple drawing system with a design inspired by the Region
and Pan languages that we looked at in class.

The module src/Shapes.hs contains a simple shape language which lets
one specify "Drawings". A Drawing is a collection of shapes and
transformations.  Only a few simple shapes are supported right
now. Each "Shape" is of unit size and is centered on the origin, but
each one can also have a "Transform" applied to it. Each transform can
either be a single simple operation - resize ("Scale") the shape by
some x and y factor, move it away from the origin ("Transform" by some
vector), and rotate it (around the origin) by some angle, or it can be
a compound of two transforms joined together with the "<+>" operator.

The module src/Render.hs takes a Drawing and paints it into a PNG
image. A "Window" value defined in this module sets up the
correspondence between the abstract "points" of the Drawing and the
concrete "Pixels" of the output drawing (so a 500x500 pixel image
might be drawing only a region of the drawing from (-1,-1) to (1,1) in
the coordinates used in the drawing).

The module uses the JuicyPixels library to do the actual file-format
management. As noted in the source code, the mapping from abstract
coordinates to pixel coordinates is done in a pretty simplistic and
inefficient way (basically I build a list of all possible coordinates
in the PNG image, with their mapping into the Drawing coordinate
space, and then do lookups). The image is black and white, there's no
way (at the moment) to specify different colours for different shapes.

Your job is to make this conversion more efficient so that we can
render images into larger PNG files.

The module src/Main.hs uses the other two modules to create a file
"output.png" which contains a black-and-white rendering of an
ellipse. By default this is a 50x50 pixel image.