Presentation
============

This program makes an SVG presentation out of a layered SVG image.
The input (given on `stdin`) is processed as follows:

 * all the "lib" layers are removed;
 * a slide show is created with the "regular" layers.

The output, the SVG presentation, is put on `stdout`.

We take a layer to be any "g" element below the root which is not labeled as "master"
--i.e has an attribute whose local name is "label" and whose value equals "master",
ignoring case and leading/trailing white-space.
A "lib" layer is any "g" element below the root which is not labeled with a name
starting with "lib-", ignoring case and leading white-space.

When opened in a web browser, the presentation scales to fit the view port and the
first slide is shown---i.e. the first "regular" layer, in document order.  Use right
and left arrow keys, to navigate, respectively, to the next and previous slide.  
Up and down arrows zoom in and out, respectively; alternatively, the mouse wheel can
be used.  Panning happens whenever the mouse is dragged.
Hitting the space bar will reset the current slide to the initial zoom factor and
position; also, zoom factor and position are reset when navigating to a different
slide.


Usage
-----

        cat original-file.svg | presentation > new-file.svg


Assumptions
-----------
The input SVG must have `height` and `width` attributes (on the root `svg` element).

