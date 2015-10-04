Inject View Box
===============

Given an SVG image on `stdin`, this program will produce a new SVG on `stdout` that:

 * has the *same* graphics as the original one;
 * when opened in a web browser, *scales to fit* the view port;
 * when pressing the space bar, restores the original width and height of the image. 

Hit the space bar further to toggle between 'scale to fit' and original dimensions.
This is useful because in the 'scale to fit' mode the image can't be zoomed in or
out; in this mode the `viewBox` attribute is set, so the size of the image depends
on the view port and to scale you have to resize the window instead.


Usage
-----

        cat original-file.svg | inject-view-box > new-file.svg


Assumptions
-----------
The input SVG must have `height` and `width` attributes (on the root `svg` element).


Notes
-----
1. To manually scale (respecting img aspect ratio) the svg to fit the window, open 
   svg file
     * remove height (h) and width (w) from `svg` root
     * add `viewBox` to `svg` root: `viewBox="0 0 w h"`, e.g. `viewBox="0 0 1600 1200"`

   See: 
     * [web inkscape](http://tavmjong.free.fr/INKSCAPE/MANUAL/html/Web-Inkscape.html)
     * [svg tutorial](http://tutorials.jenkov.com/svg/svg-viewport-view-box.html)
     * [svg reference](http://www.w3.org/TR/SVG/coords.html#ViewBoxAttribute)

2. To use "Save As..." -> "Optimized SVG" in Inkscape you need to install

          sudo pacman -S python2-lxml

   Not `python-lxml` b/c Inkscape still uses python2 on Arch. 
   (See: [forum](https://bbs.archlinux.org/viewtopic.php?id=139068))
   However, you can still save as "Plain SVG", which removes the Inkscape extensions.
