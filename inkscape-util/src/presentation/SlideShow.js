/**
 * Cycles through each element in slides, showing one at a time and allowing to zoom 
 * and to pan it.
 * The first slide is initially shown, then right and left arrow keys can be used, 
 * respectively, to navigate to the next and previous slide.  
 * Up and down arrows zoom in and out, respectively; alternatively, the mouse wheel
 * can be used.
 * Panning happens whenever the mouse is dragged.
 * Hitting the space bar will reset the current slide to the initial zoom factor and
 * position; also, zoom factor and position are reset when navigating to a different
 * slide.
 * 
 * Usage
 * -----
 *     var doc          = d3.select("svg");
 *     var slides       = d3.selectAll("svg g.layer");
 *     var presentation = new SlideShow(doc, slides);
 *
 * Params
 * ------
 * + doc: the SVG root, selected using d3.
 * + slides: must be a collection of SVG elements within doc (typically top level "g" 
 *        elements), selected using d3.
 * 
 * Assumptions
 * -----------
 * + The d3 library (version 3) is loaded before running this script. 
 *
 * TODOs
 * -----
 * + Zooming with the mouse wheel scales about the current mouse location whereas when
 *   using the arrow keys no translation happens.
 * + Bind swipe right/left gestures to navigate forward/back.
 * + Bind pinch gestures to zoom.
 */
function SlideShow(doc, slides)
{
    var currentSlideIndex = 0;
    var currentScaleFactor = 1;
    var scaleIncrement = 0.1;
    var zoom = d3.behavior.zoom();
 
    var zoomHandler = function()
    {
        currentScaleFactor = d3.event.scale;
        slides.attr("transform", 
                    "translate(" + d3.event.translate + 
                    ")scale(" + d3.event.scale + ")");
    }

    var keyHandler = function()
    {
        switch (d3.event.keyCode)
        {
            case 32:  // space bar 
                resetZoom(); 
                break;  
            case 37:  // left arrow 
                previous(); 
                break;
            case 38:  // up arrow
                zoomIn();
                break;
            case 39:  // right arrow 
                next(); 
                break;
            case 40:  // down arrow
                zoomOut();
                break;
        }
    }
 
    var resetZoom = function()
    {
        currentScaleFactor = 1;
        zoom.scale(1);
        zoom.translate([0, 0]);
        zoom.event(doc);
    }

    var zoomIn = function()
    {
        currentScaleFactor = currentScaleFactor + scaleIncrement;
        zoom.scale(currentScaleFactor);
        zoom.event(doc);
    }
 
    var zoomOut = function()
    {
        currentScaleFactor = Math.max(0.1, currentScaleFactor - scaleIncrement);
        zoom.scale(currentScaleFactor);
        zoom.event(doc);
    }

    var showSlide = function(which)
    {
        //slides.style("visibility", "hidden");
        slides.style("display", "none");
        resetZoom();
    
        d3.select(slides[0][which]).style("visibility", "visible");
        d3.select(slides[0][which]).style("display", "inline");
        // NB: does nothing if which is out of bounds
    }
 
    var navigate = function(increment)
    {
        var n = slides[0].length;
        currentSlideIndex = rem(n, currentSlideIndex + increment)
        showSlide(currentSlideIndex);
    }
 
    var next = function()
    {
        navigate(+1);
    }
 
    var previous = function()
    {
        navigate(-1);
    }
 
    var rem = function(n, m)  // = r where m = q*n + r,  r in [0,n)
    {
        return ((m % n) + n) % n;  // NB: in JS: 5 % 3 = 2, but -5 % 3 = -2 !!!
    }

    doc.call(zoom.on("zoom", zoomHandler));
    doc.on("keydown", keyHandler);
    navigate(0);
}
