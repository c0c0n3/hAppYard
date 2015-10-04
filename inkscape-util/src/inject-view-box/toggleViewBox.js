
var root = document.documentElement;      
var width = root.getAttribute("width");
var height = root.getAttribute("height");

function toggle()
{
    if (root.getAttribute("viewBox"))
    {
        root.removeAttribute("viewBox");
        root.setAttribute("width", width);
        root.setAttribute("height", height);
    }
    else
    {
        root.setAttribute("width", "100%");
        root.setAttribute("height", "100%");
        root.setAttribute("viewBox", "0 0 " + width + " " + height);
    }
}

function keyHanlder(evt)
{
    if (evt.keyCode == 32) toggle();
}

root.addEventListener('keydown', keyHanlder, false);
toggle();
