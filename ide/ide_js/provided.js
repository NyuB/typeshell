//Provides: effectiveDimensions const
function effectiveDimensions(el) {
    var cs = getComputedStyle(el);
    var width = parseInt(cs.getPropertyValue('width'), 10);
    var height = parseInt(cs.getPropertyValue('height'), 10);
    return { width, height };
}


//Provides: preventWhiteSpaceScrolling const
function preventWhiteSpaceScrolling() {
    // https://stackoverflow.com/questions/22559830/html-prevent-space-bar-from-scrolling-page
    window.addEventListener('keydown', function(e) {
        if(e.key === ' ' && e.target == document.body) {
            e.preventDefault();
        }
    });
}
