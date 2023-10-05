//Provides: effectiveDimensions const
function effectiveDimensions(el) {
    var cs = getComputedStyle(el);
    var width = parseInt(cs.getPropertyValue('width'), 10);
    var height = parseInt(cs.getPropertyValue('height'), 10);
    return { width, height };
}
