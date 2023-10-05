const canvas = document.getElementById("renderer_canvas");
const canvas_container = document.getElementById("renderer");
const resize = () => {
    var cs = getComputedStyle(canvas_container);

    /// these will return dimensions in *pixel* regardless of what
    /// you originally specified for image:
    var width = parseInt(cs.getPropertyValue('width'), 10);
    var height = parseInt(cs.getPropertyValue('height'), 10);

    canvas.width = width;
    canvas.height = height;
}

const draw = (msg) => {
    const ctx = canvas.getContext("2d");
    ctx.beginPath();
    ctx.moveTo(canvas.width / 2, canvas.height / 2);
    ctx.lineTo(0, canvas.height / 4);
    ctx.lineTo(canvas.width, canvas.height / 4);
    ctx.fill();
    ctx.closePath();
    ctx.fillText(`${msg} ${ctx.font}`, 15,15)
}

if (canvas.getContext) {
    draw("Start");
    window.addEventListener("resize", () => {
        resize();
        draw("Hey");
    })
  }