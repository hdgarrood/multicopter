function drawWorld(world, canvas) {
    var context = canvas.getContext('2d'),
        width = canvas.width,
        slices = world.slices,
        sliceWidth = width / slices.length

    for (var i=0; i < slices.length; i++) {
        drawSlice(slices[i],
                  context,
                  sliceWidth,
                  { x: sliceWidth * i, y: 0 })
    }
}

function drawSlice(slice, context, width, position) {
    var isInsideObstacle = true,
        xStart = position.x,
        xEnd = xStart + width,
        currentY = position.y

    slice.forEach(function(boundary) {
        context.fillStyle = isInsideObstacle ? "green" : "white"
        context.fillRect(xStart, currentY, xEnd, boundary)

        isInsideObstacle = !isInsideObstacle
        currentY = boundary
    })

    context.fillStyle = isInsideObstacle ? "green" : "white"
    context.fillRect(xStart, currentY, xEnd, context.canvas.height)
}

function start() {
    setInterval(iterate, 120)
}

function iterate() {
    var canvas = document.getElementById('canvas')

    $.getJSON("/iterate").done(function(world) {
        drawWorld(world, canvas)
    })
}
