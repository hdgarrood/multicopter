function drawWorld(slices, canvas) {
    var context = canvas.getContext('2d'),
        width = canvas.width,
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

// Create an Array containing 'n' copies of 'value'.
function replicate(n, value) {
    return Array.apply(null, {length: n}).map(function() { return value })
}

function start() {
    var interval = 120,
        worldWidth = 30,
        initialWorld = replicate(worldWidth, [0])
        requestStream = Bacon.interval(interval, { url: '/iterate' }),
        world = requestStream.ajax().map('.newSlice')
            .scan(initialWorld, function(slices, newSlice) {
                slices.shift()
                slices.push(newSlice)
                return slices
            }),
        canvas = document.getElementById('canvas')

    world.onValue(function(world) {
        drawWorld(world, canvas)
    })
}
