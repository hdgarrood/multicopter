drawWorld = (slices, canvas) ->
    context = canvas.getContext('2d')
    width = canvas.width
    sliceWidth = width / slices.length

    [0..slices.length].forEach (i) ->
        drawSlice(slices[i],
                  context,
                  sliceWidth,
                  { x: sliceWidth * i, y: 0 })

drawSlice = (slice, context, width, position) ->
    isInsideObstacle = true
    xStart = position.x
    xEnd = xStart + width - 1
    currentY = position.y

    slice.forEach (boundary) ->
        context.fillStyle = if isInsideObstacle then "green" else "white"
        context.fillRect(xStart, currentY, xEnd, boundary)

        isInsideObstacle = !isInsideObstacle
        currentY = boundary

    context.fillStyle = if isInsideObstacle then "green" else "white"
    context.fillRect(xStart, currentY, xEnd, context.canvas.height)

# Create an Array containing 'n' copies of 'value'.
replicate = (n, value) ->
    Array.apply(null, {length: n}).map(-> value)

start = ->
    interval = 120
    worldWidth = 30
    initialWorld = replicate(worldWidth, [0])
    requestStream = Bacon.interval(interval, { url: '/iterate' })
    world = requestStream.ajax().map('.newSlice')
            .scan(initialWorld, (slices, newSlice) ->
                slices.shift()
                slices.push(newSlice)
                slices
            )
    canvas = document.getElementById('canvas')

    world.onValue((world) ->
        drawWorld(world, canvas)
    )

window.start = start
